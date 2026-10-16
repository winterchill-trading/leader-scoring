open! Core
open Leader_scoring

(* Adversarial fixtures: a wash trader, a self-trader, a pair trading with each other, a
   sybil cluster of five wallets trading together, a front-runner, an account filled off
   the market, and honest traders that must not be flagged: twenty independent noise
   traders and two that also react to the same ten announcements. *)

let coins = [| "BTC"; "ETH"; "SOL" |]
let days = 30.

(* a smooth deterministic price per coin, so honest fills sit on one market *)
let price coin t =
  let d = Time_ns.Span.to_day (Time_ns.diff t Helpers.t0) in
  let base =
    match coin with
    | "BTC" -> 60_000.
    | "ETH" -> 3_000.
    | _ -> 150.
  in
  base *. (1. +. (0.05 *. Float.sin (d /. 3.)))
;;

let next_id = ref 0

let fill ?id ?px ~time ~coin sz =
  incr next_id;
  let id = Option.value id ~default:!next_id in
  Helpers.fill
    ~time
    ~coin
    ~px:(Option.value px ~default:(price coin time))
    sz
  |> fun f -> { f with Fill.id }
;;

let history account fills =
  { (Helpers.history fills) with account }
;;

let random_time rs = Helpers.at (Random.State.float rs days)

let noise rs i =
  let fills =
    List.init 150 ~f:(fun _ ->
      let coin = coins.(Random.State.int rs 3) in
      let side = if Random.State.bool rs then 1. else -1. in
      fill ~time:(random_time rs) ~coin (side *. (0.1 +. Random.State.float rs 1.)))
  in
  history (sprintf "noise-%02d" i) fills
;;

(* two honest traders who also both react to the same ten announcements *)
let news rs =
  let events = List.init 10 ~f:(fun _ -> random_time rs) in
  List.map [ "news-a"; "news-b" ] ~f:(fun account ->
    let h = noise rs 0 in
    { h with
      account
    ; fills =
        h.fills
        @ List.map events ~f:(fun t ->
          fill ~time:(Time_ns.add t (Time_ns.Span.of_sec (Random.State.float rs 30.))) ~coin:"BTC" 1.)
    })
;;

let wash rs =
  let pairs =
    List.concat (List.init 30 ~f:(fun _ ->
      let t = random_time rs in
      let coin = coins.(Random.State.int rs 3) in
      let px = price coin t in
      [ fill ~time:t ~coin ~px 1.
      ; fill ~time:(Time_ns.add t (Time_ns.Span.of_ms 200.)) ~coin ~px (-1.)
      ]))
  in
  let honest = List.init 40 ~f:(fun _ -> fill ~time:(random_time rs) ~coin:"BTC" 0.5) in
  history "wash" (pairs @ honest)
;;

let self_trader rs =
  let crossed =
    List.concat (List.init 4 ~f:(fun _ ->
      let t = random_time rs in
      incr next_id;
      let id = !next_id in
      [ fill ~id ~time:t ~coin:"ETH" 2.; fill ~id ~time:t ~coin:"ETH" (-2.) ]))
  in
  history "self" (crossed @ List.init 30 ~f:(fun _ -> fill ~time:(random_time rs) ~coin:"SOL" 3.))
;;

(* two accounts on the two sides of the same trades *)
let pair rs =
  let shared =
    List.init 8 ~f:(fun _ ->
      let t = random_time rs in
      incr next_id;
      t, !next_id)
  in
  let side account sign =
    history
      account
      (List.map shared ~f:(fun (t, id) -> fill ~id ~time:t ~coin:"BTC" (sign *. 0.5))
       @ List.init 20 ~f:(fun _ -> fill ~time:(random_time rs) ~coin:"ETH" 1.))
  in
  [ side "pair-a" 1.; side "pair-b" (-1.) ]
;;

(* five wallets placing the same order within ten seconds, 60 times *)
let sybils rs =
  let signals =
    List.init 60 ~f:(fun _ ->
      random_time rs, coins.(Random.State.int rs 3), if Random.State.bool rs then 1. else -1.)
  in
  List.init 5 ~f:(fun i ->
    history
      (sprintf "sybil-%d" i)
      (List.map signals ~f:(fun (t, coin, side) ->
         fill
           ~time:(Time_ns.add t (Time_ns.Span.of_sec (Random.State.float rs 10.)))
           ~coin
           (side *. 0.3))
       @ List.init 20 ~f:(fun _ -> fill ~time:(random_time rs) ~coin:"SOL" 1.)))
;;

(* buys BTC and the flow piles in right after, 40 times; plus background flow *)
let front_runner rs =
  let entries = List.init 40 ~f:(fun _ -> random_time rs) in
  let h =
    history "front" (List.map entries ~f:(fun t -> fill ~time:t ~coin:"BTC" 0.2))
  in
  let follower_flow =
    List.map entries ~f:(fun t ->
      { Integrity.Flow.time = Time_ns.add t (Time_ns.Span.of_sec 2.)
      ; coin = "BTC"
      ; notional = 200_000.
      })
  in
  let background =
    List.init 2000 ~f:(fun _ ->
      { Integrity.Flow.time = random_time rs
      ; coin = "BTC"
      ; notional = (if Random.State.bool rs then 1. else -1.) *. Random.State.float rs 80_000.
      })
  in
  h, follower_flow @ background
;;

(* fills 3% away from the recorded quotes *)
let off_book rs =
  let times = List.init 12 ~f:(fun _ -> random_time rs) in
  let quotes =
    List.map times ~f:(fun t ->
      let p = price "ETH" t in
      { Integrity.Quote.time = t; coin = "ETH"; bid = p *. 0.9995; ask = p *. 1.0005 })
  in
  let h =
    history
      "offbook"
      (List.map times ~f:(fun t -> fill ~time:t ~coin:"ETH" ~px:(price "ETH" t *. 1.03) 1.))
  in
  h, quotes
;;

let%expect_test "every planted pattern is flagged, independent noise traders are not" =
  let rs = Random.State.make [| 7 |] in
  let noise = List.init 20 ~f:(noise rs) @ news rs in
  let front, flow = front_runner rs in
  let offbook, quotes = off_book rs in
  let sybils = sybils rs in
  let histories =
    noise @ [ wash rs; self_trader rs ] @ pair rs @ sybils @ [ front; offbook ]
  in
  let created =
    (* the sybils were made the same afternoon; the noise traders over a year *)
    List.mapi noise ~f:(fun i h -> h.account, Helpers.at (-400. +. (20. *. Float.of_int i)))
    @ List.mapi sybils ~f:(fun i h -> h.account, Helpers.at ~h:(Float.of_int i) (-30.))
    |> String.Map.of_alist_exn
  in
  let flags = Integrity.check ~flow ~quotes ~created histories in
  Map.iteri flags ~f:(fun ~key ~data ->
    List.iter data ~f:(fun (f : Integrity.Flag.t) ->
      printf
        "%-8s %-15s %-8s %s\n"
        key
        (Integrity.Kind.to_string f.kind)
        (Alerts.Severity.to_string f.severity)
        f.message));
  printf
    "honest traders flagged: %d of %d\n"
    (List.count noise ~f:(fun h -> Map.mem flags h.account))
    (List.length noise);
  [%expect {|
    front    front_running   critical 100% of orders followed within 5s by 50k USD or more of same-side flow (0% at other times)
    offbook  off_book_price  critical 12 of 12 checked fills more than 50 bps outside the market (worst 295 bps)
    pair-a   counterparty    critical 8 trades against 1 other watched account (80% of volume)
    pair-b   counterparty    critical 8 trades against 1 other watched account (80% of volume)
    self     self_trade      critical 4 trades with itself (77% of volume)
    sybil-0  co_trading      critical trades in step with 4 other watched accounts (cluster of 5)
    sybil-0  creation_burst  warning  one of 5 accounts first seen within 4h, and linked by its trades
    sybil-1  co_trading      critical trades in step with 4 other watched accounts (cluster of 5)
    sybil-1  creation_burst  warning  one of 5 accounts first seen within 4h, and linked by its trades
    sybil-2  co_trading      critical trades in step with 4 other watched accounts (cluster of 5)
    sybil-2  creation_burst  warning  one of 5 accounts first seen within 4h, and linked by its trades
    sybil-3  co_trading      critical trades in step with 4 other watched accounts (cluster of 5)
    sybil-3  creation_burst  warning  one of 5 accounts first seen within 4h, and linked by its trades
    sybil-4  co_trading      critical trades in step with 4 other watched accounts (cluster of 5)
    sybil-4  creation_burst  warning  one of 5 accounts first seen within 4h, and linked by its trades
    wash     wash_trade      critical 30 buy and sell pairs at one price within 1s (57% of volume)
    honest traders flagged: 0 of 22
    |}];
  let reasons account =
    Integrity.Policy.reasons
      Integrity.Policy.default
      (Map.find flags account |> Option.value ~default:[])
  in
  print_s [%message (reasons "wash" : string list) (reasons "noise-00" : string list)];
  [%expect {|
    (("reasons \"wash\""
      ("integrity: 30 buy and sell pairs at one price within 1s (57% of volume)"))
     ("reasons \"noise-00\"" ()))
    |}]
;;

let%expect_test "evidence JSON" =
  (* round prices, so the JSON does not depend on the last bits of [sin] *)
  let crossed i =
    let t = Helpers.at (Float.of_int i) in
    [ fill ~id:(9000 + i) ~px:3000. ~time:t ~coin:"ETH" 2.
    ; fill ~id:(9000 + i) ~px:3000. ~time:t ~coin:"ETH" (-2.)
    ]
  in
  let h =
    history
      "self"
      (List.concat_map [ 1; 2 ] ~f:crossed
       @ [ fill ~px:3000. ~time:(Helpers.at 5.) ~coin:"ETH" 4. ])
  in
  let flags = Integrity.check [ h ] in
  List.iter (Map.find_exn flags "self") ~f:(fun f ->
    print_endline (Yojson.Safe.pretty_to_string (Integrity.flag_json f)));
  [%expect {|
    {
      "kind": "self_trade",
      "severity": "critical",
      "message": "2 trades with itself (67% of volume)",
      "evidence": {
        "trades": 2,
        "notional": 12000.0,
        "share_of_volume": 0.6666666666666666,
        "examples": [
          {
            "time_ms": 1767312000000,
            "coin": "ETH",
            "px": 3000.0,
            "sz": 2.0,
            "note": "both sides"
          },
          {
            "time_ms": 1767398400000,
            "coin": "ETH",
            "px": 3000.0,
            "sz": 2.0,
            "note": "both sides"
          }
        ]
      }
    }
    |}]
;;

(* A simulator's market: every trade has two watched accounts on its two sides, so all of
   them are counterparties of each other and trade in the same seconds. Random matching
   is not a cluster and not a sybil pair. *)
let%expect_test "a closed market of honest accounts is not flagged" =
  let rs = Random.State.make [| 11 |] in
  let accounts = Array.init 30 ~f:(sprintf "trader-%02d") in
  let fills = Array.map accounts ~f:(fun _ -> ref []) in
  for _ = 1 to 6000 do
    let t = random_time rs in
    let coin = coins.(Random.State.int rs 3) in
    let taker = Random.State.int rs 30 in
    let maker = (taker + 1 + Random.State.int rs 29) % 30 in
    let side = if Random.State.bool rs then 1. else -1. in
    let sz = 0.1 +. Random.State.float rs 1. in
    incr next_id;
    let id = !next_id in
    fills.(taker) := fill ~id ~time:t ~coin (side *. sz) :: !(fills.(taker));
    fills.(maker) := fill ~id ~time:t ~coin (-.side *. sz) :: !(fills.(maker))
  done;
  let histories = Array.to_list (Array.mapi accounts ~f:(fun i a -> history a !(fills.(i)))) in
  let flags = Integrity.check histories in
  printf "flagged: %d of 30\n" (Map.length flags);
  Map.iteri flags ~f:(fun ~key ~data ->
    List.iter data ~f:(fun (f : Integrity.Flag.t) -> printf "%s %s\n" key f.message));
  [%expect {| flagged: 0 of 30 |}]
;;
