open! Core

module Profile = struct
  type t =
    { mu : float
    ; wander : float
    ; regime_days : float
    ; leverage : float
    ; trades_per_day : float
    ; hold_days : float
    ; long_bias : float
    ; coins : int
    ; martingale : bool
    }
  [@@deriving sexp]
end

let coin_names = [| "BTC"; "ETH"; "SOL"; "HYPE"; "DOGE"; "XRP" |]
let fee_rate = 0.00045

(* Standard normal by Box-Muller from Core's splittable PRNG. *)
let gaussian rs =
  let u1 = Float.max 1e-12 (Random.State.float rs 1.) in
  let u2 = Random.State.float rs 1. in
  Float.sqrt (-2. *. Float.log u1) *. Float.cos (2. *. Float.pi *. u2)
;;

let market ~seed ~coins ~days =
  let rs = Random.State.make [| seed; 17 |] in
  Array.init coins ~f:(fun c ->
    (* daily volatility 3% to 6%, no drift *)
    let sigma = 0.03 +. (0.006 *. Float.of_int c) in
    let p = Array.create ~len:(days + 1) 100. in
    for d = 1 to days do
      let z = gaussian rs in
      p.(d) <- p.(d - 1) *. Float.exp ((sigma *. z) -. (0.5 *. sigma *. sigma))
    done;
    p)
;;

type open_trade =
  { coin : int
  ; size : float
  ; close_day : int
  }

let leader ~seed ~start ~(market : float array array) ~account (p : Profile.t) =
  let rs = Random.State.make [| seed; 29 |] in
  let days = Array.length market.(0) - 1 in
  let n_coins = Int.min p.coins (Array.length market) in
  let at day hour =
    Time_ns.add start (Time_ns.Span.of_hr ((24. *. Float.of_int day) +. hour))
  in
  let fills = Queue.create () in
  let snapshots = Queue.create () in
  let id = ref 0 in
  let positions = Array.create ~len:n_coins 0. in
  let entry = Array.create ~len:n_coins 0. in
  let cash = ref 100_000. in
  let equity_at day =
    let unrealized = ref 0. in
    for c = 0 to n_coins - 1 do
      unrealized := !unrealized +. (positions.(c) *. (market.(c).(day) -. entry.(c)))
    done;
    !cash +. !unrealized
  in
  let trade ~coin ~day ~hour ~qty =
    let px = market.(coin).(day) in
    incr id;
    let before = positions.(coin) in
    Queue.enqueue
      fills
      { Fill.time = at day hour
      ; coin = coin_names.(coin)
      ; px
      ; sz = qty
      ; fee = Float.abs qty *. px *. fee_rate
      ; position_before = Some before
      ; liquidation = false
      ; id = !id
      };
    cash := !cash -. (Float.abs qty *. px *. fee_rate);
    (* every trade here opens from flat or closes to flat *)
    if Float.equal before 0.
    then (
      positions.(coin) <- qty;
      entry.(coin) <- px)
    else (
      cash := !cash +. (before *. (px -. entry.(coin)));
      positions.(coin) <- 0.;
      entry.(coin) <- 0.)
  in
  let mu = ref p.mu
  and x = ref 0.
  and streak = ref 0
  and alive = ref true in
  let open_trades = ref [] in
  for day = 0 to days - 1 do
    (* the edge wanders and occasionally jumps to a new level *)
    x := (0.9 *. !x) +. (p.wander *. gaussian rs);
    if Float.( < ) (Random.State.float rs 1.) (1. /. p.regime_days)
    then mu := 0.04 *. gaussian rs;
    let skill = Float.clamp_exn (!mu +. !x) ~min:(-0.3) ~max:0.3 in
    (* close what is due *)
    let due, rest = List.partition_tf !open_trades ~f:(fun t -> t.close_day <= day) in
    List.iter due ~f:(fun t ->
      let won = Float.( > ) (t.size *. (market.(t.coin).(day) -. entry.(t.coin))) 0. in
      trade ~coin:t.coin ~day ~hour:(Random.State.float rs 8.) ~qty:(-.t.size);
      streak := if won then 0 else !streak + 1);
    open_trades := rest;
    if !alive && Float.( < ) (equity_at day) 10_000. then alive := false;
    if !alive
    then
      for c = 0 to n_coins - 1 do
        let busy = List.exists !open_trades ~f:(fun t -> t.coin = c) in
        let chance = p.trades_per_day /. Float.of_int n_coins in
        if (not busy) && Float.( < ) (Random.State.float rs 1.) chance
        then (
          let hold =
            Int.max
              1
              (Float.iround_nearest_exn
                 (p.hold_days *. -.Float.log (Float.max 1e-9 (Random.State.float rs 1.))))
          in
          let close_day = Int.min days (day + hold) in
          let move = market.(c).(close_day) -. market.(c).(day) in
          let right = if Float.( >= ) move 0. then 1. else -1. in
          let side =
            if Float.( < ) (Random.State.float rs 1.) (0.5 +. skill)
            then right
            else -.right
          in
          let side =
            if Float.( < ) (Random.State.float rs 1.) (Float.abs p.long_bias *. 0.5)
            then Float.copysign 1. p.long_bias
            else side
          in
          let boost =
            if p.martingale then Float.of_int (1 lsl Int.min 3 !streak) else 1.
          in
          let notional = p.leverage *. boost *. equity_at day /. Float.of_int n_coins in
          let qty = side *. notional /. market.(c).(day) in
          trade ~coin:c ~day ~hour:(8. +. Random.State.float rs 8.) ~qty;
          open_trades := { coin = c; size = qty; close_day } :: !open_trades)
      done;
    Queue.enqueue
      snapshots
      { Snapshot.time = at day 23.99
      ; account_value = equity_at day
      ; positions =
          List.filter_map (List.range 0 n_coins) ~f:(fun c ->
            if Float.equal positions.(c) 0.
            then None
            else
              Some
                { Snapshot.Position.coin = coin_names.(c)
                ; size = positions.(c)
                ; entry_px = Some entry.(c)
                ; mark_px = Some market.(c).(day)
                ; leverage = Some p.leverage
                })
      }
  done;
  { History.account
  ; venue = "synthetic"
  ; fetched_at = at days 0.
  ; fills = Queue.to_list fills
  ; funding = []
  ; snapshots = Queue.to_list snapshots
  ; equity_curve = []
  ; fills_truncated = false
  }
;;

let profile rs : Profile.t =
  let pick l = List.nth_exn l (Random.State.int rs (List.length l)) in
  { mu = 0.04 *. gaussian rs
  ; wander = pick [ 0.; 0.005; 0.01; 0.02 ]
  ; regime_days = pick [ 60.; 120.; 240.; 1000. ]
  ; leverage = pick [ 0.5; 1.; 2.; 3.; 5.; 10. ]
  ; trades_per_day = pick [ 0.2; 0.5; 1.; 2.; 4. ]
  ; hold_days = pick [ 1.; 2.; 4.; 8. ]
  ; long_bias = pick [ 0.; 0.; 0.3; 0.6; -0.3 ]
  ; coins = 1 + Random.State.int rs 5
  ; martingale = Float.( < ) (Random.State.float rs 1.) 0.1
  }
;;

let universe ~seed ~start ~leaders ~days =
  let market = market ~seed ~coins:(Array.length coin_names) ~days in
  let rs = Random.State.make [| seed; 41 |] in
  List.init leaders ~f:(fun i ->
    let p = profile rs in
    ( leader
        ~seed:((seed * 100_003) + i)
        ~start
        ~market
        ~account:(sprintf "synthetic-%04d" i)
        p
    , p ))
;;
