open! Core

module Kind = struct
  type t =
    | Self_trade
    | Wash_trade
    | Counterparty
    | Co_trading
    | Front_running
    | Creation_burst
    | Off_book_price
  [@@deriving sexp, compare, equal, enumerate]

  let to_string = function
    | Self_trade -> "self_trade"
    | Wash_trade -> "wash_trade"
    | Counterparty -> "counterparty"
    | Co_trading -> "co_trading"
    | Front_running -> "front_running"
    | Creation_burst -> "creation_burst"
    | Off_book_price -> "off_book_price"
  ;;
end

module Example = struct
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; coin : string
    ; px : float
    ; sz : float
    ; note : string
    }
  [@@deriving sexp]
end

module Peer = struct
  type t =
    { account : string
    ; matches : int
    ; similarity : float
    ; lift : float
    }
  [@@deriving sexp]
end

module Evidence = struct
  type t =
    | Self_trade of
        { trades : int
        ; notional : float
        ; share_of_volume : float
        ; examples : Example.t list
        }
    | Wash_trade of
        { pairs : int
        ; notional : float
        ; share_of_volume : float
        ; window_ms : int
        ; examples : Example.t list
        }
    | Counterparty of
        { counterparties : (string * int * float) list
        ; trades : int
        ; share_of_volume : float
        ; group : string list
        }
    | Co_trading of
        { cluster : string list
        ; peers : Peer.t list
        }
    | Front_running of
        { orders : int
        ; followed : int
        ; rate : float
        ; base_rate : float
        ; z : float
        ; horizon_ms : int
        ; flow_usd : float
        }
    | Creation_burst of
        { accounts : string list
        ; first : Time_ns.Alternate_sexp.t
        ; last : Time_ns.Alternate_sexp.t
        ; linked : bool
        }
    | Off_book_price of
        { checked : int
        ; outside : int
        ; share : float
        ; worst_bps : float
        ; examples : Example.t list
        }
  [@@deriving sexp]
end

module Flag = struct
  type t =
    { account : string
    ; kind : Kind.t
    ; severity : Alerts.Severity.t
    ; message : string
    ; evidence : Evidence.t
    }
  [@@deriving sexp]
end

module Flow = struct
  type t =
    { time : Time_ns.t
    ; coin : string
    ; notional : float
    }
end

module Quote = struct
  type t =
    { time : Time_ns.t
    ; coin : string
    ; bid : float
    ; ask : float
    }
end

module Config = struct
  type t =
    { wash_window : Time_ns.Span.t
    ; wash_price_bps : float
    ; wash_size_tolerance : float
    ; wash_min_pairs : int
    ; wash_min_share : float
    ; counterparty_min_trades : int
    ; counterparty_min_share : float
    ; counterparty_min_lift : float
    ; order_merge : Time_ns.Span.t
    ; co_window : Time_ns.Span.t
    ; co_min_matches : int
    ; co_min_similarity : float
    ; co_min_lift : float
    ; front_run_horizon : Time_ns.Span.t
    ; front_run_flow_usd : float
    ; front_run_min_orders : int
    ; front_run_min_z : float
    ; burst_window : Time_ns.Span.t
    ; burst_min_accounts : int
    ; reference_age : Time_ns.Span.t
    ; off_book_bps : float
    ; off_book_min_fills : int
    ; off_book_min_share : float
    }
  [@@deriving sexp]

  let default =
    { wash_window = Time_ns.Span.of_sec 1.
    ; wash_price_bps = 1.
    ; wash_size_tolerance = 0.1
    ; (* a market maker matches a few round trips at one price by chance; a fifth of the
         volume is not chance *)
      wash_min_pairs = 5
    ; wash_min_share = 0.2
    ; (* a simulator run of twenty seconds put six trades of one random pair together;
         a real pair of colluding accounts trades together far more than ten times *)
      counterparty_min_trades = 10
    ; counterparty_min_share = 0.05
    ; (* in a market of only watched accounts (a simulator) everyone trades with everyone;
         five times the share random matching gives is not that *)
      counterparty_min_lift = 5.
    ; order_merge = Time_ns.Span.of_sec 1.
    ; co_window = Time_ns.Span.of_sec 60.
    ; (* leaders reacting to the same news co-trade now and then; twenty matched orders,
         three times the shifted-time count and 30% of both accounts' orders do not
         happen by reacting to news *)
      co_min_matches = 20
    ; co_min_similarity = 0.3
    ; co_min_lift = 3.
    ; front_run_horizon = Time_ns.Span.of_sec 5.
    ; front_run_flow_usd = 50_000.
    ; front_run_min_orders = 20
    ; front_run_min_z = 3.
    ; burst_window = Time_ns.Span.of_day 1.
    ; burst_min_accounts = 3
    ; reference_age = Time_ns.Span.of_ms 500.
    ; (* a mainnet leader selling STRK through the book filled 70 to 82 bps below another
         leader's prints of the same two seconds; a prearranged off-market price is
         further out than a sweep of a small perp *)
      off_book_bps = 200.
    ; off_book_min_fills = 3
    ; off_book_min_share = 0.1
    }
  ;;
end

module Policy = struct
  type t =
    { exclude : Kind.t list
    ; min_severity : Alerts.Severity.t
    }
  [@@deriving sexp]

  let default = { exclude = Kind.all; min_severity = Warning }
  let none = { exclude = []; min_severity = Critical }

  let reasons t flags =
    List.filter_map flags ~f:(fun (f : Flag.t) ->
      if List.mem t.exclude f.kind ~equal:Kind.equal
         && Alerts.Severity.compare f.severity t.min_severity >= 0
      then Some (sprintf "integrity: %s" f.message)
      else None)
  ;;
end

let ms t = Time_ns.to_int_ns_since_epoch t / 1_000_000
let sign x = if Float.( > ) x 0. then 1 else if Float.( < ) x 0. then -1 else 0

let example ?(note = "") (f : Fill.t) =
  { Example.time = f.time; coin = f.coin; px = f.px; sz = f.sz; note }
;;

let volume (h : History.t) = List.sum (module Float) h.fills ~f:Fill.notional
let share part total = if Float.( > ) total 0. then part /. total else 0.
let take n l = List.take l n

(* ---- one account ---- *)

let self_trades (config : Config.t) (h : History.t) =
  ignore config;
  let by_trade = Hashtbl.Poly.create () in
  List.iter h.fills ~f:(fun f ->
    Hashtbl.add_multi by_trade ~key:(f.id, f.coin, ms f.time, f.px) ~data:f);
  let matched =
    Hashtbl.fold by_trade ~init:[] ~f:(fun ~key:_ ~data acc ->
      match List.find data ~f:(fun f -> sign f.sz > 0), List.find data ~f:(fun f -> sign f.sz < 0) with
      | Some buy, Some _ -> buy :: acc
      | _ -> acc)
    |> List.sort ~compare:Fill.compare_by_time
  in
  match matched with
  | [] -> None
  | _ :: _ ->
    let notional = List.sum (module Float) matched ~f:Fill.notional in
    let total = volume h in
    let trades = List.length matched in
    Some
      { Flag.account = h.account
      ; kind = Self_trade
      ; severity = Critical
      ; message =
          sprintf
            "%d trades with itself (%.0f%% of volume)"
            trades
            (100. *. share (2. *. notional) total)
      ; evidence =
          Self_trade
            { trades
            ; notional
            ; share_of_volume = share (2. *. notional) total
            ; examples = take 5 (List.map matched ~f:(example ~note:"both sides"))
            }
      }
;;

let wash_trades (config : Config.t) (h : History.t) =
  let window = Time_ns.Span.to_int_ns config.wash_window in
  let by_coin =
    List.map h.fills ~f:(fun f -> f.coin, f)
    |> String.Map.of_alist_multi
    |> Map.map ~f:(fun l -> Array.of_list (List.sort l ~compare:Fill.compare_by_time))
  in
  let pairs = ref [] in
  Map.iter by_coin ~f:(fun fills ->
    let used = Array.create ~len:(Array.length fills) false in
    Array.iteri fills ~f:(fun i (a : Fill.t) ->
      if not used.(i)
      then (
        let t_a = Time_ns.to_int_ns_since_epoch a.time in
        let rec scan j =
          if j < Array.length fills
          then (
            let b = fills.(j) in
            if Time_ns.to_int_ns_since_epoch b.time - t_a > window
            then ()
            else if (not used.(j))
                    && sign b.sz = -sign a.sz
                    && b.id <> a.id
                    && Float.( <= )
                         (Float.abs (b.px -. a.px) /. a.px *. 10_000.)
                         config.wash_price_bps
                    && Float.( <= )
                         (Float.abs (Float.abs b.sz -. Float.abs a.sz))
                         (config.wash_size_tolerance *. Float.abs a.sz)
            then (
              used.(i) <- true;
              used.(j) <- true;
              pairs := (a, b) :: !pairs)
            else scan (j + 1))
        in
        scan (i + 1))));
  let pairs = List.rev !pairs in
  let n = List.length pairs in
  let notional =
    List.sum (module Float) pairs ~f:(fun (a, b) -> Fill.notional a +. Fill.notional b)
  in
  let total = volume h in
  let s = share notional total in
  if n < config.wash_min_pairs || Float.( < ) s config.wash_min_share
  then None
  else
    Some
      { Flag.account = h.account
      ; kind = Wash_trade
      ; severity = (if Float.( >= ) s 0.5 then Critical else Warning)
      ; message =
          sprintf
            "%d buy and sell pairs at one price within %s (%.0f%% of volume)"
            n
            (Time_ns.Span.to_string_hum config.wash_window)
            (100. *. s)
      ; evidence =
          Wash_trade
            { pairs = n
            ; notional
            ; share_of_volume = s
            ; window_ms = Time_ns.Span.to_int_ms config.wash_window
            ; examples =
                take 5 pairs
                |> List.concat_map ~f:(fun (a, b) ->
                  [ example a ~note:"open"; example b ~note:"opposite" ])
            }
      }
;;

(* Fills of one coin and side close in time are one order. *)
module Order = struct
  type t =
    { time : int (* ns *)
    ; coin : string
    ; side : int
    ; notional : float
    }
end

let orders (config : Config.t) (h : History.t) =
  let merge = Time_ns.Span.to_int_ns config.order_merge in
  let sorted = List.sort h.fills ~compare:Fill.compare_by_time in
  let last = String.Table.create () in
  List.fold sorted ~init:[] ~f:(fun acc (f : Fill.t) ->
    let t = Time_ns.to_int_ns_since_epoch f.time in
    let side = sign f.sz in
    let key = f.coin in
    match Hashtbl.find last key with
    | Some (t0, s0) when s0 = side && t - t0 <= merge ->
      Hashtbl.set last ~key ~data:(t, side);
      (match acc with
       | (o : Order.t) :: rest when String.equal o.coin f.coin && o.side = side ->
         { o with notional = o.notional +. Fill.notional f } :: rest
       | _ -> { Order.time = t; coin = f.coin; side; notional = Fill.notional f } :: acc)
    | _ ->
      Hashtbl.set last ~key ~data:(t, side);
      { Order.time = t; coin = f.coin; side; notional = Fill.notional f } :: acc)
  |> List.rev
  |> Array.of_list
;;

(* ---- across accounts ---- *)

module Edge = struct
  type t =
    { trades : int
    ; notional : float
    }
end

let components ~accounts (edges : (string * string) list) =
  let parent = String.Table.create () in
  List.iter accounts ~f:(fun a -> Hashtbl.set parent ~key:a ~data:a);
  let rec find a =
    let p = Hashtbl.find parent a |> Option.value ~default:a in
    if String.equal p a
    then a
    else (
      let r = find p in
      Hashtbl.set parent ~key:a ~data:r;
      r)
  in
  List.iter edges ~f:(fun (a, b) ->
    let ra = find a
    and rb = find b in
    if not (String.equal ra rb) then Hashtbl.set parent ~key:ra ~data:rb);
  List.map accounts ~f:(fun a -> find a, a)
  |> String.Map.of_alist_multi
  |> Map.data
  |> List.map ~f:(List.sort ~compare:String.compare)
;;

let counterparty_flags (config : Config.t) histories =
  let by_trade = Hashtbl.Poly.create () in
  List.iter histories ~f:(fun (h : History.t) ->
    List.iter h.fills ~f:(fun f ->
      Hashtbl.add_multi
        by_trade
        ~key:(f.id, f.coin, ms f.time, f.px)
        ~data:(h.account, sign f.sz, Fill.notional f)));
  let edges = Hashtbl.Poly.create () in
  Hashtbl.iter by_trade ~f:(fun sides ->
    List.iter sides ~f:(fun (a, sa, na) ->
      List.iter sides ~f:(fun (b, sb, _) ->
        if String.( < ) a b && sa = -sb
        then
          Hashtbl.update edges (a, b) ~f:(function
            | None -> { Edge.trades = 1; notional = na }
            | Some e -> { trades = e.trades + 1; notional = e.notional +. na }))));
  let volume_of =
    List.map histories ~f:(fun h -> h.account, volume h)
    |> String.Map.of_alist_reduce ~f:( +. )
  in
  let total = Map.data volume_of |> List.sum (module Float) ~f:Fn.id in
  let vol x = Map.find volume_of x |> Option.value ~default:0. in
  let strong =
    Hashtbl.to_alist edges
    |> List.filter ~f:(fun ((a, b), (e : Edge.t)) ->
      let s x = share e.notional (vol x) in
      (* the share of [x]'s volume against [y] when every watched account trades with
         every other in proportion to its volume; watched accounts are only part of a real
         market, so this overstates chance and keeps the lift conservative *)
      let lift x y = share (s x) (share (vol y) total) in
      e.trades >= config.counterparty_min_trades
      && Float.( >= ) (Float.max (s a) (s b)) config.counterparty_min_share
      && Float.( >= ) (Float.max (lift a b) (lift b a)) config.counterparty_min_lift)
  in
  let accounts =
    List.concat_map strong ~f:(fun ((a, b), _) -> [ a; b ])
    |> List.dedup_and_sort ~compare:String.compare
  in
  let groups = components ~accounts (List.map strong ~f:fst) in
  List.map accounts ~f:(fun account ->
    let counterparties =
      List.filter_map strong ~f:(fun ((a, b), (e : Edge.t)) ->
        if String.equal a account
        then Some (b, e.trades, e.notional)
        else if String.equal b account
        then Some (a, e.trades, e.notional)
        else None)
      |> List.sort ~compare:(fun (_, x, _) (_, y, _) -> Int.compare y x)
    in
    let trades = List.sum (module Int) counterparties ~f:(fun (_, n, _) -> n) in
    let notional = List.sum (module Float) counterparties ~f:(fun (_, _, x) -> x) in
    let s = share notional (Map.find volume_of account |> Option.value ~default:0.) in
    let group =
      List.find groups ~f:(fun g -> List.mem g account ~equal:String.equal)
      |> Option.value ~default:[ account ]
    in
    ( account
    , { Flag.account
      ; kind = Counterparty
      ; severity = (if Float.( >= ) s 0.25 || List.length group >= 3 then Critical else Warning)
      ; message =
          sprintf
            "%d trades against %d other watched account%s (%.0f%% of volume)"
            trades
            (List.length counterparties)
            (if List.length counterparties = 1 then "" else "s")
            (100. *. s)
      ; evidence = Counterparty { counterparties; trades; share_of_volume = s; group }
      } ))
;;

(* Shifts for the chance count: far beyond the window, both directions, so each account
   keeps its own rhythm (time of day, bursts) and only the alignment is broken. *)
(* The chance count shifts one side's times circularly inside the observed span
   [lo, hi] by sevenths of it: each account keeps its own rhythm (bursts, quiet hours) and
   its density, and only the alignment with the other is broken. A span shorter than
   [min_spans] windows is too short for that; the check is then skipped. *)
let min_spans = 20

let circular_shifts ~lo ~hi =
  let span = hi - lo in
  List.map [ 1; 2; 3; 4; 5; 6 ] ~f:(fun k ->
    let delta = k * span / 7 in
    fun t -> lo + ((t - lo + delta) % span))
;;

let co_trading_flags (config : Config.t) histories =
  let w = Time_ns.Span.to_int_ns config.co_window in
  let accounts = List.map histories ~f:(fun (h : History.t) -> h.account) |> Array.of_list in
  let orders = List.map histories ~f:(orders config) |> Array.of_list in
  let n = Array.length accounts in
  (* (coin, side, bucket) -> (account index, time) *)
  let index = Hashtbl.Poly.create () in
  Array.iteri orders ~f:(fun i os ->
    Array.iter os ~f:(fun (o : Order.t) ->
      Hashtbl.add_multi index ~key:(o.coin, o.side, o.time / w) ~data:(i, o.time)));
  (* [matches.(i)] counts, per other account, orders of [i] with an order of it within
     the window, at [shift] *)
  let count ~shift =
    Array.mapi orders ~f:(fun i os ->
      let m = Int.Table.create () in
      Array.iter os ~f:(fun (o : Order.t) ->
        let t = shift o.time in
        let seen = Int.Hash_set.create () in
        List.iter [ -1; 0; 1 ] ~f:(fun d ->
          Hashtbl.find index (o.coin, o.side, (t / w) + d)
          |> Option.iter
               ~f:
                 (List.iter ~f:(fun (j, tj) ->
                    if j <> i && Int.abs (tj - t) <= w then Hash_set.add seen j)));
        Hash_set.iter seen ~f:(fun j -> Hashtbl.incr m j));
      m)
  in
  let times = Array.concat_map orders ~f:(Array.map ~f:(fun (o : Order.t) -> o.time)) in
  match Array.min_elt times ~compare:Int.compare, Array.max_elt times ~compare:Int.compare with
  | Some lo, Some hi when hi - lo >= min_spans * w ->
  let observed = count ~shift:Fn.id in
  let chance = List.map (circular_shifts ~lo ~hi) ~f:(fun shift -> count ~shift) in
  let get tbl j = Hashtbl.find tbl j |> Option.value ~default:0 in
  let links =
    List.concat_map (List.range 0 n) ~f:(fun i ->
      List.filter_map (List.range (i + 1) n) ~f:(fun j ->
        let m = get observed.(i) j + get observed.(j) i in
        let expected =
          List.sum (module Int) chance ~f:(fun c -> get c.(i) j + get c.(j) i)
          |> Float.of_int
          |> fun x -> x /. Float.of_int (List.length chance)
        in
        let total = Array.length orders.(i) + Array.length orders.(j) in
        let similarity = share (Float.of_int m) (Float.of_int total) in
        (* one expected match at least, so a pair that never meets by chance still needs
           [co_min_lift] matches per expected one *)
        let lift = Float.of_int m /. Float.max expected 1. in
        if m >= config.co_min_matches
           && Float.( >= ) similarity config.co_min_similarity
           && Float.( >= ) lift config.co_min_lift
        then Some (i, j, { Peer.account = ""; matches = m; similarity; lift })
        else None))
  in
  let linked =
    List.concat_map links ~f:(fun (i, j, _) -> [ accounts.(i); accounts.(j) ])
    |> List.dedup_and_sort ~compare:String.compare
  in
  let clusters =
    components
      ~accounts:linked
      (List.map links ~f:(fun (i, j, _) -> accounts.(i), accounts.(j)))
  in
  List.map linked ~f:(fun account ->
    let peers =
      List.filter_map links ~f:(fun (i, j, p) ->
        if String.equal accounts.(i) account
        then Some { p with account = accounts.(j) }
        else if String.equal accounts.(j) account
        then Some { p with account = accounts.(i) }
        else None)
      |> List.sort ~compare:(fun (a : Peer.t) b -> Float.compare b.similarity a.similarity)
    in
    let cluster =
      List.find clusters ~f:(fun c -> List.mem c account ~equal:String.equal)
      |> Option.value ~default:[ account ]
    in
    ( account
    , { Flag.account
      ; kind = Co_trading
      ; severity = (if List.length cluster >= 3 then Critical else Warning)
      ; message =
          sprintf
            "trades in step with %d other watched account%s (cluster of %d)"
            (List.length peers)
            (if List.length peers = 1 then "" else "s")
            (List.length cluster)
      ; evidence = Co_trading { cluster; peers }
      } ))
  | _ -> []
;;

let front_running_flag (config : Config.t) ~flow (h : History.t) =
  let horizon = Time_ns.Span.to_int_ns config.front_run_horizon in
  let os = orders config h in
  (* net signed flow after [t] within the horizon, per coin, from prefix sums *)
  let net coin t =
    match Map.find flow coin with
    | None -> 0.
    | Some (times, cum) ->
      let upto x =
        match
          Array.binary_search times ~compare:Int.compare `Last_less_than_or_equal_to x
        with
        | None -> 0.
        | Some k -> cum.(k)
      in
      upto (t + horizon) -. upto t
  in
  let followed ~shift =
    Array.count os ~f:(fun (o : Order.t) ->
      Map.mem flow o.coin
      && Float.( >= )
           (Float.of_int o.side *. net o.coin (shift o.time))
           config.front_run_flow_usd)
  in
  let n = Array.count os ~f:(fun (o : Order.t) -> Map.mem flow o.coin) in
  let span =
    Map.fold flow ~init:None ~f:(fun ~key:_ ~data:(times, _) acc ->
      if Array.is_empty times
      then acc
      else (
        let lo = times.(0)
        and hi = times.(Array.length times - 1) in
        match acc with
        | None -> Some (lo, hi)
        | Some (a, b) -> Some (Int.min a lo, Int.max b hi)))
  in
  match span with
  | Some (lo, hi) when n >= config.front_run_min_orders && hi - lo >= min_spans * horizon ->
    let shifts = circular_shifts ~lo ~hi in
    let k = followed ~shift:Fn.id in
    let base =
      List.sum (module Int) shifts ~f:(fun shift -> followed ~shift)
      |> Float.of_int
      |> fun x -> x /. Float.of_int (n * List.length shifts)
    in
    let rate = Float.of_int k /. Float.of_int n in
    (* a base rate of zero would make any hit infinitely significant: floor it at one
       hit in all the shifted samples *)
    let p = Float.max base (1. /. Float.of_int (n * List.length shifts)) in
    let z =
      (Float.of_int k -. (Float.of_int n *. p))
      /. Float.sqrt (Float.of_int n *. p *. (1. -. p))
    in
    if Float.( >= ) z config.front_run_min_z && Float.( >= ) rate (2. *. p)
    then
      Some
        { Flag.account = h.account
        ; kind = Front_running
        ; severity = (if Float.( >= ) z 5. then Critical else Warning)
        ; message =
            sprintf
              "%.0f%% of orders followed within %s by %.0fk USD or more of same-side \
               flow (%.0f%% at other times)"
              (100. *. rate)
              (Time_ns.Span.to_string_hum config.front_run_horizon)
              (config.front_run_flow_usd /. 1000.)
              (100. *. base)
        ; evidence =
            Front_running
              { orders = n
              ; followed = k
              ; rate
              ; base_rate = base
              ; z
              ; horizon_ms = Time_ns.Span.to_int_ms config.front_run_horizon
              ; flow_usd = config.front_run_flow_usd
              }
        }
    else None
  | _ -> None
;;

let creation_bursts (config : Config.t) ~created ~linked =
  let sorted =
    Map.to_alist created
    |> List.sort ~compare:(fun (a, x) (b, y) ->
      match Time_ns.compare x y with
      | 0 -> String.compare a b
      | c -> c)
  in
  (* a group is every account within [burst_window] of its first one, not a chain of
     accounts each close to the next, which a steady stream of sign-ups would form *)
  let rec groups acc = function
    | [] -> List.rev acc
    | ((_, first) :: _ as rest) ->
      let g, after =
        List.split_while rest ~f:(fun (_, t) ->
          Time_ns.Span.( <= ) (Time_ns.diff t first) config.burst_window)
      in
      if List.length g >= config.burst_min_accounts
      then groups (g :: acc) after
      else groups acc (List.tl_exn rest)
  in
  let groups = groups [] sorted in
  List.concat_map groups ~f:(fun g ->
    let accounts = List.map g ~f:fst in
    let first = snd (List.hd_exn g) in
    let last = snd (List.last_exn g) in
    let group_linked = List.count accounts ~f:(Set.mem linked) >= 2 in
    List.map accounts ~f:(fun account ->
      (* only the members that are themselves linked; one linked pair in a large burst
         says nothing about the others *)
      let linked = group_linked && Set.mem linked account in
      ( account
      , { Flag.account
        ; kind = Creation_burst
        ; severity = (if linked then Warning else Info)
        ; message =
            sprintf
              "one of %d accounts first seen within %s%s"
              (List.length accounts)
              (Time_ns.Span.to_string_hum (Time_ns.diff last first))
              (if linked then ", and linked by its trades" else "")
        ; evidence = Creation_burst { accounts; first; last; linked }
        } )))
;;

let off_book_flags (config : Config.t) ~quotes histories =
  let age = Time_ns.Span.to_int_ns config.reference_age in
  (* reference prints per coin: (time, px, account, trade id), by time *)
  let prints =
    List.concat_map histories ~f:(fun (h : History.t) ->
      List.map h.fills ~f:(fun f ->
        f.coin, (Time_ns.to_int_ns_since_epoch f.time, f.px, h.account, f.id)))
    |> String.Map.of_alist_multi
    |> Map.map ~f:(fun l ->
      Array.of_list (List.sort l ~compare:(fun (a, _, _, _) (b, _, _, _) -> Int.compare a b)))
  in
  let quotes =
    List.map quotes ~f:(fun (q : Quote.t) ->
      q.coin, (Time_ns.to_int_ns_since_epoch q.time, q.bid, q.ask))
    |> String.Map.of_alist_multi
    |> Map.map ~f:(fun l ->
      Array.of_list (List.sort l ~compare:(fun (a, _, _) (b, _, _) -> Int.compare a b)))
  in
  (* the first index whose time is at least [t], [None] past the end *)
  let first_at arr ~time_of t =
    let lo = ref 0
    and hi = ref (Array.length arr) in
    while !lo < !hi do
      let mid = (!lo + !hi) / 2 in
      if time_of arr.(mid) < t then lo := mid + 1 else hi := mid
    done;
    if !lo < Array.length arr then Some !lo else None
  in
  let range (h : History.t) (f : Fill.t) =
    let t = Time_ns.to_int_ns_since_epoch f.time in
    let lo = ref Float.infinity
    and hi = ref Float.neg_infinity in
    (match Map.find quotes f.coin with
     | None -> ()
     | Some qs ->
       let start = first_at qs ~time_of:(fun (x, _, _) -> x) (t - age) in
       Option.iter start ~f:(fun k ->
         let k = ref k in
         while !k < Array.length qs && (fun (x, _, _) -> x) qs.(!k) <= t + age do
           let _, bid, ask = qs.(!k) in
           lo := Float.min !lo bid;
           hi := Float.max !hi ask;
           incr k
         done));
    (match Map.find prints f.coin with
     | None -> ()
     | Some ps ->
       let start = first_at ps ~time_of:(fun (x, _, _, _) -> x) (t - age) in
       Option.iter start ~f:(fun k ->
         let k = ref k in
         while !k < Array.length ps && (fun (x, _, _, _) -> x) ps.(!k) <= t + age do
           let _, px, account, id = ps.(!k) in
           (* other accounts only, and never the fill's own trade *)
           if (not (String.equal account h.account)) && id <> f.id
           then (
             lo := Float.min !lo px;
             hi := Float.max !hi px);
           incr k
         done));
    if Float.is_finite !lo && Float.is_finite !hi then Some (!lo, !hi) else None
  in
  List.filter_map histories ~f:(fun (h : History.t) ->
    let checked = ref 0 in
    let outside = ref [] in
    List.iter h.fills ~f:(fun f ->
      match range h f with
      | None -> ()
      | Some (lo, hi) ->
        incr checked;
        let bps =
          if Float.( > ) f.px hi
          then (f.px -. hi) /. hi *. 10_000.
          else if Float.( < ) f.px lo
          then (lo -. f.px) /. lo *. 10_000.
          else 0.
        in
        if Float.( > ) bps config.off_book_bps then outside := (f, bps) :: !outside);
    let k = List.length !outside in
    let s = share (Float.of_int k) (Float.of_int !checked) in
    if k < config.off_book_min_fills || Float.( < ) s config.off_book_min_share
    then None
    else (
      let worst =
        List.fold !outside ~init:0. ~f:(fun m (_, b) -> Float.max m b)
      in
      Some
        ( h.account
        , { Flag.account = h.account
          ; kind = Off_book_price
          ; severity = (if Float.( >= ) s 0.3 then Critical else Warning)
          ; message =
              sprintf
                "%d of %d checked fills more than %.0f bps outside the market (worst \
                 %.0f bps)"
                k
                !checked
                config.off_book_bps
                worst
          ; evidence =
              Off_book_price
                { checked = !checked
                ; outside = k
                ; share = s
                ; worst_bps = worst
                ; examples =
                    List.rev !outside
                    |> take 5
                    |> List.map ~f:(fun (f, b) -> example f ~note:(sprintf "%.0f bps" b))
                }
          } )))
;;

let check ?(config = Config.default) ?flow ?(quotes = []) ?created histories =
  let histories = List.map histories ~f:History.normalize in
  let own =
    List.concat_map histories ~f:(fun h ->
      List.filter_opt [ self_trades config h; wash_trades config h ]
      |> List.map ~f:(fun f -> f.account, f))
  in
  let counterparty = counterparty_flags config histories in
  let co = co_trading_flags config histories in
  let front =
    match flow with
    | None | Some [] -> []
    | Some flow ->
      let flow =
        List.map flow ~f:(fun (x : Flow.t) ->
          x.coin, (Time_ns.to_int_ns_since_epoch x.time, x.notional))
        |> String.Map.of_alist_multi
        |> Map.map ~f:(fun l ->
          let a = Array.of_list (List.sort l ~compare:(fun (x, _) (y, _) -> Int.compare x y)) in
          let cum = Array.create ~len:(Array.length a) 0. in
          Array.iteri a ~f:(fun i (_, v) ->
            cum.(i) <- (if i = 0 then v else cum.(i - 1) +. v));
          Array.map a ~f:fst, cum)
      in
      List.filter_map histories ~f:(fun h ->
        front_running_flag config ~flow h |> Option.map ~f:(fun f -> f.account, f))
  in
  let off_book = off_book_flags config ~quotes histories in
  let linked =
    List.map (counterparty @ co) ~f:fst |> String.Set.of_list
  in
  let bursts =
    match created with
    | None -> []
    | Some created -> creation_bursts config ~created ~linked
  in
  own @ counterparty @ co @ front @ bursts @ off_book
  |> String.Map.of_alist_multi
  |> Map.map ~f:(List.sort ~compare:(fun (a : Flag.t) b -> Kind.compare a.kind b.kind))
;;

let num x = if Float.is_finite x then `Float x else `Null

let example_json (e : Example.t) =
  `Assoc
    [ "time_ms", `Int (ms e.time)
    ; "coin", `String e.coin
    ; "px", num e.px
    ; "sz", num e.sz
    ; "note", `String e.note
    ]
;;

let strings l = `List (List.map l ~f:(fun s -> `String s))

let evidence_json : Evidence.t -> Yojson.Safe.t = function
  | Self_trade { trades; notional; share_of_volume; examples } ->
    `Assoc
      [ "trades", `Int trades
      ; "notional", num notional
      ; "share_of_volume", num share_of_volume
      ; "examples", `List (List.map examples ~f:example_json)
      ]
  | Wash_trade { pairs; notional; share_of_volume; window_ms; examples } ->
    `Assoc
      [ "pairs", `Int pairs
      ; "notional", num notional
      ; "share_of_volume", num share_of_volume
      ; "window_ms", `Int window_ms
      ; "examples", `List (List.map examples ~f:example_json)
      ]
  | Counterparty { counterparties; trades; share_of_volume; group } ->
    `Assoc
      [ ( "counterparties"
        , `List
            (List.map counterparties ~f:(fun (a, n, x) ->
               `Assoc [ "account", `String a; "trades", `Int n; "notional", num x ])) )
      ; "trades", `Int trades
      ; "share_of_volume", num share_of_volume
      ; "group", strings group
      ]
  | Co_trading { cluster; peers } ->
    `Assoc
      [ "cluster", strings cluster
      ; ( "peers"
        , `List
            (List.map peers ~f:(fun (p : Peer.t) ->
               `Assoc
                 [ "account", `String p.account
                 ; "matches", `Int p.matches
                 ; "similarity", num p.similarity
                 ; "lift", num p.lift
                 ])) )
      ]
  | Front_running { orders; followed; rate; base_rate; z; horizon_ms; flow_usd } ->
    `Assoc
      [ "orders", `Int orders
      ; "followed", `Int followed
      ; "rate", num rate
      ; "base_rate", num base_rate
      ; "z", num z
      ; "horizon_ms", `Int horizon_ms
      ; "flow_usd", num flow_usd
      ]
  | Creation_burst { accounts; first; last; linked } ->
    `Assoc
      [ "accounts", strings accounts
      ; "first_ms", `Int (ms first)
      ; "last_ms", `Int (ms last)
      ; "linked", `Bool linked
      ]
  | Off_book_price { checked; outside; share; worst_bps; examples } ->
    `Assoc
      [ "checked", `Int checked
      ; "outside", `Int outside
      ; "share", num share
      ; "worst_bps", num worst_bps
      ; "examples", `List (List.map examples ~f:example_json)
      ]
;;

let flag_json (f : Flag.t) =
  `Assoc
    [ "kind", `String (Kind.to_string f.kind)
    ; "severity", `String (Alerts.Severity.to_string f.severity)
    ; "message", `String f.message
    ; "evidence", evidence_json f.evidence
    ]
;;
