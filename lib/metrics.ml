open! Core

module Config = struct
  type t =
    { bar : Time_ns.Span.t option
    ; window_bars : int option
    ; open_interest : float String.Map.t
    }

  let default = { bar = None; window_bars = None; open_interest = String.Map.empty }
end

let auto_bar ~span =
  if Time_ns.Span.( >= ) span (Time_ns.Span.of_day 30.)
  then Time_ns.Span.day
  else if Time_ns.Span.( >= ) span (Time_ns.Span.of_day 2.)
  then Time_ns.Span.hour
  else Time_ns.Span.minute
;;

let auto_window ~bar ~bars =
  let target =
    if Time_ns.Span.( >= ) bar Time_ns.Span.day
    then 7
    else if Time_ns.Span.( >= ) bar Time_ns.Span.hour
    then 24
    else 60
  in
  Int.max 2 (Int.min target (bars / 2))
;;

type t =
  { values : float option Metric.Map.t
  ; bar : Time_ns.Span.t
  ; returns : float array
  }
[@@deriving sexp_of]

let year = Time_ns.Span.of_day 365.
let days span = Time_ns.Span.to_day span
let finite x = if Float.is_finite x then Some x else None
let ratio a b = if Float.( > ) (Float.abs b) 0. then finite (a /. b) else None

let returns_of_bars bars =
  let n = Array.length bars in
  if n < 2
  then [||]
  else
    Array.filter_mapi (Array.sub bars ~pos:1 ~len:(n - 1)) ~f:(fun i e ->
      let prev = bars.(i) in
      if Float.( > ) prev 0. then Some ((e /. prev) -. 1.) else None)
;;

(* Drawdown over the equity points: (max drawdown, longest duration, current). *)
let drawdowns (equity : Equity_point.t array) ~until =
  if Array.is_empty equity
  then None
  else (
    let peak = ref equity.(0).equity
    and peak_at = ref equity.(0).time
    and max_dd = ref 0.
    and longest = ref Time_ns.Span.zero in
    Array.iter equity ~f:(fun p ->
      if Float.( >= ) p.equity !peak
      then (
        longest := Time_ns.Span.max !longest (Time_ns.diff p.time !peak_at);
        peak := p.equity;
        peak_at := p.time)
      else if Float.( > ) !peak 0.
      then max_dd := Float.max !max_dd ((!peak -. p.equity) /. !peak));
    let last = equity.(Array.length equity - 1).equity in
    (* an unrecovered drawdown lasts until the end of the period *)
    if Float.( < ) last !peak
    then longest := Time_ns.Span.max !longest (Time_ns.diff until !peak_at);
    let current = if Float.( > ) !peak 0. then (!peak -. last) /. !peak else 0. in
    Some (Float.min !max_dd 1., days !longest, Float.clamp_exn current ~min:0. ~max:1.))
;;

let compute ?(config = Config.default) (tr : Track_record.t) =
  let span = Time_ns.diff tr.until tr.from in
  let span_days = days span in
  let bar = Option.value config.bar ~default:(auto_bar ~span) in
  let bars = Track_record.bars tr ~period:bar in
  let r = returns_of_bars bars in
  let ppy = Time_ns.Span.( // ) year bar in
  let values = Metric.Table.create () in
  let set m v = Option.iter v ~f:(fun v -> if Float.is_finite v then Hashtbl.set values ~key:m ~data:v) in
  (* returns and risk *)
  let e0 = if Array.is_empty bars then None else Some bars.(0) in
  let en = if Array.is_empty bars then None else Some bars.(Array.length bars - 1) in
  let total_return =
    match e0, en with
    | Some a, Some b when Float.( > ) a 0. -> Some ((b /. a) -. 1.)
    | _ -> None
  in
  set Total_return total_return;
  let cagr =
    match total_return with
    | Some tr when Float.( >= ) span_days 7. ->
      if Float.( <= ) (1. +. tr) 0.
      then Some (-1.)
      else finite (((1. +. tr) **. (365. /. span_days)) -. 1.)
    | _ -> None
  in
  set Cagr cagr;
  let mean = Stats.mean r
  and sd = Stats.stdev r in
  set Volatility (Option.map sd ~f:(fun s -> s *. Float.sqrt ppy));
  (match mean, sd with
   | Some m, Some s when Float.( > ) s 0. -> set Sharpe (Some (m /. s *. Float.sqrt ppy))
   | _ -> ());
  (match mean with
   | Some m when Array.length r >= 2 ->
     let downside =
       Array.sum (module Float) r ~f:(fun x -> Float.min x 0. ** 2.)
       /. Float.of_int (Array.length r)
     in
     if Float.( > ) downside 0.
     then set Sortino (Some (m /. Float.sqrt downside *. Float.sqrt ppy))
   | _ -> ());
  let dd = drawdowns tr.equity ~until:tr.until in
  Option.iter dd ~f:(fun (mdd, longest, current) ->
    set Max_drawdown (Some mdd);
    set Max_drawdown_duration_days (Some longest);
    set Current_drawdown (Some current);
    Option.iter cagr ~f:(fun c -> set Calmar (ratio c mdd));
    Option.iter total_return ~f:(fun t -> set Return_over_drawdown (ratio t mdd)));
  set Worst_bar (Array.min_elt r ~compare:Float.compare);
  (match Stats.quantile r 0.05 with
   | Some q ->
     set Var_95 (Some (-.q));
     let tail = Array.filter r ~f:(fun x -> Float.( <= ) x q) in
     set Cvar_95 (Option.map (Stats.mean tail) ~f:Float.neg)
   | None -> ());
  let w =
    Option.value config.window_bars ~default:(auto_window ~bar ~bars:(Array.length bars))
  in
  if Array.length bars > w
  then (
    let windows =
      Array.init (Array.length bars - w) ~f:(fun i ->
        let a = bars.(i)
        and b = bars.(i + w) in
        if Float.( > ) a 0. then Some ((b /. a) -. 1.) else None)
      |> Array.filter_opt
    in
    if not (Array.is_empty windows)
    then (
      let positive = Array.count windows ~f:(fun x -> Float.( > ) x 0.) in
      set Rolling_positive_share
        (Some (Float.of_int positive /. Float.of_int (Array.length windows)));
      set Rolling_worst_return (Array.min_elt windows ~compare:Float.compare)));
  (* trades *)
  let closed =
    List.filter tr.trades ~f:(fun (t : Track_record.Trade.t) ->
      match t.closed_at with
      | Some c -> Time_ns.between c ~low:tr.from ~high:tr.until
      | None -> false)
  in
  let n_closed = List.length closed in
  set Trades (Some (Float.of_int n_closed));
  let nets = List.map closed ~f:Track_record.Trade.net_pnl in
  let wins = List.filter nets ~f:(fun x -> Float.( > ) x 0.)
  and losses = List.filter nets ~f:(fun x -> Float.( < ) x 0.) in
  if n_closed > 0
  then (
    set Win_rate (Some (Float.of_int (List.length wins) /. Float.of_int n_closed));
    set
      Avg_hold_hours
      (Some
         (List.sum
            (module Float)
            closed
            ~f:(fun t -> Time_ns.Span.to_hr (Track_record.Trade.hold t ~until:tr.until))
          /. Float.of_int n_closed)));
  let sum = List.sum (module Float) ~f:Fn.id in
  set Profit_factor (ratio (sum wins) (Float.abs (sum losses)));
  (match wins, losses with
   | _ :: _, _ :: _ ->
     let avg l = sum l /. Float.of_int (List.length l) in
     set Payoff_ratio (ratio (avg wins) (Float.abs (avg losses)))
   | _ -> ());
  let opened =
    List.count tr.trades ~f:(fun t -> Time_ns.between t.opened_at ~low:tr.from ~high:tr.until)
  in
  if Float.( > ) span_days 0.
  then set Trades_per_day (Some (Float.of_int opened /. span_days));
  (* leverage and exposure *)
  let lev = Track_record.leverage_samples tr in
  let weighted = Array.map lev ~f:(fun (d, l) -> l, Time_ns.Span.to_sec d) in
  set Leverage_median (Stats.weighted_quantile weighted 0.5);
  set Leverage_p95 (Stats.weighted_quantile weighted 0.95);
  set Leverage_max (Array.max_elt (Array.map lev ~f:snd) ~compare:Float.compare);
  let covered = Time_ns.Span.to_sec span in
  let n_points = Array.length tr.points in
  let durations =
    Array.mapi tr.points ~f:(fun i (p : Track_record.Point.t) ->
      let next = if i + 1 < n_points then tr.points.(i + 1).time else tr.until in
      p, Time_ns.Span.to_sec (Time_ns.diff next p.time))
  in
  if Float.( > ) covered 0.
  then (
    let exposed =
      Array.sum (module Float) durations ~f:(fun ((p : Track_record.Point.t), d) ->
        if p.open_positions > 0 then d else 0.)
    in
    set Time_in_market (Some (Float.min 1. (exposed /. covered))));
  let bias (sel : (Track_record.Point.t * float) array) =
    let num = ref 0.
    and den = ref 0. in
    Array.iter sel ~f:(fun (p, d) ->
      if Float.( > ) p.gross 0.
      then (
        num := !num +. (p.net /. p.gross *. d);
        den := !den +. d));
    if Float.( > ) !den 0. then Some (!num /. !den) else None
  in
  set Long_bias (bias durations);
  let mid = Time_ns.add tr.from (Time_ns.Span.scale span 0.5) in
  (match
     ( bias (Array.filter durations ~f:(fun (p, _) -> Time_ns.( < ) p.time mid))
     , bias (Array.filter durations ~f:(fun (p, _) -> Time_ns.( >= ) p.time mid)) )
   with
   | Some a, Some b -> set Bias_drift (Some (Float.abs (a -. b)))
   | _ -> ());
  let flips =
    Array.fold tr.points ~init:(0, 0) ~f:(fun (flips, last) (p : Track_record.Point.t) ->
      let s = if Float.( > ) p.net 0. then 1 else if Float.( < ) p.net 0. then -1 else 0 in
      if s = 0 then flips, last else if last <> 0 && s <> last then flips + 1, s else flips, s)
    |> fst
  in
  if Float.( > ) span_days 0.
  then set Bias_flips_per_30d (Some (Float.of_int flips *. 30. /. span_days));
  (* concentration *)
  let total_notional = List.sum (module Float) tr.notional_by_coin ~f:snd in
  set Coins (Some (Float.of_int (List.length tr.notional_by_coin)));
  if Float.( > ) total_notional 0.
  then (
    let shares = List.map tr.notional_by_coin ~f:(fun (_, n) -> n /. total_notional) in
    set Top_coin_share (List.max_elt shares ~compare:Float.compare);
    set Coin_hhi (Some (List.sum (module Float) shares ~f:(fun s -> s *. s))));
  (* costs and turnover *)
  let mean_equity = Stats.mean (Array.map tr.equity ~f:(fun p -> p.equity)) in
  (match mean_equity with
   | Some m when Float.( > ) m 0. && Float.( > ) span_days 0. ->
     set Turnover_per_day (Some (tr.turnover /. m /. span_days))
   | _ -> ());
  if Float.( > ) tr.initial_equity 0.
  then set Fees_to_equity (Some (tr.fees /. tr.initial_equity));
  let oi_shares =
    List.filter_map tr.trades ~f:(fun t ->
      match Map.find config.open_interest t.coin with
      | Some oi when Float.( > ) oi 0. -> Some (t.max_notional /. oi)
      | _ -> None)
  in
  set Oi_share_max (List.max_elt oi_shares ~compare:Float.compare);
  let values =
    List.map Metric.all ~f:(fun m -> m, Hashtbl.find values m) |> Metric.Map.of_alist_exn
  in
  { values; bar; returns = r }
;;

let get t m = Map.find t.values m |> Option.join
let to_alist t = List.map Metric.all ~f:(fun m -> m, get t m)
let bar t = t.bar
let periods_per_year t = Time_ns.Span.( // ) year t.bar
let returns t = t.returns

let to_json t =
  `Assoc
    (List.map (to_alist t) ~f:(fun (m, v) ->
       ( Metric.name m
       , match v with
         | Some x -> `Float x
         | None -> `Null )))
;;

let of_alist ?(returns = [||]) ~bar alist =
  let given = Metric.Map.of_alist_reduce alist ~f:(fun _ b -> b) in
  { values = List.map Metric.all ~f:(fun m -> m, Option.join (Map.find given m)) |> Metric.Map.of_alist_exn
  ; bar
  ; returns
  }
;;

let of_json ~bar = function
  | `Assoc fields ->
    Ok
      (of_alist
         ~bar
         (List.filter_map fields ~f:(fun (name, v) ->
            Option.map (Metric.of_name name) ~f:(fun m ->
              ( m
              , match v with
                | `Float x -> Some x
                | `Int x -> Some (Float.of_int x)
                | `Intlit s -> Float.of_string_opt s
                | _ -> None )))))
  | json -> Or_error.errorf "metrics: expected an object, got %s" (Yojson.Safe.to_string json)
;;
