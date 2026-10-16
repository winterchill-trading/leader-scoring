open! Core
module Bs = Backtest_stats

module Config = struct
  type t =
    { bar : Time_ns.Span.t
    ; common_bars : int
    ; bootstrap_samples : int
    ; seed : int
    ; policy : Bs.Gate.Policy.t option
    }

  let default_policy =
    { Bs.Gate.Policy.permissive with
      min_psr = Some 0.95
    ; min_dsr = Some 0.95
    ; (* a ranking whose in-sample winner lands below the median out of sample more often
         than not is no better than a coin flip *)
      max_pbo = Some 0.5
    }
  ;;

  let default =
    { bar = Time_ns.Span.day
    ; common_bars = 90
    ; bootstrap_samples = 500
    ; seed = 0
    ; policy = Some default_policy
    }
  ;;
end

module Universe = struct
  type t =
    { leaders : int
    ; bar : Time_ns.Span.t
    ; periods_per_year : float
    ; trials : Bs.Report.Trials.t option
    ; common_from : Time_ns.Alternate_sexp.t option
    ; common_until : Time_ns.Alternate_sexp.t option
    ; common_leaders : int
    ; common_periods : int
    ; pbo : Bs.Multiple_testing.Pbo.t option
    ; spa : Bs.Multiple_testing.Spa.t option
    ; spa_best : string option
    ; reality_check : Bs.Multiple_testing.Reality_check.t option
    ; accepted : int
    }
  [@@deriving sexp_of]
end

type t =
  { universe : Universe.t
  ; reports : Bs.Report.t String.Map.t
  }

let simple_returns e =
  (* a rebuilt curve can cross zero (losses beyond the equity, or an assumed starting
     equity); backtest-stats takes returns above -1, so a wipe-out counts as -99.99% and
     the bars after it, from a non-positive equity, are left out *)
  Array.filter_mapi e ~f:(fun i x ->
    if i = 0 || Float.( <= ) e.(i - 1) 0. || not (Float.is_finite x)
    then None
    else Some (Float.max (-0.9999) ((x /. e.(i - 1)) -. 1.)))
;;

let returns (r : Track_record.t) ~bar = simple_returns (Track_record.bars r ~period:bar)

let common_window (config : Config.t) records =
  (* the median end, so one record that stops early does not cut every other short *)
  let untils =
    List.map records ~f:(fun (r : Track_record.t) -> r.until)
    |> List.sort ~compare:Time_ns.compare
    |> Array.of_list
  in
  if Array.is_empty untils
  then None
  else (
    let until = untils.(Array.length untils / 2) in
    let from = Time_ns.sub until (Time_ns.Span.scale_int config.bar config.common_bars) in
    let rows =
      List.filter_map records ~f:(fun (r : Track_record.t) ->
        if Time_ns.( <= ) r.from from && Time_ns.( >= ) r.until until
        then (
          let e =
            Array.init (config.common_bars + 1) ~f:(fun k ->
              Track_record.equity_at
                r
                (Time_ns.add from (Time_ns.Span.scale_int config.bar k))
              |> Option.value ~default:Float.nan)
          in
          if Array.for_all e ~f:(fun x -> Float.is_finite x && Float.( > ) x 0.)
          then
            Some
              ( r.account
              , Array.init config.common_bars ~f:(fun k -> (e.(k + 1) /. e.(k)) -. 1.) )
          else None)
        else None)
    in
    Some (from, until, rows))
;;

let pbo_splits periods =
  (* blocks of at least four bars, at most 16 blocks (12870 combinations) *)
  let s = Int.min 16 (periods / 4) in
  s - (s % 2)
;;

let run ?(config = Config.default) records =
  let periods_per_year = Time_ns.Span.( // ) (Time_ns.Span.of_day 365.25) config.bar in
  let series =
    List.filter_map records ~f:(fun (r : Track_record.t) ->
      let rs = returns r ~bar:config.bar in
      if Array.length rs < 3 then None else Some (r, rs))
  in
  let finite_rows =
    List.filter_map series ~f:(fun (_, rs) ->
      if Float.is_finite (Bs.Metrics.sharpe rs) then Some rs else None)
    |> Array.of_list
  in
  let trials =
    if Array.length finite_rows < 2
    then None
    else
      Some
        { Bs.Report.Trials.count = List.length series
        ; sharpe_variance = Bs.Metrics.trials_sharpe_variance finite_rows
        }
  in
  let rng = Random.State.make [| config.seed |] in
  let common = common_window config records in
  let rows = Option.value_map common ~default:[] ~f:(fun (_, _, rows) -> rows) in
  let matrix = Array.of_list (List.map rows ~f:snd) in
  let splits = pbo_splits config.common_bars in
  let pbo =
    if Array.length matrix >= 2 && splits >= 2
    then Option.try_with (fun () -> Bs.Multiple_testing.Pbo.run ~splits matrix)
    else None
  in
  let spa, reality_check =
    if Array.length matrix >= 2
    then
      ( Option.try_with (fun () ->
          Bs.Multiple_testing.Spa.run ~samples:config.bootstrap_samples ~rng matrix)
      , Option.try_with (fun () ->
          Bs.Multiple_testing.Reality_check.run
            ~samples:config.bootstrap_samples
            ~rng
            matrix) )
    else None, None
  in
  let with_universe (r : Bs.Report.t) =
    let evidence =
      { r.evidence with
        pbo = Option.map pbo ~f:(fun p -> p.pbo)
      ; spa_p_value = Option.map spa ~f:(fun s -> s.p_value_consistent)
      }
    in
    { r with
      pbo = Option.map pbo ~f:(fun p -> p.pbo)
    ; spa
    ; reality_check_p_value = Option.map reality_check ~f:(fun c -> c.p_value)
    ; evidence
    ; verdict = Option.map config.policy ~f:(fun p -> Bs.Gate.evaluate p evidence)
    }
  in
  let reports =
    List.filter_map series ~f:(fun ((r : Track_record.t), rs) ->
      let s =
        Bs.Series.of_returns ~start:(Time_ns.add r.from config.bar) ~step:config.bar rs
      in
      match
        Bs.Report.create
          ~name:r.account
          ?trials
          ~bootstrap_samples:config.bootstrap_samples
          ~seed:config.seed
          ?policy:config.policy
          ~periods_per_year
          s
      with
      | Ok report -> Some (r.account, with_universe report)
      | Error _ -> None)
    |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
  in
  { universe =
      { leaders = Map.length reports
      ; bar = config.bar
      ; periods_per_year
      ; trials
      ; common_from = Option.map common ~f:(fun (f, _, _) -> f)
      ; common_until = Option.map common ~f:(fun (_, u, _) -> u)
      ; common_leaders = Array.length matrix
      ; common_periods = (if Array.is_empty matrix then 0 else config.common_bars)
      ; pbo
      ; spa
      ; spa_best = Option.bind spa ~f:(fun s -> List.nth rows s.best |> Option.map ~f:fst)
      ; reality_check
      ; accepted =
          Map.count reports ~f:(fun r ->
            Option.value_map r.verdict ~default:false ~f:Bs.Gate.Verdict.is_accept)
      }
  ; reports
  }
;;

let num x = if Float.is_finite x then `Float x else `Null

let opt = function
  | Some x -> num x
  | None -> `Null
;;

let ms = function
  | Some t -> `Int (Time_ns.to_int_ns_since_epoch t / 1_000_000)
  | None -> `Null
;;

let universe_json (u : Universe.t) =
  `Assoc
    [ "leaders", `Int u.leaders
    ; "bar_days", num (Time_ns.Span.to_day u.bar)
    ; "periods_per_year", num u.periods_per_year
    ; ( "trials"
      , match u.trials with
        | None -> `Null
        | Some t ->
          `Assoc [ "count", `Int t.count; "sharpe_variance", num t.sharpe_variance ] )
    ; "common_from_ms", ms u.common_from
    ; "common_until_ms", ms u.common_until
    ; "common_leaders", `Int u.common_leaders
    ; "common_periods", `Int u.common_periods
    ; "pbo", opt (Option.map u.pbo ~f:(fun p -> p.pbo))
    ; ( "pbo_combinations"
      , Option.value_map u.pbo ~default:`Null ~f:(fun p -> `Int p.combinations) )
    ; "pbo_degradation_slope", opt (Option.map u.pbo ~f:(fun p -> p.degradation_slope))
    ; ( "pbo_probability_of_loss"
      , opt (Option.map u.pbo ~f:(fun p -> p.probability_of_loss)) )
    ; "spa_p_value", opt (Option.map u.spa ~f:(fun s -> s.p_value_consistent))
    ; "spa_best", Option.value_map u.spa_best ~default:`Null ~f:(fun s -> `String s)
    ; "reality_check_p_value", opt (Option.map u.reality_check ~f:(fun c -> c.p_value))
    ; "accepted", `Int u.accepted
    ]
;;

let leader_json (r : Bs.Report.t) =
  `Assoc
    [ "periods", `Int r.periods
    ; "periods_per_year", num r.periods_per_year
    ; "sharpe_annualized", num r.sharpe_annualized
    ; "psr", num r.psr
    ; "dsr", opt r.dsr
    ; "min_track_record_periods", opt r.min_track_record_length
    ; "skewness", num r.skewness
    ; "kurtosis", num r.kurtosis
    ; "pbo", opt r.pbo
    ; "spa_p_value", opt (Option.map r.spa ~f:(fun s -> s.p_value_consistent))
    ; ( "accepted"
      , Option.value_map r.verdict ~default:`Null ~f:(fun v ->
          `Bool (Bs.Gate.Verdict.is_accept v)) )
    ; ( "reasons"
      , `List
          (match r.verdict with
           | Some (Reject reasons) ->
             List.map reasons ~f:(fun x -> `String (Bs.Gate.Reason.to_string_hum x))
           | Some Accept | None -> []) )
    ]
;;
