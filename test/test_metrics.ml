open! Core
open Leader_scoring
open Helpers

let print_metrics m =
  List.iter (Metrics.to_alist m) ~f:(fun (metric, v) ->
    printf
      "%-28s %s\n"
      (Metric.name metric)
      (match v with
       | None -> "-"
       | Some x -> Float.to_string (round x)))
;;

let%expect_test "a 60-day venue curve and four round trips" =
  (* equity grows 1% a day except a 10% fall on day 20 and 3% losses on days 40-42 *)
  let equity_curve =
    List.init 61 ~f:(fun d -> d)
    |> List.folding_map ~init:10_000. ~f:(fun e d ->
      let r =
        if d = 0
        then 0.
        else if d = 20
        then -0.10
        else if d >= 40 && d <= 42
        then -0.03
        else 0.01
      in
      let e = e *. (1. +. r) in
      e, { Equity_point.time = at (Float.of_int d); equity = e })
  in
  let fills =
    [ fill ~time:(at 1.) ~coin:"BTC" ~px:100. ~fee:1. 50.
    ; fill ~time:(at 3.) ~coin:"BTC" ~px:110. ~fee:1. (-50.)
    ; fill ~time:(at 10.) ~coin:"ETH" ~px:10. ~fee:1. (-100.)
    ; fill ~time:(at 11.) ~coin:"ETH" ~px:11. ~fee:1. 100.
    ; fill ~time:(at 30.) ~coin:"BTC" ~px:100. ~fee:1. 20.
    ; fill ~time:(at 31.) ~coin:"BTC" ~px:130. ~fee:1. (-20.)
    ; fill ~time:(at 50.) ~coin:"BTC" ~px:100. ~fee:1. (-10.)
    ; fill ~time:(at 55.) ~coin:"BTC" ~px:90. ~fee:1. 10.
    ]
  in
  let tr = Track_record.build (history ~equity_curve fills) in
  let m =
    Metrics.compute
      ~config:
        { Metrics.Config.default with
          open_interest = String.Map.of_alist_exn [ "BTC", 100_000. ]
        }
      tr
  in
  print_s [%message "" ~bar:(Metrics.bar m : Time_ns.Span.t)];
  print_metrics m;
  [%expect
    {|
    (bar 1d)
    total_return                 0.434018
    cagr                         -
    volatility                   0.314796
    sharpe                       7.150135
    sortino                      8.097868
    calmar                       -
    return_over_drawdown         4.340181
    max_drawdown                 0.1
    max_drawdown_duration_days   13.
    current_drawdown             0.
    trades                       4.
    win_rate                     0.75
    profit_factor                11.705882
    payoff_ratio                 3.901961
    avg_hold_hours               54.
    trades_per_day               0.066667
    leverage_median              0.
    leverage_p95                 0.16652
    leverage_max                 0.49505
    coins                        2.
    top_coin_share               0.890052
    coin_hhi                     0.804282
    worst_bar                    -0.1
    var_95                       0.03
    cvar_95                      0.0475
    rolling_positive_share       0.740741
    rolling_worst_return         -0.050269
    time_in_market               0.15
    long_bias                    -0.333333
    bias_flips_per_30d           1.5
    bias_drift                   1.
    turnover_per_day             0.026448
    fees_to_equity               0.0008
    oi_share_max                 0.05
    |}]
;;

let close ~what a b =
  match a, b with
  | None, None -> true
  | Some a, Some b ->
    Float.( <= )
      (Float.abs (a -. b))
      ((1e-6 *. Float.max (Float.abs a) (Float.abs b)) +. 1e-9)
  | _ ->
    print_s
      [%message
        "defined on one side only" (what : string) (a : float option) (b : float option)];
    false
;;

let same_metrics a b =
  List.for_all2_exn (Metrics.to_alist a) (Metrics.to_alist b) ~f:(fun (m, x) (_, y) ->
    close ~what:(Metric.name m) x y)
;;

let metrics h = Metrics.compute (Track_record.build h)

let%test_unit "scaling every size and amount by k leaves every metric unchanged" =
  Quickcheck.test
    ~trials:200
    (Quickcheck.Generator.both Gen.history (Float.gen_incl 0.01 100.))
    ~f:(fun (h, k) -> assert (same_metrics (metrics h) (metrics (Gen.scale h k))))
;;

let%test_unit "moving the whole history in time leaves every metric unchanged" =
  Quickcheck.test
    ~trials:100
    (Quickcheck.Generator.both Gen.history (Int.gen_incl (-400) 400))
    ~f:(fun (h, days) ->
      assert (
        same_metrics (metrics h) (metrics (Gen.shift h (Time_ns.Span.of_int_day days)))))
;;

let%test_unit "bounds" =
  Quickcheck.test ~trials:300 Gen.history ~f:(fun h ->
    let m = metrics h in
    let within metric lo hi =
      match Metrics.get m metric with
      | None -> ()
      | Some v ->
        if Float.( < ) v (lo -. 1e-12) || Float.( > ) v (hi +. 1e-12)
        then raise_s [%message "out of bounds" (metric : Metric.t) (v : float)]
    in
    within Max_drawdown 0. 1.;
    within Current_drawdown 0. 1.;
    within Win_rate 0. 1.;
    within Time_in_market 0. 1.;
    within Long_bias (-1.) 1.;
    within Top_coin_share 0. 1.;
    within Coin_hhi 0. 1.;
    within Rolling_positive_share 0. 1.;
    within Volatility 0. Float.infinity;
    within Leverage_median 0. Float.infinity;
    (match Metrics.get m Var_95, Metrics.get m Cvar_95 with
     | Some var, Some cvar -> assert (Float.( >= ) cvar (var -. 1e-12))
     | _ -> ());
    match Metrics.get m Coin_hhi, Metrics.get m Coins with
    | Some hhi, Some n -> assert (Float.( >= ) hhi ((1. /. n) -. 1e-12))
    | _ -> ())
;;

(* Only a point below an earlier peak: lowering a peak itself can remove a drawdown. *)
let%test_unit "a deeper trough never lowers the max drawdown" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind n = Int.gen_incl 3 80 in
    let%bind es = List.gen_with_length n (Float.gen_incl 50. 150.) in
    let%bind i = Int.gen_incl 0 (n - 1) in
    let%map cut = Float.gen_incl 0. 0.9 in
    es, i, cut
  in
  Quickcheck.test ~trials:300 gen ~f:(fun (es, i, cut) ->
    let curve es =
      List.mapi es ~f:(fun d equity ->
        { Equity_point.time = at (Float.of_int d); equity })
    in
    let mdd es =
      Metrics.get (metrics (history ~equity_curve:(curve es) [])) Max_drawdown
      |> Option.value ~default:0.
    in
    let earlier_peak = List.take es i |> List.max_elt ~compare:Float.compare in
    match earlier_peak with
    | Some p when Float.( < ) (List.nth_exn es i) p ->
      let lowered = List.mapi es ~f:(fun j e -> if j = i then e *. (1. -. cut) else e) in
      assert (Float.( >= ) (mdd lowered) (mdd es -. 1e-12))
    | _ -> ())
;;

let%test_unit "an extra winning round trip never lowers win rate or profit factor" =
  Quickcheck.test ~trials:200 Gen.history ~f:(fun h ->
    let last = (List.last_exn h.fills).time in
    let win =
      [ fill ~time:(Time_ns.add last (Time_ns.Span.of_hr 2.)) ~coin:"WIN" ~px:10. 1.
      ; fill ~time:(Time_ns.add last (Time_ns.Span.of_hr 3.)) ~coin:"WIN" ~px:20. (-1.)
      ]
    in
    let before = metrics h
    and after = metrics { h with fills = h.fills @ win } in
    let ge metric =
      match Metrics.get before metric, Metrics.get after metric with
      | Some b, Some a -> assert (Float.( >= ) a (b -. 1e-12))
      | None, _ -> ()
      | Some _, None -> assert false
    in
    ge Win_rate;
    ge Profit_factor;
    ge Trades)
;;
