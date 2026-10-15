open! Core
open Leader_scoring
open Leader_scoring_hl

let load dir =
  Async.Thread_safe.block_on_async_exn (fun () -> Raw.load ~dir:("fixtures" ^/ dir))
  |> Or_error.ok_exn
;;

let summary ?(window = "month") dir =
  let raw = load dir in
  let h = Raw.to_history ~portfolio_window:window raw |> Or_error.ok_exn in
  let tr = Track_record.build h in
  let m = Metrics.compute tr in
  print_s
    [%message
      ""
        ~fills:(List.length h.fills : int)
        ~funding:(List.length h.funding : int)
        ~snapshot_positions:
          (List.concat_map h.snapshots ~f:(fun s ->
             List.map s.positions ~f:(fun p ->
               p.coin, Float.round_decimal p.size ~decimal_digits:4))
           : (string * float) list)
        ~curve_points:(List.length h.equity_curve : int)
        ~truncated:(h.fills_truncated : bool)
        ~source:(tr.equity_source : Track_record.Equity_source.t)
        ~trades:(List.length tr.trades : int)
        ~seeded:(List.count tr.trades ~f:(fun t -> t.seeded) : int)
        ~mismatches:(tr.position_mismatches : int)];
  List.iter
    [ Metric.Total_return; Max_drawdown; Sharpe; Win_rate; Leverage_p95; Long_bias ]
    ~f:(fun metric ->
      printf
        "%-14s %s\n"
        (Metric.name metric)
        (Option.value_map (Metrics.get m metric) ~default:"-" ~f:(sprintf "%.4f")))
;;

let%expect_test "a mainnet account (read-only recording)" =
  summary "mainnet-0x7737";
  [%expect
    {|
    ((fills 0) (funding 150)
     (snapshot_positions
      ((BTC -25.1552) (ETH -799.7919) (SOL -20384.06) (AVAX -171568.06)
       (BNB -2646.179) (XRP -1413466) (ADA -9091466) (NEAR 5756507.7)))
     (curve_points 45) (truncated false) (source Venue) (trades 8) (seeded 8)
     (mismatches 0))
    total_return   0.5682
    max_drawdown   0.1463
    sharpe         5.3396
    win_rate       -
    leverage_p95   1.3730
    long_bias      0.3744
    |}]
;;

let%expect_test "an hl-sim trend leader" =
  summary "sim-trend-0";
  [%expect
    {|
    ((fills 246) (funding 0) (snapshot_positions ((BTC -2.5014)))
     (curve_points 4) (truncated false) (source Anchored) (trades 1) (seeded 0)
     (mismatches 0))
    total_return   0.0298
    max_drawdown   0.0011
    sharpe         344.8943
    win_rate       -
    leverage_p95   2.5049
    long_bias      -1.0000
    |}]
;;
