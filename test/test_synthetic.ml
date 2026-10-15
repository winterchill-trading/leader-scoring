open! Core
open Leader_scoring

let%expect_test "synthetic leaders replay without position mismatches" =
  let universe = Synthetic.universe ~seed:1 ~start:Helpers.t0 ~leaders:6 ~days:120 in
  List.iter universe ~f:(fun (h, p) ->
    let tr = Track_record.build h in
    let m = Metrics.compute tr in
    let g metric = Option.value_map (Metrics.get m metric) ~default:"-" ~f:(sprintf "%.3f") in
    printf
      "%s mu=%+.3f lev=%4.1f mart=%b fills=%4d mism=%d src=%s ret=%s sharpe=%s mdd=%s lev95=%s\n"
      h.account
      p.mu
      p.leverage
      p.martingale
      tr.fills
      tr.position_mismatches
      (Track_record.Equity_source.to_string tr.equity_source)
      (g Total_return)
      (g Sharpe)
      (g Max_drawdown)
      (g Leverage_p95));
  [%expect {|
    synthetic-0000 mu=-0.028 lev= 1.0 mart=false fills=  70 mism=0 src=anchored ret=-0.041 sharpe=-0.895 mdd=0.128 lev95=0.264
    synthetic-0001 mu=+0.016 lev= 3.0 mart=false fills= 177 mism=0 src=anchored ret=0.265 sharpe=1.191 mdd=0.557 lev95=3.630
    synthetic-0002 mu=-0.022 lev= 2.0 mart=false fills=  49 mism=0 src=anchored ret=-0.034 sharpe=0.178 mdd=0.268 lev95=2.062
    synthetic-0003 mu=+0.019 lev= 1.0 mart=false fills=  42 mism=0 src=anchored ret=0.140 sharpe=2.117 mdd=0.070 lev95=0.508
    synthetic-0004 mu=-0.022 lev= 0.5 mart=false fills=  54 mism=0 src=anchored ret=-0.044 sharpe=-2.269 mdd=0.051 lev95=0.125
    synthetic-0005 mu=-0.021 lev= 5.0 mart=false fills=  72 mism=0 src=anchored ret=-0.920 sharpe=-3.168 mdd=0.940 lev95=5.908
    |}]
;;
