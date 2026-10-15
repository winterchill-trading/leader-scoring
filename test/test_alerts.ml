open! Core
open Leader_scoring
open Helpers

(* 40 days of one small BTC long a day on a 100k account, then a week of large DOGE
   shorts, a liquidation and a 30% fall. *)
let record =
  let calm =
    List.concat_map (List.range 0 40) ~f:(fun d ->
      let d = Float.of_int d in
      [ fill ~time:(at d) ~coin:"BTC" ~px:100. 100.
      ; fill ~time:(at ~h:6. d) ~coin:"BTC" ~px:101. (-100.)
      ])
  in
  let wild =
    List.concat_map (List.range 40 47) ~f:(fun d ->
      let d = Float.of_int d in
      [ fill ~time:(at d) ~coin:"DOGE" ~px:1. (-500_000.)
      ; fill
          ~time:(at ~h:12. d)
          ~coin:"DOGE"
          ~px:1.02
          ~liquidation:Float.(d = 46.)
          500_000.
      ])
  in
  let equity_curve =
    List.init 48 ~f:(fun d ->
      let equity =
        if d < 40
        then 100_000. +. (100. *. Float.of_int d)
        else 104_000. *. (1. -. (0.05 *. Float.of_int (d - 39)))
      in
      { Equity_point.time = at (Float.of_int d); equity })
  in
  Track_record.build (history ~equity_curve (calm @ wild))
;;

let print alerts =
  List.iter alerts ~f:(fun (a : Alerts.Alert.t) ->
    printf
      "%s %-8s %-16s %s\n"
      (Time_ns.to_string_iso8601_basic a.time ~zone:Time_float.Zone.utc
       |> fun s -> String.prefix s 10)
      (Alerts.Severity.to_string a.severity)
      (Alerts.Kind.to_string a.kind)
      a.message)
;;

let%expect_test "detect at the end of the wild week" =
  print (Alerts.detect Alerts.Config.default record ~now:(at 47.));
  [%expect
    {|
    2026-02-17 warning  drawdown_breach  39.9% below the peak; the baseline's worst was 0.0%
    2026-02-17 warning  leverage_spike   leverage reached 7.4x; baseline p95 0.1x
    2026-02-17 warning  style_drift      long bias moved from +1.00 to -1.00
    2026-02-17 info     style_drift      now trades mostly DOGE (100.0% of notional), not among BTC
    2026-02-17 critical liquidation      liquidated on DOGE
    |}]
;;

let%expect_test "nothing to report during the calm weeks" =
  print (Alerts.detect Alerts.Config.default record ~now:(at 38.));
  [%expect {| |}]
;;

let%expect_test "scan: when each alert turned on" =
  print (Alerts.scan ~step:Time_ns.Span.day Alerts.Config.default record);
  [%expect
    {|
    2026-02-10 info     style_drift      now trades mostly DOGE (78.0% of notional), not among BTC
    2026-02-11 warning  leverage_spike   leverage reached 5.1x; baseline p95 0.1x
    2026-02-15 warning  drawdown_breach  29.9% below the peak; the baseline's worst was 0.0%
    2026-02-17 critical liquidation      liquidated on DOGE
    |}]
;;
