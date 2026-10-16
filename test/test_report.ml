open! Core
open Leader_scoring

let%expect_test "report JSON of two synthetic leaders, ranked" =
  let universe = Synthetic.universe ~seed:3 ~start:Helpers.t0 ~leaders:2 ~days:60 in
  let book coin =
    let side sign =
      Array.init 20 ~f:(fun i ->
        { Copyability.Book.px = 100. +. (sign *. 0.05 *. Float.of_int (i + 1))
        ; size = 500.
        })
    in
    { Copyability.Book.coin; bids = side (-1.); asks = side 1. }
  in
  let books = String.Map.of_alist_exn [ "BTC", book "BTC"; "ETH", book "ETH" ] in
  let reports =
    List.map universe ~f:(fun (h, _) ->
      Report.create ~books ~model:(Lazy.force Model.bundled) h)
  in
  List.iter (Report.rank reports) ~f:(fun (r, ranked) ->
    let json = Report.to_json ~max_points:5 r (Some ranked) in
    let open Yojson.Safe.Util in
    let keys = keys json in
    print_s
      [%message
        ""
          ~account:(r.record.account : string)
          ~score:(Float.round_decimal ranked.score ~decimal_digits:2 : float)
          ~curve_points:(List.length (member "equity_curve" json |> to_list) : int)
          ~metrics:(List.length (member "metrics" json |> to_list) : int)
          ~capacity:
            (* rounded: the bisection goes through [10 ** x], whose last bits may differ
               between macOS and glibc *)
            (member "capacity" json
             |> member "capacity_usd"
             |> to_number
             |> Float.iround_nearest_exn
             : int)
          ~alerts:(List.length r.alerts : int)
          (keys : string list)]);
  print_s
    [%message
      "" ~catalog:(List.length (Yojson.Safe.Util.to_list (Report.catalog_json ())) : int)];
  [%expect
    {|
    ((account synthetic-0001) (score 70) (curve_points 5) (metrics 34)
     (capacity 377502) (alerts 2)
     (keys
      (account equity_source from_ms until_ms fills fills_truncated bar_seconds
       score predicted_forward_sharpe metrics equity_curve capacity alerts
       persistence stats integrity)))
    ((account synthetic-0000) (score 30) (curve_points 5) (metrics 34)
     (capacity 297776) (alerts 2)
     (keys
      (account equity_source from_ms until_ms fills fills_truncated bar_seconds
       score predicted_forward_sharpe metrics equity_curve capacity alerts
       persistence stats integrity)))
    (catalog 34)
    |}]
;;

let%expect_test "analyse: persistence, stats and integrity on every report; flagged \
                 leaders leave the ranking"
  =
  let universe = Synthetic.universe ~seed:3 ~start:Helpers.t0 ~leaders:12 ~days:120 in
  let reports = List.map universe ~f:(fun (h, _) -> Report.create h) in
  let flag account =
    { Integrity.Flag.account
    ; kind = Wash_trade
    ; severity = Warning
    ; message = "planted"
    ; evidence =
        Wash_trade
          { pairs = 9
          ; notional = 1.
          ; share_of_volume = 0.3
          ; window_ms = 1000
          ; examples = []
          }
    }
  in
  let integrity = String.Map.singleton "synthetic-0004" [ flag "synthetic-0004" ] in
  let universe, reports =
    Report.analyse
      ~persistence:
        { Persistence.Config.default with
          min_leaders = 5
        ; bootstrap_samples = 100
        ; random_draws = 100
        }
      ~stats:
        { Leader_stats.Config.default with bootstrap_samples = 100; common_bars = 60 }
      ~integrity
      reports
  in
  let json = Report.Universe.to_json universe in
  let open Yojson.Safe.Util in
  print_s
    [%message
      ""
        ~universe_keys:(keys json : string list)
        ~persistence_periods:
          (member "persistence" json |> member "periods" |> to_int : int)
        ~stats_leaders:(member "stats" json |> member "leaders" |> to_int : int)
        ~excluded:(universe.excluded : int)];
  let ranked = Report.rank reports in
  List.iter ranked ~f:(fun (r, s) ->
    if not s.eligible
    then
      print_s
        [%message
          r.record.account
            ~reasons:(s.reasons : string list)
            ~stats:(Option.is_some r.stats : bool)
            ~persistence:
              (Report.to_json r (Some s)
               |> member "persistence"
               |> member "windows"
               |> Yojson.Safe.to_string
               : string)]);
  [%expect
    {|
    ((universe_keys (persistence stats integrity)) (persistence_periods 2)
     (stats_leaders 12) (excluded 1))
    (synthetic-0004 (reasons ("integrity: planted")) (stats true)
     (persistence 3))
    |}]
;;
