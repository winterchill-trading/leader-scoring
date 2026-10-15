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
       score predicted_forward_sharpe metrics equity_curve capacity alerts)))
    ((account synthetic-0000) (score 30) (curve_points 5) (metrics 34)
     (capacity 297776) (alerts 2)
     (keys
      (account equity_source from_ms until_ms fills fills_truncated bar_seconds
       score predicted_forward_sharpe metrics equity_curve capacity alerts)))
    (catalog 34)
    |}]
;;
