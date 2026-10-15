(* Cost of scoring one leader: replaying its history, the metrics, a ranking share, the
   learned model, copyability and alerts. A synthetic leader with 400 days of daily
   snapshots stands in for a real one (sizes below). *)

open! Core
open Core_bench
open Leader_scoring

let start = Time_ns.of_string_with_utc_offset "2025-01-01T00:00:00Z"

let busiest =
  Synthetic.universe ~seed:7 ~start ~leaders:40 ~days:400
  |> List.max_elt ~compare:(fun (a, _) (b, _) ->
    Int.compare (List.length a.History.fills) (List.length b.History.fills))
  |> Option.value_exn
  |> fst
;;

let record = Track_record.build busiest
let metrics = Metrics.compute record

let universe =
  Synthetic.universe ~seed:9 ~start ~leaders:300 ~days:120
  |> List.map ~f:(fun (h, _) ->
    let tr = Track_record.build h in
    h.account, Metrics.compute tr, 120.)
;;

let book coin =
  let side sign =
    Array.init 50 ~f:(fun i ->
      { Copyability.Book.px = 100. +. (sign *. 0.01 *. Float.of_int (i + 1)); size = 100. })
  in
  { Copyability.Book.coin; bids = side (-1.); asks = side 1. }
;;

let books =
  List.map [ "BTC"; "ETH"; "SOL"; "HYPE"; "DOGE"; "XRP" ] ~f:(fun c -> c, book c)
  |> String.Map.of_alist_exn
;;

(* a model of the exported size: 200 trees of 7 leaves over the feature schema *)
let model =
  let n = List.length Dataset.feature_names in
  let tree k =
    let nodes = 13 in
    `Assoc
      [ "feature", `List (List.init nodes ~f:(fun i -> `Int (if i >= 6 then -1 else (i + k) % n)))
      ; "threshold", `List (List.init nodes ~f:(fun i -> `Float (Float.of_int i *. 0.1)))
      ; "missing_left", `List (List.init nodes ~f:(fun _ -> `Int 1))
      ; "left", `List (List.init nodes ~f:(fun i -> `Int (if i >= 6 then -1 else (2 * i) + 1)))
      ; "right", `List (List.init nodes ~f:(fun i -> `Int (if i >= 6 then -1 else (2 * i) + 2)))
      ; "value", `List (List.init nodes ~f:(fun i -> `Float (Float.of_int i *. 0.001)))
      ]
  in
  Model.of_json
    (`Assoc
        [ "format", `Int 1
        ; "name", `String "bench"
        ; "features", `List (List.map Dataset.feature_names ~f:(fun s -> `String s))
        ; "base_score", `Float 0.
        ; "trees", `List (List.init 200 ~f:tree)
        ; "evaluation", `Null
        ])
  |> Or_error.ok_exn
;;

let () =
  eprintf
    "leader: %d fills, %d snapshots, %d trades; universe of %d\n%!"
    (List.length busiest.fills)
    (List.length busiest.snapshots)
    (List.length record.trades)
    (List.length universe);
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"track_record_build" (fun () -> Track_record.build busiest)
       ; Bench.Test.create ~name:"metrics_compute" (fun () -> Metrics.compute record)
       ; Bench.Test.create ~name:"score_rank_300" (fun () ->
           Score.rank Score.Config.default universe)
       ; Bench.Test.create ~name:"model_predict" (fun () ->
           Model.predict_metrics model metrics)
       ; Bench.Test.create ~name:"copyability_estimate" (fun () ->
           Copyability.estimate ~books record)
       ; Bench.Test.create ~name:"alerts_detect" (fun () ->
           Alerts.detect Alerts.Config.default record ~now:record.until)
       ; Bench.Test.create ~name:"alerts_scan_weekly" (fun () ->
           Alerts.scan ~step:(Time_ns.Span.of_day 7.) Alerts.Config.default record)
       ; Bench.Test.create ~name:"report_create" (fun () -> Report.create ~books ~model busiest)
       ])
;;
