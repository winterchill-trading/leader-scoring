open! Core

type t =
  { record : Track_record.t
  ; metrics : Metrics.t
  ; predicted : float option
  ; copyability : Copyability.t option
  ; alerts : Alerts.Alert.t list
  ; max_cost_bps : float
  }

let alerts_config (tr : Track_record.t) =
  let d = Alerts.Config.default in
  let span = Time_ns.diff tr.until tr.from in
  if Time_ns.Span.( >= ) span (Time_ns.Span.( + ) d.baseline d.recent)
  then d
  else Alerts.Config.scaled ~span
;;

let create
  ?books
  ?(open_interest = String.Map.empty)
  ?model
  ?(copy_config = Copyability.Config.default)
  history
  =
  let record = Track_record.build history in
  let metrics =
    Metrics.compute ~config:{ Metrics.Config.default with open_interest } record
  in
  { record
  ; metrics
  ; predicted = Option.map model ~f:(fun m -> Model.predict_metrics m metrics)
  ; copyability =
      Option.bind books ~f:(fun books ->
        if Map.is_empty books
        then None
        else Some (Copyability.estimate ~config:copy_config ~books record))
  ; alerts = Alerts.scan (alerts_config record) record
  ; max_cost_bps = copy_config.max_cost_bps
  }
;;

let rank ?(config = Score.Config.default) reports =
  let by_account =
    List.map reports ~f:(fun r -> r.record.account, r) |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
  in
  Score.rank
    config
    (List.map reports ~f:(fun r ->
       ( r.record.account
       , r.metrics
       , Time_ns.Span.to_day (Time_ns.diff r.record.until r.record.from) )))
  |> List.map ~f:(fun (ranked : Score.Ranked.t) -> Map.find_exn by_account ranked.account, ranked)
;;

let ms t = `Int (Time_ns.to_int_ns_since_epoch t / 1_000_000)

let num x = if Float.is_finite x then `Float x else `Null

let opt = function
  | Some x -> num x
  | None -> `Null
;;

let thin (points : Equity_point.t array) ~max_points =
  let n = Array.length points in
  if n <= max_points || max_points < 2
  then points
  else
    Array.init max_points ~f:(fun i ->
      points.(i * (n - 1) / (max_points - 1)))
;;

let metric_json metric value =
  `Assoc
    [ "name", `String (Metric.name metric)
    ; "label", `String (Metric.label metric)
    ; "value", opt value
    ; "unit", `String (Metric.Unit.to_string (Metric.unit metric))
    ; "better", `String (Metric.Better.to_string (Metric.better metric))
    ]
;;

let score_json (r : Score.Ranked.t) =
  `Assoc
    [ "value", num r.score
    ; "rank", Option.value_map r.rank ~default:`Null ~f:(fun i -> `Int i)
    ; "percentile", opt r.percentile
    ; "eligible", `Bool r.eligible
    ; "reasons", `List (List.map r.reasons ~f:(fun s -> `String s))
    ; ( "components"
      , `List
          (List.map r.components ~f:(fun c ->
             `Assoc
               [ "metric", `String (Metric.name c.metric)
               ; "label", `String (Metric.label c.metric)
               ; "value", opt c.value
               ; "percentile", num c.percentile
               ; "weight", num c.weight
               ; "contribution", num c.contribution
               ])) )
    ]
;;

let capacity_json (c : Copyability.t) ~max_cost_bps =
  `Assoc
    [ "max_cost_bps", num max_cost_bps
    ; "capacity_usd", opt c.capacity
    ; "orders", `Int c.orders
    ; "coins_without_book", `List (List.map c.coins_without_book ~f:(fun s -> `String s))
    ; ( "curve"
      , `List
          (List.map c.curve ~f:(fun p ->
             `Assoc
               [ "follower_equity", num p.follower_equity
               ; "orders", `Int p.orders
               ; "mean_cost_bps", num p.mean_cost_bps
               ; "median_cost_bps", num p.median_cost_bps
               ; "p90_cost_bps", num p.p90_cost_bps
               ; "unfilled_share", num p.unfilled_share
               ])) )
    ]
;;

let alert_json (a : Alerts.Alert.t) =
  `Assoc
    [ "kind", `String (Alerts.Kind.to_string a.kind)
    ; "severity", `String (Alerts.Severity.to_string a.severity)
    ; "time_ms", ms a.time
    ; "message", `String a.message
    ; "value", num a.value
    ; "threshold", num a.threshold
    ]
;;

let to_json ?(max_points = 400) t ranked =
  let tr = t.record in
  `Assoc
    [ "account", `String tr.account
    ; "equity_source", `String (Track_record.Equity_source.to_string tr.equity_source)
    ; "from_ms", ms tr.from
    ; "until_ms", ms tr.until
    ; "fills", `Int tr.fills
    ; "fills_truncated", `Bool tr.fills_truncated
    ; "bar_seconds", `Float (Time_ns.Span.to_sec (Metrics.bar t.metrics))
    ; "score", Option.value_map ranked ~default:`Null ~f:score_json
    ; "predicted_forward_sharpe", opt t.predicted
    ; ( "metrics"
      , `List (List.map (Metrics.to_alist t.metrics) ~f:(fun (m, v) -> metric_json m v)) )
    ; ( "equity_curve"
      , `List
          (thin tr.equity ~max_points
           |> Array.to_list
           |> List.map ~f:(fun (p : Equity_point.t) -> `List [ ms p.time; num p.equity ])) )
    ; ( "capacity"
      , Option.value_map t.copyability ~default:`Null ~f:(fun c ->
          capacity_json c ~max_cost_bps:t.max_cost_bps) )
    ; "alerts", `List (List.map t.alerts ~f:alert_json)
    ]
;;

let catalog_json () =
  `List
    (List.map Metric.all ~f:(fun m ->
       `Assoc
         [ "name", `String (Metric.name m)
         ; "label", `String (Metric.label m)
         ; "unit", `String (Metric.Unit.to_string (Metric.unit m))
         ; "better", `String (Metric.Better.to_string (Metric.better m))
         ; "formula", `String (Metric.formula m)
         ]))
;;
