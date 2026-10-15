open! Core

let feature_metrics =
  List.filter Metric.all ~f:(function
    | Cagr | Calmar | Oi_share_max -> false
    | _ -> true)
;;

let feature_names = List.map feature_metrics ~f:Metric.name

let features m =
  Array.of_list_map feature_metrics ~f:(fun metric ->
    Option.value (Metrics.get m metric) ~default:Float.nan)
;;

let label (tr : Track_record.t) ~from ~until =
  let w = Track_record.window tr ~from ~until in
  let bars = Track_record.bars w ~period:Time_ns.Span.day in
  if Array.exists bars ~f:(fun e -> Float.( <= ) e 0.)
  then None
  else (
    let n = Array.length bars in
    let r =
      Array.init (Int.max 0 (n - 1)) ~f:(fun i -> (bars.(i + 1) /. bars.(i)) -. 1.)
    in
    if Array.length r < 10
    then None
    else (
      let mean = Option.value_exn (Stats.mean r) in
      let sd = Option.value_exn (Stats.stdev r) in
      let y = mean /. Float.max sd 0.001 *. Float.sqrt (Float.of_int (Array.length r)) in
      Some (Float.clamp_exn y ~min:(-5.) ~max:5.)))
;;

module Row = struct
  type t =
    { group : string
    ; account : string
    ; cutoff : Time_ns.t
    ; features : float array
    ; label : float option
    }

  let csv_header =
    String.concat
      ~sep:","
      ([ "group"; "account"; "cutoff_ms" ] @ feature_names @ [ "label" ])
  ;;

  let num x = if Float.is_nan x then "" else sprintf "%.17g" x

  let to_csv t =
    String.concat
      ~sep:","
      ([ t.group
       ; t.account
       ; Int.to_string (Time_ns.to_int_ns_since_epoch t.cutoff / 1_000_000)
       ]
       @ Array.to_list (Array.map t.features ~f:num)
       @ [ Option.value_map t.label ~default:"" ~f:num ])
  ;;
end

let rows
  ?(lookback = Time_ns.Span.of_day 90.)
  ?(horizon = Time_ns.Span.of_day 30.)
  ?(step = Time_ns.Span.of_day 15.)
  ~group
  (tr : Track_record.t)
  =
  let config = { Metrics.Config.default with bar = Some Time_ns.Span.day } in
  let rec go cutoff acc =
    if Time_ns.( > ) (Time_ns.add cutoff horizon) tr.until
    then List.rev acc
    else (
      let past =
        Track_record.window tr ~from:(Time_ns.sub cutoff lookback) ~until:cutoff
      in
      let row =
        { Row.group
        ; account = tr.account
        ; cutoff
        ; features = features (Metrics.compute ~config past)
        ; label = label tr ~from:cutoff ~until:(Time_ns.add cutoff horizon)
        }
      in
      go (Time_ns.add cutoff step) (row :: acc))
  in
  (* Cutoffs sit on one grid for every account (multiples of [step] since the epoch), so
     the rows of one cutoff form a cross-section across leaders. *)
  let first =
    let earliest = Time_ns.to_int_ns_since_epoch (Time_ns.add tr.from lookback) in
    let step_ns = Time_ns.Span.to_int_ns step in
    Time_ns.of_int_ns_since_epoch ((earliest + step_ns - 1) / step_ns * step_ns)
  in
  go first []
;;
