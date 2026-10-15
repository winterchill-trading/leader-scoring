open! Core

module Kind = struct
  type t =
    | Drawdown_breach
    | Leverage_spike
    | Style_drift
    | Frequency_change
    | Liquidation
  [@@deriving sexp, compare, equal, enumerate]

  let to_string = function
    | Drawdown_breach -> "drawdown_breach"
    | Leverage_spike -> "leverage_spike"
    | Style_drift -> "style_drift"
    | Frequency_change -> "frequency_change"
    | Liquidation -> "liquidation"
  ;;
end

module Severity = struct
  type t =
    | Info
    | Warning
    | Critical
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Info -> "info"
    | Warning -> "warning"
    | Critical -> "critical"
  ;;
end

module Alert = struct
  type t =
    { kind : Kind.t
    ; severity : Severity.t
    ; time : Time_ns.Alternate_sexp.t
    ; message : string
    ; value : float
    ; threshold : float
    }
  [@@deriving sexp, compare, equal]
end

module Config = struct
  type t =
    { recent : Time_ns.Span.t
    ; baseline : Time_ns.Span.t
    ; drawdown_limit : float
    ; drawdown_vs_baseline : float
    ; leverage_floor : float
    ; leverage_vs_baseline : float
    ; bias_change : float
    ; frequency_ratio : float
    }
  [@@deriving sexp]

  let default =
    { recent = Time_ns.Span.of_day 7.
    ; baseline = Time_ns.Span.of_day 30.
    ; drawdown_limit = 0.25
    ; drawdown_vs_baseline = 1.5
    ; leverage_floor = 3.
    ; leverage_vs_baseline = 2.
    ; bias_change = 1.
    ; frequency_ratio = 3.
    }
  ;;

  let scaled ~span =
    { default with
      recent = Time_ns.Span.scale span (1. /. 6.)
    ; baseline = Time_ns.Span.scale span 0.5
    }
  ;;
end

let pct x = sprintf "%.1f%%" (100. *. x)

let detect (c : Config.t) (tr : Track_record.t) ~now =
  let recent_from = Time_ns.sub now c.recent in
  let base_from = Time_ns.sub recent_from c.baseline in
  let recent = Track_record.window tr ~from:recent_from ~until:now in
  (* the baseline stops just before the recent window, so an event at its first instant
     counts as recent only *)
  let base =
    Track_record.window
      tr
      ~from:base_from
      ~until:(Time_ns.sub recent_from (Time_ns.Span.of_int_ns 1))
  in
  let config bars =
    (* both windows on the same bar, chosen for the baseline's length *)
    { Metrics.Config.default with bar = Some (Metrics.auto_bar ~span:bars) }
  in
  let mc = config c.baseline in
  let rm = Metrics.compute ~config:mc recent
  and bm = Metrics.compute ~config:mc base in
  let get m metric = Metrics.get m metric in
  let alert ?(severity = Severity.Warning) kind message ~value ~threshold =
    { Alert.kind; severity; time = now; message; value; threshold }
  in
  let drawdown =
    (* from the running peak over the whole record up to now *)
    let upto = Track_record.window tr ~from:tr.from ~until:now in
    match get (Metrics.compute ~config:mc upto) Current_drawdown with
    | None -> None
    | Some dd ->
      let base_mdd = get bm Max_drawdown |> Option.value ~default:0. in
      let relative = c.drawdown_vs_baseline *. base_mdd in
      let threshold =
        if Float.( > ) base_mdd 0.
        then Float.min c.drawdown_limit relative
        else c.drawdown_limit
      in
      if Float.( >= ) dd threshold && Float.( > ) dd 0.
      then
        Some
          (alert
             ~severity:
               (if Float.( >= ) dd (2. *. c.drawdown_limit) then Critical else Warning)
             Drawdown_breach
             (sprintf
                "%s below the peak; the baseline's worst was %s"
                (pct dd)
                (pct base_mdd))
             ~value:dd
             ~threshold)
      else None
  in
  let leverage =
    match get rm Leverage_max, get bm Leverage_p95 with
    | Some recent_max, base ->
      let base = Option.value base ~default:0. in
      let threshold = Float.max c.leverage_floor (c.leverage_vs_baseline *. base) in
      if Float.( >= ) recent_max threshold
      then
        Some
          (alert
             Leverage_spike
             (sprintf "leverage reached %.1fx; baseline p95 %.1fx" recent_max base)
             ~value:recent_max
             ~threshold)
      else None
    | None, _ -> None
  in
  let bias =
    match get rm Long_bias, get bm Long_bias with
    | Some r, Some b when Float.( >= ) (Float.abs (r -. b)) c.bias_change ->
      Some
        (alert
           Style_drift
           (sprintf "long bias moved from %+.2f to %+.2f" b r)
           ~value:(Float.abs (r -. b))
           ~threshold:c.bias_change)
    | _ -> None
  in
  let coin_shift =
    match recent.notional_by_coin, base.notional_by_coin with
    | (top, n) :: _, (_ :: _ as base_coins) ->
      let total = List.sum (module Float) recent.notional_by_coin ~f:snd in
      let share = if Float.( > ) total 0. then n /. total else 0. in
      let base_top = List.take base_coins 3 |> List.map ~f:fst in
      if Float.( >= ) share 0.5 && not (List.mem base_top top ~equal:String.equal)
      then
        Some
          (alert
             ~severity:Info
             Style_drift
             (sprintf
                "now trades mostly %s (%s of notional), not among %s"
                top
                (pct share)
                (String.concat ~sep:", " base_top))
             ~value:share
             ~threshold:0.5)
      else None
    | _ -> None
  in
  let frequency =
    match get rm Trades_per_day, get bm Trades_per_day with
    | Some r, Some b when Float.( > ) b 0. && Float.( > ) r 0. ->
      let ratio = r /. b in
      if Float.( >= ) ratio c.frequency_ratio
         || Float.( <= ) ratio (1. /. c.frequency_ratio)
      then
        Some
          (alert
             ~severity:Info
             Frequency_change
             (sprintf "%.2f trades a day against %.2f before" r b)
             ~value:ratio
             ~threshold:c.frequency_ratio)
      else None
    | _ -> None
  in
  let liquidation =
    List.find recent.trades ~f:(fun t ->
      t.liquidated
      &&
      match t.closed_at with
      | Some at -> Time_ns.( >= ) at recent_from
      | None -> true)
    |> Option.map ~f:(fun t ->
      alert
        ~severity:Critical
        Liquidation
        (sprintf "liquidated on %s" t.coin)
        ~value:1.
        ~threshold:1.)
  in
  List.filter_opt [ drawdown; leverage; bias; coin_shift; frequency; liquidation ]
;;

let scan ?step (c : Config.t) (tr : Track_record.t) =
  let step =
    match step with
    | Some s -> s
    | None -> Time_ns.Span.max (Time_ns.Span.scale c.recent 0.25) Time_ns.Span.second
  in
  let start = Time_ns.add tr.from (Time_ns.Span.( + ) c.baseline c.recent) in
  let rec go now active acc =
    if Time_ns.( > ) now tr.until
    then List.rev acc
    else (
      let alerts = detect c tr ~now in
      let on = List.map alerts ~f:(fun a -> a.kind) in
      let fresh =
        List.filter alerts ~f:(fun a -> not (List.mem active a.kind ~equal:Kind.equal))
      in
      go (Time_ns.add now step) on (List.rev_append fresh acc))
  in
  if Time_ns.( > ) start tr.until then [] else go start [] []
;;
