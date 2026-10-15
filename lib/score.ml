open! Core

module Config = struct
  type t =
    { weights : (Metric.t * float) list
    ; min_trades : int
    ; min_days : float
    }
  [@@deriving sexp]

  let default =
    { weights =
        [ Sharpe, 0.15
        ; Sortino, 0.10
        ; Return_over_drawdown, 0.10
        ; Total_return, 0.05
        ; Max_drawdown, 0.15
        ; Cvar_95, 0.05
        ; Rolling_positive_share, 0.10
        ; Profit_factor, 0.05
        ; Win_rate, 0.05
        ; Leverage_p95, 0.10
        ; Coin_hhi, 0.05
        ; Bias_drift, 0.05
        ]
    ; min_trades = 5
    ; min_days = 0.
    }
  ;;
end

module Component = struct
  type t =
    { metric : Metric.t
    ; value : float option
    ; percentile : float
    ; weight : float
    ; contribution : float
    }
  [@@deriving sexp_of]
end

module Ranked = struct
  type t =
    { account : string
    ; score : float
    ; rank : int option
    ; percentile : float option
    ; eligible : bool
    ; reasons : string list
    ; components : Component.t list
    }
  [@@deriving sexp_of]
end

let percentile_of xs x =
  let n = Array.length xs in
  if n <= 1
  then 0.5
  else (
    let below = Array.count xs ~f:(fun y -> Float.( < ) y x) in
    let equal = Array.count xs ~f:(fun y -> Float.equal y x) in
    (Float.of_int below +. (Float.of_int (Int.max 0 (equal - 1)) /. 2.))
    /. Float.of_int (n - 1))
;;

let rank (config : Config.t) universe =
  let weights = List.filter config.weights ~f:(fun (_, w) -> Float.( > ) w 0.) in
  let total_weight = List.sum (module Float) weights ~f:snd in
  let eligibility (m : Metrics.t) days =
    let trades =
      Metrics.get m Trades |> Option.value ~default:0. |> Float.iround_nearest_exn
    in
    List.filter_opt
      [ Option.some_if
          (trades < config.min_trades)
          (sprintf "%d closed trades, %d needed" trades config.min_trades)
      ; Option.some_if
          Float.(days < config.min_days)
          (sprintf "%.1f days of history, %.1f needed" days config.min_days)
      ]
  in
  let entries =
    List.map universe ~f:(fun (account, metrics, days) ->
      account, metrics, eligibility metrics days)
  in
  (* Percentiles are taken among eligible leaders; when nobody is eligible, among all. *)
  let pool =
    match List.filter entries ~f:(fun (_, _, r) -> List.is_empty r) with
    | [] -> entries
    | eligible -> eligible
  in
  let pool_values =
    List.map weights ~f:(fun (metric, _) ->
      ( metric
      , List.filter_map pool ~f:(fun (_, m, _) -> Metrics.get m metric) |> Array.of_list ))
  in
  let scored =
    List.map entries ~f:(fun (account, metrics, reasons) ->
      let components =
        List.map2_exn weights pool_values ~f:(fun (metric, w) (_, xs) ->
          let value = Metrics.get metrics metric in
          let percentile =
            match value with
            | None -> 0.5
            | Some v ->
              (* a non-eligible leader is placed among the pool as if it were one more *)
              let xs =
                if Array.mem xs v ~equal:Float.equal then xs else Array.append xs [| v |]
              in
              let p = percentile_of xs v in
              (match Metric.better metric with
               | Lower -> 1. -. p
               | Higher | Neither -> p)
          in
          let weight = if Float.( > ) total_weight 0. then w /. total_weight else 0. in
          { Component.metric
          ; value
          ; percentile
          ; weight
          ; contribution = 100. *. weight *. percentile
          })
      in
      let score = List.sum (module Float) components ~f:(fun c -> c.contribution) in
      account, score, reasons, components)
  in
  let by_score l =
    List.stable_sort l ~compare:(fun (_, a, _, _) (_, b, _, _) -> Float.compare b a)
  in
  let eligible, rest =
    List.partition_tf scored ~f:(fun (_, _, r, _) -> List.is_empty r)
  in
  let eligible = by_score eligible in
  let scores = List.map eligible ~f:(fun (_, s, _, _) -> s) |> Array.of_list in
  let n = Array.length scores in
  let ranked =
    List.mapi eligible ~f:(fun i (account, score, reasons, components) ->
      let below = Array.count scores ~f:(fun s -> Float.( < ) s score) in
      { Ranked.account
      ; score
      ; rank = Some (i + 1)
      ; percentile =
          Some (if n <= 1 then 1. else Float.of_int below /. Float.of_int (n - 1))
      ; eligible = true
      ; reasons
      ; components
      })
  in
  let unranked =
    List.map (by_score rest) ~f:(fun (account, score, reasons, components) ->
      { Ranked.account
      ; score
      ; rank = None
      ; percentile = None
      ; eligible = false
      ; reasons
      ; components
      })
  in
  ranked @ unranked
;;
