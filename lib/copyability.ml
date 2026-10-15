open! Core

module Book = struct
  type level =
    { px : float
    ; size : float
    }
  [@@deriving sexp, compare, equal]

  type t =
    { coin : string
    ; bids : level array
    ; asks : level array
    }
  [@@deriving sexp, compare, equal]

  let mid t =
    if Array.is_empty t.bids || Array.is_empty t.asks
    then None
    else Some ((t.bids.(0).px +. t.asks.(0).px) /. 2.)
  ;;

  let depth_within t ~bps =
    match mid t with
    | None -> 0., 0.
    | Some m ->
      let side levels ~ok =
        Array.fold levels ~init:0. ~f:(fun acc l ->
          if ok l.px then acc +. (l.px *. l.size) else acc)
      in
      let band = m *. bps /. 10_000. in
      ( side t.bids ~ok:(fun px -> Float.( >= ) px (m -. band))
      , side t.asks ~ok:(fun px -> Float.( <= ) px (m +. band)) )
  ;;

  let of_l2_book ~coin book =
    let conv (l : Market_data.L2_update.Level.t) =
      { px = Market_data.Price.to_float l.price; size = Market_data.Size.to_float l.size }
    in
    { coin
    ; bids = Array.of_list_map (Market_data.L2_book.bids book) ~f:conv
    ; asks = Array.of_list_map (Market_data.L2_book.asks book) ~f:conv
    }
  ;;
end

module Estimate = struct
  type t =
    { notional : float
    ; filled : float
    ; cost_bps : float
    }
  [@@deriving sexp_of]
end

let walk ?(unfilled_penalty_bps = 100.) (book : Book.t) ~side ~notional =
  match Book.mid book with
  | None -> None
  | Some mid ->
    let levels =
      match side with
      | `Buy -> book.asks
      | `Sell -> book.bids
    in
    let remaining = ref notional
    and filled = ref 0.
    and qty = ref 0. in
    Array.iter levels ~f:(fun l ->
      if Float.( > ) !remaining 0.
      then (
        let take = Float.min !remaining (l.px *. l.size) in
        remaining := !remaining -. take;
        filled := !filled +. take;
        qty := !qty +. (take /. l.px)));
    let filled_cost =
      if Float.( > ) !qty 0.
      then (
        let avg = !filled /. !qty in
        match side with
        | `Buy -> (avg -. mid) /. mid *. 10_000.
        | `Sell -> (mid -. avg) /. mid *. 10_000.)
      else 0.
    in
    let unfilled = Float.max 0. (notional -. !filled) in
    let cost_bps =
      if Float.( > ) notional 0.
      then ((filled_cost *. !filled) +. (unfilled_penalty_bps *. unfilled)) /. notional
      else 0.
    in
    Some { Estimate.notional; filled = !filled; cost_bps }
;;

module Config = struct
  type t =
    { follower_equities : float list
    ; max_cost_bps : float
    ; unfilled_penalty_bps : float
    }
  [@@deriving sexp]

  let default =
    { follower_equities = [ 1e3; 1e4; 1e5; 1e6; 1e7 ]
    ; max_cost_bps = 10.
    ; unfilled_penalty_bps = 100.
    }
  ;;
end

module Point = struct
  type t =
    { follower_equity : float
    ; orders : int
    ; mean_cost_bps : float
    ; median_cost_bps : float
    ; p90_cost_bps : float
    ; unfilled_share : float
    }
  [@@deriving sexp_of]
end

type t =
  { curve : Point.t list
  ; capacity : float option
  ; orders : int
  ; coins_without_book : string list
  }
[@@deriving sexp_of]

let leader_orders (tr : Track_record.t) =
  let groups = Hashtbl.Poly.create () in
  let order = Queue.create () in
  Array.iter tr.fill_log ~f:(fun f ->
    let ms = Time_ns.to_int_ns_since_epoch f.time / 1_000_000 in
    let key = ms, f.coin, Float.( > ) f.sz 0. in
    match Hashtbl.find groups key with
    | Some (time, signed) -> Hashtbl.set groups ~key ~data:(time, signed +. (Float.copysign f.notional f.sz))
    | None ->
      Queue.enqueue order key;
      Hashtbl.set groups ~key ~data:(f.time, Float.copysign f.notional f.sz));
  Queue.to_array order
  |> Array.map ~f:(fun ((_, coin, _) as key) ->
    let time, signed = Hashtbl.find_exn groups key in
    time, coin, signed)
;;

let costs ?(unfilled_penalty_bps = 100.) ~books tr ~follower_equity =
  leader_orders tr
  |> Array.filter_map ~f:(fun (time, coin, signed) ->
    match Map.find books coin, Track_record.equity_at tr time with
    | Some book, Some e when Float.( > ) e 0. ->
      let notional = Float.abs signed *. follower_equity /. e in
      let side = if Float.( > ) signed 0. then `Buy else `Sell in
      walk ~unfilled_penalty_bps book ~side ~notional
    | _ -> None)
;;

let weighted_mean (estimates : Estimate.t array) =
  let total = Array.sum (module Float) estimates ~f:(fun e -> e.notional) in
  if Float.( > ) total 0.
  then Some (Array.sum (module Float) estimates ~f:(fun e -> e.cost_bps *. e.notional) /. total)
  else None
;;

let cost_at ?unfilled_penalty_bps ~books tr ~follower_equity =
  weighted_mean (costs ?unfilled_penalty_bps ~books tr ~follower_equity)
;;

let estimate ?(config = Config.default) ~books (tr : Track_record.t) =
  let unfilled_penalty_bps = config.unfilled_penalty_bps in
  let point follower_equity =
    let es = costs ~unfilled_penalty_bps ~books tr ~follower_equity in
    let bps = Array.map es ~f:(fun e -> e.cost_bps) in
    let total = Array.sum (module Float) es ~f:(fun e -> e.notional) in
    let unfilled = Array.sum (module Float) es ~f:(fun e -> e.notional -. e.filled) in
    { Point.follower_equity
    ; orders = Array.length es
    ; mean_cost_bps = Option.value (weighted_mean es) ~default:0.
    ; median_cost_bps = Option.value (Stats.quantile bps 0.5) ~default:0.
    ; p90_cost_bps = Option.value (Stats.quantile bps 0.9) ~default:0.
    ; unfilled_share = (if Float.( > ) total 0. then unfilled /. total else 0.)
    }
  in
  let cost x = cost_at ~unfilled_penalty_bps ~books tr ~follower_equity:x in
  let capacity =
    (* bisection on log10 equity between 1 USD and 10^10 USD; 40 steps is far below a
       cent of resolution at any size *)
    match cost 1. with
    | None -> None
    | Some c when Float.( > ) c config.max_cost_bps -> Some 0.
    | Some _ ->
      let ok x = Option.value_map (cost x) ~default:false ~f:(fun c -> Float.( <= ) c config.max_cost_bps) in
      if ok 1e10
      then Some 1e10
      else (
        let lo = ref 0.
        and hi = ref 10. in
        for _ = 1 to 40 do
          let m = (!lo +. !hi) /. 2. in
          if ok (10. **. m) then lo := m else hi := m
        done;
        Some (10. **. !lo))
  in
  let orders = leader_orders tr in
  let coins_without_book =
    Array.to_list orders
    |> List.filter_map ~f:(fun (_, coin, _) -> Option.some_if (not (Map.mem books coin)) coin)
    |> List.dedup_and_sort ~compare:String.compare
  in
  { curve = List.map config.follower_equities ~f:point
  ; capacity
  ; orders = Array.length orders
  ; coins_without_book
  }
;;
