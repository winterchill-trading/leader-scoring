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

(* One side of a book as running totals, so a walk is a binary search instead of a scan:
   [cum_notional.(k)] and [cum_qty.(k)] hold levels [0 .. k - 1]. *)
module Depth = struct
  type t =
    { px : float array
    ; cum_notional : float array
    ; cum_qty : float array
    }

  let of_levels (levels : Book.level array) =
    let n = Array.length levels in
    let cum_notional = Array.create ~len:(n + 1) 0.
    and cum_qty = Array.create ~len:(n + 1) 0. in
    for k = 0 to n - 1 do
      cum_notional.(k + 1) <- cum_notional.(k) +. (levels.(k).px *. levels.(k).size);
      cum_qty.(k + 1) <- cum_qty.(k) +. levels.(k).size
    done;
    { px = Array.map levels ~f:(fun l -> l.px); cum_notional; cum_qty }
  ;;

  (* [(filled notional, quantity)] for [notional] taken from the best level on. *)
  let[@inline] take t ~notional =
    let n = Array.length t.px in
    if n = 0
    then #(0., 0.)
    else if Float.( >= ) notional t.cum_notional.(n)
    then #(t.cum_notional.(n), t.cum_qty.(n))
    else (
      (* smallest k with cum_notional.(k + 1) >= notional *)
      let lo = ref 0
      and hi = ref (n - 1) in
      while !lo < !hi do
        let mid = (!lo + !hi) / 2 in
        if Float.( >= ) t.cum_notional.(mid + 1) notional then hi := mid else lo := mid + 1
      done;
      let k = !lo in
      let rest = notional -. t.cum_notional.(k) in
      #(notional, t.cum_qty.(k) +. (rest /. t.px.(k))))
  ;;
end

module Prepared = struct
  type t =
    { mid : float
    ; bids : Depth.t
    ; asks : Depth.t
    }

  let of_book (b : Book.t) =
    Option.map (Book.mid b) ~f:(fun mid ->
      { mid; bids = Depth.of_levels b.bids; asks = Depth.of_levels b.asks })
  ;;

  let walk ~unfilled_penalty_bps t ~side ~notional =
    let depth =
      match side with
      | `Buy -> t.asks
      | `Sell -> t.bids
    in
    let #(filled, qty) = Depth.take depth ~notional in
    let filled_cost =
      if Float.( > ) qty 0.
      then (
        let avg = filled /. qty in
        match side with
        | `Buy -> (avg -. t.mid) /. t.mid *. 10_000.
        | `Sell -> (t.mid -. avg) /. t.mid *. 10_000.)
      else 0.
    in
    let unfilled = Float.max 0. (notional -. filled) in
    let cost_bps =
      if Float.( > ) notional 0.
      then ((filled_cost *. filled) +. (unfilled_penalty_bps *. unfilled)) /. notional
      else 0.
    in
    { Estimate.notional; filled; cost_bps }
  ;;
end

let walk ?(unfilled_penalty_bps = 100.) book ~side ~notional =
  Option.map (Prepared.of_book book) ~f:(fun p ->
    Prepared.walk ~unfilled_penalty_bps p ~side ~notional)
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

(* Each costed order as (book, side, notional per follower dollar), computed once. *)
let prepare ~books tr =
  let prepared = Map.filter_map books ~f:Prepared.of_book in
  leader_orders tr
  |> Array.filter_map ~f:(fun (time, coin, signed) ->
    match Map.find prepared coin, Track_record.equity_at tr time with
    | Some book, Some e when Float.( > ) e 0. ->
      Some (book, (if Float.( > ) signed 0. then `Buy else `Sell), Float.abs signed /. e)
    | _ -> None)
;;

let costs ~unfilled_penalty_bps orders ~follower_equity =
  Array.map orders ~f:(fun (book, side, per_dollar) ->
    Prepared.walk ~unfilled_penalty_bps book ~side ~notional:(per_dollar *. follower_equity))
;;

(* The notional-weighted mean cost without building an estimate per order: the
   capacity bisection calls this about 45 times. *)
let mean_cost ~unfilled_penalty_bps orders ~follower_equity =
  let mutable total = 0. in
  let mutable weighted = 0. in
  for i = 0 to Array.length orders - 1 do
    let (book : Prepared.t), side, per_dollar = orders.(i) in
    let notional = per_dollar *. follower_equity in
    let depth =
      match side with
      | `Buy -> book.asks
      | `Sell -> book.bids
    in
    let #(filled, qty) = Depth.take depth ~notional in
    let filled_cost =
      if Float.( > ) qty 0.
      then (
        let avg = filled /. qty in
        match side with
        | `Buy -> (avg -. book.mid) /. book.mid *. 10_000.
        | `Sell -> (book.mid -. avg) /. book.mid *. 10_000.)
      else 0.
    in
    let unfilled = Float.max 0. (notional -. filled) in
    total <- total +. notional;
    weighted <- weighted +. (filled_cost *. filled) +. (unfilled_penalty_bps *. unfilled)
  done;
  if Float.( > ) total 0. then Some (weighted /. total) else None
;;

let weighted_mean (estimates : Estimate.t array) =
  let total = Array.sum (module Float) estimates ~f:(fun e -> e.notional) in
  if Float.( > ) total 0.
  then Some (Array.sum (module Float) estimates ~f:(fun e -> e.cost_bps *. e.notional) /. total)
  else None
;;

let cost_at ?(unfilled_penalty_bps = 100.) ~books tr ~follower_equity =
  weighted_mean (costs ~unfilled_penalty_bps (prepare ~books tr) ~follower_equity)
;;

let estimate ?(config = Config.default) ~books (tr : Track_record.t) =
  let unfilled_penalty_bps = config.unfilled_penalty_bps in
  let orders = prepare ~books tr in
  let point follower_equity =
    let es = costs ~unfilled_penalty_bps orders ~follower_equity in
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
  let cost x = mean_cost ~unfilled_penalty_bps orders ~follower_equity:x in
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
  let all_orders = leader_orders tr in
  let coins_without_book =
    Array.to_list all_orders
    |> List.filter_map ~f:(fun (_, coin, _) -> Option.some_if (not (Map.mem books coin)) coin)
    |> List.dedup_and_sort ~compare:String.compare
  in
  { curve = List.map config.follower_equities ~f:point
  ; capacity
  ; orders = Array.length all_orders
  ; coins_without_book
  }
;;
