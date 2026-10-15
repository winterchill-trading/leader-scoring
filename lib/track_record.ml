open! Core

module Direction = struct
  type t =
    | Long
    | Short
  [@@deriving bin_io, sexp, compare, equal, enumerate]
end

module Trade = struct
  module Stable = struct
    module V1 = struct
      type t =
        { coin : string
        ; direction : Direction.t
        ; opened_at : Time_ns.Stable.Alternate_sexp.V1.t
        ; closed_at : Time_ns.Stable.Alternate_sexp.V1.t option
        ; entry_px : float
        ; exit_px : float option
        ; max_size : float
        ; max_notional : float
        ; realized : float
        ; fees : float
        ; funding : float
        ; fills : int
        ; liquidated : bool
        ; seeded : bool
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    { coin : string
    ; direction : Direction.t
    ; opened_at : Time_ns.Alternate_sexp.t
    ; closed_at : Time_ns.Alternate_sexp.t option
    ; entry_px : float
    ; exit_px : float option
    ; max_size : float
    ; max_notional : float
    ; realized : float
    ; fees : float
    ; funding : float
    ; fills : int
    ; liquidated : bool
    ; seeded : bool
    }
  [@@deriving bin_io, sexp, compare, equal]

  let net_pnl t = t.realized -. t.fees +. t.funding
  let hold t ~until = Time_ns.diff (Option.value t.closed_at ~default:until) t.opened_at
  let is_closed t = Option.is_some t.closed_at
end

module Point = struct
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; equity : float
    ; gross : float
    ; net : float
    ; open_positions : int
    }
  [@@deriving sexp, compare, equal]
end

module Fill_entry = struct
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; coin : string
    ; notional : float
    ; fee : float
    }
  [@@deriving sexp, compare, equal]
end

module Equity_source = struct
  type t =
    | Venue
    | Anchored
    | Given
    | Assumed
  [@@deriving bin_io, sexp, compare, equal, enumerate]

  let to_string = function
    | Venue -> "venue"
    | Anchored -> "anchored"
    | Given -> "given"
    | Assumed -> "assumed"
  ;;
end

type t =
  { account : string
  ; venue : string
  ; from : Time_ns.Alternate_sexp.t
  ; until : Time_ns.Alternate_sexp.t
  ; trades : Trade.t list
  ; points : Point.t array
  ; equity : Equity_point.t array
  ; equity_source : Equity_source.t
  ; initial_equity : float
  ; fills : int
  ; fills_truncated : bool
  ; turnover : float
  ; fees : float
  ; funding : float
  ; notional_by_coin : (string * float) list
  ; position_mismatches : int
  ; fill_log : Fill_entry.t array
  ; funding_log : (Time_ns.Alternate_sexp.t * float) array
  }
[@@deriving sexp_of]

(* A trade being built. *)
module Open_trade = struct
  type t =
    { coin : string
    ; direction : Direction.t
    ; opened_at : Time_ns.t
    ; seeded : bool
    ; mutable entry_qty : float
    ; mutable entry_cost : float
    ; mutable exit_qty : float
    ; mutable exit_cost : float
    ; mutable max_size : float
    ; mutable max_notional : float
    ; mutable realized : float
    ; mutable fees : float
    ; mutable funding : float
    ; mutable fills : int
    ; mutable liquidated : bool
    }

  let create ~coin ~size ~px ~at ~seeded =
    { coin
    ; direction = (if Float.( > ) size 0. then Long else Short)
    ; opened_at = at
    ; seeded
    ; entry_qty = Float.abs size
    ; entry_cost = Float.abs size *. px
    ; exit_qty = 0.
    ; exit_cost = 0.
    ; max_size = Float.abs size
    ; max_notional = Float.abs size *. px
    ; realized = 0.
    ; fees = 0.
    ; funding = 0.
    ; fills = (if seeded then 0 else 1)
    ; liquidated = false
    }
  ;;

  let finish t ~closed_at : Trade.t =
    { coin = t.coin
    ; direction = t.direction
    ; opened_at = t.opened_at
    ; closed_at
    ; entry_px = (if Float.( > ) t.entry_qty 0. then t.entry_cost /. t.entry_qty else 0.)
    ; exit_px =
        (if Float.( > ) t.exit_qty 0. then Some (t.exit_cost /. t.exit_qty) else None)
    ; max_size = t.max_size
    ; max_notional = t.max_notional
    ; realized = t.realized
    ; fees = t.fees
    ; funding = t.funding
    ; fills = t.fills
    ; liquidated = t.liquidated
    ; seeded = t.seeded
    }
  ;;
end

module Book = struct
  type t =
    { mutable pos : float
    ; mutable entry : float
    ; mutable mark : float
    ; mutable seen : bool (** a fill or a snapshot position has been applied *)
    ; mutable trade : Open_trade.t option
    }

  let create () = { pos = 0.; entry = 0.; mark = 0.; seen = false; trade = None }
end

module Event = struct
  type t =
    | Fill of Fill.t
    | Funding of Funding.t
    | Snapshot of Snapshot.t

  let time = function
    | Fill f -> f.time
    | Funding f -> f.time
    | Snapshot s -> s.time
  ;;

  (* Within one instant: fills, then funding, then the snapshot that reports the result. *)
  let rank = function
    | Fill _ -> 0
    | Funding _ -> 1
    | Snapshot _ -> 2
  ;;

  let compare a b =
    match Time_ns.compare (time a) (time b) with
    | 0 -> Int.compare (rank a) (rank b)
    | c -> c
  ;;
end

(* Positions below this are treated as flat: venues print sizes with at most 8 decimals,
   and average-entry arithmetic leaves residues around 1e-12. *)
let eps = 1e-9
let is_flat x = Float.( <= ) (Float.abs x) eps

(* "Disagrees with the snapshot" means more than 0.1% of the size apart, so rounding of
   printed sizes does not count. *)
let mismatch ~expected ~actual =
  Float.( > ) (Float.abs (expected -. actual)) ((0.001 *. Float.abs expected) +. eps)
;;

let sort_by_notional =
  List.sort ~compare:(fun (c1, a) (c2, b) ->
    match Float.compare b a with
    | 0 -> String.compare c1 c2
    | c -> c)
;;

let build ?initial_equity ?until (history : History.t) =
  let h = History.normalize history in
  let books = String.Table.create () in
  let book coin = Hashtbl.find_or_add books coin ~default:Book.create in
  let closed = ref [] in
  let realized = ref 0.
  and fees = ref 0.
  and funding = ref 0.
  and turnover = ref 0.
  and mismatches = ref 0 in
  let notional = String.Table.create () in
  let open_trade (b : Book.t) ~coin ~size ~px ~at ~seeded =
    b.trade <- Some (Open_trade.create ~coin ~size ~px ~at ~seeded)
  in
  let close_trade (b : Book.t) ~at =
    Option.iter b.trade ~f:(fun o -> closed := Open_trade.finish o ~closed_at:(Some at) :: !closed);
    b.trade <- None
  in
  (* Apply a signed quantity [q] at [px]; returns the realized price pnl. [fee] is split
     between the closing and the opening part by size. *)
  let apply (b : Book.t) ~coin ~q ~px ~fee ~at ~seeded ~liquidation =
    let open Float.O in
    let touch (o : Open_trade.t) =
      o.max_size <- Float.max o.max_size (Float.abs b.pos);
      o.max_notional <- Float.max o.max_notional (Float.abs b.pos * px);
      if liquidation then o.liquidated <- true
    in
    if is_flat b.pos
    then (
      b.pos <- q;
      b.entry <- px;
      open_trade b ~coin ~size:q ~px ~at ~seeded;
      Option.iter b.trade ~f:(fun o ->
        o.fees <- o.fees + fee;
        touch o))
    else if q * b.pos > 0.
    then (
      let size = b.pos + q in
      b.entry <- ((Float.abs b.pos * b.entry) + (Float.abs q * px)) / Float.abs size;
      b.pos <- size;
      Option.iter b.trade ~f:(fun o ->
        o.entry_qty <- o.entry_qty + Float.abs q;
        o.entry_cost <- o.entry_cost + (Float.abs q * px);
        o.fees <- o.fees + fee;
        if not seeded then o.fills <- Int.(o.fills + 1);
        touch o))
    else (
      let closing = Float.min (Float.abs q) (Float.abs b.pos) in
      let pnl = closing * (px - b.entry) * if b.pos > 0. then 1. else -1. in
      realized := !realized + pnl;
      let close_fee = if Float.( > ) (Float.abs q) 0. then fee * closing / Float.abs q else 0. in
      Option.iter b.trade ~f:(fun o ->
        o.realized <- o.realized + pnl;
        o.exit_qty <- o.exit_qty + closing;
        o.exit_cost <- o.exit_cost + (closing * px);
        o.fees <- o.fees + close_fee;
        if not seeded then o.fills <- Int.(o.fills + 1);
        if liquidation then o.liquidated <- true);
      let rest = Float.abs q - closing in
      if Float.( > ) rest eps
      then (
        close_trade b ~at;
        let size = Float.copysign rest q in
        b.pos <- size;
        b.entry <- px;
        open_trade b ~coin ~size ~px ~at ~seeded;
        Option.iter b.trade ~f:(fun o ->
          o.fees <- o.fees + (fee - close_fee);
          touch o))
      else (
        b.pos <- b.pos + q;
        if is_flat b.pos
        then (
          b.pos <- 0.;
          close_trade b ~at)))
  in
  let on_fill (f : Fill.t) =
    let b = book f.coin in
    (* Seed or repair the position from the venue's own count when it disagrees. *)
    (match f.position_before with
     | Some before when mismatch ~expected:before ~actual:b.pos ->
       if b.seen then incr mismatches;
       let seed_at = Time_ns.sub f.time (Time_ns.Span.of_int_ms 1) in
       apply
         b
         ~coin:f.coin
         ~q:(before -. b.pos)
         ~px:f.px
         ~fee:0.
         ~at:seed_at
         ~seeded:true
         ~liquidation:false
     | _ -> ());
    b.seen <- true;
    b.mark <- f.px;
    fees := !fees +. f.fee;
    turnover := !turnover +. Fill.notional f;
    Hashtbl.update notional f.coin ~f:(fun v ->
      Option.value v ~default:0. +. Fill.notional f);
    apply b ~coin:f.coin ~q:f.sz ~px:f.px ~fee:f.fee ~at:f.time ~seeded:false
      ~liquidation:f.liquidation
  in
  let on_funding (f : Funding.t) =
    funding := !funding +. f.usd;
    let b = book f.coin in
    Option.iter b.trade ~f:(fun o -> o.funding <- o.funding +. f.usd)
  in
  let anchor = ref None in
  let pnl () =
    Hashtbl.fold books ~init:(!realized -. !fees +. !funding) ~f:(fun ~key:_ ~data:(b : Book.t) acc ->
      acc +. (b.pos *. (b.mark -. b.entry)))
  in
  let on_snapshot (s : Snapshot.t) =
    List.iter s.positions ~f:(fun (p : Snapshot.Position.t) ->
      let b = book p.coin in
      Option.iter p.mark_px ~f:(fun m -> b.mark <- m);
      if (not b.seen) && not (is_flat p.size)
      then (
        let px = Option.first_some p.entry_px p.mark_px |> Option.value ~default:b.mark in
        if Float.( = ) b.mark 0. then b.mark <- px;
        apply b ~coin:p.coin ~q:p.size ~px ~fee:0. ~at:s.time ~seeded:true ~liquidation:false)
      else if b.seen && mismatch ~expected:p.size ~actual:b.pos
      then incr mismatches;
      b.seen <- true);
    anchor := Some (s.account_value -. pnl ())
  in
  let events =
    List.concat
      [ List.map h.fills ~f:(fun f -> Event.Fill f)
      ; List.map h.funding ~f:(fun f -> Event.Funding f)
      ; List.map h.snapshots ~f:(fun s -> Event.Snapshot s)
      ]
    |> List.stable_sort ~compare:Event.compare
  in
  let raw_points = Queue.create () in
  let peak_gross = ref 0. in
  let push time =
    let gross, net, count =
      Hashtbl.fold books ~init:(0., 0., 0) ~f:(fun ~key:_ ~data:(b : Book.t) (g, n, c) ->
        if is_flat b.pos
        then g, n, c
        else g +. (Float.abs b.pos *. b.mark), n +. (b.pos *. b.mark), c + 1)
    in
    peak_gross := Float.max !peak_gross gross;
    Queue.enqueue raw_points (time, pnl (), gross, net, count)
  in
  let rec walk = function
    | [] -> ()
    | e :: rest ->
      (match e with
       | Event.Fill f -> on_fill f
       | Funding f -> on_funding f
       | Snapshot s -> on_snapshot s);
      (match rest with
       | next :: _ when Time_ns.equal (Event.time next) (Event.time e) -> ()
       | _ -> push (Event.time e));
      walk rest
  in
  walk events;
  let curve_times = List.map h.equity_curve ~f:(fun p -> p.time) in
  let all_times = List.map events ~f:Event.time @ curve_times in
  let first = List.min_elt all_times ~compare:Time_ns.compare in
  let last = List.max_elt all_times ~compare:Time_ns.compare in
  let from = Option.value first ~default:h.fetched_at in
  let until =
    match until with
    | Some u -> u
    | None -> Option.value last ~default:h.fetched_at
  in
  let source, e0 =
    match !anchor, initial_equity with
    | Some a, _ -> Equity_source.Anchored, a
    | None, Some e -> Given, e
    | None, None -> Assumed, Float.max !peak_gross 1.
  in
  let points =
    Queue.to_array raw_points
    |> Array.map ~f:(fun (time, pnl, gross, net, open_positions) ->
      { Point.time; equity = e0 +. pnl; gross; net; open_positions })
  in
  let equity_source, equity, initial =
    if List.length h.equity_curve >= 2
    then (
      let curve = Array.of_list h.equity_curve in
      Equity_source.Venue, curve, curve.(0).equity)
    else
      ( source
      , Array.map points ~f:(fun p -> { Equity_point.time = p.time; equity = p.equity })
      , e0 )
  in
  let still_open =
    Hashtbl.data books
    |> List.filter_map ~f:(fun (b : Book.t) ->
      Option.map b.trade ~f:(Open_trade.finish ~closed_at:None))
  in
  let trades =
    List.rev_append !closed still_open
    |> List.stable_sort ~compare:(fun (a : Trade.t) b ->
      match Time_ns.compare a.opened_at b.opened_at with
      | 0 -> String.compare a.coin b.coin
      | c -> c)
  in
  { account = h.account
  ; venue = h.venue
  ; from
  ; until
  ; trades
  ; points
  ; equity
  ; equity_source
  ; initial_equity = initial
  ; fills = List.length h.fills
  ; fills_truncated = h.fills_truncated
  ; turnover = !turnover
  ; fees = !fees
  ; funding = !funding
  ; notional_by_coin = sort_by_notional (Hashtbl.to_alist notional)
  ; position_mismatches = !mismatches
  ; fill_log =
      Array.of_list_map h.fills ~f:(fun (f : Fill.t) ->
        { Fill_entry.time = f.time; coin = f.coin; notional = Fill.notional f; fee = f.fee })
  ; funding_log = Array.of_list_map h.funding ~f:(fun (f : Funding.t) -> f.time, f.usd)
  }
;;

(* Index of the latest point at or before [time], or [0]. *)
let index_at (series : Equity_point.t array) time =
  let rec go lo hi =
    (* invariant: series.(lo).time <= time < series.(hi).time *)
    if hi - lo <= 1
    then lo
    else (
      let mid = (lo + hi) / 2 in
      if Time_ns.( <= ) series.(mid).time time then go mid hi else go lo mid)
  in
  let n = Array.length series in
  if n = 0 || Time_ns.( < ) time series.(0).time
  then 0
  else if Time_ns.( >= ) time series.(n - 1).time
  then n - 1
  else go 0 (n - 1)
;;

let equity_at t time =
  if Array.is_empty t.equity then None else Some t.equity.(index_at t.equity time).equity
;;

let bars t ~period =
  if Array.is_empty t.equity || Time_ns.Span.( <= ) period Time_ns.Span.zero
  then [||]
  else (
    let span = Time_ns.diff t.until t.from in
    let n =
      Float.iround_up_exn (Time_ns.Span.( // ) span period) |> Int.max 1
    in
    Array.init (n + 1) ~f:(fun k ->
      let at = Time_ns.add t.from (Time_ns.Span.scale_int period k) in
      t.equity.(index_at t.equity at).equity))
;;

let leverage_samples t =
  let n = Array.length t.points in
  Array.filter_mapi t.points ~f:(fun i (p : Point.t) ->
    let next = if i + 1 < n then t.points.(i + 1).time else t.until in
    let duration = Time_ns.diff next p.time in
    match equity_at t p.time with
    | Some e when Float.( > ) e 0. && Time_ns.Span.( > ) duration Time_ns.Span.zero ->
      Some (duration, p.gross /. e)
    | _ -> None)
;;

let window t ~from ~until =
  let inside time = Time_ns.between time ~low:from ~high:until in
  let start_equity = Option.value (equity_at t from) ~default:t.initial_equity in
  let equity =
    Array.append
      [| { Equity_point.time = from; equity = start_equity } |]
      (Array.filter t.equity ~f:(fun p -> Time_ns.( > ) p.time from && inside p.time))
  in
  let points =
    let before =
      Array.fold t.points ~init:None ~f:(fun acc (p : Point.t) ->
        if Time_ns.( <= ) p.time from then Some p else acc)
    in
    let rest = Array.filter t.points ~f:(fun p -> Time_ns.( > ) p.time from && inside p.time) in
    match before with
    | Some p -> Array.append [| { p with time = from } |] rest
    | None -> rest
  in
  let trades =
    List.filter t.trades ~f:(fun (tr : Trade.t) ->
      Time_ns.( < ) tr.opened_at until
      &&
      match tr.closed_at with
      | None -> true
      | Some c -> Time_ns.( > ) c from)
  in
  let fill_log = Array.filter t.fill_log ~f:(fun f -> inside f.time) in
  let funding_log = Array.filter t.funding_log ~f:(fun (time, _) -> inside time) in
  let notional = String.Table.create () in
  Array.iter fill_log ~f:(fun f ->
    Hashtbl.update notional f.coin ~f:(fun v -> Option.value v ~default:0. +. f.notional));
  { t with
    from
  ; until
  ; trades
  ; points
  ; equity
  ; initial_equity = start_equity
  ; fills = Array.length fill_log
  ; turnover = Array.sum (module Float) fill_log ~f:(fun f -> f.notional)
  ; fees = Array.sum (module Float) fill_log ~f:(fun f -> f.fee)
  ; funding = Array.sum (module Float) funding_log ~f:snd
  ; notional_by_coin = sort_by_notional (Hashtbl.to_alist notional)
  ; fill_log
  ; funding_log
  }
;;
