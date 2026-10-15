open! Core

module Stable = struct
  module V1 = struct
    type t =
      { account : string
      ; venue : string
      ; fetched_at : Time_ns.Stable.Alternate_sexp.V1.t
      ; fills : Fill.Stable.V1.t list
      ; funding : Funding.Stable.V1.t list
      ; snapshots : Snapshot.Stable.V1.t list
      ; equity_curve : Equity_point.Stable.V1.t list
      ; fills_truncated : bool
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { account : string
  ; venue : string
  ; fetched_at : Time_ns.Alternate_sexp.t
  ; fills : Fill.t list
  ; funding : Funding.t list
  ; snapshots : Snapshot.t list
  ; equity_curve : Equity_point.t list
  ; fills_truncated : bool
  }
[@@deriving bin_io, sexp, compare, equal]

let by_time get a b = Time_ns.compare (get a) (get b)

let normalize t =
  let fills =
    List.stable_sort t.fills ~compare:Fill.compare_by_time
    |> List.remove_consecutive_duplicates ~equal:(fun (a : Fill.t) b ->
      a.id = b.id && Time_ns.equal a.time b.time && String.equal a.coin b.coin)
  in
  { t with
    fills
  ; funding =
      List.stable_sort t.funding ~compare:(by_time (fun (f : Funding.t) -> f.time))
  ; snapshots =
      List.stable_sort t.snapshots ~compare:(by_time (fun (s : Snapshot.t) -> s.time))
  ; equity_curve =
      List.stable_sort
        t.equity_curve
        ~compare:(by_time (fun (p : Equity_point.t) -> p.time))
  }
;;

let restrict t ~from ~until =
  let inside time = Time_ns.between time ~low:from ~high:until in
  { t with
    fills = List.filter t.fills ~f:(fun f -> inside f.time)
  ; funding = List.filter t.funding ~f:(fun f -> inside f.time)
  ; snapshots = List.filter t.snapshots ~f:(fun s -> inside s.time)
  ; equity_curve = List.filter t.equity_curve ~f:(fun p -> inside p.time)
  }
;;

let span t =
  let times =
    List.concat
      [ List.map t.fills ~f:(fun f -> f.time)
      ; List.map t.funding ~f:(fun f -> f.time)
      ; List.map t.snapshots ~f:(fun s -> s.time)
      ; List.map t.equity_curve ~f:(fun p -> p.time)
      ]
  in
  match
    ( List.min_elt times ~compare:Time_ns.compare
    , List.max_elt times ~compare:Time_ns.compare )
  with
  | Some lo, Some hi -> Some (lo, hi)
  | _ -> None
;;
