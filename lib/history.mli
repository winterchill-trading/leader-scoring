(** Everything a source returned for one account: the input of {!Track_record.build}.

    Lists may arrive in any order; {!normalize} sorts them and drops repeated fills. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      { account : string
      ; venue : string
      ; fetched_at : Time_ns.Stable.Alternate_sexp.V1.t
      ; fills : Fill.Stable.V1.t list
      ; funding : Funding.Stable.V1.t list
      ; snapshots : Snapshot.Stable.V1.t list
      ; equity_curve : Equity_point.Stable.V1.t list
      (** the venue's deposit-free curve; empty when it has none *)
      ; fills_truncated : bool
      (** the venue keeps only recent fills and the oldest one returned is later than the
          period asked for *)
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

(** Sorts every list by time (fills by time then id) and drops fills repeated with the
    same [(id, time, coin, side)], as overlapping pages return them. *)
val normalize : t -> t

(** Keeps what happened in [[from, until]]; snapshots and curve points too. *)
val restrict : t -> from:Time_ns.t -> until:Time_ns.t -> t

(** Earliest and latest time over fills, funding, snapshots and the curve. *)
val span : t -> (Time_ns.t * Time_ns.t) option
