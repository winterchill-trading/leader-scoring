(** One execution of the leader's account, venue neutral.

    Sizes are signed: a buy is positive, a sell negative. Prices and amounts are plain
    floats in the quote currency (USD on Hyperliquid): scoring is statistics over many
    fills, not bookkeeping, and venues report more digits than a fixed-point type keeps. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      { time : Time_ns.Stable.Alternate_sexp.V1.t
      ; coin : string (** the venue's instrument name, e.g. [BTC] *)
      ; px : float
      ; sz : float (** signed: positive buys, negative sells *)
      ; fee : float (** paid, in quote currency; a rebate is negative *)
      ; position_before : float option
      (** the signed position in [coin] just before this fill, when the venue reports
          it (Hyperliquid's [startPosition]); used to seed positions opened before the
          history starts *)
      ; liquidation : bool
      ; id : int (** venue trade id, for ordering fills in the same instant *)
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { time : Time_ns.Alternate_sexp.t
  ; coin : string
  ; px : float
  ; sz : float
  ; fee : float
  ; position_before : float option
  ; liquidation : bool
  ; id : int
  }
[@@deriving bin_io, sexp, compare, equal]

(** [|sz| * px]. *)
val notional : t -> float

(** Orders by [time], then [id]. *)
val compare_by_time : t -> t -> int
