(** One point of an equity curve the venue publishes (Hyperliquid's [portfolio]), already
    adjusted so deposits and withdrawals are not gains: account value at the start of the
    window plus cumulative pnl. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      { time : Time_ns.Stable.Alternate_sexp.V1.t
      ; equity : float
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { time : Time_ns.Alternate_sexp.t
  ; equity : float
  }
[@@deriving bin_io, sexp, compare, equal]
