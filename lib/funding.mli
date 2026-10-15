(** One funding payment on a perpetual position. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      { time : Time_ns.Stable.Alternate_sexp.V1.t
      ; coin : string
      ; usd : float (** signed: positive when the account received funding *)
      ; position : float option (** signed position the payment was charged on *)
      ; rate : float option (** the funding rate of the interval *)
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { time : Time_ns.Alternate_sexp.t
  ; coin : string
  ; usd : float
  ; position : float option
  ; rate : float option
  }
[@@deriving bin_io, sexp, compare, equal]
