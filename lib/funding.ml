open! Core

module Stable = struct
  module V1 = struct
    type t =
      { time : Time_ns.Stable.Alternate_sexp.V1.t
      ; coin : string
      ; usd : float
      ; position : float option
      ; rate : float option
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
