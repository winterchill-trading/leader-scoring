open! Core

module Stable = struct
  module V1 = struct
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
