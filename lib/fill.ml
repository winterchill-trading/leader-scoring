open! Core

module Stable = struct
  module V1 = struct
    type t =
      { time : Time_ns.Stable.Alternate_sexp.V1.t
      ; coin : string
      ; px : float
      ; sz : float
      ; fee : float
      ; position_before : float option
      ; liquidation : bool
      ; id : int
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

let notional t = Float.abs t.sz *. t.px

let compare_by_time a b =
  match Time_ns.compare a.time b.time with
  | 0 -> Int.compare a.id b.id
  | c -> c
;;
