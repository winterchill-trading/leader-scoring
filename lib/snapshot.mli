(** The account as the venue reported it at one moment: its value and open positions
    (Hyperliquid's [clearinghouseState]). Snapshots anchor the equity curve rebuilt from
    fills and give the marks and leverage that fills alone do not carry. *)

open! Core

module Position : sig
  module Stable : sig
    module V1 : sig
      type t =
        { coin : string
        ; size : float (** signed *)
        ; entry_px : float option
        ; mark_px : float option (** position value over size, when reported *)
        ; leverage : float option (** the venue's leverage setting for the position *)
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    { coin : string
    ; size : float
    ; entry_px : float option
    ; mark_px : float option
    ; leverage : float option
    }
  [@@deriving bin_io, sexp, compare, equal]
end

module Stable : sig
  module V1 : sig
    type t =
      { time : Time_ns.Stable.Alternate_sexp.V1.t
      ; account_value : float (** equity, including unrealized pnl *)
      ; positions : Position.Stable.V1.t list
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { time : Time_ns.Alternate_sexp.t
  ; account_value : float
  ; positions : Position.t list
  }
[@@deriving bin_io, sexp, compare, equal]

(** Sum of [|size| * mark] over positions with a mark. *)
val gross_notional : t -> float
