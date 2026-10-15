(** Can a follower of a given size copy this leader without moving the market?

    Each leader order (its fills of one coin, one millisecond and one side, summed) is
    scaled to a follower with equity [X]: [n_f = n_l * X / E_l(t)], [E_l(t)] the leader's
    equity then. The follower's order walks the coin's order book from the best level; its
    cost is the average fill price against the mid, in basis points
    ([(avg - mid) / mid * 10^4] for a buy, the mirror for a sell), so it includes half the
    spread. What the book cannot fill is charged [unfilled_penalty_bps]. A leader's cost
    at [X] is the notional-weighted mean over its orders; the capacity is the largest [X]
    whose cost stays at or below [max_cost_bps] (cost only grows with [X], so a bisection
    on [log X] finds it).

    The books are snapshots taken when the estimate runs (from [market-data] L2 books or a
    venue's [l2Book]), applied to past orders: a thin moment at snapshot time makes every
    past order look expensive. Fees are not included. *)

open! Core

module Book : sig
  type level =
    { px : float
    ; size : float
    }
  [@@deriving sexp, compare, equal]

  type t =
    { coin : string
    ; bids : level array (** best (highest) first *)
    ; asks : level array (** best (lowest) first *)
    }
  [@@deriving sexp, compare, equal]

  val mid : t -> float option

  (** Total notional on each side within [bps] of the mid: [(bids, asks)]. *)
  val depth_within : t -> bps:float -> float * float

  (** A [market-data] L2 book, best levels first, under the name [coin]. *)
  val of_l2_book : coin:string -> Market_data.L2_book.t -> t
end

module Estimate : sig
  type t =
    { notional : float (** asked for *)
    ; filled : float (** notional the book could fill *)
    ; cost_bps : float
    (** filled part against the mid, plus the penalty on the unfilled part, over the whole
        notional *)
    }
  [@@deriving sexp_of]
end

(** One order of [notional] (USD) on [side] ([`Buy] lifts asks, [`Sell] hits bids). *)
val walk
  :  ?unfilled_penalty_bps:float
  -> Book.t
  -> side:[ `Buy | `Sell ]
  -> notional:float
  -> Estimate.t option

module Config : sig
  type t =
    { follower_equities : float list (** the capacity curve's points, USD *)
    ; max_cost_bps : float (** the cost that defines capacity *)
    ; unfilled_penalty_bps : float
    }
  [@@deriving sexp]

  (** 1k, 10k, 100k, 1M and 10M USD; capacity at 10 bps; 100 bps on unfilled notional. *)
  val default : t
end

module Point : sig
  type t =
    { follower_equity : float
    ; orders : int (** leader orders on coins with a book *)
    ; mean_cost_bps : float (** notional-weighted *)
    ; median_cost_bps : float
    ; p90_cost_bps : float
    ; unfilled_share : float (** share of follower notional the books could not fill *)
    }
  [@@deriving sexp_of]
end

type t =
  { curve : Point.t list (** one per [follower_equities] *)
  ; capacity : float option
  (** largest follower equity costing at most [max_cost_bps]; [None] when no order has a
      book; [Some 0.] when even 1 USD costs more (a wide spread) *)
  ; orders : int (** leader orders in the record *)
  ; coins_without_book : string list
  }
[@@deriving sexp_of]

(** Leader orders of a track record: [(time, coin, signed notional)], fills of one coin,
    side and millisecond summed. *)
val leader_orders : Track_record.t -> (Time_ns.t * string * float) array

val estimate : ?config:Config.t -> books:Book.t String.Map.t -> Track_record.t -> t

(** [cost_at] the notional-weighted mean cost (bps) for one follower equity, or [None]
    without any costed order. *)
val cost_at
  :  ?unfilled_penalty_bps:float
  -> books:Book.t String.Map.t
  -> Track_record.t
  -> follower_equity:float
  -> float option
