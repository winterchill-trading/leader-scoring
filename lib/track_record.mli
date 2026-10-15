(** A leader's track record: round-trip trades and an equity curve with exposure, rebuilt
    from a {!History.t}.

    {b Positions.} Fills are replayed per coin on average-entry books. Positions held
    when the history starts are seeded at its first instant: a coin whose first fill
    reports a non-zero [position_before] at that fill's price, and a coin in the latest
    snapshot with no fill at all at the snapshot's entry price, marked at the snapshot's
    price throughout. The real entries are older than the history; such trades say
    [seeded]. A later fill whose [position_before] disagrees with the replay repairs the
    position at its price and counts as a mismatch.

    {b Trades.} A trade opens when a coin's position leaves zero and closes when it
    returns to zero; a fill that flips the position closes one trade and opens the next,
    and its fee is split between them by size. Funding is credited to the trade open on
    that coin.

    {b Equity.} [pnl(t) = realized - fees + funding + sum(position * (mark - entry))],
    with each coin marked at its latest fill price or snapshot mark. The curve is
    [E0 + pnl(t)], where [E0] makes the curve equal the latest snapshot's account value
    at its time ([Anchored]); without a snapshot, [E0] is [?initial_equity] when given
    ([Given]) and otherwise the peak gross notional, as if the leader never used more
    than 1x ([Assumed]; returns are then only indicative). When the history carries a
    venue curve with at least two points, that curve is the equity series ([Venue]) and
    the rebuilt one is used for exposure only. Deposits and withdrawals never move either
    curve. *)

open! Core

module Direction : sig
  type t =
    | Long
    | Short
  [@@deriving bin_io, sexp, compare, equal, enumerate]
end

module Trade : sig
  module Stable : sig
    module V1 : sig
      type t =
        { coin : string
        ; direction : Direction.t
        ; opened_at : Time_ns.Stable.Alternate_sexp.V1.t
        ; closed_at : Time_ns.Stable.Alternate_sexp.V1.t option (** [None] while open *)
        ; entry_px : float (** volume-weighted over the fills that built the trade *)
        ; exit_px : float option (** volume-weighted over the closing fills *)
        ; max_size : float (** largest absolute position during the trade *)
        ; max_notional : float
        ; realized : float (** price pnl of the closed part, before fees *)
        ; fees : float
        ; funding : float
        ; fills : int
        ; liquidated : bool (** a liquidation fill touched the trade *)
        ; seeded : bool (** opened before the history started *)
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    { coin : string
    ; direction : Direction.t
    ; opened_at : Time_ns.Alternate_sexp.t
    ; closed_at : Time_ns.Alternate_sexp.t option
    ; entry_px : float
    ; exit_px : float option
    ; max_size : float
    ; max_notional : float
    ; realized : float
    ; fees : float
    ; funding : float
    ; fills : int
    ; liquidated : bool
    ; seeded : bool
    }
  [@@deriving bin_io, sexp, compare, equal]

  (** [realized - fees + funding]. *)
  val net_pnl : t -> float

  (** [closed_at - opened_at], or [until - opened_at] for an open trade. *)
  val hold : t -> until:Time_ns.t -> Time_ns.Span.t

  val is_closed : t -> bool
end

module Point : sig
  (** The account right after the events of one instant. *)
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; equity : float (** the rebuilt curve, [E0 + pnl] *)
    ; gross : float (** sum of [|position| * mark] *)
    ; net : float (** sum of [position * mark]; positive is net long *)
    ; open_positions : int
    }
  [@@deriving sexp, compare, equal]
end

module Fill_entry : sig
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; coin : string
    ; sz : float (** signed *)
    ; px : float
    ; notional : float
    ; fee : float
    }
  [@@deriving sexp, compare, equal]
end

module Equity_source : sig
  type t =
    | Venue
    | Anchored
    | Given
    | Assumed
  [@@deriving bin_io, sexp, compare, equal, enumerate]

  val to_string : t -> string
end

type t =
  { account : string
  ; venue : string
  ; from : Time_ns.Alternate_sexp.t
  ; until : Time_ns.Alternate_sexp.t
  ; trades : Trade.t list (** by [opened_at] *)
  ; points : Point.t array (** exposure, by time; one point per instant with events *)
  ; equity : Equity_point.t array (** the equity series metrics use, by time *)
  ; equity_source : Equity_source.t
  ; initial_equity : float (** [E0] *)
  ; fills : int
  ; fills_truncated : bool
  ; turnover : float (** sum of fill notionals *)
  ; fees : float
  ; funding : float
  ; notional_by_coin : (string * float) list (** traded notional per coin, largest first *)
  ; position_mismatches : int
  (** snapshot positions that disagreed with the replayed fills by more than 0.1% *)
  ; fill_log : Fill_entry.t array (** every fill, by time, for windowed figures *)
  ; funding_log : (Time_ns.Alternate_sexp.t * float) array
  }
[@@deriving sexp_of]

(** [build history] replays the history. [until] (default: the latest event) closes the
    period: open trades are held up to it. *)
val build : ?initial_equity:float -> ?until:Time_ns.t -> History.t -> t

(** The equity series' value at [time]: the latest point at or before it, or the first
    point when [time] is earlier. [None] for an empty series. *)
val equity_at : t -> Time_ns.t -> float option

(** End-of-bar equity on the grid [from + k * period] (the last bar ends at or after
    [until]), each the latest equity point at or before the bar end. *)
val bars : t -> period:Time_ns.Span.t -> float array

(** Leverage samples [(duration, gross / equity)] between consecutive points, the last
    one held until [until]; durations sum to the covered time. *)
val leverage_samples : t -> (Time_ns.Span.t * float) array

(** The record cut to [\[from, until\]]: equity and exposure series start with a point at
    [from] holding the state then; trades are those open at some time inside the window
    (closed ones keep their whole pnl); fills and funding inside the window give
    [turnover], [fees], [funding] and [notional_by_coin]. [initial_equity] becomes the
    equity at [from]. *)
val window : t -> from:Time_ns.t -> until:Time_ns.t -> t
