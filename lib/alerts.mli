(** Behaviour changes a follower should hear about.

    {!detect} compares a recent window ending at [now] with the baseline window just
    before it:

    - {b Drawdown breach}: the drawdown from the running peak at [now] reaches
      [drawdown_limit], or [drawdown_vs_baseline] times the baseline's max drawdown
      (critical from twice the limit).
    - {b Leverage spike}: the recent max leverage is at least [leverage_floor] and at
      least [leverage_vs_baseline] times the baseline's 95th percentile.
    - {b Style drift}: the long bias moved by [bias_change] or more, or the recent top
      coin (at least half the recent notional) is not among the baseline's top three.
    - {b Frequency change}: recent trades per day are [frequency_ratio] times the
      baseline's or more, or that much less.
    - {b Liquidation}: a liquidation fill in the recent window (critical).

    {!scan} replays {!detect} along the record and keeps the moments an alert turns on, so
    a page can show when the leader changed. *)

open! Core

module Kind : sig
  type t =
    | Drawdown_breach
    | Leverage_spike
    | Style_drift
    | Frequency_change
    | Liquidation
  [@@deriving sexp, compare, equal, enumerate]

  val to_string : t -> string
end

module Severity : sig
  type t =
    | Info
    | Warning
    | Critical
  [@@deriving sexp, compare, equal]

  val to_string : t -> string
end

module Alert : sig
  type t =
    { kind : Kind.t
    ; severity : Severity.t
    ; time : Time_ns.Alternate_sexp.t
    ; message : string
    ; value : float
    ; threshold : float
    }
  [@@deriving sexp, compare, equal]
end

module Config : sig
  type t =
    { recent : Time_ns.Span.t
    ; baseline : Time_ns.Span.t
    ; drawdown_limit : float
    ; drawdown_vs_baseline : float
    ; leverage_floor : float
    ; leverage_vs_baseline : float
    ; bias_change : float
    ; frequency_ratio : float
    }
  [@@deriving sexp]

  (** 7 recent days against the 30 before; 25% drawdown or 1.5x the baseline's; leverage
      of 3x and 2x the baseline p95; a bias move of 1 (neutral to fully one side); trade
      frequency 3x either way. *)
  val default : t

  (** {!default} with the windows set to a sixth and a half of [span], for short records
      such as a simulator run. *)
  val scaled : span:Time_ns.Span.t -> t
end

val detect : Config.t -> Track_record.t -> now:Time_ns.t -> Alert.t list

(** Runs {!detect} every [step] from [from + baseline + recent] to [until] and returns
    each alert the first time its kind turns on (again after it was off). *)
val scan : ?step:Time_ns.Span.t -> Config.t -> Track_record.t -> Alert.t list
