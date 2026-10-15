(** A transparent composite score and percentile ranking across a universe of leaders.

    For each weighted metric [m] the leader's value is turned into a percentile among the
    eligible leaders that have it: [p = (below + (equal - 1) / 2) / (n - 1)] (0.5 when
    [n = 1]), flipped to [1 - p] when lower is better ({!Metric.better}). A leader without
    a value gets 0.5, so missing data is neither rewarded nor punished. The score is
    [100 * sum(w_m * p_m) / sum(w_m)], and every component is reported with its value,
    percentile, weight and contribution so a page can show why a leader ranks where it
    does. Leaders below the eligibility floors are scored but not ranked. *)

open! Core

module Config : sig
  type t =
    { weights : (Metric.t * float) list (** non-negative; zero weights are ignored *)
    ; min_trades : int (** closed trades needed to be ranked *)
    ; min_days : float (** covered days needed to be ranked *)
    }
  [@@deriving sexp]

  (** Risk-adjusted return first, then drawdown and tail risk, consistency, trade
      quality, and penalties for leverage and concentration:
      {v
      sharpe 0.15, sortino 0.10, return_over_drawdown 0.10, total_return 0.05,
      max_drawdown 0.15, cvar_95 0.05, rolling_positive_share 0.10, profit_factor 0.05,
      win_rate 0.05, leverage_p95 0.10, coin_hhi 0.05, bias_drift 0.05
      v}
      with [min_trades = 5] and [min_days = 0]. *)
  val default : t
end

module Component : sig
  type t =
    { metric : Metric.t
    ; value : float option
    ; percentile : float (** after flipping, so higher is always better *)
    ; weight : float (** normalized: weights sum to 1 *)
    ; contribution : float (** [100 * weight * percentile] *)
    }
  [@@deriving sexp_of]
end

module Ranked : sig
  type t =
    { account : string
    ; score : float (** 0 to 100 *)
    ; rank : int option (** 1 is best; [None] when not eligible *)
    ; percentile : float option (** share of ranked leaders scoring below *)
    ; eligible : bool
    ; reasons : string list (** why a leader is not eligible *)
    ; components : Component.t list
    }
  [@@deriving sexp_of]
end

(** [rank config universe], with [(account, metrics, covered days)] per leader. The
    result is sorted: ranked leaders by score, then the rest by score. Ties keep the
    input order. *)
val rank : Config.t -> (string * Metrics.t * float) list -> Ranked.t list

(** Percentile helper used by {!rank}: where [x] falls in [xs] (which includes it). *)
val percentile_of : float array -> float -> float
