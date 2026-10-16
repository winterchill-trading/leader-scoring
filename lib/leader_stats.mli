(** Selection-bias statistics for a ranking of leaders, from backtest-stats.

    Picking the best of many leaders is picking the best of many backtests: some look good
    by luck. Per leader this computes a {!Backtest_stats.Report.t} over its bar returns:
    the probabilistic Sharpe ratio (PSR, the chance the true Sharpe ratio is above zero
    given the track record's length, skewness and kurtosis), the deflated Sharpe ratio
    (DSR: the same against the best Sharpe ratio expected from that many leaders by luck,
    with the number of leaders as the number of trials and the variance of their Sharpe
    ratios), and the minimum track record length for 95% confidence.

    Across the universe, on the bars every leader in a common window shares, it computes
    the probability of backtest overfitting (PBO: how often the leader that looks best in
    one half of the time is below the median in the other half) and the SPA test (is the
    best leader better than zero after accounting for having picked it among all). Those
    universe figures are copied into every leader's report, whose verdict then judges the
    leader under {!Config.t.policy}. *)

open! Core

module Config : sig
  type t =
    { bar : Time_ns.Span.t
    ; common_bars : int (** length of the common window for PBO and SPA *)
    ; bootstrap_samples : int
    ; seed : int
    ; policy : Backtest_stats.Gate.Policy.t option
    }

  (** Daily bars, 90 common bars, 500 bootstrap samples, seed 0, and a policy of PSR and
      DSR at least 0.95 and PBO at most 0.5. *)
  val default : t

  (** PSR and DSR at least 0.95, PBO at most 0.5. *)
  val default_policy : Backtest_stats.Gate.Policy.t
end

module Universe : sig
  type t =
    { leaders : int (** with a report *)
    ; bar : Time_ns.Span.t
    ; periods_per_year : float
    ; trials : Backtest_stats.Report.Trials.t option
    ; common_from : Time_ns.Alternate_sexp.t option
    ; common_until : Time_ns.Alternate_sexp.t option
    ; common_leaders : int (** covering the common window *)
    ; common_periods : int
    ; pbo : Backtest_stats.Multiple_testing.Pbo.t option
    ; spa : Backtest_stats.Multiple_testing.Spa.t option
    ; spa_best : string option (** the leader with the largest studentized mean *)
    ; reality_check : Backtest_stats.Multiple_testing.Reality_check.t option
    ; accepted : int (** leaders whose verdict is accept *)
    }
  [@@deriving sexp_of]
end

type t =
  { universe : Universe.t
  ; reports : Backtest_stats.Report.t String.Map.t (** by account *)
  }

(** Simple returns between consecutive end-of-bar equities ({!Track_record.bars}); bars
    whose starting equity is not positive are left out and a fall below zero counts as
    -99.99%. *)
val returns : Track_record.t -> bar:Time_ns.Span.t -> float array

val run : ?config:Config.t -> Track_record.t list -> t

(** [{"leaders", "bar_days", "periods_per_year", "trials": {"count", "sharpe_variance"} | null, "common_from_ms", "common_until_ms", "common_leaders", "common_periods", "pbo", "pbo_combinations", "pbo_degradation_slope", "pbo_probability_of_loss", "spa_p_value", "spa_best", "reality_check_p_value", "accepted"}], probabilities as fractions, [null] where not computed. *)
val universe_json : Universe.t -> Yojson.Safe.t

(** A leader's report in the short form a page shows:
    [{"periods", "periods_per_year", "sharpe_annualized", "psr", "dsr", "min_track_record_periods", "skewness", "kurtosis", "pbo", "spa_p_value", "accepted", "reasons": [...]}]. *)
val leader_json : Backtest_stats.Report.t -> Yojson.Safe.t
