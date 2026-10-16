(** Does a ranking of leaders persist? The honesty check behind any "top leaders" list:
    rank the universe on one window, then see whether that order says anything about the
    next window.

    Time is cut into consecutive windows of [window] on a grid of multiples of [window]
    since the epoch, so every leader's windows line up. A leader takes part in window [k]
    when its record covers the whole window. Within a window a leader is measured on the
    end-of-bar equity ([bar] apart): {!Rule.Return} is the window's return,
    {!Rule.Sharpe} the per-bar Sharpe ratio (not annualized), {!Rule.Composite} the
    {!Score} composite of {!Metrics} computed on the window.

    For each window [k] followed by window [k + h]:

    - the {b rank correlation} (Spearman) between the [formation] measure in [k] and the
      [outcome] measure in [k + 1] among leaders present in both; the mean over windows
      has a bootstrap interval (leaders resampled within each window) and a t statistic
      over windows;
    - a {b transition matrix}: counts of leaders moving from formation quantile [i] to
      outcome quantile [j] (quantile 0 is the bottom), pooled over windows;
    - a {b decay curve}: the mean rank correlation for horizons [h = 1 .. horizons];
    - a {b random-selection baseline}: the mean next-window return of the [top_k]
      leaders by formation measure, against the same mean for [top_k] leaders drawn at
      random from the same window's participants ([random_draws] draws, each averaged over
      every window). A ranking that cannot beat random picks should not be shown as one.

    The ranking is called {e persistent} when the rank-correlation interval lies above
    zero and the top [top_k] beat at least 95% of the random draws. *)

open! Core

module Rule : sig
  type t =
    | Return (** window return *)
    | Sharpe (** per-bar Sharpe ratio, [mean / stdev] *)
    | Composite of Score.Config.t (** {!Score} of the window's {!Metrics} *)
  [@@deriving sexp_of]

  val to_string : t -> string
end

module Config : sig
  type t =
    { window : Time_ns.Span.t
    ; bar : Time_ns.Span.t
    ; formation : Rule.t (** what ranks leaders in window [k] *)
    ; outcome : Rule.t (** what the ranking is checked against in window [k + h] *)
    ; quantiles : int (** rows and columns of the transition matrix *)
    ; top_k : int (** leaders a follower would pick; capped at half the participants *)
    ; horizons : int (** points of the decay curve *)
    ; min_leaders : int (** windows with fewer common participants are skipped *)
    ; bootstrap_samples : int
    ; random_draws : int
    ; level : float (** of the bootstrap interval *)
    ; seed : int
    }
  [@@deriving sexp_of]

  (** 30-day windows of daily bars, Sharpe against Sharpe, quintiles, top 10, 6 horizons,
      at least 10 leaders, 1000 bootstrap samples and random draws, 95%, seed 0. *)
  val default : t
end

module Interval : sig
  type t =
    { estimate : float
    ; lower : float
    ; upper : float
    ; level : float
    }
  [@@deriving sexp_of]
end

(** One formation window and the window after it. *)
module Period : sig
  type t =
    { formation_start : Time_ns.Alternate_sexp.t
    ; leaders : int (** present in both windows *)
    ; rank_correlation : float option
    ; top_k_return : float option (** mean next-window return of the top [top_k] *)
    ; mean_return : float option (** mean next-window return of every participant *)
    }
  [@@deriving sexp_of]
end

module Baseline : sig
  type t =
    { top_k : int
    ; periods : int
    ; top_k_return : float (** mean over periods of the top [top_k]'s mean return *)
    ; random_mean : float (** mean over draws of the same for random picks *)
    ; random_p05 : float
    ; random_p95 : float
    ; percentile : float (** share of random draws below the top [top_k], ties half *)
    ; p_value : float (** [(1 + draws at or above) / (1 + draws)] *)
    ; draws : int
    }
  [@@deriving sexp_of]
end

module Decay_point : sig
  type t =
    { horizon : int (** windows ahead *)
    ; periods : int
    ; mean_rank_correlation : float option
    }
  [@@deriving sexp_of]
end

(** Where one leader stood in each formation window it took part in. *)
module Leader : sig
  type t =
    { windows : int
    ; percentiles : (Time_ns.Alternate_sexp.t * float) list
    (** window start and the leader's formation percentile there, 1 is the best *)
    ; mean_percentile : float option
    ; top_quantile_windows : int (** windows spent in the top quantile *)
    }
  [@@deriving sexp_of]
end

type t =
  { config : Config.t
  ; leaders : int (** with at least one complete window *)
  ; windows : int (** grid windows with at least [min_leaders] participants *)
  ; periods : Period.t list (** consecutive pairs, oldest first *)
  ; rank_correlation : Interval.t option (** mean over periods, bootstrap interval *)
  ; rank_correlation_t : float option (** mean / sd * sqrt(periods) over periods *)
  ; positive_share : float option (** periods with a positive rank correlation *)
  ; recent_rank_correlation : float option
  (** mean over the last third of the periods (at least two), to see a fading ranking *)
  ; transition : int array array (** [transition.(i).(j)]: formation [i], outcome [j] *)
  ; top_stays_top_half : float option
  (** share of the top formation quantile whose outcome is in the top half *)
  ; decay : Decay_point.t list
  ; baseline : Baseline.t option
  ; persistent : bool
  ; fading : bool
  (** persistent overall, but the recent rank correlation is zero or below *)
  ; by_leader : Leader.t String.Map.t
  }
[@@deriving sexp_of]

(** The windows of the grid that [records] cover. *)
val run : ?config:Config.t -> Track_record.t list -> t

(** A leader's own row, [None] when it has no complete window. *)
val leader : t -> string -> Leader.t option

(** [transition] as shares of each row. *)
val transition_shares : t -> float array array

(** The universe-level JSON:
    [{"formation", "outcome", "window_days", "bar_days", "leaders", "windows", "periods", "rank_correlation": {"estimate", "lower", "upper", "level"} | null, "rank_correlation_t", "positive_share", "recent_rank_correlation", "transition": [[...]], "top_stays_top_half", "decay": [{"horizon", "periods", "mean_rank_correlation"}], "baseline": {...} | null, "persistent", "fading"}]. *)
val to_json : t -> Yojson.Safe.t

(** [{"windows", "mean_percentile", "top_quantile_windows", "percentiles": [[ms, p], ...], "universe_persistent", "universe_rank_correlation"}]. *)
val leader_json : t -> Leader.t -> Yojson.Safe.t
