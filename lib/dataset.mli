(** Training rows for the learned score: features from the past [lookback] of a track
    record and the label from the [horizon] after it.

    The label is the forward risk-adjusted return: over the daily bar returns [r] of the
    [horizon] window, [mean(r) / max(stdev(r), 0.001) * sqrt(n)] (a [horizon]-long Sharpe
    ratio, not annualized; the floor keeps a flat account from dividing by zero), clipped
    to [[-5, 5]]. Features are {!feature_metrics} computed by {!Metrics} on the lookback
    window, so training and serving share one implementation. *)

open! Core

(** Every metric but CAGR and Calmar (undefined under 90 days, redundant at 90) and the
    open-interest share (needs market data the history does not carry). *)
val feature_metrics : Metric.t list

val feature_names : string list

(** The model's input row; [nan] where a metric is undefined. *)
val features : Metrics.t -> float array

(** The label over [[from, until]]; [None] with fewer than 10 daily returns or an equity
    that is not positive. *)
val label : Track_record.t -> from:Time_ns.t -> until:Time_ns.t -> float option

module Row : sig
  type t =
    { group : string (** where the row comes from, e.g. [synthetic] or [mainnet] *)
    ; account : string
    ; cutoff : Time_ns.t
    ; features : float array
    ; label : float option
    }

  val csv_header : string
  val to_csv : t -> string
end

(** One row per cutoff between [from + lookback] and [until - horizon], on the grid of
    multiples of [step] since the epoch, so every account's rows of one cutoff line up
    (defaults 90, 30 and 15 days). *)
val rows
  :  ?lookback:Time_ns.Span.t
  -> ?horizon:Time_ns.Span.t
  -> ?step:Time_ns.Span.t
  -> group:string
  -> Track_record.t
  -> Row.t list
