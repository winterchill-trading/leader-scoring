(** Every {!Metric.t} of one track record. Formulas are on {!Metric.formula} and in the
    README; a metric is [None] when it is undefined for the record (for example a Sharpe
    ratio with fewer than two bars, or a profit factor with no losing trade). *)

open! Core

module Config : sig
  type t =
    { bar : Time_ns.Span.t option (** [None]: {!auto_bar} of the covered span *)
    ; window_bars : int option (** rolling window [W]; [None]: {!auto_window} *)
    ; open_interest : float String.Map.t
    (** open interest per coin in USD, for {!Metric.Oi_share_max} *)
    }

  val default : t
end

(** One day for spans of 30 days or more, one hour from two days, one minute below: a
    month of history gets daily returns, a simulator run of minutes gets minute ones. *)
val auto_bar : span:Time_ns.Span.t -> Time_ns.Span.t

(** Seven days of bars for daily bars, one day for hourly, one hour for minutes, always at
    least 2 and at most half the bars. *)
val auto_window : bar:Time_ns.Span.t -> bars:int -> int

type t [@@deriving sexp_of]

val compute : ?config:Config.t -> Track_record.t -> t
val get : t -> Metric.t -> float option

(** Every metric in {!Metric.all} order. *)
val to_alist : t -> (Metric.t * float option) list

(** The bar the returns were taken over. *)
val bar : t -> Time_ns.Span.t

(** Bars in a year: [365 days / bar]. *)
val periods_per_year : t -> float

(** The bar returns [r_i]. *)
val returns : t -> float array

(** [{"name": value or null, ...}] in {!Metric.all} order. *)
val to_json : t -> Yojson.Safe.t

(** Metrics from stored values, for example read back from {!to_json}; missing metrics are
    [None]. [returns] defaults to empty. *)
val of_alist
  :  ?returns:float array
  -> bar:Time_ns.Span.t
  -> (Metric.t * float option) list
  -> t

(** Reads {!to_json} output: unknown names are ignored. *)
val of_json : bar:Time_ns.Span.t -> Yojson.Safe.t -> t Or_error.t
