(** Everything a leader page shows, from one history: the track record, metrics,
    composite score, learned prediction, capacity curve and alerts, with a JSON form for
    services (hl-leaderboard serves it per leader). *)

open! Core

type t =
  { record : Track_record.t
  ; metrics : Metrics.t
  ; predicted : float option (** {!Model} prediction of the forward 30-day Sharpe *)
  ; copyability : Copyability.t option (** [None] without books *)
  ; alerts : Alerts.Alert.t list (** {!Alerts.scan} over the record, oldest first *)
  ; max_cost_bps : float (** the cost that defines capacity in [copyability] *)
  }

(** The alert windows used for a record: {!Alerts.Config.default} when it covers at
    least the default baseline and recent windows (37 days), otherwise
    {!Alerts.Config.scaled}. *)
val alerts_config : Track_record.t -> Alerts.Config.t

val create
  :  ?books:Copyability.Book.t String.Map.t
  -> ?open_interest:float String.Map.t
  -> ?model:Model.t
  -> ?copy_config:Copyability.Config.t
  -> History.t
  -> t

(** Score and rank reports together ({!Score.rank}); the result follows the ranking. *)
val rank : ?config:Score.Config.t -> t list -> (t * Score.Ranked.t) list

(** [{"account", "equity_source", "from_ms", "until_ms", "fills", "fills_truncated",
    "bar_seconds", "score": {...} | null, "predicted_forward_sharpe", "metrics": [...],
    "equity_curve": [[ms, equity], ...], "capacity": {...} | null, "alerts": [...]}]. The
    curve is thinned to at most [max_points] (default 400) evenly spaced points, always
    keeping the first and last. *)
val to_json : ?max_points:int -> t -> Score.Ranked.t option -> Yojson.Safe.t

(** Every metric with its name, label, unit, direction and formula. *)
val catalog_json : unit -> Yojson.Safe.t
