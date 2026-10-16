(** Everything a leader page shows, from one history: the track record, metrics, composite
    score, learned prediction, capacity curve and alerts, with a JSON form for services
    (hl-leaderboard serves it per leader). *)

open! Core

type t =
  { record : Track_record.t
  ; metrics : Metrics.t
  ; predicted : float option (** {!Model} prediction of the forward 30-day Sharpe *)
  ; copyability : Copyability.t option (** [None] without books *)
  ; alerts : Alerts.Alert.t list (** {!Alerts.scan} over the record, oldest first *)
  ; max_cost_bps : float (** the cost that defines capacity in [copyability] *)
  ; persistence : Persistence.t option
  (** the universe's persistence test, shared by every report; set by {!analyse} *)
  ; stats : Backtest_stats.Report.t option
  (** PSR, DSR, minimum track record, with the universe's PBO and SPA; set by {!analyse} *)
  ; integrity : Integrity.Flag.t list (** set by {!analyse} *)
  }

(** What {!analyse} measures across the universe. *)
module Universe : sig
  type t =
    { persistence : Persistence.t
    ; stats : Leader_stats.Universe.t
    ; flagged : int (** leaders with any integrity flag *)
    ; excluded : int (** leaders the policy takes out of the ranking *)
    ; policy : Integrity.Policy.t
    }

  (** [{"persistence": {...}, "stats": {...}, "integrity": {"flagged", "excluded", "policy": {"exclude": [...], "min_severity"}}}]. *)
  val to_json : t -> Yojson.Safe.t
end

(** The alert windows used for a record: {!Alerts.Config.default} when it covers at least
    the default baseline and recent windows (37 days), otherwise {!Alerts.Config.scaled}. *)
val alerts_config : Track_record.t -> Alerts.Config.t

val create
  :  ?books:Copyability.Book.t String.Map.t
  -> ?open_interest:float String.Map.t
  -> ?model:Model.t
  -> ?copy_config:Copyability.Config.t
  -> History.t
  -> t

(** Runs the universe-level checks over [reports] and stores each leader's part in its
    report: {!Persistence.run} on the records, {!Leader_stats.run} (the number of leaders
    is the number of trials), and the given integrity flags ({!Integrity.check}, which
    needs the histories). *)
val analyse
  :  ?persistence:Persistence.Config.t
  -> ?stats:Leader_stats.Config.t
  -> ?integrity:Integrity.Flag.t list String.Map.t
  -> ?policy:Integrity.Policy.t
  -> t list
  -> Universe.t * t list

(** Score and rank reports together ({!Score.rank}); the result follows the ranking.
    Leaders whose integrity flags the [policy] (default {!Integrity.Policy.default})
    excludes are scored but not ranked. *)
val rank
  :  ?config:Score.Config.t
  -> ?policy:Integrity.Policy.t
  -> t list
  -> (t * Score.Ranked.t) list

(** [{"account", "equity_source", "from_ms", "until_ms", "fills", "fills_truncated", "bar_seconds", "score": {...} | null, "predicted_forward_sharpe", "metrics": [...], "equity_curve": [[ms, equity], ...], "capacity": {...} | null, "alerts": [...], "persistence": {...} | null, "stats": {...} | null, "integrity": [...]}]
    ({!Persistence.leader_json}, {!Leader_stats.leader_json}, {!Integrity.flag_json}). The
    curve is thinned to at most [max_points] (default 400) evenly spaced points, always
    keeping the first and last. *)
val to_json : ?max_points:int -> t -> Score.Ranked.t option -> Yojson.Safe.t

(** Every metric with its name, label, unit, direction and formula. *)
val catalog_json : unit -> Yojson.Safe.t
