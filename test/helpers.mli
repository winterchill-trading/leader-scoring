open! Core
open Leader_scoring

val t0 : Time_ns.t

(** [at ?h d]: [d] days (and [h] hours) after [t0]. *)
val at : ?h:float -> float -> Time_ns.t

val fill
  :  ?fee:float
  -> ?position_before:float
  -> ?liquidation:bool
  -> time:Time_ns.t
  -> coin:string
  -> px:float
  -> float
  -> Fill.t

(** Positions are [(coin, size, mark)]. *)
val snapshot
  :  time:Time_ns.t
  -> account_value:float
  -> (string * float * float) list
  -> Snapshot.t

val history
  :  ?funding:Funding.t list
  -> ?snapshots:Snapshot.t list
  -> ?equity_curve:Equity_point.t list
  -> Fill.t list
  -> History.t

(** Six decimals, so expect output does not depend on the last bits. *)
val round : float -> float
