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

(** [leaders] synthetic leaders over [days] trading one market in the same style: with
    [skill] their edges are fixed and spread from -0.1 to 0.1 (hit rates 40% to 60%),
    without it every edge is zero, so any ranking is luck. *)
val planted : skill:bool -> leaders:int -> days:int -> Track_record.t list
