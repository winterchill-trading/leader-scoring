open! Core
open Leader_scoring

(** Random fill histories over a few coins, closed by a snapshot. *)
val history : History.t Quickcheck.Generator.t

(** Every size, fee, funding amount and account value times [k]. *)
val scale : History.t -> float -> History.t

(** Every time moved by [span]. *)
val shift : History.t -> Time_ns.Span.t -> History.t
