(** Where histories come from. A venue adapter (for example [Leader_scoring_hl.Hl_source]
    in the [leader-scoring-hl] package) implements {!S}; the scoring code only sees
    {!History.t}. *)

open! Core
open Async_kernel

module type S = sig
  type t

  (** Short venue name stored in {!History.venue}. *)
  val venue : string

  (** Fills, funding and snapshots of [account] between [from] and [until], and the
      venue's equity curve when it has one. Read only. *)
  val fetch
    :  t
    -> account:string
    -> from:Time_ns.t
    -> until:Time_ns.t
    -> History.t Or_error.t Deferred.t
end

(** Histories held in memory, restricted to the period asked for: for tests, fixtures and
    replays of recorded data. *)
module Static : sig
  type t

  val create : History.t list -> t

  include S with type t := t
end
