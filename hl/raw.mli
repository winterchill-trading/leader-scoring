(** The raw [/info] answers for one account, as recorded: what fixtures are made of, and
    what {!Hl_source} decodes. On disk a recording is a directory:

    {v
    <dir>/meta.sexp              address, fetched_at, from, until
    <dir>/fills-<i>.json         userFillsByTime pages, oldest first
    <dir>/funding-<i>.json       userFunding pages (absent on hl-sim)
    <dir>/state.json             clearinghouseState
    <dir>/portfolio.json         portfolio (absent on hl-sim)
    v} *)

open! Core
open Async

module Meta : sig
  type t =
    { address : string
    ; fetched_at : Time_ns.Alternate_sexp.t
    ; from : Time_ns.Alternate_sexp.t
    ; until : Time_ns.Alternate_sexp.t
    ; fills_capped : bool (** the page limit stopped the fill walk *)
    }
  [@@deriving sexp]
end

type t =
  { meta : Meta.t
  ; fills_pages : string list
  ; funding_pages : string list
  ; state : string option
  ; portfolio : string option
  }

val save : t -> dir:string -> unit Deferred.t
val load : dir:string -> t Or_error.t Deferred.t

(** Decodes every part into a {!Leader_scoring.History.t}. The portfolio's [window] curve
    (default ["month"]) becomes the equity curve. [fills_truncated] is set when the walk
    was capped, or when at least 2000 fills start later than a tenth of the way into the
    period (the venue keeps only an account's most recent fills). *)
val to_history
  :  ?portfolio_window:string
  -> ?include_spot:bool
  -> t
  -> Leader_scoring.History.t Or_error.t
