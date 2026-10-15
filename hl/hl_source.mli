(** Read-only Hyperliquid source: [userFillsByTime] (paged), [userFunding] (paged),
    [clearinghouseState] and [portfolio] over [POST /info], against mainnet, testnet or
    hl-sim. No keys; nothing is signed or sent to the exchange endpoint.

    Requests are spaced by [pause] (default 1.2 s: most info requests weigh 20 of the 1200
    per minute an IP may spend). A non-2xx answer to [userFunding] or [portfolio] is
    treated as "none" (hl-sim serves neither); for fills and state it is an error.
    Transport failures are retried twice. *)

open! Core
open Async

type t

val create
  :  ?pause:Time_ns.Span.t
  -> ?max_fill_pages:int
  -> ?max_funding_pages:int
  -> ?portfolio_window:string
  -> ?include_spot:bool
  -> Hl_client.Config.t
  -> t

(** One read-only [/info] request; the body on a 2xx answer. *)
val info : t -> Yojson.Safe.t -> string Or_error.t Deferred.t

(** Every answer for [account] between [from] and [until], unparsed. *)
val fetch_raw
  :  t
  -> account:string
  -> from:Time_ns.t
  -> until:Time_ns.t
  -> Raw.t Or_error.t Deferred.t

include Leader_scoring.Source.S with type t := t
