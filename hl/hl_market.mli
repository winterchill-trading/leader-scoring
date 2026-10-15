(** Market context for copyability: order books ([l2Book]) and open interest
    ([metaAndAssetCtxs]), read only. *)

open! Core
open Async

(** Books for [coins]; coins whose request fails are left out. *)
val books : Hl_source.t -> coins:string list -> Leader_scoring.Copyability.Book.t String.Map.t Deferred.t

(** Open interest per perpetual in USD; empty when the venue does not answer
    [metaAndAssetCtxs] (hl-sim). *)
val open_interest : Hl_source.t -> float String.Map.t Deferred.t
