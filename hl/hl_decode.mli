(** Hyperliquid [/info] answers decoded into leader-scoring's venue-neutral types.

    Amounts arrive as decimal strings with up to ten decimals; they are read as floats.
    Spot fills ([@107], [PURR/USDC]) are dropped unless [include_spot]: a perpetual
    account cannot mirror them. *)

open! Core
open Leader_scoring

(** A [userFills] or [userFillsByTime] page. *)
val fills : ?include_spot:bool -> Yojson.Safe.t -> Fill.t list Or_error.t

(** A [userFunding] page: [[{"time", "delta": {"type": "funding", "coin", "usdc", "szi",
    "fundingRate"}}]]; other delta types are skipped. *)
val funding : Yojson.Safe.t -> Funding.t list Or_error.t

(** A [clearinghouseState] answer; [time] is used when the answer carries none. *)
val snapshot : time:Time_ns.t -> Yojson.Safe.t -> Snapshot.t Or_error.t

(** A [portfolio] answer: the [window] curve (["day"], ["week"], ["month"],
    ["allTime"], or the ["perp..."] variants) as the account value at the window start
    plus cumulative pnl, so transfers are not gains. Empty when the window is missing. *)
val portfolio : window:string -> Yojson.Safe.t -> Equity_point.t list Or_error.t

(** A [metaAndAssetCtxs] answer: open interest per perpetual in USD
    ([openInterest * markPx]). *)
val open_interest : Yojson.Safe.t -> float String.Map.t Or_error.t

(** An [l2Book] answer. *)
val book : Yojson.Safe.t -> Copyability.Book.t Or_error.t

val is_spot : string -> bool
val time_of_ms : int -> Time_ns.t
