(** Made-up leaders with known traits, for training and testing the learned score.

    A shared market of a few coins follows daily geometric Brownian motion. Each leader
    has a hidden skill: on every trade it picks the side the price will move over the
    holding period with probability [0.5 + skill], where [skill = mu + x], [x] an AR(1)
    wander, and [mu] is redrawn at random moments (a leader's edge decays or changes
    regime). Trades are sized at [leverage * equity / coins]; a martingale leader doubles
    its size after each loss (up to 8x) and resets after a win. A leader whose equity
    falls under 10% of its start stops trading. Every trade pays a taker fee on both
    fills. The history has the fills and one snapshot at the end of each day with the
    positions marked at that day's close, so the rebuilt equity curve is marked daily.

    Nothing here is calibrated to a real venue: the generator exists so the pipeline
    has a target with a known, partly persistent signal. *)

open! Core

module Profile : sig
  type t =
    { mu : float (** initial edge: hit rate is [0.5 + mu] *)
    ; wander : float (** stdev of the daily AR(1) shock to the edge *)
    ; regime_days : float (** mean days between redraws of [mu] *)
    ; leverage : float
    ; trades_per_day : float
    ; hold_days : float (** mean holding period *)
    ; long_bias : float (** in [-1, 1]: chance of overriding the chosen side *)
    ; coins : int
    ; martingale : bool
    }
  [@@deriving sexp]
end

(** Prices, one array per coin, day 0 to [days]. *)
val market : seed:int -> coins:int -> days:int -> float array array

(** One leader over [days] days from [start], trading [market]. *)
val leader
  :  seed:int
  -> start:Time_ns.t
  -> market:float array array
  -> account:string
  -> Profile.t
  -> History.t

(** [leaders] leaders with profiles drawn from [seed]. *)
val universe
  :  seed:int
  -> start:Time_ns.t
  -> leaders:int
  -> days:int
  -> (History.t * Profile.t) list
