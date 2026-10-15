(** The metrics {!Metrics} computes, with their names, units and formulas.

    Notation: [e_0 .. e_n] is the equity series sampled at the end of each bar (see
    {!Metrics.Config}), [r_i = e_i / e_(i-1) - 1] the bar returns, [P] the number of bars
    in a year, [D] the covered span in days, and "closed trades" the round trips of
    {!Track_record.Trade} that ended inside the period. *)

open! Core

type t =
  | Total_return
  | Cagr
  | Volatility
  | Sharpe
  | Sortino
  | Calmar
  | Return_over_drawdown
  | Max_drawdown
  | Max_drawdown_duration_days
  | Current_drawdown
  | Trades
  | Win_rate
  | Profit_factor
  | Payoff_ratio
  | Avg_hold_hours
  | Trades_per_day
  | Leverage_median
  | Leverage_p95
  | Leverage_max
  | Coins
  | Top_coin_share
  | Coin_hhi
  | Worst_bar
  | Var_95
  | Cvar_95
  | Rolling_positive_share
  | Rolling_worst_return
  | Time_in_market
  | Long_bias
  | Bias_flips_per_30d
  | Bias_drift
  | Turnover_per_day
  | Fees_to_equity
  | Oi_share_max
[@@deriving sexp, compare, equal, enumerate, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

(** Snake-case name, used in JSON, CSV headers and the model's feature schema. Never
    renamed: models and dashboards key on it. *)
val name : t -> string

val of_name : string -> t option

(** Short human label for tables. *)
val label : t -> string

(** The formula, in the notation above. *)
val formula : t -> string

module Unit : sig
  type t =
    | Ratio (** a fraction; shown as a percentage *)
    | Number (** a plain number, e.g. a Sharpe ratio *)
    | Count
    | Days
    | Hours
    | Per_day
    | Multiple (** times equity, e.g. leverage *)
  [@@deriving sexp, compare, equal]

  val to_string : t -> string
end

val unit : t -> Unit.t

module Better : sig
  type t =
    | Higher
    | Lower
    | Neither
  [@@deriving sexp, compare, equal]

  val to_string : t -> string
end

(** Which direction is better for a follower; [Neither] for descriptive metrics. *)
val better : t -> Better.t
