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

module Unit = struct
  type t =
    | Ratio
    | Number
    | Count
    | Days
    | Hours
    | Per_day
    | Multiple
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Ratio -> "ratio"
    | Number -> "number"
    | Count -> "count"
    | Days -> "days"
    | Hours -> "hours"
    | Per_day -> "per_day"
    | Multiple -> "multiple"
  ;;
end

module Better = struct
  type t =
    | Higher
    | Lower
    | Neither
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Higher -> "higher"
    | Lower -> "lower"
    | Neither -> "neither"
  ;;
end

type info =
  { name : string
  ; label : string
  ; formula : string
  ; unit : Unit.t
  ; better : Better.t
  }

let info : t -> info = function
  | Total_return ->
    { name = "total_return"
    ; label = "Return"
    ; formula = "e_n / e_0 - 1"
    ; unit = Ratio
    ; better = Higher
    }
  | Cagr ->
    { name = "cagr"
    ; label = "CAGR"
    ; formula = "(e_n / e_0) ^ (365 / D) - 1; only when D >= 7"
    ; unit = Ratio
    ; better = Higher
    }
  | Volatility ->
    { name = "volatility"
    ; label = "Volatility"
    ; formula = "stdev(r) * sqrt(P), sample standard deviation"
    ; unit = Ratio
    ; better = Lower
    }
  | Sharpe ->
    { name = "sharpe"
    ; label = "Sharpe"
    ; formula = "mean(r) / stdev(r) * sqrt(P), risk-free rate 0"
    ; unit = Number
    ; better = Higher
    }
  | Sortino ->
    { name = "sortino"
    ; label = "Sortino"
    ; formula = "mean(r) / sqrt(mean(min(r, 0)^2)) * sqrt(P)"
    ; unit = Number
    ; better = Higher
    }
  | Calmar ->
    { name = "calmar"
    ; label = "Calmar"
    ; formula = "cagr / max_drawdown"
    ; unit = Number
    ; better = Higher
    }
  | Return_over_drawdown ->
    { name = "return_over_drawdown"
    ; label = "Return / drawdown"
    ; formula = "total_return / max_drawdown"
    ; unit = Number
    ; better = Higher
    }
  | Max_drawdown ->
    { name = "max_drawdown"
    ; label = "Max drawdown"
    ; formula = "max over points of (peak - e) / peak, peak the running maximum"
    ; unit = Ratio
    ; better = Lower
    }
  | Max_drawdown_duration_days ->
    { name = "max_drawdown_duration_days"
    ; label = "Longest drawdown"
    ; formula = "longest time from a peak to the first point back at it (or the end)"
    ; unit = Days
    ; better = Lower
    }
  | Current_drawdown ->
    { name = "current_drawdown"
    ; label = "Current drawdown"
    ; formula = "(peak - e_last) / peak"
    ; unit = Ratio
    ; better = Lower
    }
  | Trades ->
    { name = "trades"
    ; label = "Trades"
    ; formula = "closed trades"
    ; unit = Count
    ; better = Neither
    }
  | Win_rate ->
    { name = "win_rate"
    ; label = "Win rate"
    ; formula = "closed trades with net pnl > 0 / closed trades"
    ; unit = Ratio
    ; better = Higher
    }
  | Profit_factor ->
    { name = "profit_factor"
    ; label = "Profit factor"
    ; formula = "sum of winning net pnl / |sum of losing net pnl|"
    ; unit = Number
    ; better = Higher
    }
  | Payoff_ratio ->
    { name = "payoff_ratio"
    ; label = "Payoff ratio"
    ; formula = "mean winning net pnl / |mean losing net pnl|"
    ; unit = Number
    ; better = Higher
    }
  | Avg_hold_hours ->
    { name = "avg_hold_hours"
    ; label = "Average hold"
    ; formula = "mean of closed_at - opened_at over closed trades"
    ; unit = Hours
    ; better = Neither
    }
  | Trades_per_day ->
    { name = "trades_per_day"
    ; label = "Trades per day"
    ; formula = "trades opened in the period / D"
    ; unit = Per_day
    ; better = Neither
    }
  | Leverage_median ->
    { name = "leverage_median"
    ; label = "Leverage (median)"
    ; formula = "time-weighted median of gross notional / equity"
    ; unit = Multiple
    ; better = Lower
    }
  | Leverage_p95 ->
    { name = "leverage_p95"
    ; label = "Leverage (p95)"
    ; formula = "time-weighted 95th percentile of gross notional / equity"
    ; unit = Multiple
    ; better = Lower
    }
  | Leverage_max ->
    { name = "leverage_max"
    ; label = "Leverage (max)"
    ; formula = "max of gross notional / equity"
    ; unit = Multiple
    ; better = Lower
    }
  | Coins ->
    { name = "coins"
    ; label = "Coins traded"
    ; formula = "distinct coins with fills"
    ; unit = Count
    ; better = Neither
    }
  | Top_coin_share ->
    { name = "top_coin_share"
    ; label = "Top coin share"
    ; formula = "largest coin's traded notional / total traded notional"
    ; unit = Ratio
    ; better = Lower
    }
  | Coin_hhi ->
    { name = "coin_hhi"
    ; label = "Concentration (HHI)"
    ; formula = "sum over coins of (coin notional / total notional)^2"
    ; unit = Number
    ; better = Lower
    }
  | Worst_bar ->
    { name = "worst_bar"
    ; label = "Worst bar"
    ; formula = "min(r); the worst day when bars are days"
    ; unit = Ratio
    ; better = Higher
    }
  | Var_95 ->
    { name = "var_95"
    ; label = "VaR 95%"
    ; formula = "-quantile(r, 0.05), historical, linear interpolation"
    ; unit = Ratio
    ; better = Lower
    }
  | Cvar_95 ->
    { name = "cvar_95"
    ; label = "CVaR 95%"
    ; formula = "-mean(r_i for r_i <= quantile(r, 0.05))"
    ; unit = Ratio
    ; better = Lower
    }
  | Rolling_positive_share ->
    { name = "rolling_positive_share"
    ; label = "Positive windows"
    ; formula = "share of rolling windows (W bars, step 1) with e_(i+W) > e_i"
    ; unit = Ratio
    ; better = Higher
    }
  | Rolling_worst_return ->
    { name = "rolling_worst_return"
    ; label = "Worst window"
    ; formula = "min over rolling windows of e_(i+W) / e_i - 1"
    ; unit = Ratio
    ; better = Higher
    }
  | Time_in_market ->
    { name = "time_in_market"
    ; label = "Time in market"
    ; formula = "time with any open position / covered time"
    ; unit = Ratio
    ; better = Neither
    }
  | Long_bias ->
    { name = "long_bias"
    ; label = "Long bias"
    ; formula = "time-weighted mean of net / gross notional while exposed, in [-1, 1]"
    ; unit = Number
    ; better = Neither
    }
  | Bias_flips_per_30d ->
    { name = "bias_flips_per_30d"
    ; label = "Bias flips per 30d"
    ; formula = "sign changes of net exposure (flat periods skipped) * 30 / D"
    ; unit = Per_day
    ; better = Lower
    }
  | Bias_drift ->
    { name = "bias_drift"
    ; label = "Bias drift"
    ; formula = "|long_bias of the first half - long_bias of the second half|"
    ; unit = Number
    ; better = Lower
    }
  | Turnover_per_day ->
    { name = "turnover_per_day"
    ; label = "Turnover per day"
    ; formula = "traded notional / mean equity / D"
    ; unit = Multiple
    ; better = Lower
    }
  | Fees_to_equity ->
    { name = "fees_to_equity"
    ; label = "Fees / equity"
    ; formula = "fees paid / e_0"
    ; unit = Ratio
    ; better = Lower
    }
  | Oi_share_max ->
    { name = "oi_share_max"
    ; label = "Max share of open interest"
    ; formula = "max over trades of max_notional / open interest of the coin (USD)"
    ; unit = Ratio
    ; better = Lower
    }
;;

let name t = (info t).name
let label t = (info t).label
let formula t = (info t).formula
let unit t = (info t).unit
let better t = (info t).better
let by_name = String.Map.of_alist_exn (List.map all ~f:(fun t -> name t, t))
let of_name s = Map.find by_name s

(* Last, so the comparison operators on [t] do not shadow the float ones above. *)
include Comparable.Make (struct
    type nonrec t = t [@@deriving sexp, compare]
  end)

include Hashable.Make (struct
    type nonrec t = t [@@deriving sexp, compare, hash]
  end)
