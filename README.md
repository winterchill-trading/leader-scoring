# leader-scoring

Who is worth copying? This library turns a trader's fills, funding payments and account
snapshots into a track record (round-trip trades and an equity curve), computes 34
documented performance and risk metrics, ranks a universe of traders with a transparent
weighted score, adds a learned forecast (a gradient-boosted model trained in Python and
scored here), estimates what copying would cost a follower of a given size from
order-book depth, and flags behaviour changes a follower should hear about.

Two opam packages:

| Package | Library | What it needs |
| --- | --- | --- |
| `leader-scoring` | `Leader_scoring` | venue neutral; `core`, `async_kernel`, `yojson`, `market-data` (for L2 books) |
| `leader-scoring-hl` | `Leader_scoring_hl`, binary `leader-scoring` | the Hyperliquid source over `hl-client`, read only |

**Read only.** The Hyperliquid source sends `POST /info` requests (`userFillsByTime`,
`userFunding`, `clearinghouseState`, `portfolio`, `l2Book`, `metaAndAssetCtxs`) and
nothing else: no keys, no signatures, no exchange endpoint. Reading mainnet is safe.

## Quick start

```ocaml
open! Core
open Leader_scoring

(* one leader *)
let report (h : History.t) ~books =
  let r = Report.create ~books ~model:(Lazy.force Model.bundled) h in
  Metrics.get r.metrics Sharpe, r.predicted, Option.bind r.copyability ~f:(fun c -> c.capacity)

(* a universe: composite score, rank and percentile per leader *)
let ranking histories = Report.rank (List.map histories ~f:(Report.create ?books:None))
```

```sh
# record one account's raw answers (read only), then score recordings
leader-scoring record -network mainnet -address 0x... -days 30 -out rec/0x...
leader-scoring score rec/*            # metrics and composite ranking; -json for JSON lines
leader-scoring metrics                # the metric table below
# training data for the learned score
leader-scoring portfolios -top 300 -out data/mainnet-portfolios     # mainnet, read only
leader-scoring dataset -synthetic 1500 -days 400 -out data/synthetic.csv
leader-scoring dataset -replay data/mainnet-portfolios -out data/mainnet.csv
```

[hl-leaderboard](https://github.com/winterchill-trading/hl-leaderboard) runs all of it
for its collected leaders and serves the result; the copy-trading web app shows it on
each leader's page.

## Modules

| Module | What it does |
| --- | --- |
| `Fill`, `Funding`, `Snapshot`, `Equity_point`, `History` | Venue-neutral inputs with `Stable.V1` Bin_prot forms |
| `Source` | `Source.S` (fetch a history) and an in-memory `Static` source |
| `Track_record` | Trades, the equity curve and exposure rebuilt from a history; time windows |
| `Metric`, `Metrics` | The metric catalogue (names, units, formulas) and its computation |
| `Score` | Weighted percentile composite and ranking with eligibility floors |
| `Copyability` | Slippage per follower size from book depth, capacity curve |
| `Alerts` | Drawdown, leverage, style, frequency and liquidation alerts |
| `Synthetic`, `Dataset`, `Model` | Training data, the forward label, the exported model's scorer |
| `Report` | Everything a leader page shows, and its JSON |
| `Stats` | Quantiles, weighted quantiles, rank correlation |
| `Leader_scoring_hl.Hl_source`, `Raw`, `Hl_decode`, `Hl_market` | Hyperliquid fetch, recordings, decoders, books and open interest |

## Track record

`Track_record.build` replays the fills per coin on average-entry books:

- **Positions held when the history starts** are seeded at its first instant: from the
  `position_before` of a coin's first fill (Hyperliquid's `startPosition`), or from the
  latest snapshot for a coin with no fill at all (marked at the snapshot's price
  throughout). Such trades say `seeded`; their real entry is older.
- **Missed fills** show up as a fill whose `position_before` disagrees with the replay;
  the position is repaired at that fill's price and counted in `position_mismatches`.
- **Trades** open when a coin's position leaves zero and close when it returns to zero;
  a flip closes one and opens the next, splitting the fee by size. Funding is credited
  to the trade open on that coin.
- **Equity** is `E0 + realized - fees + funding + sum(position * (mark - entry))`, each
  coin marked at its latest fill or snapshot price. `E0` makes the curve meet the latest
  snapshot's account value (`anchored`), else a given initial equity (`given`), else the
  peak gross notional (`assumed`: returns are then indicative only). A venue curve of
  at least ten points (Hyperliquid's `portfolio` month window: account value at the
  window start plus cumulative pnl) is preferred for returns (`venue`). Deposits and
  withdrawals never move either curve.

Hyperliquid specifics in `leader-scoring-hl`: spot fills (`@107`, `PURR/USDC`) and
`#<id>` instruments are dropped (a perp account cannot mirror them and they never appear
in `clearinghouseState`); the venue keeps only an account's 10,000 most recent fills, so a
history of 2000 or more fills that starts after a tenth of the period is marked
`fills_truncated`.

## Metrics

Notation: `e_0 .. e_n` the equity at the end of each bar, `r_i = e_i / e_(i-1) - 1`, `P`
bars per year, `D` covered days. The bar is a day for 30 days or more of history, an
hour from two days, a minute below (a simulator run); `P` follows it, so annualized
figures of minute bars are large and only comparable among records with the same bar.
Every metric is `None` (JSON `null`) where undefined.

| Metric | Name | Unit | Better | Formula |
| --- | --- | --- | --- | --- |
| Return | `total_return` | ratio | higher | e_n / e_0 - 1 |
| CAGR | `cagr` | ratio | higher | (e_n / e_0) ^ (365 / D) - 1; only when D >= 90 |
| Volatility | `volatility` | ratio | lower | stdev(r) * sqrt(P), sample standard deviation |
| Sharpe | `sharpe` | number | higher | mean(r) / stdev(r) * sqrt(P), risk-free rate 0 |
| Sortino | `sortino` | number | higher | mean(r) / sqrt(mean(min(r, 0)^2)) * sqrt(P) |
| Calmar | `calmar` | number | higher | cagr / max_drawdown |
| Return / drawdown | `return_over_drawdown` | number | higher | total_return / max_drawdown |
| Max drawdown | `max_drawdown` | ratio | lower | max over points of (peak - e) / peak, peak the running maximum |
| Longest drawdown | `max_drawdown_duration_days` | days | lower | longest time from a peak to the first point back at it (or the end) |
| Current drawdown | `current_drawdown` | ratio | lower | (peak - e_last) / peak |
| Trades | `trades` | count | neither | closed trades |
| Win rate | `win_rate` | ratio | higher | closed trades with net pnl > 0 / closed trades |
| Profit factor | `profit_factor` | number | higher | sum of winning net pnl / \|sum of losing net pnl\| |
| Payoff ratio | `payoff_ratio` | number | higher | mean winning net pnl / \|mean losing net pnl\| |
| Average hold | `avg_hold_hours` | hours | neither | mean of closed_at - opened_at over closed trades |
| Trades per day | `trades_per_day` | per_day | neither | trades opened in the period / D |
| Leverage (median) | `leverage_median` | multiple | lower | time-weighted median of gross notional / equity |
| Leverage (p95) | `leverage_p95` | multiple | lower | time-weighted 95th percentile of gross notional / equity |
| Leverage (max) | `leverage_max` | multiple | lower | max of gross notional / equity |
| Coins traded | `coins` | count | neither | distinct coins with fills |
| Top coin share | `top_coin_share` | ratio | lower | largest coin's traded notional / total traded notional |
| Concentration (HHI) | `coin_hhi` | number | lower | sum over coins of (coin notional / total notional)^2 |
| Worst bar | `worst_bar` | ratio | higher | min(r); the worst day when bars are days |
| VaR 95% | `var_95` | ratio | lower | -quantile(r, 0.05), historical, linear interpolation |
| CVaR 95% | `cvar_95` | ratio | lower | -mean(r_i for r_i <= quantile(r, 0.05)) |
| Positive windows | `rolling_positive_share` | ratio | higher | share of rolling windows (W bars, step 1) with e_(i+W) > e_i |
| Worst window | `rolling_worst_return` | ratio | higher | min over rolling windows of e_(i+W) / e_i - 1 |
| Time in market | `time_in_market` | ratio | neither | time with any open position / covered time |
| Long bias | `long_bias` | number | neither | time-weighted mean of net / gross notional while exposed, in [-1, 1] |
| Bias flips per 30d | `bias_flips_per_30d` | per_day | lower | sign changes of net exposure (flat periods skipped) * 30 / D |
| Bias drift | `bias_drift` | number | lower | \|long_bias of the first half - long_bias of the second half\| |
| Turnover per day | `turnover_per_day` | multiple | lower | traded notional / mean equity / D |
| Fees / equity | `fees_to_equity` | ratio | lower | fees paid / e_0 |
| Max share of open interest | `oi_share_max` | ratio | lower | max over trades of max_notional / open interest of the coin (USD) |

Property tests (`test/test_metrics.ml`, Base_quickcheck over random fill histories):

- **Scaling invariance**: multiplying every size, fee, funding amount and account value
  by `k` in `[0.01, 100]` leaves every metric unchanged (relative 1e-6).
- **Time invariance**: moving the whole history by up to 400 days leaves every metric
  unchanged.
- **Bounds**: drawdowns, win rate, time in market, shares and HHI in `[0, 1]`, long bias
  in `[-1, 1]`, volatility and leverage non-negative, `cvar_95 >= var_95`,
  `coin_hhi >= 1 / coins`.
- **Monotonicity**: a deeper trough below an earlier peak never lowers the max
  drawdown; an extra winning round trip never lowers the win rate, the profit factor or
  the trade count. `Score` (`test/test_score.ml`): improving one metric never lowers that
  leader's score, and the input order never changes scores. `Copyability`: the cost never
  falls as the follower grows.

## Score

For each weighted metric, a leader's percentile among the eligible leaders that have it,
`p = (below + (equal - 1) / 2) / (n - 1)`, flipped to `1 - p` where lower is better; a
missing value counts 0.5. The score is `100 * sum(w * p) / sum(w)`; every component is
reported with its value, percentile, weight and points.

| Metric | Weight | | Metric | Weight |
| --- | ---: | --- | --- | ---: |
| `sharpe` | 0.15 | | `rolling_positive_share` | 0.10 |
| `sortino` | 0.10 | | `profit_factor` | 0.05 |
| `return_over_drawdown` | 0.10 | | `win_rate` | 0.05 |
| `total_return` | 0.05 | | `leverage_p95` (lower) | 0.10 |
| `max_drawdown` (lower) | 0.15 | | `coin_hhi` (lower) | 0.05 |
| `cvar_95` (lower) | 0.05 | | `bias_drift` (lower) | 0.05 |

Leaders with fewer than 5 closed trades (`Score.Config.min_trades`) are scored but not
ranked. Weights and floors are configuration (`Score.Config.t`, sexp).

## Copyability

Each leader order (fills of one coin, millisecond and side) is scaled to a follower of
equity `X`: `n_f = n_l * X / E_l(t)`. The order walks the coin's book from the best
level; its cost is the average fill price against the mid in basis points (so it
includes half the spread), and notional the book cannot fill is charged 100 bps. The
leader's cost at `X` is the notional-weighted mean; the capacity is the largest `X`
costing at most 10 bps (a bisection on `log X`, since the cost only grows with `X`). The
curve is reported at 1k, 10k, 100k, 1M and 10M USD with mean, median, p90 and the
unfilled share. Limits: books are snapshots taken when the estimate runs, applied to
past orders; fees are not included; open interest only enters through `oi_share_max`.

## Alerts

A recent window ending now is compared with the baseline window before it (7 and 30
days by default; a sixth and a half of the record for short records):

- drawdown from the running peak at least 25%, or 1.5 times the baseline's worst
  (critical from 50%);
- recent max leverage at least 3x and 2 times the baseline's p95;
- long bias moved by 1 or more, or a new dominant coin (half the recent notional, not
  in the baseline's top three);
- trade frequency 3 times higher or lower;
- a liquidation fill in the recent window (critical).

`Alerts.scan` replays this along the record and keeps the moment each alert turned on.

## Learned score

> **Mostly synthetic, weak signal.** The model below learns from made-up leaders and a
> coarse replay of real ones. Its forecasts are a ranking aid next to the transparent
> score, not a prediction to trade on.

- **Target** (`Dataset.label`): the forward 30-day risk-adjusted return, the Sharpe
  ratio of the 30 daily returns after the cutoff, not annualized
  (`mean / max(stdev, 0.001) * sqrt(n)`), clipped to `[-5, 5]`.
- **Features**: 31 metrics (all but CAGR, Calmar and the open-interest share) of the 90
  days before the cutoff, computed by `Metrics` itself and written by
  `leader-scoring dataset`, so training and serving share one implementation. Cutoffs
  every 15 days on one grid, so each cutoff is a cross-section of leaders.
- **Data**: 1500 synthetic leaders over 400 days (`Synthetic`: a hidden edge that
  wanders and changes regime, leverage from 0.5x to 10x, a tenth of them martingale
  sizers; 25,067 labelled rows) and the `portfolio` all-time curves of the 300 largest
  mainnet accounts by all-time pnl (read only, 2026-10-15; 287 usable, 9,092 labelled
  rows). The mainnet curves are sampled about every two weeks, so their daily returns
  are mostly zero with occasional jumps, and only curve metrics are defined (no fills).
- **Model**: scikit-learn 1.9.1 `HistGradientBoostingRegressor`, 200 trees of 7
  leaves, learning rate 0.03, at least 300 rows per leaf. Exported as JSON
  (`models/leader-forward-sharpe-gbdt/model.json`, 124 KB), embedded in the library
  (`Model.bundled`) and scored in OCaml; the 200-row parity fixture matches scikit-learn
  exactly (difference 0).
- **Walk-forward**: for each cutoff, fit on rows whose label window had ended, predict
  the cross-section, and take the Spearman rank correlation (IC) with the label.
  Synthetic folds train on synthetic rows; mainnet folds train on every synthetic row
  plus the mainnet rows already in the past.

| Information coefficient | Synthetic (15 folds, 20,839 rows) | Mainnet replay (69 folds, 9,063 rows) |
| --- | --- | --- |
| Model | 0.061 (sd 0.021, t 11.3, positive in 100% of folds) | 0.026 (sd 0.249, t 0.9, 52%) |
| Composite score | 0.031 (t 4.3, 93%) | 0.005 (t 0.2, 49%) |
| Past Sharpe alone | 0.060 (t 7.7, 100%) | 0.040 (t 1.5, 54%) |
| Past return alone | 0.038 (t 4.9, 93%) | 0.007 (t 0.2, 51%) |
| Top fifth by model: label minus cross-section mean | +0.076 (t 7.8) | +0.080 (t 2.0) |

Read honestly: on synthetic leaders the model is as good as past Sharpe on average and
more stable across folds; on the mainnet replay nothing is significant, and past Sharpe
alone does at least as well. The hyperparameters were picked between two settings on
the same synthetic folds (the other, 150 trees of 15 leaves, gave IC 0.053), which
flatters the synthetic numbers slightly. Retrain on real fill histories collected over
months before giving the forecast any weight. Reproduce:

```sh
cd python && uv sync
uv run python train.py --data ../data/synthetic.csv --data ../data/mainnet.csv
```

## Cost

`bench/main.exe` (core_bench, Apple M-series, dev profile), one synthetic leader with
2038 fills, 400 daily snapshots and 1019 trades:

| Step | Time | Minor words |
| --- | ---: | ---: |
| `Track_record.build` | 1.51 ms | 516k |
| `Metrics.compute` | 1.13 ms | 184k |
| `Score.rank`, 300 leaders | 3.17 ms (10.6 us a leader) | 1.31M |
| `Model.predict_metrics` | 2.2 us | 1.4k |
| `Copyability.estimate`, 6 books of 50 levels | 4.53 ms | 1.46M |
| `Alerts.detect` | 1.43 ms | 244k |
| `Alerts.scan`, weekly over 400 days | 56 ms | 8.9M |
| `Report.create` (all of the above, scan every 1.75 days) | 241 ms | 38M |

The book walk was 65 ms before it moved to running depth totals and a binary search
(`Depth.take`); the alert scan dominates a whole report and grows with the record's
length divided by the step.

## Tests and fixtures

```sh
dune build @all @runtest
dune exec bench/main.exe -- -quota 2
```

`hl/test/fixtures/` holds two read-only recordings: an hl-sim trend leader (246 fills)
and a mainnet account (perp positions held through the month, funding trimmed to the
oldest 150 payments, `#` instruments in its fills). `test/fixtures/model-parity.csv` is
written by `train.py`.
