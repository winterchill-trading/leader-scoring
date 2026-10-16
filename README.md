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
| `leader-scoring` | `Leader_scoring` | venue neutral; `core`, `async_kernel`, `yojson`, `market-data` (for L2 books), `backtest-stats` |
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
| `Persistence` | Does a ranking persist into the next window: rank correlation, transitions, decay, random-pick baseline |
| `Leader_stats` | PSR, DSR and minimum track record per leader; PBO and SPA across the universe (backtest-stats) |
| `Integrity` | Flags for wash and self trades, counterparties, co-trading clusters, front-running, creation bursts, off-book fills |
| `Report` | Everything a leader page shows, and its JSON; `analyse` adds the universe checks |
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

## Persistence

A "top leaders" list is only worth showing if the order it shows says something about the
next period. `Persistence.run` cuts time into windows on a common grid (multiples of the
window since the epoch), ranks the leaders that cover each window on a formation measure
(window return, per-bar Sharpe ratio, or the composite score of the window's metrics), and
compares with the window after it:

- **Rank correlation**: Spearman between the formation measure in window `k` and the
  outcome measure in `k + 1`, averaged over windows, with a bootstrap interval (leaders
  resampled within each window), a t statistic over windows and the mean of the last
  third of the windows (a ranking that used to work and stopped is `fading`).
- **Transition matrix**: how many leaders move from formation quintile `i` to outcome
  quintile `j`, and the share of the top quintile still in the top half.
- **Decay**: the mean rank correlation `h = 1 .. 6` windows ahead.
- **Random-selection baseline**: the mean next-window return of the top `K` leaders against
  the same for `K` leaders drawn at random from the same window (1000 draws, each averaged
  over every window): the percentile of the top `K` among the draws and
  `p = (1 + draws at or above) / (1 + draws)`. Returns are capped at 100% a window, since a
  curve that starts from a nearly empty account shows thousands of percent no follower gets.

A ranking is **persistent** when the interval lies above zero and the top `K` beat at least
95% of the random draws. hl-leaderboard publishes this next to its ranking; a page should
not call anyone "top" while it is false.

`leader-scoring persistence -synthetic 1500 -days 400 -top-k 100` (30-day windows, daily
bars, Sharpe against Sharpe) and `leader-scoring persistence -replay data/mainnet-portfolios
-window-days 91 -bar-days 7 -common-bars 52 -top-k 10` (the all-time `portfolio` curves of
the 300 largest mainnet accounts by all-time pnl, read only, 2026-10-15; the curves are
sampled weekly, so 13-week windows of weekly bars):

| | Synthetic (1500 leaders, 11 periods) | Mainnet (282 leaders, 10 periods) |
| --- | --- | --- |
| Rank correlation, next window | 0.047 [0.030, 0.065], t 3.2, positive in 73% | 0.051 [-0.025, 0.120], t 0.7, positive in 60% |
| Decay, 1 to 6 windows ahead | 0.047, 0.043, 0.034, 0.018, 0.014, 0.040 | 0.051, 0.039, 0.039, 0.104, 0.136, -0.087 |
| Top quintile still in the top half | 52% | 49% |
| Top K next-window return vs random | K 100: +0.56% vs -1.50% (p05 -2.77%, p95 -0.23%), percentile 0.996 | K 10: 33.7% vs 33.8% (p05 26.7%, p95 40.4%), percentile 0.48 |
| Persistent | yes | no |

Other rules on the mainnet curves are worse, not better: ranking on window return gives a
rank correlation of -0.172 [-0.254, -0.071] (last quarter's winners lag), and the top 30 by
Sharpe earned 27.4% against 33.8% for random picks (percentile 0.003). On synthetic leaders
(a hidden edge that wanders and changes regime) the Sharpe ranking persists weakly and its
top 100 beat random picks, while ranking on raw return does not (percentile 0.05), and the
composite score persists about as well as Sharpe (0.029 [0.014, 0.046], percentile 0.994).
Read honestly: on this mainnet sample, which is itself selected by past success, no ranking
we tried says anything about the next quarter.

## Selection statistics

Picking the best of many leaders is picking the best of many backtests. `Leader_stats.run`
computes, per leader over its bar returns, a backtest-stats `Report`: the probabilistic
Sharpe ratio (PSR), the deflated Sharpe ratio (DSR, with the number of leaders as the number
of trials and the variance of their Sharpe ratios), and the minimum track record length for
95% confidence. Across the leaders that cover a common window it computes the probability
of backtest overfitting (PBO, combinatorially symmetric cross-validation) and the SPA test,
and judges each leader with a policy (PSR and DSR at least 0.95, PBO at most 0.5).

| | Synthetic (1500 leaders, daily) | Mainnet (299 leaders, weekly) |
| --- | --- | --- |
| PSR at least 0.95 | 106 | 237 |
| DSR at least 0.95 | 0 | 2 |
| Median PSR, DSR | 0.43, 0.00 | 0.99, 0.03 |
| Common window | 1380 leaders, 90 days | 92 leaders, 52 weeks |
| PBO | 0.41 | 0.14 |
| SPA p-value (best leader) | 0.012 | 0.20 |

The mainnet PSRs are high because the sample is the 300 accounts with the largest all-time
pnl: survivors, whose curves mostly rise. The DSR takes the selection into account and
leaves two.

## Integrity

`Integrity.check` looks for leaders that are manipulated or cannot be copied, from their
fills (and, when given, a flow of trades, recorded quotes and account creation times). Each
check returns a typed flag with its evidence:

| Flag | Evidence | Default threshold |
| --- | --- | --- |
| `self_trade` | the same trade id, coin, time and price on both sides of one account | any |
| `wash_trade` | opposite fills within 1 s at one price (1 bp) and size (10%) | 5 pairs and 20% of volume |
| `counterparty` | watched accounts on the two sides of the same trades, and their group | 10 trades, 5% of volume, 5 times the share random matching among watched accounts gives |
| `co_trading` | orders in one coin and side within 60 s of another account's, against the count with one side shifted circularly by sevenths of the observed span (at least 20 windows); clusters are connected components | 20 matches, similarity 0.3, 3 times the shifted count |
| `front_running` | orders followed within 5 s by 50k USD of net same-side flow, against shifted times | 20 orders, binomial z 3, twice the base rate |
| `creation_burst` | accounts first seen within a day of the first of them | 3 accounts; a warning only for members also linked by the flags above |
| `off_book_price` | fills more than 200 bps outside quotes within 0.5 s or other accounts' fills then | 3 fills and 10% |

`Integrity.Policy.default` takes every flag of severity warning or above out of the
composite ranking (`Report.rank`): the leader is still scored and shown, with the reasons.
The adversarial fixtures in `test/test_integrity.ml` (a wash trader, a self-trader, a pair
trading with each other, five sybil wallets placing the same orders, a front-runner, an
account filled 3% off the market) are all flagged; 20 independent noise traders, two
honest traders who react to the same ten announcements, and a closed market of 30
accounts matched at random (every one a counterparty of the others) are not. On a mainnet
collection of 34 leaders (146,761 fills, hl-leaderboard, read only) the only flags are
three accounts first seen within 40 minutes (info). The first thresholds (3 shared trades,
shifts of hours, 50 bps within 2 s) excluded all 43 accounts of a simulator run and one
mainnet leader sweeping a small perp; the defaults above come from those runs. Limits: counterparties are
only visible among watched accounts, front-running needs a trade flow (hl-leaderboard does
not collect one yet), and Hyperliquid does not publish account creation times (the first
point of the portfolio curve stands in).

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
