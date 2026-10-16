# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[Semantic Versioning](https://semver.org/).

## Unreleased

### Added

- `Persistence`: does a ranking persist into the next window (rank correlation with a
  bootstrap interval, transition matrix, decay over horizons, top-K against random picks);
  per-leader window percentiles.
- `Leader_stats`: backtest-stats reports per leader (PSR, DSR with the leaders as trials,
  minimum track record) and PBO, SPA and the reality check across the universe.
- `Integrity`: typed flags with evidence for self and wash trades, counterparties among
  watched accounts, co-trading clusters, front-running of a trade flow, creation bursts and
  off-book fill prices; `Integrity.Policy` decides which flags exclude a leader.
- `Report.t` gains `persistence`, `stats` and `integrity`; `Report.analyse` fills them and
  `Report.Universe` holds the universe-level results. `Report.rank` takes `?policy` and
  leaves excluded leaders unranked; `Score.rank` takes `?excluded`.
- CLI: `leader-scoring persistence` over synthetic leaders or recordings.

### Fixed

- `History.normalize` kept only one side of a self trade (same id, time and coin).

## 0.1.0

First version prepared for opam; not yet published.

### Added

- Venue-neutral inputs with `Stable.V1` Bin_prot forms: `Fill`, `Funding`, `Snapshot`,
  `Equity_point`, `History`; `Source.S` and an in-memory `Source.Static`.
- `Track_record`: round-trip trades and an equity curve rebuilt from fills, funding and
  snapshots, with positions held at the start seeded, missed fills repaired and the
  curve anchored on the latest snapshot or taken from a venue curve; time windows.
- `Metric` and `Metrics`: 34 metrics (returns, CAGR, volatility, Sharpe, Sortino,
  Calmar, drawdowns and their duration, trade statistics, leverage distribution,
  concentration, VaR and CVaR, rolling consistency, time in market, long bias and its
  drift, turnover, fees, open-interest share) with formulas, units and directions.
- `Score`: weighted percentile composite with eligibility floors and per-component
  reporting.
- `Copyability`: follower slippage from order-book depth and a capacity curve.
- `Alerts`: drawdown breach, leverage spike, style drift, frequency change and
  liquidation, detected per moment or scanned along a record.
- `Synthetic`, `Dataset` and `Model`: training rows with a forward 30-day risk-adjusted
  label, and an embedded gradient-boosted model trained in Python (`python/`) and scored
  in OCaml with exact parity.
- `Report`: everything a leader page shows, as JSON.
- `leader-scoring-hl`: read-only Hyperliquid source (`Hl_source`, `Raw` recordings,
  `Hl_decode`, `Hl_market`) and the `leader-scoring` CLI (`record`, `score`,
  `portfolios`, `dataset`, `metrics`).
- core_bench benchmarks of every scoring step.
