# bin/

The `leader-scoring` executable (package `leader-scoring-hl`):

- `record -network mainnet|testnet|local -address A -days D -out DIR`
- `score DIR...` (`-json` for one JSON line per account)
- `portfolios -top N -out DIR` (mainnet leaderboard, read only)
- `dataset [-synthetic N] [-replay DIR] -out FILE.csv`
- `metrics` (the catalogue as a Markdown table)
- `persistence [-synthetic N | -replay DIR] [-window-days D] [-bar-days D] [-formation R] [-outcome R] [-top-k K] [-json]`
  (rank persistence, random-pick baseline, PSR, DSR, PBO and SPA over a universe)

`dune exec leader-scoring -- <command> -help` lists the flags. See [../AGENTS.md](../AGENTS.md).
