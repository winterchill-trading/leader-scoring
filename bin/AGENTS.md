# bin/

The `leader-scoring` executable (package `leader-scoring-hl`):

- `record -network mainnet|testnet|local -address A -days D -out DIR`
- `score DIR...` (`-json` for one JSON line per account)
- `portfolios -top N -out DIR` (mainnet leaderboard, read only)
- `dataset [-synthetic N] [-replay DIR] -out FILE.csv`
- `metrics` (the catalogue as a Markdown table)

`dune exec leader-scoring -- <command> -help` lists the flags. See [../AGENTS.md](../AGENTS.md).
