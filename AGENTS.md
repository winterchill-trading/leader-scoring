# AGENTS.md (leader-scoring)

## Purpose

Scores traders for copy trading: rebuilds a track record from fills, funding and account
snapshots, computes documented metrics, ranks a universe with a transparent composite and
a learned forecast, estimates copy cost and capacity from book depth, and raises
behaviour alerts. The core is venue neutral; a second package reads Hyperliquid, read only.

Workspace-wide rules, the repository map and the CI model are in
`../oxcaml-claude-skills/AGENTS.md`. Read that first.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | `leader-scoring` (`Leader_scoring`): inputs, `Track_record`, `Metrics`, `Score`, `Copyability`, `Alerts`, `Persistence`, `Leader_stats`, `Integrity`, `Synthetic`, `Dataset`, `Model`, `Report` ([lib/AGENTS.md](lib/AGENTS.md)) |
| `hl/` | `leader-scoring-hl`: Hyperliquid source, raw recordings, decoders, books and open interest ([hl/AGENTS.md](hl/AGENTS.md)) |
| `hl/test/` | Decoding tests over recorded mainnet and hl-sim answers in `hl/test/fixtures/` |
| `bin/` | The `leader-scoring` CLI: `record`, `score`, `portfolios`, `dataset`, `metrics`, `persistence` ([bin/AGENTS.md](bin/AGENTS.md)) |
| `test/` | Expect tests and Quickcheck properties of the core ([test/AGENTS.md](test/AGENTS.md)) |
| `bench/` | core_bench of every scoring step ([bench/AGENTS.md](bench/AGENTS.md)) |
| `python/` | Training, walk-forward evaluation and export of the learned score ([python/AGENTS.md](python/AGENTS.md)) |
| `models/` | The exported model and its evaluation, embedded into the library at build time |
| `.github/` | CI workflow ([.github/AGENTS.md](.github/AGENTS.md)) |
| `README.md` | Formulas, the score, copyability, alerts, model results, costs |

## Build, test, docs

From this repository, inside the workspace:

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @leader-scoring/all @leader-scoring/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @leader-scoring/doc
```

Give parallel work its own build directory: `DUNE_BUILD_DIR=_build-leader-scoring` in
front of the same command. Benchmark: `dune exec bench/main.exe -- -quota 2`. CLI:
`dune exec leader-scoring -- -help`.

Standalone: `opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`,
then `opam install . --deps-only --with-test` and `dune build && dune test`.

Training (Python 3.12+, `uv`): see `python/AGENTS.md`.

## Conventions

- Format only this repository: `dune build --root .. @leader-scoring/fmt --auto-promote`,
  after the tests pass. Never `dune fmt` at the workspace root.
- `open! Core`, an `.mli` for every module, labelled arguments, expect tests and
  Base_quickcheck properties; odoc on every public value.
- Inputs are `Stable.V1` types; a changed format is a new version.
- Amounts are floats: this is statistics, not bookkeeping. Print rounded values in
  expect tests (`Float.round_decimal`), since `log10`, `exp` and `**` can differ in the
  last bit between macOS and Linux.
- A new metric goes into `Metric` (name, label, formula, unit, direction) and
  `Metrics.compute`; it then joins the model's features, so the model must be retrained
  and re-exported (`Model.of_json` refuses a model with other features).
- Comments for humans at every threshold and tolerance, saying why.
- User-visible changes get a line under "Unreleased" in `CHANGES.md`.

## Dependencies and dependents

- `leader-scoring` depends on `market-data` (and through it `fixed-decimal`,
  `exchange-core`, `parquet`) and `backtest-stats`; `leader-scoring-hl` adds
  `hl-protocol`, `venue-net` and `hl-client`. CI pins them in that order. Every repository
  that pins `leader-scoring` must pin `backtest-stats` before it.
- Used by `../hl-leaderboard` (`Scoring`: reports per leader), and through it by
  `../hl-copy-api` and `../hl-copy-web`; `../hl-copy-core`'s backtest job pins it because
  it builds `hl-leaderboard`.
- Spec: `../specs/repos/leader-scoring.md`; design: `../specs/design/copytrading.md`.

## CI

`.github/workflows/ci.yml`: `build` runs the `ox-setup` action (switch and pins, cached),
`dune build`, `dune test`, then the odoc build with warnings as errors; `obj-magic` calls
the shared workflow from `infra`. Check with one `gh run list --limit 1` and at most one
`gh run watch <id> --exit-status`. Never poll in a loop.

## Pitfalls

- Hyperliquid fills include spot (`@107`, `PURR/USDC`) and `#<id>` instruments; they are
  not perpetuals and never appear in `clearinghouseState`. Leaving them in breaks the
  replay against snapshots.
- The venue keeps only an account's 10,000 most recent fills; a busy account's history
  covers hours. Check `fills_truncated` before reading trade statistics.
- Positions held before the history are seeded from `startPosition`; most large accounts
  never go flat, so without seeding their pnl reads zero.
- Annualized metrics of minute bars (simulator runs) are huge; compare them only among
  records with the same bar.
- `Persistence` and `Leader_stats` are universe statistics: run them once over every
  leader (`Report.analyse`), never per leader. Mainnet `portfolio` all-time curves are
  sampled weekly; use weekly bars and windows of several weeks for them.
- Integrity thresholds were chosen against honest-trader fixtures (`test/test_integrity.ml`);
  a change that flags a noise or news trader there is a regression.
- Dataset cutoffs must sit on one grid (multiples of the step since the epoch), or each
  account gets its own cutoffs and cross-sectional correlations fall apart.
- `hl-sim` answers `userFunding` and `metaAndAssetCtxs` with 400; the source treats that
  as "none".
- Another agent's half-written sibling breaks the shared workspace build; build against
  a scratch workspace of symlinks and `git archive` snapshots when that happens.

## Where to read more

- `README.md`, `../specs/repos/leader-scoring.md`, `../specs/design/copytrading.md`
- `../oxcaml-claude-skills/AGENTS.md`, `../oxcaml-claude-skills/GOTCHAS.md`
- Skills: `../oxcaml-claude-skills/skills/oxcaml-performance` (before optimising),
  `../oxcaml-claude-skills/skills/janestreet-ocaml-style`

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch. Dry-run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`.
- Add a request that signs anything or reaches an exchange endpoint.
- Publish, tag or release.
- Present the learned score as predictive of real returns; the README's numbers say why.
- Commit collected account data beyond the small, read-only fixtures already here.
