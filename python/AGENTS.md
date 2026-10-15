# python/

Training of the learned score: `train.py` (walk-forward evaluation, final fit, export of
`../models/leader-forward-sharpe-gbdt/` and `../test/fixtures/model-parity.csv`),
`leader_scoring_train/` (CSV loader, the composite score as a baseline).

```sh
uv sync
uv run python train.py --data ../data/synthetic.csv --data ../data/mainnet.csv
```

- The CSVs come from `leader-scoring dataset`; features are computed in OCaml, never here.
- `train.py` asserts its reference scorer equals scikit-learn before writing the model.
- `../data/` is gitignored; do not commit datasets.

See [../AGENTS.md](../AGENTS.md).
