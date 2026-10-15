"""Walk-forward evaluation, final fit and export of the learned leader score.

Target: Dataset.label in leader-scoring, the forward 30-day risk-adjusted return (a
30-day Sharpe ratio of daily returns, not annualized, clipped to [-5, 5]) given the
metrics of the 90 days before the cutoff.

Walk-forward: for every cutoff of a group (after the first MIN_FOLD_TRAIN cutoffs), fit
on rows whose label window ended at or before that cutoff, predict the cutoff's cross
section, and measure the Spearman rank correlation (IC) between prediction and label.
Synthetic folds train on synthetic rows only. Replayed (mainnet) folds train on every
synthetic row (made-up data cannot leak the real future) plus the replayed rows that
are already in the past. Baselines over the same cross sections: the transparent
composite score (Score.Config.default), past Sharpe, and past total return.

Usage:
  uv run python train.py --data ../data/synthetic.csv [--data ../data/mainnet.csv] \
      [--out ../models/leader-forward-sharpe-gbdt]
"""

import argparse
import csv
import json
import os

import numpy as np
from scipy.stats import spearmanr
from sklearn.ensemble import HistGradientBoostingRegressor

from leader_scoring_train import composite, data

HORIZON_MS = 30 * 86_400_000
MIN_FOLD_TRAIN = 3
NAME = "leader-forward-sharpe-gbdt"
# Chosen between two settings on the same walk-forward folds (the other: 150 trees of 15
# leaves, 80 rows per leaf, learning rate 0.05, IC 0.053); see the README.
PARAMS = dict(max_iter=200, learning_rate=0.03, max_leaf_nodes=7, min_samples_leaf=300,
              l2_regularization=1.0, early_stopping=False, random_state=0)


def fit(X, y):
    return HistGradientBoostingRegressor(**PARAMS).fit(X, y)


def ic(a, b):
    ok = np.isfinite(a) & np.isfinite(b)
    if ok.sum() < 10 or np.std(a[ok]) == 0 or np.std(b[ok]) == 0:
        return np.nan
    return float(spearmanr(a[ok], b[ok]).statistic)


def top_spread(pred, y, q=0.2):
    """Mean label of the top fifth by prediction minus the mean label of all."""
    k = max(1, int(len(pred) * q))
    order = np.argsort(-pred, kind="stable")
    return float(np.mean(y[order[:k]]) - np.mean(y))


def summarize(values):
    v = np.array([x for x in values if np.isfinite(x)])
    if len(v) == 0:
        return dict(folds=0)
    sd = float(np.std(v, ddof=1)) if len(v) > 1 else float("nan")
    return dict(folds=int(len(v)), mean=float(np.mean(v)), std=sd,
                t=float(np.mean(v) / sd * np.sqrt(len(v))) if len(v) > 1 and sd > 0 else None,
                positive_share=float(np.mean(v > 0)))


def walk_forward(rows, test_group, train_groups_always=()):
    labelled = np.isfinite(rows.y)
    in_group = rows.group == test_group
    cutoffs = np.unique(rows.cutoff[in_group & labelled])
    idx = {f: i for i, f in enumerate(rows.features)}
    comp = composite.score(rows.X, rows.features, rows.cutoff)
    out = dict(model=[], composite=[], sharpe=[], total_return=[], spread=[], rows=0)
    for c in cutoffs[MIN_FOLD_TRAIN:]:
        past = labelled & (rows.cutoff + HORIZON_MS <= c)
        train = past & (in_group | np.isin(rows.group, train_groups_always))
        if test_group != "synthetic":
            train = train | (labelled & np.isin(rows.group, train_groups_always))
        test = labelled & in_group & (rows.cutoff == c)
        if train.sum() < 200 or test.sum() < 10:
            continue
        m = fit(rows.X[train], rows.y[train])
        pred = m.predict(rows.X[test])
        y = rows.y[test]
        out["model"].append(ic(pred, y))
        out["composite"].append(ic(comp[test], y))
        out["sharpe"].append(ic(rows.X[test, idx["sharpe"]], y))
        out["total_return"].append(ic(rows.X[test, idx["total_return"]], y))
        out["spread"].append(top_spread(pred, y))
        out["rows"] += int(test.sum())
    spread = out.pop("spread")
    return {k: (summarize(v) if isinstance(v, list) else v) for k, v in out.items()} | {
        "top_fifth_label_spread": summarize(spread)}


def export_tree(pred):
    nodes = pred.nodes
    leaf = nodes["is_leaf"].astype(bool)
    feature = np.where(leaf, -1, nodes["feature_idx"]).astype(int)
    left = np.where(leaf, -1, nodes["left"].astype(np.int64))
    right = np.where(leaf, -1, nodes["right"].astype(np.int64))
    threshold = np.where(leaf, 0.0, nodes["num_threshold"])
    # +inf separates missing from present values; JSON has no infinity, and the largest
    # finite double routes every finite input the same way.
    threshold = np.where(threshold == np.inf, np.finfo(np.float64).max, threshold)
    assert np.isfinite(threshold).all()
    assert not nodes["is_categorical"].any()
    return dict(feature=feature.tolist(), threshold=threshold.tolist(),
                missing_left=nodes["missing_go_to_left"].astype(int).tolist(),
                left=left.tolist(), right=right.tolist(),
                value=nodes["value"].astype(float).tolist())


def reference_predict(model, x):
    """The OCaml scorer's algorithm, to check the export before writing it."""
    acc = model["base_score"]
    for t in model["trees"]:
        i = 0
        while t["feature"][i] >= 0:
            v = x[t["feature"][i]]
            go_left = t["missing_left"][i] == 1 if np.isnan(v) else v <= t["threshold"][i]
            i = t["left"][i] if go_left else t["right"][i]
        acc += t["value"][i]
    return acc


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", action="append", required=True)
    ap.add_argument("--out", default=f"../models/{NAME}")
    ap.add_argument("--fixture", default="../test/fixtures/model-parity.csv")
    a = ap.parse_args()
    rows = data.load(*a.data)
    groups = sorted(set(rows.group.tolist()))
    labelled = np.isfinite(rows.y)
    evaluation = dict(
        target="forward 30-day Sharpe ratio of daily returns (not annualized), clipped to [-5, 5]",
        lookback_days=90, horizon_days=30, step_days=15,
        rows={g: int(((rows.group == g) & labelled).sum()) for g in groups},
        accounts={g: int(len(set(rows.account[rows.group == g]))) for g in groups},
        walk_forward={g: walk_forward(rows, g, train_groups_always=("synthetic",)) for g in groups})
    model = fit(rows.X[labelled], rows.y[labelled])
    export = dict(format=1, name=NAME, kind="gbdt_regressor", target=evaluation["target"],
                  features=rows.features, feature_digest=data.feature_digest(rows.features),
                  base_score=float(model._baseline_prediction.ravel()[0]),
                  trees=[export_tree(p) for (p,) in model._predictors],
                  params={k: v for k, v in PARAMS.items()}, evaluation=evaluation)
    # parity: the reference walk must equal scikit-learn's predictions
    sample = np.flatnonzero(labelled)[:: max(1, labelled.sum() // 200)][:200]
    sk = model.predict(rows.X[sample])
    ref = np.array([reference_predict(export, x) for x in rows.X[sample]])
    worst = float(np.max(np.abs(sk - ref)))
    assert worst < 1e-12, f"reference scorer disagrees with scikit-learn by {worst}"
    os.makedirs(a.out, exist_ok=True)
    with open(os.path.join(a.out, "model.json"), "w") as f:
        json.dump(export, f, separators=(",", ":"))
    with open(os.path.join(a.out, "evaluation.json"), "w") as f:
        json.dump(evaluation, f, indent=2)
    with open(a.fixture, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(rows.features + ["prediction"])
        for x, p in zip(rows.X[sample], sk):
            w.writerow(["" if np.isnan(v) else repr(float(v)) for v in x] + [repr(float(p))])
    print(json.dumps(evaluation, indent=2))
    print(f"parity worst {worst:.2e}; {len(export['trees'])} trees")


if __name__ == "__main__":
    main()
