"""The transparent composite score of leader-scoring's Score module, recomputed per
cross-section so the learned model has an honest baseline.

Percentile of each weighted metric among the leaders of one cutoff (mid-rank for ties,
0.5 for a missing value), flipped where lower is better, weighted average, times 100.
Score.rank also restricts the pool to eligible leaders (5 closed trades); here every
row of the cross-section is in the pool.
"""

import numpy as np

# Score.Config.default, metric -> (weight, higher_is_better)
WEIGHTS = {
    "sharpe": (0.15, True),
    "sortino": (0.10, True),
    "return_over_drawdown": (0.10, True),
    "total_return": (0.05, True),
    "max_drawdown": (0.15, False),
    "cvar_95": (0.05, False),
    "rolling_positive_share": (0.10, True),
    "profit_factor": (0.05, True),
    "win_rate": (0.05, True),
    "leverage_p95": (0.10, False),
    "coin_hhi": (0.05, False),
    "bias_drift": (0.05, False),
}


def percentile(xs):
    """Score.percentile_of for every finite value of xs; NaN stays NaN."""
    out = np.full(len(xs), np.nan)
    ok = np.isfinite(xs)
    v = xs[ok]
    n = len(v)
    if n == 0:
        return out
    if n == 1:
        out[ok] = 0.5
        return out
    below = np.array([(v < x).sum() for x in v])
    equal = np.array([(v == x).sum() for x in v])
    out[ok] = (below + np.maximum(equal - 1, 0) / 2) / (n - 1)
    return out


def score(X, features, cutoff):
    idx = {f: i for i, f in enumerate(features)}
    total = sum(w for w, _ in WEIGHTS.values())
    s = np.zeros(len(X))
    for c in np.unique(cutoff):
        m = cutoff == c
        acc = np.zeros(m.sum())
        for name, (w, higher) in WEIGHTS.items():
            p = percentile(X[m, idx[name]])
            if not higher:
                p = 1 - p
            acc += w * np.where(np.isnan(p), 0.5, p)
        s[m] = 100 * acc / total
    return s
