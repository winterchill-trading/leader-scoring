"""Reads the CSV written by `leader-scoring dataset`.

Columns: group, account, cutoff_ms, one column per feature (empty = missing), label
(empty = no label). The feature columns are exactly leader-scoring's
Dataset.feature_names, in order; the exported model records them and the OCaml loader
refuses any other list.
"""

import csv
import hashlib
from dataclasses import dataclass

import numpy as np


@dataclass
class Rows:
    group: np.ndarray  # str
    account: np.ndarray  # str
    cutoff: np.ndarray  # int, ms
    X: np.ndarray  # float, NaN = missing
    y: np.ndarray  # float, NaN = no label
    features: list

    def take(self, mask):
        return Rows(self.group[mask], self.account[mask], self.cutoff[mask],
                    self.X[mask], self.y[mask], self.features)


def num(s):
    return float(s) if s != "" else np.nan


def load(*paths):
    header, rows = None, []
    for p in paths:
        with open(p, newline="") as f:
            r = csv.reader(f)
            h = next(r)
            if header is None:
                header = h
            assert h == header, f"{p}: different columns"
            rows.extend(r)
    features = header[3:-1]
    return Rows(
        group=np.array([r[0] for r in rows]),
        account=np.array([r[1] for r in rows]),
        cutoff=np.array([int(r[2]) for r in rows], dtype=np.int64),
        X=np.array([[num(v) for v in r[3:-1]] for r in rows], dtype=np.float64),
        y=np.array([num(r[-1]) for r in rows], dtype=np.float64),
        features=features)


def feature_digest(features):
    """Hex MD5 of the names joined by newlines, as Model.feature_digest in OCaml."""
    return hashlib.md5("\n".join(features).encode()).hexdigest()
