(** The learned score: a gradient-boosted regression forest trained in Python
    ([python/train.py], scikit-learn's HistGradientBoostingRegressor) and exported as
    JSON ([python/export.py]); scored here with no Python at run time.

    Format 1:
    {v
    {"format": 1, "name": "...", "kind": "gbdt_regressor", "target": "...",
     "features": ["total_return", ...], "feature_digest": "<md5 hex>",
     "base_score": 0.01,
     "trees": [{"feature": [...], "threshold": [...], "missing_left": [...],
                "left": [...], "right": [...], "value": [...]}],
     "evaluation": {...}}
    v}

    [feature = -1] marks a leaf. A split sends [x <= threshold] left and NaN to
    [missing_left]. The prediction is [base_score] plus each tree's leaf value, summed
    in tree order, which reproduces scikit-learn's own prediction to the last bit. The
    model predicts {!Dataset.label}: the forward 30-day risk-adjusted return. *)

open! Core

type t

(** Hex MD5 of the feature names joined by newlines; identical in Python. *)
val feature_digest : string list -> string

(** Refuses a model whose features are not {!Dataset.feature_names} in order. *)
val of_json : Yojson.Safe.t -> t Or_error.t

val load : string -> t Or_error.t
val name : t -> string
val trees : t -> int

(** The [evaluation] object exported with the model (walk-forward results). *)
val evaluation : t -> Yojson.Safe.t

(** Prediction for one row in {!Dataset.feature_names} order ([nan] for missing). *)
val predict : t -> float array -> float

val predict_metrics : t -> Metrics.t -> float

(** The model shipped with this library ([models/leader-forward-sharpe-gbdt]), parsed on
    first use. *)
val bundled : t Lazy.t
