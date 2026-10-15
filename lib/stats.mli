(** Small statistics helpers over float arrays. NaN inputs are the caller's problem:
    every function here assumes finite values. *)

open! Core

val mean : float array -> float option

(** Sample standard deviation (divides by [n - 1]); [None] below two values. *)
val stdev : float array -> float option

(** [quantile xs q] with linear interpolation between order statistics (numpy's default
    method); [q] in [\[0, 1\]]. *)
val quantile : float array -> float -> float option

(** Quantile of values carrying weights (durations): the smallest value whose cumulative
    weight reaches [q] of the total. *)
val weighted_quantile : (float * float) array -> float -> float option

(** Spearman rank correlation with average ranks for ties; [None] below three pairs or
    when either side is constant. *)
val spearman : float array -> float array -> float option

(** Pearson correlation; [None] below three pairs or for a constant side. *)
val pearson : float array -> float array -> float option

(** Average ranks starting at 1, ties sharing the mean of their positions. *)
val ranks : float array -> float array
