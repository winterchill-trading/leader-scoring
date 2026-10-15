open! Core
open Leader_scoring

let%expect_test "quantiles match numpy's linear method" =
  let xs = [| 5.; 1.; 4.; 2.; 3. |] in
  List.iter [ 0.; 0.05; 0.25; 0.5; 0.9; 1. ] ~f:(fun q ->
    printf "%.2f -> %g\n" q (Option.value_exn (Stats.quantile xs q)));
  [%expect
    {|
    0.00 -> 1
    0.05 -> 1.2
    0.25 -> 2
    0.50 -> 3
    0.90 -> 4.6
    1.00 -> 5
    |}]
;;

let%expect_test "weighted quantile, ranks with ties, correlations" =
  print_s
    [%sexp (Stats.weighted_quantile [| 1., 1.; 10., 8.; 100., 1. |] 0.5 : float option)];
  print_s [%sexp (Stats.ranks [| 3.; 1.; 3.; 2. |] : float array)];
  print_s
    [%sexp
      (Stats.spearman [| 1.; 2.; 3.; 4. |] [| 10.; 20.; 30.; 1000. |] : float option)];
  print_s [%sexp (Stats.pearson [| 1.; 2.; 3. |] [| 3.; 2.; 1. |] : float option)];
  print_s [%sexp (Stats.stdev [| 2.; 4.; 4.; 4.; 5.; 5.; 7.; 9. |] : float option)];
  [%expect {|
    (10)
    (3.5 1 3.5 2)
    (1)
    (-1)
    (2.1380899352993952)
    |}]
;;
