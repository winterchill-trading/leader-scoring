open! Core
open Leader_scoring

let universe = Helpers.planted

let config =
  { Persistence.Config.default with bootstrap_samples = 300; random_draws = 300 }
;;

let r3 x = Float.round_decimal x ~decimal_digits:3

let show (t : Persistence.t) =
  print_s
    [%message
      ""
        ~leaders:(t.leaders : int)
        ~windows:(t.windows : int)
        ~periods:(List.length t.periods : int)
        ~rank_correlation:
          (Option.map t.rank_correlation ~f:(fun i ->
             r3 i.estimate, r3 i.lower, r3 i.upper)
           : (float * float * float) option)
        ~top_stays_top_half:(Option.map t.top_stays_top_half ~f:r3 : float option)
        ~baseline_percentile:
          (Option.map t.baseline ~f:(fun b -> r3 b.percentile) : float option)
        ~decay:
          (List.map t.decay ~f:(fun d -> Option.map d.mean_rank_correlation ~f:r3)
           : float option list)
        ~persistent:(t.persistent : bool)]
;;

let%expect_test "planted skill persists, luck does not" =
  show (Persistence.run ~config (universe ~skill:true ~leaders:40 ~days:300));
  [%expect
    {|
    ((leaders 40) (windows 9) (periods 8) (rank_correlation ((0.182 0.072 0.28)))
     (top_stays_top_half (0.641)) (baseline_percentile (1))
     (decay ((0.182) (0.221) (0.297) (0.277) (0.309) (0.24))) (persistent true))
    |}];
  show (Persistence.run ~config (universe ~skill:false ~leaders:40 ~days:300));
  [%expect
    {|
    ((leaders 40) (windows 9) (periods 8)
     (rank_correlation ((-0.103 -0.221 0.006))) (top_stays_top_half (0.484))
     (baseline_percentile (0.337))
     (decay ((-0.103) (0.038) (0.13) (0.065) (0.107) (-0.046)))
     (persistent false))
    |}]
;;
