open! Core
open Leader_scoring

let r3 x = Float.round_decimal x ~decimal_digits:3

let show (t : Leader_stats.t) ~accounts =
  let u = t.universe in
  print_s
    [%message
      ""
        ~leaders:(u.leaders : int)
        ~trials:(Option.map u.trials ~f:(fun t -> t.count) : int option)
        ~common:(u.common_leaders, u.common_periods : int * int)
        ~pbo:(Option.map u.pbo ~f:(fun p -> r3 p.pbo) : float option)
        ~spa_p_value:(Option.map u.spa ~f:(fun s -> r3 s.p_value_consistent) : float option)
        ~spa_best:(u.spa_best : string option)
        ~accepted:(u.accepted : int)];
  List.iter accounts ~f:(fun a ->
    let r = Map.find_exn t.reports a in
    print_s
      [%message
        a
          ~psr:(r3 r.psr : float)
          ~dsr:(Option.map r.dsr ~f:r3 : float option)
          ~min_track_record:
            (Option.map r.min_track_record_length ~f:Float.iround_nearest_exn : int option)
          ~accepted:
            (Option.map r.verdict ~f:Backtest_stats.Gate.Verdict.is_accept : bool option)])
;;

let config = { Leader_stats.Config.default with bootstrap_samples = 200; common_bars = 120 }

(* The deflated Sharpe ratio measures the best leader against the best Sharpe ratio
   expected from 40 leaders whose ratios vary this much; with edges spread from -0.1 to
   0.1 that variance is large, so even the most skilled leader stays well below 0.95. *)
let%expect_test "skilled universe: PBO low, SPA significant, DSR still harsh; luck: neither" =
  show
    (Leader_stats.run ~config (Helpers.planted ~skill:true ~leaders:40 ~days:300))
    ~accounts:[ "leader-039"; "leader-020"; "leader-000" ];
  [%expect {|
    ((leaders 40) (trials (40)) (common (40 120)) (pbo (0.238))
     (spa_p_value (0.025)) (spa_best (leader-034)) (accepted 0))
    (leader-039 (psr 1) (dsr (0.149)) (min_track_record (73)) (accepted (false)))
    (leader-020 (psr 0.828) (dsr (0)) (min_track_record (905))
     (accepted (false)))
    (leader-000 (psr 0.23) (dsr (0)) (min_track_record ()) (accepted (false)))
    |}];
  show
    (Leader_stats.run ~config (Helpers.planted ~skill:false ~leaders:40 ~days:300))
    ~accounts:[ "leader-039"; "leader-020" ];
  [%expect {|
    ((leaders 40) (trials (40)) (common (40 120)) (pbo (0.616))
     (spa_p_value (0.64)) (spa_best (leader-019)) (accepted 0))
    (leader-039 (psr 0.888) (dsr (0.103)) (min_track_record (546))
     (accepted (false)))
    (leader-020 (psr 0.818) (dsr (0.054)) (min_track_record (984))
     (accepted (false)))
    |}]
;;
