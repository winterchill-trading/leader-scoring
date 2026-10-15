open! Core
open Leader_scoring
open Helpers

(* Mid 100, one-cent spread each side, then 10k USD of depth per 1 bp step. *)
let book coin : Copyability.Book.t =
  let side sign =
    Array.init 50 ~f:(fun i ->
      let px = 100. +. (sign *. (0.01 +. (Float.of_int i *. 0.01))) in
      { Copyability.Book.px; size = 10_000. /. px })
  in
  { coin; bids = side (-1.); asks = side 1. }
;;

let books = String.Map.of_alist_exn [ "BTC", book "BTC" ]

let%expect_test "walking the book" =
  let show side notional =
    let e = Option.value_exn (Copyability.walk (book "BTC") ~side ~notional) in
    printf "%-4s %9.0f filled=%9.0f cost=%.3f bps\n"
      (match side with `Buy -> "buy" | `Sell -> "sell")
      notional e.filled e.cost_bps
  in
  show `Buy 5_000.;
  show `Buy 50_000.;
  show `Sell 50_000.;
  show `Buy 1_000_000.;
  print_s [%sexp (Copyability.Book.depth_within (book "BTC") ~bps:5. : float * float)];
  [%expect {|
    buy       5000 filled=     5000 cost=1.000 bps
    buy      50000 filled=    50000 cost=3.000 bps
    sell     50000 filled=    50000 cost=3.000 bps
    buy    1000000 filled=   500000 cost=62.740 bps
    (50000 50000)
    |}]
;;

let record =
  let fills =
    [ fill ~time:(at 0.) ~coin:"BTC" ~px:100. 1_000.
    ; fill ~time:(at 1.) ~coin:"BTC" ~px:101. (-1_000.)
    ; fill ~time:(at 2.) ~coin:"ETH" ~px:10. 100.
    ; fill ~time:(at 3.) ~coin:"ETH" ~px:10. (-100.)
    ]
  in
  Track_record.build ~initial_equity:100_000. (history fills)
;;

let%expect_test "capacity curve of a leader trading its whole equity in BTC" =
  let est = Copyability.estimate ~books record in
  List.iter est.curve ~f:(fun p ->
    printf "%10.0f orders=%d mean=%.3f median=%.3f p90=%.3f unfilled=%.3f\n"
      p.follower_equity p.orders p.mean_cost_bps p.median_cost_bps p.p90_cost_bps p.unfilled_share);
  print_s
    [%message
      ""
        ~capacity:(Option.map est.capacity ~f:Float.iround_nearest_exn : int option)
        ~orders:(est.orders : int)
        ~coins_without_book:(est.coins_without_book : string list)];
  [%expect {|
          1000 orders=2 mean=1.000 median=1.000 p90=1.000 unfilled=0.000
         10000 orders=2 mean=1.000 median=1.000 p90=1.000 unfilled=0.000
        100000 orders=2 mean=5.500 median=5.500 p90=5.501 unfilled=0.000
       1000000 orders=2 mean=62.750 median=62.750 p90=62.758 unfilled=0.500
      10000000 orders=2 mean=96.275 median=96.275 p90=96.276 unfilled=0.950
    ((capacity (190000)) (orders 4) (coins_without_book (ETH)))
    |}]
;;

let%test_unit "cost never falls as the follower grows" =
  Quickcheck.test
    ~trials:200
    (Quickcheck.Generator.both (Float.gen_incl 1. 1e7) (Float.gen_incl 1. 1e7))
    ~f:(fun (a, b) ->
      let lo = Float.min a b
      and hi = Float.max a b in
      let c x = Option.value_exn (Copyability.cost_at ~books record ~follower_equity:x) in
      assert (Float.( >= ) (c hi) (c lo -. 1e-9)))
;;
