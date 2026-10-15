open! Core
open Leader_scoring
open Helpers

let print_trades (tr : Track_record.t) =
  List.iter tr.trades ~f:(fun (t : Track_record.Trade.t) ->
    print_s
      [%message
        ""
          ~coin:t.coin
          ~dir:(t.direction : Track_record.Direction.t)
          ~entry:(round t.entry_px : float)
          ~exit:(Option.map t.exit_px ~f:round : float option)
          ~realized:(round t.realized : float)
          ~fees:(round t.fees : float)
          ~funding:(round t.funding : float)
          ~net:(round (Track_record.Trade.net_pnl t) : float)
          ~closed:(Track_record.Trade.is_closed t : bool)
          ~seeded:(t.seeded : bool)
          ~max_size:(round t.max_size : float)])
;;

let print_points (tr : Track_record.t) =
  Array.iter tr.points ~f:(fun p ->
    printf
      "%s equity=%g gross=%g net=%g open=%d\n"
      (Time_ns.to_string_iso8601_basic p.time ~zone:Time_float.Zone.utc)
      (round p.equity)
      (round p.gross)
      (round p.net)
      p.open_positions)
;;

let%expect_test "round trips, a flip, fees split by size, funding, anchored equity" =
  let fills =
    [ fill ~time:(at 0.) ~coin:"BTC" ~px:100. ~fee:0.1 1.
    ; fill ~time:(at 1.) ~coin:"BTC" ~px:110. ~fee:0.1 1.
    ; fill ~time:(at 2.) ~coin:"BTC" ~px:120. ~fee:0.3 (-3.)
    ; fill ~time:(at 3.) ~coin:"BTC" ~px:115. ~fee:0.1 1.
    ]
  in
  let funding =
    [ { Funding.time = at 0.5; coin = "BTC"; usd = -0.5; position = Some 1.; rate = None }
    ; { Funding.time = at 2.5; coin = "BTC"; usd = 0.2; position = Some (-1.); rate = None }
    ]
  in
  let snapshots = [ snapshot ~time:(at 3.) ~account_value:1000. [] ] in
  let tr = Track_record.build (history ~funding ~snapshots fills) in
  print_trades tr;
  print_points tr;
  print_s
    [%message
      ""
        ~source:(tr.equity_source : Track_record.Equity_source.t)
        ~initial_equity:(round tr.initial_equity : float)
        ~turnover:(round tr.turnover : float)
        ~fees:(round tr.fees : float)
        ~funding:(round tr.funding : float)
        ~mismatches:(tr.position_mismatches : int)];
  [%expect
    {|
    ((coin BTC) (dir Long) (entry 105) (exit (120)) (realized 30) (fees 0.4)
     (funding -0.5) (net 29.1) (closed true) (seeded false) (max_size 2))
    ((coin BTC) (dir Short) (entry 120) (exit (115)) (realized 5) (fees 0.2)
     (funding 0.2) (net 5) (closed true) (seeded false) (max_size 1))
    2026-01-01T00:00:00.000000000Z equity=965.8 gross=100 net=100 open=1
    2026-01-01T12:00:00.000000000Z equity=965.3 gross=100 net=100 open=1
    2026-01-02T00:00:00.000000000Z equity=975.2 gross=220 net=220 open=1
    2026-01-03T00:00:00.000000000Z equity=994.9 gross=120 net=-120 open=1
    2026-01-03T12:00:00.000000000Z equity=995.1 gross=120 net=-120 open=1
    2026-01-04T00:00:00.000000000Z equity=1000 gross=0 net=0 open=0
    ((source Anchored) (initial_equity 965.9) (turnover 685) (fees 0.6)
     (funding -0.3) (mismatches 0))
    |}]
;;

let%expect_test "a position held before the history is seeded from position_before" =
  let fills =
    [ fill ~time:(at 1.) ~coin:"ETH" ~px:2000. ~position_before:(-2.) 1.
    ; fill ~time:(at 2.) ~coin:"ETH" ~px:1900. ~position_before:(-1.) 1.
    ]
  in
  let tr = Track_record.build ~initial_equity:10_000. (history fills) in
  print_trades tr;
  print_s [%message "" ~source:(tr.equity_source : Track_record.Equity_source.t)];
  [%expect
    {|
    ((coin ETH) (dir Short) (entry 2000) (exit (1950)) (realized 100) (fees 0)
     (funding 0) (net 100) (closed true) (seeded true) (max_size 2))
    (source Given)
    |}]
;;

let%expect_test "missed fills are repaired from position_before and counted" =
  let fills =
    [ fill ~time:(at 0.) ~coin:"SOL" ~px:10. 5.
    ; (* the venue says 8 were held, the replay has 5 *)
      fill ~time:(at 1.) ~coin:"SOL" ~px:12. ~position_before:8. (-8.)
    ]
  in
  let tr = Track_record.build ~initial_equity:1000. (history fills) in
  print_trades tr;
  print_s [%message "" ~mismatches:(tr.position_mismatches : int)];
  [%expect
    {|
    ((coin SOL) (dir Long) (entry 10.75) (exit (12)) (realized 10) (fees 0)
     (funding 0) (net 10) (closed true) (seeded false) (max_size 8))
    (mismatches 1)
    |}]
;;

let%expect_test "venue curve wins for equity; bars carry the latest point forward" =
  let equity_curve =
    List.map [ 0., 100.; 1., 110.; 3., 99. ] ~f:(fun (d, equity) ->
      { Equity_point.time = at d; equity })
  in
  let tr =
    Track_record.build (history ~equity_curve [ fill ~time:(at 0.5) ~coin:"BTC" ~px:50. 1. ])
  in
  print_s
    [%message
      ""
        ~source:(tr.equity_source : Track_record.Equity_source.t)
        ~bars:(Track_record.bars tr ~period:Time_ns.Span.day : float array)
        ~leverage:
          (Array.map (Track_record.leverage_samples tr) ~f:(fun (d, l) ->
             Time_ns.Span.to_hr d, round l)
           : (float * float) array)];
  [%expect
    {| ((source Venue) (bars (100 110 110 99)) (leverage ((60 0.5)))) |}]
;;
