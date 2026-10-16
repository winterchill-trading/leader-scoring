open! Core
open Leader_scoring

let metrics alist = Metrics.of_alist ~bar:Time_ns.Span.day alist

let leader ?(trades = 10.) ~sharpe ~mdd ~lev () =
  metrics
    [ Sharpe, Some sharpe
    ; Max_drawdown, Some mdd
    ; Leverage_p95, Some lev
    ; Trades, Some trades
    ]
;;

let config =
  { Score.Config.weights = [ Sharpe, 0.5; Max_drawdown, 0.3; Leverage_p95, 0.2 ]
  ; min_trades = 5
  ; min_days = 0.
  }
;;

let print ranked =
  List.iter ranked ~f:(fun (r : Score.Ranked.t) ->
    printf
      "%-6s score=%5.1f rank=%s pct=%s %s\n"
      r.account
      r.score
      (Option.value_map r.rank ~default:"-" ~f:Int.to_string)
      (Option.value_map r.percentile ~default:"-" ~f:(sprintf "%.2f"))
      (String.concat ~sep:"; " r.reasons))
;;

let%expect_test "composite of percentiles, flipped where lower is better" =
  let universe =
    [ "a", leader ~sharpe:2. ~mdd:0.1 ~lev:1. (), 30.
    ; "b", leader ~sharpe:1. ~mdd:0.3 ~lev:5. (), 30.
    ; "c", leader ~sharpe:3. ~mdd:0.5 ~lev:3. (), 30.
    ; "d", metrics [ Trades, Some 20. ], 30.
    ; "e", leader ~trades:2. ~sharpe:9. ~mdd:0. ~lev:0. (), 30.
    ]
  in
  let ranked = Score.rank config universe in
  print ranked;
  let a = List.hd_exn ranked in
  List.iter a.components ~f:(fun c ->
    printf
      "  %s value=%s pct=%.2f weight=%.2f contribution=%.1f\n"
      (Metric.name c.metric)
      (Option.value_map c.value ~default:"-" ~f:Float.to_string)
      c.percentile
      c.weight
      c.contribution);
  [%expect
    {|
    a      score= 75.0 rank=1 pct=1.00
    c      score= 60.0 rank=2 pct=0.67
    d      score= 50.0 rank=3 pct=0.33
    b      score= 15.0 rank=4 pct=0.00
    e      score=100.0 rank=- pct=- 2 closed trades, 5 needed
      sharpe value=2. pct=0.50 weight=0.50 contribution=25.0
      max_drawdown value=0.1 pct=1.00 weight=0.30 contribution=30.0
      leverage_p95 value=1. pct=1.00 weight=0.20 contribution=20.0
    |}]
;;

let%expect_test "an excluded leader is scored, not ranked, and leaves the pool" =
  let universe =
    [ "a", leader ~sharpe:2. ~mdd:0.1 ~lev:1. (), 30.
    ; "b", leader ~sharpe:1. ~mdd:0.3 ~lev:5. (), 30.
    ; "c", leader ~sharpe:3. ~mdd:0.5 ~lev:3. (), 30.
    ]
  in
  let excluded = String.Map.singleton "c" [ "integrity: trades with itself" ] in
  print (Score.rank ~excluded config universe);
  [%expect
    {|
    a      score=100.0 rank=1 pct=1.00
    b      score=  0.0 rank=2 pct=0.00
    c      score= 60.0 rank=- pct=- integrity: trades with itself
    |}]
;;

let%test_unit "input order does not change scores" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    List.gen_with_length
      6
      (let%map s = Float.gen_incl (-2.) 4.
       and d = Float.gen_incl 0. 1.
       and l = Float.gen_incl 0. 20. in
       s, d, l)
  in
  Quickcheck.test ~trials:200 gen ~f:(fun rows ->
    let universe =
      List.mapi rows ~f:(fun i (sharpe, mdd, lev) ->
        sprintf "l%d" i, leader ~sharpe ~mdd ~lev (), 30.)
    in
    let scores u =
      Score.rank config u
      |> List.map ~f:(fun (r : Score.Ranked.t) -> r.account, r.score)
      |> List.sort ~compare:[%compare: string * float]
    in
    [%test_eq: (string * float) list] (scores universe) (scores (List.rev universe)))
;;

let%test_unit "improving one metric never lowers that leader's score" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind rows =
      List.gen_with_length
        5
        (let%map s = Float.gen_incl (-2.) 4.
         and d = Float.gen_incl 0. 1.
         and l = Float.gen_incl 0. 20. in
         s, d, l)
    in
    let%map bump = Float.gen_incl 0. 3. in
    rows, bump
  in
  Quickcheck.test ~trials:200 gen ~f:(fun (rows, bump) ->
    let universe rows =
      List.mapi rows ~f:(fun i (sharpe, mdd, lev) ->
        sprintf "l%d" i, leader ~sharpe ~mdd ~lev (), 30.)
    in
    let score_of u =
      Score.rank config u
      |> List.find_exn ~f:(fun (r : Score.Ranked.t) -> String.equal r.account "l0")
      |> fun (r : Score.Ranked.t) -> r.score
    in
    let improved =
      List.mapi rows ~f:(fun i (s, d, l) -> if i = 0 then s +. bump, d, l else s, d, l)
    in
    assert (Float.( >= ) (score_of (universe improved)) (score_of (universe rows) -. 1e-9)))
;;
