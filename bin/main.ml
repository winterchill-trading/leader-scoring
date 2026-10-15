open! Core
open Async
open Leader_scoring
open Leader_scoring_hl

let network_flag =
  let%map_open.Command network =
    flag
      "-network"
      (optional_with_default "mainnet" string)
      ~doc:"mainnet|testnet|local read-only info host (default mainnet)"
  and base_url = flag "-base-url" (optional string) ~doc:"URL any other info host" in
  match base_url with
  | Some u -> Hl_client.Config.create ~base_url:(Uri.of_string u) ()
  | None ->
    (match network with
     | "mainnet" -> Hl_client.Config.mainnet ()
     | "testnet" -> Hl_client.Config.testnet ()
     | "local" -> Hl_client.Config.local ()
     | other -> failwithf "unknown network %s" other ())
;;

let fmt = function
  | None -> "-"
  | Some x -> sprintf "%.4g" x
;;

let record =
  Command.async_or_error
    ~summary:"Record one account's raw info answers into a directory (read only)"
    (let%map_open.Command config = network_flag
     and address = flag "-address" (required string) ~doc:"0x... account to read"
     and days = flag "-days" (optional_with_default 30. float) ~doc:"D history to ask for"
     and pages = flag "-max-fill-pages" (optional_with_default 5 int) ~doc:"N fill pages"
     and out = flag "-out" (required string) ~doc:"DIR where to write the recording" in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let source = Hl_source.create ~max_fill_pages:pages config in
       let until = Time_ns.now () in
       let from = Time_ns.sub until (Time_ns.Span.of_day days) in
       let%bind raw = Hl_source.fetch_raw source ~account:(String.lowercase address) ~from ~until in
       let%map () = Raw.save raw ~dir:out |> Deferred.ok in
       Core.printf
         "%s: %d fill pages, %d funding pages, state %b, portfolio %b\n"
         out
         (List.length raw.fills_pages)
         (List.length raw.funding_pages)
         (Option.is_some raw.state)
         (Option.is_some raw.portfolio))
;;

let load dirs ~window =
  Deferred.Or_error.List.map ~how:`Sequential dirs ~f:(fun dir ->
    let open Deferred.Or_error.Let_syntax in
    let%bind raw = Raw.load ~dir in
    Raw.to_history ~portfolio_window:window raw |> Deferred.return)
;;

let score =
  Command.async_or_error
    ~summary:"Score recorded accounts: metrics, composite ranking, alerts"
    (let%map_open.Command dirs = anon (sequence ("DIR" %: string))
     and window =
       flag "-portfolio-window" (optional_with_default "month" string) ~doc:"W portfolio curve"
     and json = flag "-json" no_arg ~doc:" print one JSON object per account" in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%map histories = load dirs ~window in
       let records = List.map histories ~f:(fun h -> Track_record.build h) in
       let universe =
         List.map records ~f:(fun tr ->
           ( tr.account
           , Metrics.compute tr
           , Time_ns.Span.to_day (Time_ns.diff tr.until tr.from) ))
       in
       let ranked = Score.rank Score.Config.default universe in
       if json
       then
         List.iter2_exn records universe ~f:(fun tr (account, m, _) ->
           let r = List.find_exn ranked ~f:(fun r -> String.equal r.account account) in
           Core.print_endline
             (Yojson.Safe.to_string
                (`Assoc
                    [ "account", `String account
                    ; "score", `Float r.score
                    ; "rank", Option.value_map r.rank ~default:`Null ~f:(fun i -> `Int i)
                    ; "equity_source", `String (Track_record.Equity_source.to_string tr.equity_source)
                    ; "metrics", Metrics.to_json m
                    ])))
       else (
         List.iter ranked ~f:(fun r ->
           Core.printf
             "%-44s score %5.1f rank %s\n"
             r.account
             r.score
             (Option.value_map r.rank ~default:"-" ~f:Int.to_string));
         List.iter universe ~f:(fun (account, m, _) ->
           Core.printf "\n%s\n" account;
           List.iter (Metrics.to_alist m) ~f:(fun (metric, v) ->
             Core.printf "  %-28s %s\n" (Metric.name metric) (fmt v)))))
;;

let () =
  Command.group
    ~summary:"Leader track records, metrics and scores (Hyperliquid, read only)"
    [ "record", record; "score", score ]
  |> Command_unix.run
;;
