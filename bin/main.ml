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

let portfolios =
  Command.async_or_error
    ~summary:
      "Record the portfolio curves of the leaderboard's top accounts (mainnet, read only)"
    (let%map_open.Command top =
       flag "-top" (optional_with_default 300 int) ~doc:"N accounts by all-time pnl"
     and min_value =
       flag
         "-min-account-value"
         (optional_with_default 100_000. float)
         ~doc:"USD smallest account value"
     and out = flag "-out" (required string) ~doc:"DIR one recording per account" in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let source = Hl_source.create (Hl_client.Config.mainnet ()) in
       let%bind board = Hl_client.Leaderboard.fetch ~network:`Mainnet () in
       let rows =
         Hl_client.Leaderboard.rank
           ~min_account_value:min_value
           board
           ~window:All_time
           ~by:`Pnl
           ~limit:top
       in
       Core.printf "%d accounts\n%!" (List.length rows);
       Deferred.Or_error.List.iteri ~how:`Sequential rows ~f:(fun i (row, _) ->
         let address = Hl_protocol.Address.to_string row.eth_address in
         let dir = out ^/ address in
         match%bind Sys.file_exists dir |> Deferred.ok with
         | `Yes -> return ()
         | `No | `Unknown ->
           let fetched_at = Time_ns.now () in
           let%bind body =
             Hl_source.info
               source
               (`Assoc [ "type", `String "portfolio"; "user", `String address ])
           in
           let raw =
             { Raw.meta =
                 { address; fetched_at; from = Time_ns.epoch; until = fetched_at; fills_capped = false }
             ; fills_pages = []
             ; funding_pages = []
             ; state = None
             ; portfolio = Some body
             }
           in
           let%map () = Raw.save raw ~dir |> Deferred.ok in
           if i % 25 = 0 then Core.printf "%d %s\n%!" i address))
;;

let dataset =
  Command.async_or_error
    ~summary:"Write training rows: synthetic leaders and replayed recordings, as CSV"
    (let%map_open.Command synthetic =
       flag "-synthetic" (optional_with_default 0 int) ~doc:"N synthetic leaders"
     and days = flag "-days" (optional_with_default 400 int) ~doc:"D synthetic history"
     and seed = flag "-seed" (optional_with_default 1 int) ~doc:"N synthetic seed"
     and replay =
       flag "-replay" (optional string) ~doc:"DIR directory of recordings (one per account)"
     and window =
       flag
         "-portfolio-window"
         (optional_with_default "allTime" string)
         ~doc:"W portfolio curve of replayed recordings"
     and group =
       flag "-replay-group" (optional_with_default "mainnet" string) ~doc:"NAME group column"
     and out = flag "-out" (required string) ~doc:"FILE CSV to write" in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let start = Time_ns.of_string_with_utc_offset "2025-01-01T00:00:00Z" in
       let synthetic_rows =
         Synthetic.universe ~seed ~start ~leaders:synthetic ~days
         |> List.concat_map ~f:(fun (h, _) ->
           Dataset.rows ~group:"synthetic" (Track_record.build h))
       in
       let%bind replay_rows =
         match replay with
         | None -> return []
         | Some dir ->
           let%bind entries = Sys.readdir dir |> Deferred.ok in
           let dirs = Array.to_list entries |> List.sort ~compare:String.compare in
           let%map histories =
             Deferred.Or_error.List.filter_map ~how:`Sequential dirs ~f:(fun d ->
               Deferred.map (Raw.load ~dir:(dir ^/ d)) ~f:(function
                 | Error _ -> Ok None
                 | Ok raw -> Ok (Result.ok (Raw.to_history ~portfolio_window:window raw))))
           in
           List.concat_map histories ~f:(fun h -> Dataset.rows ~group (Track_record.build h))
       in
       let rows = synthetic_rows @ replay_rows in
       let%map () =
         Writer.with_file out ~f:(fun w ->
           Writer.write_line w Dataset.Row.csv_header;
           List.iter rows ~f:(fun r -> Writer.write_line w (Dataset.Row.to_csv r));
           Deferred.unit)
         |> Deferred.ok
       in
       Core.printf
         "%s: %d rows (%d synthetic, %d %s), %d labelled\n"
         out
         (List.length rows)
         (List.length synthetic_rows)
         (List.length replay_rows)
         group
         (List.count rows ~f:(fun r -> Option.is_some r.label)))
;;

let () =
  Command.group
    ~summary:"Leader track records, metrics and scores (Hyperliquid, read only)"
    [ "record", record
    ; "score", score
    ; "portfolios", portfolios
    ; "dataset", dataset
    ]
  |> Command_unix.run
;;
