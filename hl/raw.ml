open! Core
open Async
open Leader_scoring

module Meta = struct
  type t =
    { address : string
    ; fetched_at : Time_ns.Alternate_sexp.t
    ; from : Time_ns.Alternate_sexp.t
    ; until : Time_ns.Alternate_sexp.t
    ; fills_capped : bool
    }
  [@@deriving sexp]
end

type t =
  { meta : Meta.t
  ; fills_pages : string list
  ; funding_pages : string list
  ; state : string option
  ; portfolio : string option
  }

let save t ~dir =
  let%bind () = Unix.mkdir ~p:() dir in
  let write name body = Writer.save (dir ^/ name) ~contents:body in
  let%bind () = Writer.save_sexp (dir ^/ "meta.sexp") (Meta.sexp_of_t t.meta) in
  let%bind () =
    Deferred.List.iteri ~how:`Sequential t.fills_pages ~f:(fun i body ->
      write (sprintf "fills-%d.json" i) body)
  in
  let%bind () =
    Deferred.List.iteri ~how:`Sequential t.funding_pages ~f:(fun i body ->
      write (sprintf "funding-%d.json" i) body)
  in
  let optional name = function
    | Some body -> write name body
    | None -> return ()
  in
  let%bind () = optional "state.json" t.state in
  optional "portfolio.json" t.portfolio
;;

let load ~dir =
  Monitor.try_with_or_error ~extract_exn:true (fun () ->
    let%bind meta = Reader.load_sexp_exn (dir ^/ "meta.sexp") Meta.t_of_sexp in
    let%bind files = Sys.readdir dir in
    let pages prefix =
      Array.to_list files
      |> List.filter_map ~f:(fun f ->
        match String.chop_prefix f ~prefix with
        | Some rest ->
          Option.map (String.chop_suffix rest ~suffix:".json") ~f:(fun i ->
            Int.of_string i, f)
        | None -> None)
      |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
      |> Deferred.List.map ~how:`Sequential ~f:(fun (_, f) ->
        Reader.file_contents (dir ^/ f))
    in
    let optional name =
      match%bind Sys.file_exists_exn (dir ^/ name) with
      | true -> Reader.file_contents (dir ^/ name) >>| Option.some
      | false -> return None
    in
    let%bind fills_pages = pages "fills-" in
    let%bind funding_pages = pages "funding-" in
    let%bind state = optional "state.json" in
    let%map portfolio = optional "portfolio.json" in
    { meta; fills_pages; funding_pages; state; portfolio })
;;

let parse name body =
  Or_error.try_with (fun () -> Yojson.Safe.from_string body) |> Or_error.tag ~tag:name
;;

let to_history ?(portfolio_window = "month") ?include_spot t =
  let open Or_error.Let_syntax in
  let%bind fills =
    List.map t.fills_pages ~f:(fun body ->
      let%bind j = parse "fills" body in
      Hl_decode.fills ?include_spot j)
    |> Or_error.all
    >>| List.concat
  in
  let%bind funding =
    List.map t.funding_pages ~f:(fun body ->
      let%bind j = parse "funding" body in
      Hl_decode.funding j)
    |> Or_error.all
    >>| List.concat
  in
  let%bind snapshots =
    match t.state with
    | None -> return []
    | Some body ->
      let%bind j = parse "state" body in
      let%map s = Hl_decode.snapshot ~time:t.meta.fetched_at j in
      [ s ]
  in
  let%bind equity_curve =
    match t.portfolio with
    | None -> return []
    | Some body ->
      let%bind j = parse "portfolio" body in
      Hl_decode.portfolio ~window:portfolio_window j
  in
  let fills_truncated =
    t.meta.fills_capped
    ||
    match List.min_elt fills ~compare:Fill.compare_by_time with
    | Some first when List.length fills >= 2000 ->
      let tenth = Time_ns.Span.scale (Time_ns.diff t.meta.until t.meta.from) 0.1 in
      Time_ns.( > ) first.time (Time_ns.add t.meta.from tenth)
    | _ -> false
  in
  return
    (History.normalize
       { History.account = t.meta.address
       ; venue = "hyperliquid"
       ; fetched_at = t.meta.fetched_at
       ; fills
       ; funding
       ; snapshots
       ; equity_curve
       ; fills_truncated
       })
;;
