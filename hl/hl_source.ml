open! Core
open Async
open Leader_scoring

type t =
  { config : Hl_client.Config.t
  ; pause : Time_ns.Span.t
  ; max_fill_pages : int
  ; max_funding_pages : int
  ; portfolio_window : string
  ; include_spot : bool
  ; mutable last_request : Time_ns.t
  }

let venue = "hyperliquid"

let create
  ?(pause = Time_ns.Span.of_sec 1.2)
  ?(max_fill_pages = 5)
  ?(max_funding_pages = 20)
  ?(portfolio_window = "month")
  ?(include_spot = false)
  config
  =
  { config
  ; pause
  ; max_fill_pages
  ; max_funding_pages
  ; portfolio_window
  ; include_spot
  ; last_request = Time_ns.epoch
  }
;;

let info_status t body =
  let wait = Time_ns.diff (Time_ns.add t.last_request t.pause) (Time_ns.now ()) in
  let%bind () =
    if Time_ns.Span.( > ) wait Time_ns.Span.zero then Clock_ns.after wait else return ()
  in
  t.last_request <- Time_ns.now ();
  Hl_client.Http.post_json (Hl_client.Config.info_uri t.config) body
;;

(* Transport errors (a dropped TLS handshake, a reset) are retried twice, 2 s and 4 s
   later; an HTTP status is an answer and is not retried here. *)
let rec info_retrying t body ~attempt =
  match%bind info_status t body with
  | Error _ when attempt < 3 ->
    let%bind () = Clock_ns.after (Time_ns.Span.of_int_sec (2 * attempt)) in
    info_retrying t body ~attempt:(attempt + 1)
  | result -> return result
;;

let info t body =
  match%map info_retrying t body ~attempt:1 with
  | Error e -> Error e
  | Ok { status; body } when status >= 200 && status < 300 -> Ok body
  | Ok { status; body } ->
    Or_error.error_s
      [%message "info request failed" (status : int) ~body:(String.prefix body 200 : string)]
;;

let ms time = Time_ns.to_int_ns_since_epoch time / 1_000_000

(* Pages of [kind] from [from]: each next page starts at the newest time seen, and the
   walk stops at a short page, a page with nothing newer, or [max_pages]. *)
let walk t ~kind ~account ~from ~until ~page_size ~max_pages ~optional =
  let rec go start pages n =
    if n >= max_pages
    then return (Ok (List.rev pages, true))
    else (
      let body =
        `Assoc
          [ "type", `String kind
          ; "user", `String account
          ; "startTime", `Int start
          ; "endTime", `Int (ms until)
          ]
      in
      match%bind info t body with
      | Error _ when optional && List.is_empty pages -> return (Ok ([], false))
      | Error e -> return (Error e)
      | Ok page ->
        let times =
          match Yojson.Safe.from_string page with
          | `List items ->
            List.filter_map items ~f:(fun j ->
              match Yojson.Safe.Util.member "time" j with
              | `Int t -> Some t
              | _ -> None)
          | _ -> []
          | exception _ -> []
        in
        let pages = page :: pages in
        (match List.max_elt times ~compare:Int.compare with
         | Some newest when List.length times >= page_size && newest > start ->
           go newest pages (n + 1)
         | _ -> return (Ok (List.rev pages, false))))
  in
  go (ms from) [] 0
;;

let fetch_raw t ~account ~from ~until =
  let open Deferred.Or_error.Let_syntax in
  let fetched_at = Time_ns.now () in
  (* 2000 fills and 500 funding items per answer, per the info endpoint docs *)
  let%bind fills_pages, fills_capped =
    walk
      t
      ~kind:"userFillsByTime"
      ~account
      ~from
      ~until
      ~page_size:2000
      ~max_pages:t.max_fill_pages
      ~optional:false
  in
  let%bind funding_pages, _ =
    walk
      t
      ~kind:"userFunding"
      ~account
      ~from
      ~until
      ~page_size:500
      ~max_pages:t.max_funding_pages
      ~optional:true
  in
  let%bind state =
    info t (`Assoc [ "type", `String "clearinghouseState"; "user", `String account ])
  in
  let%map portfolio =
    Deferred.map
      (info t (`Assoc [ "type", `String "portfolio"; "user", `String account ]))
      ~f:(fun r -> Ok (Result.ok r))
  in
  { Raw.meta = { address = account; fetched_at; from; until; fills_capped }
  ; fills_pages
  ; funding_pages
  ; state = Some state
  ; portfolio
  }
;;

let fetch t ~account ~from ~until =
  let%map raw = fetch_raw t ~account ~from ~until in
  Or_error.bind raw ~f:(fun raw ->
    Raw.to_history ~portfolio_window:t.portfolio_window ~include_spot:t.include_spot raw
    |> Or_error.map ~f:(fun h -> History.restrict h ~from ~until:(Time_ns.max until raw.meta.fetched_at)))
;;
