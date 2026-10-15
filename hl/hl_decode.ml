open! Core
open Leader_scoring
module U = Yojson.Safe.Util

let time_of_ms ms = Time_ns.of_int_ns_since_epoch (ms * 1_000_000)

(* Spot pairs are "@<index>" or "BASE/QUOTE"; "#<id>" instruments are not perpetuals
   either (they never appear in clearinghouseState). *)
let is_spot coin =
  String.is_prefix coin ~prefix:"@"
  || String.is_prefix coin ~prefix:"#"
  || String.mem coin '/'
;;

let num = function
  | `String s -> Float.of_string s
  | `Float f -> f
  | `Int i -> Float.of_int i
  | `Intlit s -> Float.of_string s
  | j -> failwithf "expected a number, got %s" (Yojson.Safe.to_string j) ()
;;

let int = function
  | `Int i -> i
  | `Intlit s -> Int.of_string s
  | `Float f -> Float.iround_nearest_exn f
  | `String s -> Int.of_string s
  | j -> failwithf "expected an integer, got %s" (Yojson.Safe.to_string j) ()
;;

let num_opt = function
  | `Null -> None
  | j -> Some (num j)
;;

let decode name f = Or_error.try_with (fun () -> f ()) |> Or_error.tag ~tag:name

let fills ?(include_spot = false) json =
  decode "userFills" (fun () ->
    U.to_list json
    |> List.filter_map ~f:(fun j ->
      let coin = U.member "coin" j |> U.to_string in
      if (not include_spot) && is_spot coin
      then None
      else (
        let sz = num (U.member "sz" j) in
        let sign =
          match U.member "side" j with
          | `String "B" -> 1.
          | `String "A" -> -1.
          | side -> failwithf "unknown side %s" (Yojson.Safe.to_string side) ()
        in
        Some
          { Fill.time = time_of_ms (int (U.member "time" j))
          ; coin
          ; px = num (U.member "px" j)
          ; sz = sign *. sz
          ; fee = num (U.member "fee" j)
          ; position_before = num_opt (U.member "startPosition" j)
          ; liquidation =
              (match U.member "liquidation" j with
               | `Null -> false
               | _ -> true)
          ; id = int (U.member "tid" j)
          })))
;;

let funding json =
  decode "userFunding" (fun () ->
    U.to_list json
    |> List.filter_map ~f:(fun j ->
      let delta = U.member "delta" j in
      match U.member "type" delta with
      | `String "funding" ->
        Some
          { Funding.time = time_of_ms (int (U.member "time" j))
          ; coin = U.member "coin" delta |> U.to_string
          ; usd = num (U.member "usdc" delta)
          ; position = num_opt (U.member "szi" delta)
          ; rate = num_opt (U.member "fundingRate" delta)
          }
      | _ -> None))
;;

let snapshot ~time json =
  decode "clearinghouseState" (fun () ->
    let account_value = U.member "marginSummary" json |> U.member "accountValue" |> num in
    let time =
      match U.member "time" json with
      | `Null -> time
      | j -> time_of_ms (int j)
    in
    let positions =
      U.member "assetPositions" json
      |> U.to_list
      |> List.map ~f:(fun ap ->
        let p = U.member "position" ap in
        let size = num (U.member "szi" p) in
        let value = num_opt (U.member "positionValue" p) in
        { Snapshot.Position.coin = U.member "coin" p |> U.to_string
        ; size
        ; entry_px = num_opt (U.member "entryPx" p)
        ; mark_px =
            (match value with
             | Some v when Float.( > ) (Float.abs size) 0. -> Some (v /. Float.abs size)
             | _ -> None)
        ; leverage =
            (match U.member "leverage" p with
             | `Null -> None
             | l -> num_opt (U.member "value" l))
        })
    in
    { Snapshot.time; account_value; positions })
;;

let portfolio ~window json =
  decode "portfolio" (fun () ->
    let windows =
      U.to_list json
      |> List.filter_map ~f:(function
        | `List [ `String name; body ] -> Some (name, body)
        | _ -> None)
    in
    match List.Assoc.find windows window ~equal:String.equal with
    | None -> []
    | Some body ->
      let series name =
        U.member name body
        |> U.to_list
        |> List.map ~f:(function
          | `List [ t; v ] -> int t, num v
          | j -> failwithf "bad point %s" (Yojson.Safe.to_string j) ())
      in
      let values = series "accountValueHistory"
      and pnl = series "pnlHistory" in
      (match values, pnl with
       | (_, v0) :: _, (_, p0) :: _ ->
         List.map pnl ~f:(fun (t, p) ->
           { Equity_point.time = time_of_ms t; equity = v0 +. (p -. p0) })
       | _ -> []))
;;

let open_interest json =
  decode "metaAndAssetCtxs" (fun () ->
    match json with
    | `List [ meta; ctxs ] ->
      let names =
        U.member "universe" meta
        |> U.to_list
        |> List.map ~f:(fun a -> U.member "name" a |> U.to_string)
      in
      List.zip_exn names (U.to_list ctxs)
      |> List.filter_map ~f:(fun (name, ctx) ->
        match num_opt (U.member "openInterest" ctx), num_opt (U.member "markPx" ctx) with
        | Some oi, Some mark -> Some (name, oi *. mark)
        | _ -> None)
      |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
    | _ -> failwith "expected [meta, assetCtxs]")
;;

let book json =
  decode "l2Book" (fun () ->
    let level j =
      { Copyability.Book.px = num (U.member "px" j); size = num (U.member "sz" j) }
    in
    match U.member "levels" json with
    | `List [ bids; asks ] ->
      { Copyability.Book.coin = U.member "coin" json |> U.to_string
      ; bids = U.to_list bids |> Array.of_list_map ~f:level
      ; asks = U.to_list asks |> Array.of_list_map ~f:level
      }
    | _ -> failwith "expected levels [bids, asks]")
;;
