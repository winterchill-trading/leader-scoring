open! Core
open Async

let books source ~coins =
  let%map pairs =
    Deferred.List.filter_map ~how:`Sequential coins ~f:(fun coin ->
      match%map
        Hl_source.info source (`Assoc [ "type", `String "l2Book"; "coin", `String coin ])
      with
      | Error _ -> None
      | Ok body ->
        (match
           Or_error.try_with (fun () -> Yojson.Safe.from_string body)
           |> Or_error.bind ~f:Hl_decode.book
         with
         | Ok book -> Some (coin, book)
         | Error _ -> None))
  in
  String.Map.of_alist_reduce pairs ~f:(fun a _ -> a)
;;

let open_interest source =
  match%map Hl_source.info source (`Assoc [ "type", `String "metaAndAssetCtxs" ]) with
  | Error _ -> String.Map.empty
  | Ok body ->
    Or_error.try_with (fun () -> Yojson.Safe.from_string body)
    |> Or_error.bind ~f:Hl_decode.open_interest
    |> Result.ok
    |> Option.value ~default:String.Map.empty
;;
