open! Core
open Async_kernel

module type S = sig
  type t

  val venue : string

  val fetch
    :  t
    -> account:string
    -> from:Time_ns.t
    -> until:Time_ns.t
    -> History.t Or_error.t Deferred.t
end

module Static = struct
  type t = History.t String.Map.t

  let venue = "static"

  let create histories =
    List.fold histories ~init:String.Map.empty ~f:(fun m (h : History.t) ->
      Map.set m ~key:h.account ~data:h)
  ;;

  let fetch t ~account ~from ~until =
    return
      (match Map.find t account with
       | None -> Or_error.error_s [%message "no history" (account : string)]
       | Some h -> Ok (History.restrict (History.normalize h) ~from ~until))
  ;;
end
