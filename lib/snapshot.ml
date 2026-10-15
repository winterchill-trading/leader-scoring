open! Core

module Position = struct
  module Stable = struct
    module V1 = struct
      type t =
        { coin : string
        ; size : float
        ; entry_px : float option
        ; mark_px : float option
        ; leverage : float option
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t =
    { coin : string
    ; size : float
    ; entry_px : float option
    ; mark_px : float option
    ; leverage : float option
    }
  [@@deriving bin_io, sexp, compare, equal]
end

module Stable = struct
  module V1 = struct
    type t =
      { time : Time_ns.Stable.Alternate_sexp.V1.t
      ; account_value : float
      ; positions : Position.Stable.V1.t list
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { time : Time_ns.Alternate_sexp.t
  ; account_value : float
  ; positions : Position.t list
  }
[@@deriving bin_io, sexp, compare, equal]

let gross_notional t =
  List.sum
    (module Float)
    t.positions
    ~f:(fun (p : Position.t) ->
      match p.mark_px with
      | Some m -> Float.abs p.size *. m
      | None -> 0.)
;;
