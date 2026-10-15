open! Core
open Leader_scoring

let coins = [ "BTC"; "ETH"; "SOL" ]

(* Fills a few minutes to a few hours apart over up to two months, sizes small enough that
   the account below never goes broke, then a closing snapshot. *)
let history : History.t Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let%bind n = Int.gen_incl 2 60 in
  let%bind steps =
    List.gen_with_length
      n
      (let%map coin = Quickcheck.Generator.of_list coins
       and gap_min = Int.gen_incl 1 (24 * 60 * 2)
       and sz = Float.gen_incl 0.1 3.
       and buy = Bool.quickcheck_generator
       and px = Float.gen_incl 50. 150.
       and fee = Float.gen_incl 0. 0.2 in
       coin, gap_min, (if buy then sz else -.sz), px, fee)
  in
  let%map account_value = Float.gen_incl 5_000. 20_000. in
  let _, fills =
    List.fold_mapi steps ~init:Helpers.t0 ~f:(fun i time (coin, gap_min, sz, px, fee) ->
      let time = Time_ns.add time (Time_ns.Span.of_int_sec (gap_min * 60)) in
      ( time
      , { Fill.time
        ; coin
        ; px
        ; sz
        ; fee
        ; position_before = None
        ; liquidation = false
        ; id = i
        } ))
  in
  let last = (List.last_exn fills).time in
  let marks =
    List.map coins ~f:(fun c ->
      c, List.filter fills ~f:(fun f -> String.equal f.coin c) |> List.last)
  in
  let positions =
    List.filter_map marks ~f:(fun (c, f) ->
      Option.map f ~f:(fun (f : Fill.t) ->
        let size =
          List.sum (module Float) fills ~f:(fun g ->
            if String.equal g.coin c then g.sz else 0.)
        in
        c, size, f.px))
  in
  Helpers.history
    ~snapshots:
      [ Helpers.snapshot
          ~time:(Time_ns.add last Time_ns.Span.hour)
          ~account_value
          positions
      ]
    fills
;;

let scale (h : History.t) k : History.t =
  { h with
    fills = List.map h.fills ~f:(fun f -> { f with sz = f.sz *. k; fee = f.fee *. k })
  ; funding = List.map h.funding ~f:(fun f -> { f with usd = f.usd *. k })
  ; snapshots =
      List.map h.snapshots ~f:(fun s ->
        { s with
          account_value = s.account_value *. k
        ; positions = List.map s.positions ~f:(fun p -> { p with size = p.size *. k })
        })
  ; equity_curve = List.map h.equity_curve ~f:(fun p -> { p with equity = p.equity *. k })
  }
;;

let shift (h : History.t) span : History.t =
  let s t = Time_ns.add t span in
  { h with
    fills = List.map h.fills ~f:(fun f -> { f with time = s f.time })
  ; funding = List.map h.funding ~f:(fun f -> { f with time = s f.time })
  ; snapshots = List.map h.snapshots ~f:(fun x -> { x with time = s x.time })
  ; equity_curve = List.map h.equity_curve ~f:(fun p -> { p with time = s p.time })
  }
;;
