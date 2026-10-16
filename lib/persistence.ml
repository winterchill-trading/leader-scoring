open! Core

module Rule = struct
  type t =
    | Return
    | Sharpe
    | Composite of Score.Config.t
  [@@deriving sexp_of]

  let to_string = function
    | Return -> "return"
    | Sharpe -> "sharpe"
    | Composite _ -> "composite"
  ;;
end

module Config = struct
  type t =
    { window : Time_ns.Span.t
    ; bar : Time_ns.Span.t
    ; formation : Rule.t
    ; outcome : Rule.t
    ; quantiles : int
    ; top_k : int
    ; horizons : int
    ; min_leaders : int
    ; bootstrap_samples : int
    ; random_draws : int
    ; return_cap : float
    ; level : float
    ; seed : int
    }
  [@@deriving sexp_of]

  let default =
    { window = Time_ns.Span.of_day 30.
    ; bar = Time_ns.Span.of_day 1.
    ; formation = Sharpe
    ; outcome = Sharpe
    ; quantiles = 5
    ; top_k = 10
    ; horizons = 6
    ; (* below ten leaders a rank correlation is mostly noise *)
      min_leaders = 10
    ; bootstrap_samples = 1000
    ; random_draws = 1000
    ; return_cap = 1.
    ; level = 0.95
    ; seed = 0
    }
  ;;
end

module Interval = struct
  type t =
    { estimate : float
    ; lower : float
    ; upper : float
    ; level : float
    }
  [@@deriving sexp_of]
end

module Period = struct
  type t =
    { formation_start : Time_ns.Alternate_sexp.t
    ; leaders : int
    ; rank_correlation : float option
    ; top_k_return : float option
    ; mean_return : float option
    }
  [@@deriving sexp_of]
end

module Baseline = struct
  type t =
    { top_k : int
    ; periods : int
    ; top_k_return : float
    ; random_mean : float
    ; random_p05 : float
    ; random_p95 : float
    ; percentile : float
    ; p_value : float
    ; draws : int
    }
  [@@deriving sexp_of]
end

module Decay_point = struct
  type t =
    { horizon : int
    ; periods : int
    ; mean_rank_correlation : float option
    }
  [@@deriving sexp_of]
end

module Leader = struct
  type t =
    { windows : int
    ; percentiles : (Time_ns.Alternate_sexp.t * float) list
    ; mean_percentile : float option
    ; top_quantile_windows : int
    }
  [@@deriving sexp_of]
end

type t =
  { config : Config.t
  ; leaders : int
  ; windows : int
  ; periods : Period.t list
  ; rank_correlation : Interval.t option
  ; rank_correlation_t : float option
  ; positive_share : float option
  ; recent_rank_correlation : float option
  ; transition : int array array
  ; top_stays_top_half : float option
  ; decay : Decay_point.t list
  ; baseline : Baseline.t option
  ; persistent : bool
  ; fading : bool
  ; by_leader : Leader.t String.Map.t
  }
[@@deriving sexp_of]

(* One grid window: every participant's formation and outcome measures and its return. *)
module Window = struct
  type t =
    { start : Time_ns.t
    ; formation : float String.Map.t
    ; outcome : float String.Map.t
    ; return : float String.Map.t
    }
end

let covers (r : Track_record.t) ~from ~until =
  Time_ns.( <= ) r.from from && Time_ns.( >= ) r.until until
;;

(* End-of-bar equity over [from, until]: [m + 1] values, [m] the bars in the window. *)
let bar_equity (r : Track_record.t) ~from ~bars ~bar =
  Array.init (bars + 1) ~f:(fun j ->
    Track_record.equity_at r (Time_ns.add from (Time_ns.Span.scale_int bar j))
    |> Option.value ~default:Float.nan)
;;

let window_return e =
  let n = Array.length e in
  if n < 2 || Float.( <= ) e.(0) 0. || not (Float.is_finite e.(n - 1))
  then None
  else Some ((e.(n - 1) /. e.(0)) -. 1.)
;;

let window_sharpe e =
  let returns =
    Array.filter_mapi e ~f:(fun i x ->
      if i = 0 || Float.( <= ) e.(i - 1) 0. || not (Float.is_finite x)
      then None
      else Some ((x /. e.(i - 1)) -. 1.))
  in
  match Stats.mean returns, Stats.stdev returns with
  | Some m, Some s when Float.( > ) s 1e-12 -> Some (m /. s)
  | _ -> None
;;

let measure_window (rule : Rule.t) (config : Config.t) ~from ~until records =
  let bars =
    Float.iround_nearest_exn (Time_ns.Span.( // ) config.window config.bar) |> Int.max 1
  in
  match rule with
  | Return | Sharpe ->
    List.filter_map records ~f:(fun (r : Track_record.t) ->
      let e = bar_equity r ~from ~bars ~bar:config.bar in
      let v =
        match rule with
        | Return -> window_return e
        | Sharpe | Composite _ -> window_sharpe e
      in
      Option.map v ~f:(fun v -> r.account, v))
    |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
  | Composite score_config ->
    let universe =
      List.map records ~f:(fun (r : Track_record.t) ->
        let w = Track_record.window r ~from ~until in
        let m =
          Metrics.compute ~config:{ Metrics.Config.default with bar = Some config.bar } w
        in
        r.account, m, Time_ns.Span.to_day (Time_ns.diff until from))
    in
    Score.rank score_config universe
    |> List.map ~f:(fun (s : Score.Ranked.t) -> s.account, s.score)
    |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
;;

let windows (config : Config.t) records =
  match
    ( List.map records ~f:(fun (r : Track_record.t) -> r.from)
      |> List.min_elt ~compare:Time_ns.compare
    , List.map records ~f:(fun (r : Track_record.t) -> r.until)
      |> List.max_elt ~compare:Time_ns.compare )
  with
  | None, _ | _, None -> [||]
  | Some first, Some last ->
    let w = Time_ns.Span.to_int_ns config.window in
    let k0 = (Time_ns.to_int_ns_since_epoch first + w - 1) / w in
    let k1 = (Time_ns.to_int_ns_since_epoch last / w) - 1 in
    List.range k0 (k1 + 1)
    |> List.map ~f:(fun k ->
      let from = Time_ns.of_int_ns_since_epoch (k * w) in
      let until = Time_ns.add from config.window in
      let participants = List.filter records ~f:(covers ~from ~until) in
      let measure rule = measure_window rule config ~from ~until participants in
      let formation = measure config.formation in
      { Window.start = from
      ; formation
      ; outcome =
          (if phys_equal config.outcome config.formation
           then formation
           else measure config.outcome)
      ; return =
          Map.map (measure Return) ~f:(fun r ->
            Float.clamp_exn r ~min:(-1.) ~max:config.return_cap)
      })
    |> Array.of_list
;;

(* Leaders with a value on both sides, in a fixed order. *)
let pairs a b =
  Map.fold a ~init:[] ~f:(fun ~key ~data acc ->
    match Map.find b key with
    | Some y -> (key, data, y) :: acc
    | None -> acc)
  |> List.rev
  |> Array.of_list
;;

let column xs f = Array.map xs ~f
let mean_opt l = Stats.mean (Array.of_list (List.filter_opt l))

let quantile_of ~quantiles ~n rank =
  (* [rank] is an average rank from 1; quantile 0 is the bottom *)
  Int.min
    (quantiles - 1)
    (Float.iround_down_exn ((rank -. 1.) *. Float.of_int quantiles /. Float.of_int n))
;;

let percentile_of_rank ~n rank =
  if n <= 1 then 1. else (rank -. 1.) /. Float.of_int (n - 1)
;;

let interval xs ~level ~estimate =
  match
    ( Stats.quantile xs ((1. -. level) /. 2.)
    , Stats.quantile xs (1. -. ((1. -. level) /. 2.)) )
  with
  | Some lower, Some upper -> Some { Interval.estimate; lower; upper; level }
  | _ -> None
;;

let bootstrap_mean_spearman ~rng ~samples (periods : (float array * float array) list) =
  Array.init samples ~f:(fun _ ->
    List.filter_map periods ~f:(fun (f, o) ->
      let n = Array.length f in
      let idx = Array.init n ~f:(fun _ -> Random.State.int rng n) in
      Stats.spearman
        (Array.map idx ~f:(fun i -> f.(i)))
        (Array.map idx ~f:(fun i -> o.(i))))
    |> Array.of_list
    |> Stats.mean)
  |> Array.filter_opt
;;

(* [k] distinct indices below [n] (a partial Fisher-Yates shuffle of [scratch]). *)
let draw_mean ~rng ~scratch ~k (returns : float array) =
  let n = Array.length returns in
  for i = 0 to n - 1 do
    scratch.(i) <- i
  done;
  let sum = ref 0. in
  for i = 0 to k - 1 do
    let j = i + Random.State.int rng (n - i) in
    let tmp = scratch.(i) in
    scratch.(i) <- scratch.(j);
    scratch.(j) <- tmp;
    sum := !sum +. returns.(scratch.(i))
  done;
  !sum /. Float.of_int k
;;

let top_k_mean ~k (rows : (string * float * float) array) =
  let sorted = Array.copy rows in
  (* best formation first; ties by account so the pick does not depend on input order *)
  Array.sort sorted ~compare:(fun (a, fa, _) (b, fb, _) ->
    match Float.compare fb fa with
    | 0 -> String.compare a b
    | c -> c);
  Array.sub sorted ~pos:0 ~len:k
  |> Array.sum (module Float) ~f:(fun (_, _, r) -> r)
  |> fun s -> s /. Float.of_int k
;;

let run ?(config = Config.default) records =
  let ws = windows config records in
  let n_windows = Array.length ws in
  let enough m = Map.length m >= config.min_leaders in
  let rng = Random.State.make [| config.seed |] in
  let period_rows h =
    List.filter_map
      (List.range 0 (Int.max 0 (n_windows - h)))
      ~f:(fun k ->
        let rows = pairs ws.(k).formation ws.(k + h).outcome in
        if Array.length rows >= config.min_leaders then Some (k, rows) else None)
  in
  let next = period_rows 1 in
  let spearman rows =
    Stats.spearman (column rows (fun (_, f, _) -> f)) (column rows (fun (_, _, o) -> o))
  in
  let baseline_rows =
    List.filter_map next ~f:(fun (k, _) ->
      let rows = pairs ws.(k).formation ws.(k + 1).return in
      if Array.length rows >= Int.max 2 config.min_leaders then Some (k, rows) else None)
  in
  let periods =
    List.map next ~f:(fun (k, rows) ->
      let returns = List.Assoc.find baseline_rows ~equal:Int.equal k in
      let top_k =
        Option.map returns ~f:(fun r ->
          top_k_mean ~k:(Int.min config.top_k (Array.length r / 2)) r)
      in
      { Period.formation_start = ws.(k).start
      ; leaders = Array.length rows
      ; rank_correlation = spearman rows
      ; top_k_return = top_k
      ; mean_return =
          Option.bind returns ~f:(fun r -> Stats.mean (column r (fun (_, _, x) -> x)))
      })
  in
  let ics = List.filter_map periods ~f:(fun p -> p.rank_correlation) |> Array.of_list in
  let mean_ic = Stats.mean ics in
  let rank_correlation =
    Option.bind mean_ic ~f:(fun estimate ->
      let samples =
        bootstrap_mean_spearman
          ~rng
          ~samples:config.bootstrap_samples
          (List.map next ~f:(fun (_, rows) ->
             column rows (fun (_, f, _) -> f), column rows (fun (_, _, o) -> o)))
      in
      interval samples ~level:config.level ~estimate)
  in
  let rank_correlation_t =
    match mean_ic, Stats.stdev ics with
    | Some m, Some s when Float.( > ) s 0. ->
      Some (m /. s *. Float.sqrt (Float.of_int (Array.length ics)))
    | _ -> None
  in
  let recent_rank_correlation =
    let n = Array.length ics in
    if n < 3
    then None
    else (
      let len = Int.max 2 (n / 3) in
      Stats.mean (Array.sub ics ~pos:(n - len) ~len))
  in
  let q = Int.max 1 config.quantiles in
  let transition = Array.make_matrix ~dimx:q ~dimy:q 0 in
  let top_total = ref 0 in
  let top_stay = ref 0 in
  List.iter next ~f:(fun (_, rows) ->
    let n = Array.length rows in
    let rf = Stats.ranks (column rows (fun (_, f, _) -> f)) in
    let ro = Stats.ranks (column rows (fun (_, _, o) -> o)) in
    Array.iteri rf ~f:(fun i r ->
      let qf = quantile_of ~quantiles:q ~n r in
      let qo = quantile_of ~quantiles:q ~n ro.(i) in
      transition.(qf).(qo) <- transition.(qf).(qo) + 1;
      if qf = q - 1
      then (
        incr top_total;
        if Float.( >= ) (percentile_of_rank ~n ro.(i)) 0.5 then incr top_stay)));
  let decay =
    List.map
      (List.range 1 (config.horizons + 1))
      ~f:(fun h ->
        let ps = period_rows h in
        { Decay_point.horizon = h
        ; periods = List.length ps
        ; mean_rank_correlation =
            mean_opt (List.map ps ~f:(fun (_, rows) -> spearman rows))
        })
  in
  let baseline =
    match baseline_rows with
    | [] -> None
    | rows ->
      let rows = List.map rows ~f:snd in
      (* each window picks [top_k], or half its participants when it has fewer than twice
         that *)
      let k_of r = Int.max 1 (Int.min config.top_k (Array.length r / 2)) in
      let top =
        List.map rows ~f:(fun r -> top_k_mean ~k:(k_of r) r)
        |> Array.of_list
        |> Stats.mean
        |> Option.value_exn
      in
      let returns = List.map rows ~f:(fun r -> k_of r, column r (fun (_, _, x) -> x)) in
      let scratch =
        Array.create
          ~len:(List.fold returns ~init:0 ~f:(fun m (_, r) -> Int.max m (Array.length r)))
          0
      in
      let draws =
        Array.init config.random_draws ~f:(fun _ ->
          List.sum (module Float) returns ~f:(fun (k, r) -> draw_mean ~rng ~scratch ~k r)
          /. Float.of_int (List.length returns))
      in
      let d = Float.of_int (Array.length draws) in
      let below = Array.count draws ~f:(fun x -> Float.( < ) x top) in
      let equal = Array.count draws ~f:(fun x -> Float.equal x top) in
      let at_or_above = Array.count draws ~f:(fun x -> Float.( >= ) x top) in
      Some
        { Baseline.top_k = config.top_k
        ; periods = List.length rows
        ; top_k_return = top
        ; random_mean = Stats.mean draws |> Option.value ~default:Float.nan
        ; random_p05 = Stats.quantile draws 0.05 |> Option.value ~default:Float.nan
        ; random_p95 = Stats.quantile draws 0.95 |> Option.value ~default:Float.nan
        ; percentile = (Float.of_int below +. (0.5 *. Float.of_int equal)) /. d
        ; p_value = Float.of_int (1 + at_or_above) /. (1. +. d)
        ; draws = Array.length draws
        }
  in
  let persistent =
    (match rank_correlation with
     | Some i -> Float.( > ) i.lower 0.
     | None -> false)
    &&
    match baseline with
    (* the top picks beat 95% of random picks *)
    | Some b -> Float.( <= ) b.p_value 0.05
    | None -> false
  in
  let by_leader =
    Array.fold ws ~init:String.Map.empty ~f:(fun acc (w : Window.t) ->
      if not (enough w.formation)
      then acc
      else (
        let accounts = Map.keys w.formation |> Array.of_list in
        let values = Array.map accounts ~f:(Map.find_exn w.formation) in
        let n = Array.length values in
        let ranks = Stats.ranks values in
        Array.foldi accounts ~init:acc ~f:(fun i acc account ->
          Map.add_multi acc ~key:account ~data:(w.start, ranks.(i), n))))
    |> Map.map ~f:(fun rows ->
      let rows = List.rev rows in
      let percentiles =
        List.map rows ~f:(fun (start, r, n) -> start, percentile_of_rank ~n r)
      in
      { Leader.windows = List.length rows
      ; percentiles
      ; mean_percentile = Stats.mean (Array.of_list (List.map percentiles ~f:snd))
      ; top_quantile_windows =
          List.count rows ~f:(fun (_, r, n) -> quantile_of ~quantiles:q ~n r = q - 1)
      })
  in
  { config
  ; leaders = Map.length by_leader
  ; windows = Array.count ws ~f:(fun w -> enough w.formation)
  ; periods
  ; rank_correlation
  ; rank_correlation_t
  ; positive_share =
      (if Array.is_empty ics
       then None
       else
         Some
           (Float.of_int (Array.count ics ~f:(fun x -> Float.( > ) x 0.))
            /. Float.of_int (Array.length ics)))
  ; recent_rank_correlation
  ; transition
  ; top_stays_top_half =
      (if !top_total = 0
       then None
       else Some (Float.of_int !top_stay /. Float.of_int !top_total))
  ; decay
  ; baseline
  ; persistent
  ; fading =
      (persistent
       &&
       match recent_rank_correlation with
       | Some r -> Float.( <= ) r 0.
       | None -> false)
  ; by_leader
  }
;;

let leader t account = Map.find t.by_leader account

let transition_shares t =
  Array.map t.transition ~f:(fun row ->
    let total = Array.sum (module Int) row ~f:Fn.id in
    Array.map row ~f:(fun c ->
      if total = 0 then 0. else Float.of_int c /. Float.of_int total))
;;

let num x = if Float.is_finite x then `Float x else `Null

let opt = function
  | Some x -> num x
  | None -> `Null
;;

let ms t = `Int (Time_ns.to_int_ns_since_epoch t / 1_000_000)

let to_json t =
  let days span = num (Time_ns.Span.to_day span) in
  `Assoc
    [ "formation", `String (Rule.to_string t.config.formation)
    ; "outcome", `String (Rule.to_string t.config.outcome)
    ; "window_days", days t.config.window
    ; "bar_days", days t.config.bar
    ; "leaders", `Int t.leaders
    ; "windows", `Int t.windows
    ; "periods", `Int (List.length t.periods)
    ; ( "rank_correlation"
      , match t.rank_correlation with
        | None -> `Null
        | Some i ->
          `Assoc
            [ "estimate", num i.estimate
            ; "lower", num i.lower
            ; "upper", num i.upper
            ; "level", num i.level
            ] )
    ; "rank_correlation_t", opt t.rank_correlation_t
    ; "positive_share", opt t.positive_share
    ; "recent_rank_correlation", opt t.recent_rank_correlation
    ; ( "transition"
      , `List
          (Array.to_list t.transition
           |> List.map ~f:(fun row ->
             `List (Array.to_list row |> List.map ~f:(fun c -> `Int c)))) )
    ; "top_stays_top_half", opt t.top_stays_top_half
    ; ( "decay"
      , `List
          (List.map t.decay ~f:(fun d ->
             `Assoc
               [ "horizon", `Int d.horizon
               ; "periods", `Int d.periods
               ; "mean_rank_correlation", opt d.mean_rank_correlation
               ])) )
    ; ( "baseline"
      , match t.baseline with
        | None -> `Null
        | Some b ->
          `Assoc
            [ "top_k", `Int b.top_k
            ; "periods", `Int b.periods
            ; "top_k_return", num b.top_k_return
            ; "random_mean", num b.random_mean
            ; "random_p05", num b.random_p05
            ; "random_p95", num b.random_p95
            ; "percentile", num b.percentile
            ; "p_value", num b.p_value
            ; "draws", `Int b.draws
            ] )
    ; "persistent", `Bool t.persistent
    ; "fading", `Bool t.fading
    ]
;;

let leader_json t (l : Leader.t) =
  `Assoc
    [ "windows", `Int l.windows
    ; "mean_percentile", opt l.mean_percentile
    ; "top_quantile_windows", `Int l.top_quantile_windows
    ; ( "percentiles"
      , `List (List.map l.percentiles ~f:(fun (time, p) -> `List [ ms time; num p ])) )
    ; "universe_persistent", `Bool t.persistent
    ; ( "universe_rank_correlation"
      , Option.value_map t.rank_correlation ~default:`Null ~f:(fun i -> num i.estimate) )
    ]
;;
