open! Core

let mean xs =
  if Array.is_empty xs
  then None
  else Some (Array.sum (module Float) xs ~f:Fn.id /. Float.of_int (Array.length xs))
;;

let stdev xs =
  let n = Array.length xs in
  if n < 2
  then None
  else (
    let m = Array.sum (module Float) xs ~f:Fn.id /. Float.of_int n in
    let ss = Array.sum (module Float) xs ~f:(fun x -> (x -. m) *. (x -. m)) in
    Some (Float.sqrt (ss /. Float.of_int (n - 1))))
;;

let quantile xs q =
  let n = Array.length xs in
  if n = 0
  then None
  else (
    let s = Array.copy xs in
    Array.sort s ~compare:Float.compare;
    let pos = Float.clamp_exn q ~min:0. ~max:1. *. Float.of_int (n - 1) in
    let lo = Float.iround_down_exn pos in
    let hi = Int.min (lo + 1) (n - 1) in
    let frac = pos -. Float.of_int lo in
    Some (s.(lo) +. (frac *. (s.(hi) -. s.(lo)))))
;;

let weighted_quantile xs q =
  let xs = Array.filter xs ~f:(fun (_, w) -> Float.( > ) w 0.) in
  if Array.is_empty xs
  then None
  else (
    let s = Array.copy xs in
    Array.sort s ~compare:(fun (a, _) (b, _) -> Float.compare a b);
    let total = Array.sum (module Float) s ~f:snd in
    let target = Float.clamp_exn q ~min:0. ~max:1. *. total in
    let n = Array.length s in
    let rec go i acc =
      let v, w = s.(i) in
      let acc = acc +. w in
      if Float.( >= ) acc target || i = n - 1 then v else go (i + 1) acc
    in
    Some (go 0 0.))
;;

let ranks xs =
  let n = Array.length xs in
  let idx = Array.init n ~f:Fn.id in
  Array.sort idx ~compare:(fun i j -> Float.compare xs.(i) xs.(j));
  let r = Array.create ~len:n 0. in
  let rec group i =
    if i < n
    then (
      let j = ref i in
      while !j + 1 < n && Float.equal xs.(idx.(!j + 1)) xs.(idx.(i)) do
        incr j
      done;
      let avg = (Float.of_int (i + !j) /. 2.) +. 1. in
      for k = i to !j do
        r.(idx.(k)) <- avg
      done;
      group (!j + 1))
  in
  group 0;
  r
;;

let pearson xs ys =
  let n = Int.min (Array.length xs) (Array.length ys) in
  if n < 3
  then None
  else (
    let xs = Array.sub xs ~pos:0 ~len:n
    and ys = Array.sub ys ~pos:0 ~len:n in
    let mx = Array.sum (module Float) xs ~f:Fn.id /. Float.of_int n
    and my = Array.sum (module Float) ys ~f:Fn.id /. Float.of_int n in
    let sxy = ref 0.
    and sxx = ref 0.
    and syy = ref 0. in
    for i = 0 to n - 1 do
      let dx = xs.(i) -. mx
      and dy = ys.(i) -. my in
      sxy := !sxy +. (dx *. dy);
      sxx := !sxx +. (dx *. dx);
      syy := !syy +. (dy *. dy)
    done;
    if Float.( <= ) !sxx 0. || Float.( <= ) !syy 0.
    then None
    else Some (!sxy /. Float.sqrt (!sxx *. !syy)))
;;

let spearman xs ys = pearson (ranks xs) (ranks ys)
