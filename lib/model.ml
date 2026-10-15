open! Core
module U = Yojson.Safe.Util

type tree =
  { feature : int array
  ; threshold : float array
  ; missing_left : bool array
  ; left : int array
  ; right : int array
  ; value : float array
  }

type t =
  { name : string
  ; base_score : float
  ; trees : tree array
  ; evaluation : Yojson.Safe.t
  }

let feature_digest names = Md5.digest_string (String.concat ~sep:"\n" names) |> Md5.to_hex

let num = function
  | `Float f -> f
  | `Int i -> Float.of_int i
  | `Intlit s -> Float.of_string s
  | j -> failwithf "expected a number, got %s" (Yojson.Safe.to_string j) ()
;;

let of_json json =
  Or_error.try_with (fun () ->
    let format = U.member "format" json |> U.to_int in
    if format <> 1 then failwithf "model format %d, expected 1" format ();
    let features = U.member "features" json |> U.to_list |> List.map ~f:U.to_string in
    if not (List.equal String.equal features Dataset.feature_names)
    then
      failwithf
        "model features (digest %s) differ from this library's (digest %s)"
        (feature_digest features)
        (feature_digest Dataset.feature_names)
        ();
    let ints j = U.to_list j |> Array.of_list_map ~f:U.to_int in
    let floats j = U.to_list j |> Array.of_list_map ~f:num in
    let trees =
      U.member "trees" json
      |> U.to_list
      |> Array.of_list_map ~f:(fun tj ->
        { feature = ints (U.member "feature" tj)
        ; threshold = floats (U.member "threshold" tj)
        ; missing_left = ints (U.member "missing_left" tj) |> Array.map ~f:(fun i -> i <> 0)
        ; left = ints (U.member "left" tj)
        ; right = ints (U.member "right" tj)
        ; value = floats (U.member "value" tj)
        })
    in
    { name = U.member "name" json |> U.to_string
    ; base_score = U.member "base_score" json |> num
    ; trees
    ; evaluation = U.member "evaluation" json
    })
  |> Or_error.tag ~tag:"Model.of_json"
;;

let load path =
  Or_error.try_with (fun () -> Yojson.Safe.from_file path) |> Or_error.bind ~f:of_json
;;

let name t = t.name
let trees t = Array.length t.trees
let evaluation t = t.evaluation

let leaf tree (x : float array) =
  let rec go i =
    let f = tree.feature.(i) in
    if f < 0
    then tree.value.(i)
    else (
      let v = x.(f) in
      let go_left =
        if Float.is_nan v then tree.missing_left.(i) else Float.( <= ) v tree.threshold.(i)
      in
      go (if go_left then tree.left.(i) else tree.right.(i)))
  in
  go 0
;;

let predict t x =
  let acc = ref t.base_score in
  for k = 0 to Array.length t.trees - 1 do
    acc := !acc +. leaf t.trees.(k) x
  done;
  !acc
;;

let predict_metrics t m = predict t (Dataset.features m)

let bundled = lazy (Yojson.Safe.from_string Model_data.json |> of_json |> Or_error.ok_exn)
