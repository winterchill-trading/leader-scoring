open! Core
open Leader_scoring

let rows () =
  let lines =
    In_channel.read_lines "fixtures/model-parity.csv"
    |> List.map ~f:(String.rstrip ~drop:(Char.equal '\r'))
  in
  let header = String.split (List.hd_exn lines) ~on:',' in
  let rows =
    List.tl_exn lines
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:(fun line ->
      let cells = String.split line ~on:',' in
      let num s = if String.is_empty s then Float.nan else Float.of_string s in
      let values = List.map cells ~f:num in
      Array.of_list (List.drop_last_exn values), List.last_exn values)
  in
  header, rows
;;

let%expect_test "OCaml predictions equal scikit-learn's on the parity fixture" =
  let model = Lazy.force Model.bundled in
  let header, rows = rows () in
  assert (List.equal String.equal (List.drop_last_exn header) Dataset.feature_names);
  let worst =
    List.fold rows ~init:0. ~f:(fun acc (x, expected) ->
      Float.max acc (Float.abs (Model.predict model x -. expected)))
  in
  let missing = List.count rows ~f:(fun (x, _) -> Array.exists x ~f:Float.is_nan) in
  (* trees only compare and add, so the sums are bit-identical on every platform *)
  print_s
    [%message
      ""
        ~name:(Model.name model)
        ~trees:(Model.trees model : int)
        ~rows:(List.length rows : int)
        ~rows_with_missing:(missing : int)
        ~worst_difference:(worst : float)];
  [%expect
    {|
    ((name leader-forward-sharpe-gbdt) (trees 200) (rows 200)
     (rows_with_missing 73) (worst_difference 0))
    |}]
;;

let%expect_test "a model trained on other features is refused" =
  let json =
    `Assoc
      [ "format", `Int 1
      ; "name", `String "old"
      ; "features", `List [ `String "sharpe" ]
      ; "base_score", `Float 0.
      ; "trees", `List []
      ]
  in
  (match Model.of_json json with
   | Ok _ -> print_endline "accepted"
   | Error e -> print_endline (String.prefix (Error.to_string_hum e) 60));
  [%expect
    {|
    (Model.of_json
     (Failure
      "model features (digest 8ce02eb85
    |}]
;;
