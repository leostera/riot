(** Unit tests for Arbitrary module *)
open Std
open Propane

let test_arbitrary_int_generates = fun _ctx ->
  let arb = Arbitrary.int in
  let rnd = Random.State.make [|42|] in
  let value = Generator.generate rnd arb.gen in
  (* Just check it doesn't crash *)
  let _ = value in
  Ok ()

let test_arbitrary_int_has_shrinker = fun _ctx ->
  let arb = Arbitrary.int in
  match arb.shrink with
  | None -> Error "int arbitrary should have shrinker"
  | Some shrinker ->
      let shrunk = Shrinker.shrink shrinker 100 in
      (* Should produce some shrinks *)
      if List.length shrunk > 0 then
        Ok ()
      else
        Error "int shrinker should produce candidates"

let test_arbitrary_int_has_printer = fun _ctx ->
  let arb = Arbitrary.int in
  match arb.print with
  | None -> Error "int arbitrary should have printer"
  | Some printer ->
      let s = printer 42 in
      if s = "42" then
        Ok ()
      else
        Error ("printer produced wrong output: " ^ s)

let test_arbitrary_list_generates = fun _ctx ->
  let arb = Arbitrary.list Arbitrary.int in
  let rnd = Random.State.make [|42|] in
  let lst = Generator.generate rnd arb.gen in
  (* Just verify it's a list *)
  let _ = List.length lst in
  Ok ()

let test_arbitrary_list_has_printer = fun _ctx ->
  let arb = Arbitrary.list Arbitrary.int in
  match arb.print with
  | None -> Error "list arbitrary should have printer"
  | Some printer ->
      let s = printer [ 1; 2; 3 ] in
      if String.length s > 0 then
        Ok ()
      else
        Error "printer produced empty string"

let test_arbitrary_pair_generates = fun _ctx ->
  let arb = Arbitrary.pair Arbitrary.int Arbitrary.string in
  let rnd = Random.State.make [|42|] in
  let (n, s) = Generator.generate rnd arb.gen in
  (* Just verify it's a pair with correct types *)
  let _ = n + 1 in
  let _ = String.length s in
  Ok ()

let test_arbitrary_option_generates_both = fun _ctx ->
  let arb = Arbitrary.option Arbitrary.int in
  let rnd = Random.State.make [|42|] in
  (* Generate many values and check we get both None and Some *)
  let rec check n seen_none seen_some =
    if n = 0 then
      if seen_none && seen_some then
        Ok ()
      else
        Error "option should generate both None and Some"
    else
      match Generator.generate rnd arb.gen with
      | None -> check (n - 1) true seen_some
      | Some _ -> check (n - 1) seen_none true
  in
  check 100 false false

let test_arbitrary_string_generates = fun _ctx ->
  let arb = Arbitrary.string in
  let rnd = Random.State.make [|42|] in
  let s = Generator.generate rnd arb.gen in
  let _ = String.length s in
  Ok ()

let test_arbitrary_bool_generates_both = fun _ctx ->
  let arb = Arbitrary.bool in
  let rnd = Random.State.make [|42|] in
  (* Generate many values and check we get both true and false *)
  let rec check n seen_true seen_false =
    if n = 0 then
      if seen_true && seen_false then
        Ok ()
      else
        Error "bool should generate both true and false"
    else
      let b = Generator.generate rnd arb.gen in
      check (n - 1) (seen_true || b) (seen_false || not b)
  in
  check 100 false false

let test_arbitrary_int_has_small = fun _ctx ->
  let arb = Arbitrary.int in
  match arb.small with
  | None -> Error "int should have small function"
  | Some small_fn ->
      (* Check that small_fn returns reasonable values *)
      let size1 = small_fn 5 in
      let size2 = small_fn 100 in
      if size1 >= 0 && size2 >= 0 then
        Ok ()
      else
        Error "small function should return non-negative values"

let tests =
  Test.[
    case "arbitrary int generates" test_arbitrary_int_generates;
    case "arbitrary int has shrinker" test_arbitrary_int_has_shrinker;
    case "arbitrary int has printer" test_arbitrary_int_has_printer;
    case "arbitrary list generates" test_arbitrary_list_generates;
    case "arbitrary list has printer" test_arbitrary_list_has_printer;
    case "arbitrary pair generates" test_arbitrary_pair_generates;
    case "arbitrary option generates both" test_arbitrary_option_generates_both;
    case "arbitrary string generates" test_arbitrary_string_generates;
    case "arbitrary bool generates both" test_arbitrary_bool_generates_both;
    case "arbitrary int has small" test_arbitrary_int_has_small;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane/arbitrary_tests" ~tests ~args)
    ~args:Env.args
    ()
