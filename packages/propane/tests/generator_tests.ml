(** Unit tests for Generator module *)
open Std
open Propane

let test_int_range_basic = fun _ctx ->
  let gen = Generator.int_range 5 10 in
  let rnd = Random.State.make [|42|] in
  (* Generate 100 values and check they're all in range *)
  let rec check n =
    if n = 0 then
      Ok ()
    else
      let value = Generator.generate rnd gen in
      if value < 5 || value > 10 then
        Error ("int_range produced out-of-range value: " ^ Int.to_string value)
      else
        check (n - 1)
  in
  check 100

let test_int_range_single_value = fun _ctx ->
  let gen = Generator.int_range 7 7 in
  let rnd = Random.State.make [|123|] in
  let value = Generator.generate rnd gen in
  if value = 7 then
    Ok ()
  else
    Error ("int_range with low=high should return that value, got: " ^ Int.to_string value)

let test_one_of_picks_from_list = fun _ctx ->
  let gen1 = Generator.return 1 in
  let gen2 = Generator.return 2 in
  let gen3 = Generator.return 3 in
  let gen = Generator.one_of [ gen1; gen2; gen3 ] in
  let rnd = Random.State.make [|999|] in
  (* Generate several values and check they're all 1, 2, or 3 *)
  let rec check n =
    if n = 0 then
      Ok ()
    else
      let value = Generator.generate rnd gen in
      if value = 1 || value = 2 || value = 3 then
        check (n - 1)
      else
        Error ("one_of produced unexpected value: " ^ Int.to_string value)
  in
  check 50

let test_map_transforms_correctly = fun _ctx ->
  let gen = Generator.int_range 1 10 in
  let doubled =
    Generator.map (fun x -> x * 2) gen
  in
  let rnd = Random.State.make [|42|] in
  (* Generate a value and check it's even and in range [2, 20] *)
  let value = Generator.generate rnd doubled in
  if value mod 2 != 0 then
    Error "map (x * 2) should produce even numbers"
  else if value < 2 || value > 20 then
    Error ("map produced out-of-range value: " ^ Int.to_string value)
  else
    Ok ()

let test_list_generates_lists = fun _ctx ->
  let gen = Generator.list (Generator.int_range 0 100) in
  let rnd = Random.State.make [|42|] in
  (* Generate several lists and check they're valid *)
  let rec check n =
    if n = 0 then
      Ok ()
    else
      let lst = Generator.generate rnd gen in
      (* Check all elements are in range *)
      let rec check_elements = function
        | [] -> true
        | x :: xs -> (x >= 0 && x <= 100) && check_elements xs
      in
      if check_elements lst then
        check (n - 1)
      else
        Error "list generator produced invalid elements"
  in
  check 20

let test_pair_generates_pairs = fun _ctx ->
  let gen = Generator.pair (Generator.int_range 1 5) (Generator.int_range 10 20) in
  let rnd = Random.State.make [|42|] in
  let (a, b) = Generator.generate rnd gen in
  if a < 1 || a > 5 then
    Error ("pair first element out of range: " ^ Int.to_string a)
  else if b < 10 || b > 20 then
    Error ("pair second element out of range: " ^ Int.to_string b)
  else
    Ok ()

let test_deterministic_with_same_seed = fun _ctx ->
  let gen = Generator.int_range 1 1_000 in
  let rnd1 = Random.State.make [|12_345|] in
  let value1 = Generator.generate rnd1 gen in
  let rnd2 = Random.State.make [|12_345|] in
  let value2 = Generator.generate rnd2 gen in
  if value1 = value2 then
    Ok ()
  else
    Error "same seed should produce same values"

let test_string_generates_strings = fun _ctx ->
  let gen = Generator.string in
  let rnd = Random.State.make [|42|] in
  (* Just verify it produces strings without crashing *)
  let s = Generator.generate rnd gen in
  let _ = String.length s in
  Ok ()

let tests =
  Test.[
    case "int_range basic" test_int_range_basic;
    case "int_range single value" test_int_range_single_value;
    case "one_of picks from list" test_one_of_picks_from_list;
    case "map transforms correctly" test_map_transforms_correctly;
    case "list generates lists" test_list_generates_lists;
    case "pair generates pairs" test_pair_generates_pairs;
    case "deterministic with same seed" test_deterministic_with_same_seed;
    case "string generates strings" test_string_generates_strings;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane/generator_tests" ~tests ~args)
    ~args:Env.args
    ()
