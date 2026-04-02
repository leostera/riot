(** Unit tests for Shrinker module *)
open Std
open Propane

let test_towards_zero_shrinks = fun _ctx ->
  let shrinker = Shrinker.towards 0 in
  let shrunk = Shrinker.shrink shrinker 100 in
  (* Check that all shrunk values are smaller in absolute value *)
  let rec check = function
    | [] -> Ok ()
    | x :: xs ->
        let abs_x =
          if x < 0 then
            -x
          else
            x
        in
        if abs_x < 100 then
          check xs
        else
          Error ("towards 0 produced value not closer to 0: " ^ Int.to_string x)
  in
  check shrunk

let test_towards_target_shrinks = fun _ctx ->
  let shrinker = Shrinker.towards 50 in
  let shrunk = Shrinker.shrink shrinker 100 in
  (* Check that all shrunk values are between 50 and 100 *)
  let rec check = function
    | [] -> Ok ()
    | x :: xs ->
        if x >= 50 && x <= 100 then
          check xs
        else
          Error ("towards 50 produced out-of-range value: " ^ Int.to_string x)
  in
  check shrunk

let test_int_at_target_no_shrink = fun _ctx ->
  let shrinker = Shrinker.towards 0 in
  let shrunk = Shrinker.shrink shrinker 0 in
  if List.length shrunk = 0 then
    Ok ()
  else
    Error "shrinking value at target should produce empty list"

let test_list_shrinker_removes_elements = fun _ctx ->
  let shrinker = Shrinker.list Shrinker.nil in
  let original = [ 1; 2; 3; 4; 5 ] in
  let shrunk = Shrinker.shrink shrinker original in
  (* Check that shrunk lists are smaller *)
  let rec check = function
    | [] -> Ok ()
    | lst :: rest ->
        if List.length lst < List.length original then
          check rest
        else
          Error "list shrinker should produce shorter lists"
  in
  check shrunk

let test_nil_shrinker_produces_nothing = fun _ctx ->
  let shrinker = Shrinker.nil in
  let shrunk = Shrinker.shrink shrinker 42 in
  if List.length shrunk = 0 then
    Ok ()
  else
    Error "nil shrinker should produce empty list"

let test_shrinking_is_finite = fun _ctx ->
  let shrinker = Shrinker.int in
  (* Repeatedly shrink and count iterations *)
  let rec shrink_until_done value count =
    if count > 1_000 then
      Error "shrinking didn't terminate in 1000 steps"
    else
      let candidates = Shrinker.shrink shrinker value in
      match candidates with
      | [] -> Ok ()
      | x :: _ -> shrink_until_done x (count + 1)
  in
  shrink_until_done 100_000 0

let test_string_shrinker_produces_shorter = fun _ctx ->
  let shrinker = Shrinker.string in
  let original = "hello world" in
  let shrunk = Shrinker.shrink shrinker original in
  (* Check that shrunk strings are shorter or equal *)
  let rec check = function
    | [] -> Ok ()
    | s :: rest ->
        if String.length s <= String.length original then
          check rest
        else
          Error "string shrinker produced longer string"
  in
  check shrunk

let tests =
  Test.[
    case "towards zero shrinks values" test_towards_zero_shrinks;
    case "towards target shrinks values" test_towards_target_shrinks;
    case "int at target produces no shrinks" test_int_at_target_no_shrink;
    case "list shrinker removes elements" test_list_shrinker_removes_elements;
    case "nil shrinker produces nothing" test_nil_shrinker_produces_nothing;
    case "shrinking is finite" test_shrinking_is_finite;
    case "string shrinker produces shorter" test_string_shrinker_produces_shorter;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane/shrinker_tests" ~tests ~args)
    ~args:Env.args
    ()
