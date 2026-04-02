open Std
open Std.Data
open Std.Collections

(* bring Diff types into scope *)

open Diff

let test_diff_identical_nulls = Test.case "diff identical null values" @@ fun _ctx ->
  let diff = Json.diff Json.null Json.null in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Identical nulls should have no diff"

let test_diff_identical_bools = Test.case "diff identical booleans" @@ fun _ctx ->
  let diff = Json.diff (Json.bool true) (Json.bool true) in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Identical bools should have no diff"

let test_diff_different_bools = Test.case "diff different booleans" @@ fun _ctx ->
  let diff = Json.diff (Json.bool true) (Json.bool false) in
  match diff with
  | [ { path=[]; kind=Changed (Json.Bool true, Json.Bool false) } ] -> Ok ()
  | _ -> Error "Expected Changed from true to false"

let test_diff_identical_ints = Test.case "diff identical integers" @@ fun _ctx ->
  let diff = Json.diff (Json.int 42) (Json.int 42) in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Identical ints should have no diff"

let test_diff_different_ints = Test.case "diff different integers" @@ fun _ctx ->
  let diff = Json.diff (Json.int 1) (Json.int 2) in
  match diff with
  | [ { path=[]; kind=Changed (Json.Int 1, Json.Int 2) } ] -> Ok ()
  | _ -> Error "Expected Changed from 1 to 2"

let test_diff_identical_floats = Test.case "diff identical floats" @@ fun _ctx ->
  let diff = Json.diff (Json.float 3.14) (Json.float 3.14) in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Identical floats should have no diff"

let test_diff_different_floats = Test.case "diff different floats" @@ fun _ctx ->
  let diff = Json.diff (Json.float 1.0) (Json.float 2.0) in
  match diff with
  | [ { path=[]; kind=Changed _ } ] -> Ok ()
  | _ -> Error "Expected Changed for different floats"

let test_diff_identical_strings = Test.case "diff identical strings" @@ fun _ctx ->
  let diff = Json.diff (Json.string "hello") (Json.string "hello") in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Identical strings should have no diff"

let test_diff_different_strings = Test.case "diff different strings" @@ fun _ctx ->
  let diff = Json.diff (Json.string "hello") (Json.string "world") in
  match diff with
  | [ { path=[]; kind=Changed (Json.String "hello", Json.String "world") } ] -> Ok ()
  | _ -> Error "Expected Changed from hello to world"

let test_diff_different_types = Test.case "diff different JSON types" @@ fun _ctx ->
  let diff = Json.diff (Json.int 42) (Json.string "42") in
  match diff with
  | [ { path=[]; kind=Changed _ } ] -> Ok ()
  | _ -> Error "Expected Changed for different types"

let test_diff_empty_arrays = Test.case "diff empty arrays" @@ fun _ctx ->
  let diff = Json.diff (Json.array []) (Json.array []) in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Empty arrays should have no diff"

let test_diff_identical_arrays = Test.case "diff identical arrays" @@ fun _ctx ->
  let arr = Json.array [ Json.int 1; Json.int 2; Json.int 3 ] in
  let diff = Json.diff arr arr in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Identical arrays should have no diff"

let test_diff_array_element_changed = Test.case "diff array with one element changed" @@ fun _ctx ->
  let a1 = Json.array [ Json.int 1; Json.int 2; Json.int 3 ] in
  let a2 = Json.array [ Json.int 1; Json.int 99; Json.int 3 ] in
  let diff = Json.diff a1 a2 in
  match diff with
  | [ { path=[ Index 1 ]; kind=Changed (Json.Int 2, Json.Int 99) } ] -> Ok ()
  | _ -> Error ("Expected change at index 1, got " ^ Int.to_string (List.length diff) ^ " diffs")

let test_diff_array_shorter = Test.case "diff array with removed elements" @@ fun _ctx ->
  let a1 = Json.array [ Json.int 1; Json.int 2; Json.int 3 ] in
  let a2 = Json.array [ Json.int 1 ] in
  let diff = Json.diff a1 a2 in
  let removed = removals diff in
  if List.length removed >= 1 then
    Ok ()
  else
    Error "Expected removed elements"

let test_diff_array_longer = Test.case "diff array with added elements" @@ fun _ctx ->
  let a1 = Json.array [ Json.int 1 ] in
  let a2 = Json.array [ Json.int 1; Json.int 2; Json.int 3 ] in
  let diff = Json.diff a1 a2 in
  let added = additions diff in
  if List.length added >= 1 then
    Ok ()
  else
    Error "Expected added elements"

let test_diff_array_completely_different = Test.case "diff completely different arrays" @@ fun _ctx ->
  let a1 = Json.array [ Json.int 1; Json.int 2 ] in
  let a2 = Json.array [ Json.string "a"; Json.string "b" ] in
  let diff = Json.diff a1 a2 in
  if List.length diff > 0 then
    Ok ()
  else
    Error "Expected differences"

let test_diff_nested_arrays = Test.case "diff nested arrays" @@ fun _ctx ->
  let a1 = Json.array
    [ Json.array [ Json.int 1; Json.int 2 ]; Json.array [ Json.int 3; Json.int 4 ]; ] in
  let a2 = Json.array
    [ Json.array [ Json.int 1; Json.int 2 ]; Json.array [ Json.int 3; Json.int 99 ]; ] in
  let diff = Json.diff a1 a2 in
  match diff with
  | [ { path=[Index 1;Index 1]; kind=Changed (Json.Int 4, Json.Int 99) };  ] -> Ok ()
  | _ -> Error ("Expected nested change at [1][1], got " ^ Int.to_string (List.length diff) ^ " diffs")

let test_diff_empty_objects = Test.case "diff empty objects" @@ fun _ctx ->
  let diff = Json.diff (Json.obj []) (Json.obj []) in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Empty objects should have no diff"

let test_diff_identical_objects = Test.case "diff identical objects" @@ fun _ctx ->
  let obj = Json.obj [ ("name", Json.string "Alice"); ("age", Json.int 30) ] in
  let diff = Json.diff obj obj in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Identical objects should have no diff"

let test_diff_object_field_added = Test.case "diff object with added field" @@ fun _ctx ->
  let o1 = Json.obj [ ("name", Json.string "Alice") ] in
  let o2 = Json.obj [ ("name", Json.string "Alice"); ("age", Json.int 30) ] in
  let diff = Json.diff o1 o2 in
  let added = additions diff in
  match added with
  | [ { path=[ Key "age" ]; kind=Added (Json.Int 30) } ] -> Ok ()
  | _ -> Error ("Expected 1 addition at 'age', got " ^ Int.to_string (List.length added))

let test_diff_object_field_removed = Test.case "diff object with removed field" @@ fun _ctx ->
  let o1 = Json.obj [ ("name", Json.string "Alice"); ("age", Json.int 30) ] in
  let o2 = Json.obj [ ("name", Json.string "Alice") ] in
  let diff = Json.diff o1 o2 in
  let removed = removals diff in
  match removed with
  | [ { path=[ Key "age" ]; kind=Removed (Json.Int 30) } ] -> Ok ()
  | _ -> Error ("Expected 1 removal at 'age', got " ^ Int.to_string (List.length removed))

let test_diff_object_field_changed = Test.case "diff object with changed field" @@ fun _ctx ->
  let o1 = Json.obj [ ("name", Json.string "Alice"); ("age", Json.int 30) ] in
  let o2 = Json.obj [ ("name", Json.string "Alice"); ("age", Json.int 31) ] in
  let diff = Json.diff o1 o2 in
  match diff with
  | [ { path=[ Key "age" ]; kind=Changed (Json.Int 30, Json.Int 31) } ] -> Ok ()
  | _ -> Error ("Expected change in age field, got " ^ Int.to_string (List.length diff) ^ " diffs")

let test_diff_object_multiple_changes = Test.case "diff object with multiple changes" @@ fun _ctx ->
  let o1 = Json.obj [ ("a", Json.int 1); ("b", Json.int 2); ("c", Json.int 3) ] in
  let o2 = Json.obj [ ("a", Json.int 1); ("b", Json.int 99); ("d", Json.int 4) ] in
  let diff = Json.diff o1 o2 in
  if List.length diff = 3 then
    Ok ()
  else
    Error ("Expected 3 differences (1 changed, 1 removed, 1 added), got "
    ^ Int.to_string (List.length diff))

let test_diff_nested_objects = Test.case "diff deeply nested objects" @@ fun _ctx ->
  let o1 = Json.obj
    [
      (
        "user",
        Json.obj
          [
            ("name", Json.string "Alice");
            ("address", Json.obj [ ("city", Json.string "NYC"); ("zip", Json.string "10001") ]);
          ]
      );
    ] in
  let o2 = Json.obj
    [
      (
        "user",
        Json.obj
          [
            ("name", Json.string "Alice");
            ("address", Json.obj [ ("city", Json.string "SF"); ("zip", Json.string "10001") ]);
          ]
      );
    ] in
  let diff = Json.diff o1 o2 in
  match diff with
  | [
    {
      path=[Key "user";Key "address";Key "city"];
      kind=Changed (Json.String "NYC", Json.String "SF");

    };

  ] -> Ok ()
  | _ -> Error ("Expected nested change at user.address.city, got "
  ^ Int.to_string (List.length diff)
  ^ " diffs")

let test_diff_object_with_array = Test.case "diff object containing arrays" @@ fun _ctx ->
  let o1 = Json.obj [ ("tags", Json.array [ Json.string "foo"; Json.string "bar" ]) ] in
  let o2 = Json.obj [ ("tags", Json.array [ Json.string "foo"; Json.string "baz" ]) ] in
  let diff = Json.diff o1 o2 in
  match diff with
  | [ { path=[Key "tags";Index 1]; kind=Changed (Json.String "bar", Json.String "baz");  };  ] -> Ok ()
  | _ -> Error ("Expected change in tags array at index 1, got "
  ^ Int.to_string (List.length diff)
  ^ " diffs")

let test_diff_complex_nested_structure = Test.case "diff complex deeply nested structure"
@@ fun _ctx ->
  let o1 = Json.obj
    [
      (
        "users",
        Json.array
          [ Json.obj [ ("id", Json.int 1); ("roles", Json.array [ Json.string "admin" ]); ]; ]
      );
    ] in
  let o2 = Json.obj
    [
      (
        "users",
        Json.array
          [
            Json.obj
              [
                ("id", Json.int 1);
                ("roles", Json.array [ Json.string "admin"; Json.string "moderator" ]);
              ];
          ]
      );
    ] in
  let diff = Json.diff o1 o2 in
  let added = additions diff in
  if List.length added >= 1 then
    Ok ()
  else
    Error "Expected addition in nested array"

let test_diff_null_to_value = Test.case "diff null to value" @@ fun _ctx ->
  let diff = Json.diff Json.null (Json.int 42) in
  match diff with
  | [ { path=[]; kind=Changed (Json.Null, Json.Int 42) } ] -> Ok ()
  | _ -> Error "Expected change from null to int"

let test_diff_value_to_null = Test.case "diff value to null" @@ fun _ctx ->
  let diff = Json.diff (Json.int 42) Json.null in
  match diff with
  | [ { path=[]; kind=Changed (Json.Int 42, Json.Null) } ] -> Ok ()
  | _ -> Error "Expected change from int to null"

let test_diff_empty_string_values = Test.case "diff empty strings" @@ fun _ctx ->
  let diff = Json.diff (Json.string "") (Json.string "") in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Empty strings should match"

let test_diff_object_key_ordering = Test.case "diff objects with different key order" @@ fun _ctx ->
  let o1 = Json.obj [ ("a", Json.int 1); ("b", Json.int 2) ] in
  let o2 = Json.obj [ ("b", Json.int 2); ("a", Json.int 1) ] in
  let diff = Json.diff o1 o2 in
  if List.length diff = 0 then
    Ok ()
  else
    Error "Key order shouldn't matter"

let test_json_diff_with_helpers = Test.case "use helpers on JSON diff results" @@ fun _ctx ->
  let o1 = Json.obj [ ("a", Json.int 1); ("b", Json.int 2) ] in
  let o2 = Json.obj [ ("a", Json.int 99); ("c", Json.int 3) ] in
  let diff = Json.diff o1 o2 in
  let added = additions diff in
  let removed = removals diff in
  let changed = changes diff in
  if List.length added = 1 && List.length removed = 1 && List.length changed = 1 then
    Ok ()
  else
    Error ("Expected 1 add, 1 remove, 1 change; got "
    ^ Int.to_string (List.length added)
    ^ ", "
    ^ Int.to_string (List.length removed)
    ^ ", "
    ^ Int.to_string (List.length changed))

let test_json_at_path = Test.case "filter JSON diffs by path" @@ fun _ctx ->
  let o1 = Json.obj
    [
      ("user", Json.obj [ ("name", Json.string "Alice"); ("age", Json.int 30) ]);
      ("config", Json.obj [ ("port", Json.int 8_080) ]);
    ] in
  let o2 = Json.obj
    [
      ("user", Json.obj [ ("name", Json.string "Bob"); ("age", Json.int 31) ]);
      ("config", Json.obj [ ("port", Json.int 8_080) ]);
    ] in
  let diff = Json.diff o1 o2 in
  let user_name_changes = at_path [ Key "user"; Key "name" ] diff in
  let user_age_changes = at_path [ Key "user"; Key "age" ] diff in
  if List.length user_name_changes = 1 && List.length user_age_changes = 1 then
    Ok ()
  else
    Error ("Expected 1 change for each path, got "
    ^ Int.to_string (List.length user_name_changes)
    ^ " and "
    ^ Int.to_string (List.length user_age_changes))

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let all_tests = [
        test_diff_identical_nulls;
        test_diff_identical_bools;
        test_diff_different_bools;
        test_diff_identical_ints;
        test_diff_different_ints;
        test_diff_identical_floats;
        test_diff_different_floats;
        test_diff_identical_strings;
        test_diff_different_strings;
        test_diff_different_types;
        test_diff_empty_arrays;
        test_diff_identical_arrays;
        test_diff_array_element_changed;
        test_diff_array_shorter;
        test_diff_array_longer;
        test_diff_array_completely_different;
        test_diff_nested_arrays;
        test_diff_empty_objects;
        test_diff_identical_objects;
        test_diff_object_field_added;
        test_diff_object_field_removed;
        test_diff_object_field_changed;
        test_diff_object_multiple_changes;
        test_diff_nested_objects;
        test_diff_object_with_array;
        test_diff_complex_nested_structure;
        test_diff_null_to_value;
        test_diff_value_to_null;
        test_diff_empty_string_values;
        test_diff_object_key_ordering;
        test_json_diff_with_helpers;
        test_json_at_path;
      ]
      in
      Test.Cli.main ~name:"json-diff" ~tests:all_tests ~args)
    ~args:Env.args
    ()
