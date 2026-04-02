open Std
open Std.Data
open Std.Collections

let describe_json = function
  | Json.Null -> "null"
  | Json.Bool b -> "bool(" ^ Bool.to_string b ^ ")"
  | Json.Int i -> "int(" ^ Int.to_string i ^ ")"
  | Json.Float f -> "float(" ^ Float.to_string f ^ ")"
  | Json.String s -> "string(" ^ s ^ ")"
  | Json.Array items -> "array(" ^ Int.to_string (List.length items) ^ ")"
  | Json.Object fields -> "object(" ^ Int.to_string (List.length fields) ^ ")"

let test_parse_null = fun _ctx ->
  match Json.of_string "null" with
  | Ok Json.Null -> Ok ()
  | _ -> Error "Failed to parse null"

let test_parse_true = fun _ctx ->
  match Json.of_string "true" with
  | Ok (Json.Bool true) -> Ok ()
  | _ -> Error "Failed to parse true"

let test_parse_false = fun _ctx ->
  match Json.of_string "false" with
  | Ok (Json.Bool false) -> Ok ()
  | _ -> Error "Failed to parse false"

let test_parse_integer = fun _ctx ->
  match Json.of_string "42" with
  | Ok (Json.Int 42) -> Ok ()
  | Ok value -> Error ("Failed to parse integer, got " ^ describe_json value)
  | Error err -> Error ("Parse failed: " ^ Json.error_to_string err)

let test_parse_negative_integer = fun _ctx ->
  match Json.of_string "-123" with
  | Ok (Json.Int -123) -> Ok ()
  | Ok value -> Error ("Failed to parse negative integer, got " ^ describe_json value)
  | Error err -> Error ("Parse failed: " ^ Json.error_to_string err)

let test_parse_float = fun _ctx ->
  match Json.of_string "3.14" with
  | Ok (Json.Float 3.14) -> Ok ()
  | Ok value -> Error ("Failed to parse float, got " ^ describe_json value)
  | Error err -> Error ("Parse failed: " ^ Json.error_to_string err)

let test_parse_scientific_notation = fun _ctx ->
  match Json.of_string "1.5e10" with
  | Ok (Json.Float _) -> Ok ()
  | Ok value -> Error ("Failed to parse scientific notation, got " ^ describe_json value)
  | Error err -> Error ("Parse failed: " ^ Json.error_to_string err)

let test_parse_simple_string = fun _ctx ->
  match Json.of_string {|"hello"|} with
  | Ok (Json.String "hello") -> Ok ()
  | _ -> Error "Failed to parse simple string"

let test_parse_string_with_escapes = fun _ctx ->
  match Json.of_string {|"hello\nworld"|} with
  | Ok (Json.String s) when String.contains s "\n" -> Ok ()
  | _ -> Error "Failed to parse string with escapes"

let test_parse_empty_string = fun _ctx ->
  match Json.of_string {|""|} with
  | Ok (Json.String "") -> Ok ()
  | _ -> Error "Failed to parse empty string"

let test_parse_empty_array = fun _ctx ->
  match Json.of_string "[]" with
  | Ok (Json.Array []) -> Ok ()
  | _ -> Error "Failed to parse empty array"

let test_parse_array_with_numbers = fun _ctx ->
  match Json.of_string "[1, 2, 3]" with
  | Ok (Json.Array [Json.Int 1;Json.Int 2;Json.Int 3]) -> Ok ()
  | _ -> Error "Failed to parse array with numbers"

let test_parse_nested_array = fun _ctx ->
  match Json.of_string "[[1, 2], [3, 4]]" with
  | Ok (Json.Array [Json.Array _;Json.Array _]) -> Ok ()
  | _ -> Error "Failed to parse nested array"

let test_parse_empty_object = fun _ctx ->
  match Json.of_string "{}" with
  | Ok (Json.Object []) -> Ok ()
  | _ -> Error "Failed to parse empty object"

let test_parse_simple_object = fun _ctx ->
  match Json.of_string {|{"name": "Alice"}|} with
  | Ok (Json.Object [ ("name", Json.String "Alice") ]) -> Ok ()
  | _ -> Error "Failed to parse simple object"

let test_parse_object_multiple_fields = fun _ctx ->
  match Json.of_string {|{"name": "Bob", "age": 30}|} with
  | Ok (Json.Object fields) when List.length fields = 2 -> Ok ()
  | _ -> Error "Failed to parse object with multiple fields"

let test_parse_nested_object = fun _ctx ->
  match Json.of_string {|{"user": {"name": "Alice"}}|} with
  | Ok (Json.Object [ ("user", Json.Object _) ]) -> Ok ()
  | _ -> Error "Failed to parse nested object"

let test_parse_object_with_array = fun _ctx ->
  match Json.of_string {|{"tags": ["foo", "bar"]}|} with
  | Ok (Json.Object [ ("tags", Json.Array _) ]) -> Ok ()
  | _ -> Error "Failed to parse object with array"

let test_parse_whitespace = fun _ctx ->
  match Json.of_string "  \n  42  \n  " with
  | Ok (Json.Int 42) -> Ok ()
  | _ -> Error "Failed to parse with whitespace"

let test_serialize_null = fun _ctx ->
  if Json.to_string Json.null = "null" then
    Ok ()
  else
    Error "Failed to serialize null"

let test_serialize_bool = fun _ctx ->
  if Json.to_string (Json.bool true) = "true" then
    Ok ()
  else
    Error "Failed to serialize bool"

let test_serialize_int = fun _ctx ->
  if Json.to_string (Json.int 42) = "42" then
    Ok ()
  else
    Error "Failed to serialize int"

let test_serialize_string = fun _ctx ->
  if Json.to_string (Json.string "hello") = {|"hello"|} then
    Ok ()
  else
    Error "Failed to serialize string"

let test_serialize_array = fun _ctx ->
  let json = Json.array [ Json.int 1; Json.int 2 ] in
  if Json.to_string json = "[1,2]" then
    Ok ()
  else
    Error "Failed to serialize array"

let test_serialize_object = fun _ctx ->
  let json = Json.obj [ ("a", Json.int 1) ] in
  if Json.to_string json = {|{"a":1}|} then
    Ok ()
  else
    Error "Failed to serialize object"

let test_roundtrip = fun _ctx ->
  let original = Json.obj
    [ ("name", Json.string "Alice"); ("age", Json.int 30); ("active", Json.bool true); ] in
  let serialized = Json.to_string original in
  match Json.of_string serialized with
  | Ok parsed when parsed = original -> Ok ()
  | _ -> Error "Roundtrip failed"

let test_get_field = fun _ctx ->
  let json = Json.obj [ ("key", Json.string "value") ] in
  match Json.get_field "key" json with
  | Some (Json.String "value") -> Ok ()
  | _ -> Error "Failed to get field"

let test_get_string = fun _ctx ->
  match Json.get_string (Json.string "test") with
  | Some "test" -> Ok ()
  | _ -> Error "Failed to get string"

let test_get_int = fun _ctx ->
  match Json.get_int (Json.int 42) with
  | Some 42 -> Ok ()
  | _ -> Error "Failed to get int"

let test_get_array = fun _ctx ->
  let json = Json.array [ Json.int 1; Json.int 2 ] in
  match Json.get_array json with
  | Some [Json.Int 1;Json.Int 2] -> Ok ()
  | _ -> Error "Failed to get array"

let tests =
  Test.[
    case "parse null" test_parse_null;
    case "parse true" test_parse_true;
    case "parse false" test_parse_false;
    case "parse integer" test_parse_integer;
    case "parse negative integer" test_parse_negative_integer;
    case "parse float" test_parse_float;
    case "parse scientific notation" test_parse_scientific_notation;
    case "parse simple string" test_parse_simple_string;
    case "parse string with escapes" test_parse_string_with_escapes;
    case "parse empty string" test_parse_empty_string;
    case "parse empty array" test_parse_empty_array;
    case "parse array with numbers" test_parse_array_with_numbers;
    case "parse nested array" test_parse_nested_array;
    case "parse empty object" test_parse_empty_object;
    case "parse simple object" test_parse_simple_object;
    case "parse object with multiple fields" test_parse_object_multiple_fields;
    case "parse nested object" test_parse_nested_object;
    case "parse object with array" test_parse_object_with_array;
    case "parse with whitespace" test_parse_whitespace;
    case "serialize null" test_serialize_null;
    case "serialize bool" test_serialize_bool;
    case "serialize int" test_serialize_int;
    case "serialize string" test_serialize_string;
    case "serialize array" test_serialize_array;
    case "serialize object" test_serialize_object;
    case "roundtrip" test_roundtrip;
    case "get field" test_get_field;
    case "get string" test_get_string;
    case "get int" test_get_int;
    case "get array" test_get_array;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"json" ~tests ~args) ~args:Env.args ()
