open Std
open Std.Data
open Std.Collections

let test_parse_simple_row = fun _ctx ->
  let iter = Csv.of_string "a,b,c" in
  match Iter.MutIterator.next iter with
  | Some (Ok ["a";"b";"c"]) -> Ok ()
  | _ -> Error "Failed to parse simple row"

let test_parse_empty_string = fun _ctx ->
  let iter = Csv.of_string "" in
  match Iter.MutIterator.next iter with
  | None -> Ok ()
  | _ -> Error "Empty string should produce no rows"

let test_parse_single_field = fun _ctx ->
  let iter = Csv.of_string "test" in
  match Iter.MutIterator.next iter with
  | Some (Ok [ "test" ]) -> Ok ()
  | _ -> Error "Failed to parse single field"

let test_parse_multiple_rows = fun _ctx ->
  let iter = Csv.of_string "a,b\nc,d" in
  let row1 = Iter.MutIterator.next iter in
  let row2 = Iter.MutIterator.next iter in
  match (row1, row2) with
  | Some (Ok ["a";"b"]), Some (Ok ["c";"d"]) -> Ok ()
  | _ -> Error "Failed to parse multiple rows"

let test_parse_quoted_field = fun _ctx ->
  let iter = Csv.of_string {|"hello",world|} in
  match Iter.MutIterator.next iter with
  | Some (Ok ["hello";"world"]) -> Ok ()
  | _ -> Error "Failed to parse quoted field"

let test_parse_quoted_comma = fun _ctx ->
  let iter = Csv.of_string {|"a,b",c|} in
  match Iter.MutIterator.next iter with
  | Some (Ok ["a,b";"c"]) -> Ok ()
  | _ -> Error "Failed to parse quoted comma"

let test_parse_escaped_quote = fun _ctx ->
  let iter = Csv.of_string {|"a""b",c|} in
  match Iter.MutIterator.next iter with
  | Some (Ok [_;"c"]) -> Ok ()
  | _ -> Error "Failed to parse escaped quote"

let test_parse_empty_fields = fun _ctx ->
  let iter = Csv.of_string "a,,c" in
  match Iter.MutIterator.next iter with
  | Some (Ok ["a";"";"c"]) -> Ok ()
  | _ -> Error "Failed to parse empty fields"

let test_parse_trailing_comma = fun _ctx ->
  let iter = Csv.of_string "a,b," in
  match Iter.MutIterator.next iter with
  | Some (Ok ["a";"b";""]) -> Ok ()
  | _ -> Error "Failed to parse trailing comma"

let test_parse_with_newlines = fun _ctx ->
  let iter = Csv.of_string "a,b\r\nc,d" in
  let rows = Iter.MutIterator.to_list iter in
  if List.length rows = 2 then
    Ok ()
  else
    Error "Failed to parse with different newline types"

let test_custom_delimiter = fun _ctx ->
  let config = Csv.config ~delimiter:';' () in
  let iter = Csv.of_string ~config "a;b;c" in
  match Iter.MutIterator.next iter with
  | Some (Ok ["a";"b";"c"]) -> Ok ()
  | _ -> Error "Failed to parse with custom delimiter"

let test_tsv_parsing = fun _ctx ->
  let config = Csv.config ~delimiter:'\t' () in
  let iter = Csv.of_string ~config "a\tb\tc" in
  match Iter.MutIterator.next iter with
  | Some (Ok ["a";"b";"c"]) -> Ok ()
  | _ -> Error "Failed to parse TSV"

let test_trim_fields = fun _ctx ->
  let config = Csv.config ~trim_fields:true () in
  let iter = Csv.of_string ~config " a , b , c " in
  match Iter.MutIterator.next iter with
  | Some (Ok ["a";"b";"c"]) -> Ok ()
  | _ -> Error "Failed to trim fields"

let test_serialize_simple_row = fun _ctx ->
  let row = [ "a"; "b"; "c" ] in
  let str = Csv.to_string [ row ] in
  if String.contains str "a" && String.contains str "b" then
    Ok ()
  else
    Error ("Unexpected serialization: " ^ str)

let test_serialize_with_comma = fun _ctx ->
  let row = [ "a,b"; "c" ] in
  let str = Csv.to_string [ row ] in
  if String.contains str "\"" then
    Ok ()
  else
    Error "Field with comma should be quoted"

let test_serialize_with_quote = fun _ctx ->
  let row = [ "a\"b"; "c" ] in
  let str = Csv.to_string [ row ] in
  if String.contains str "\"" then
    Ok ()
  else
    Error "Field with quote should be escaped"

let test_serialize_empty_field = fun _ctx ->
  let row = [ "a"; ""; "c" ] in
  let str = Csv.to_string [ row ] in
  if String.contains str "," then
    Ok ()
  else
    Error "Empty field not serialized correctly"

let test_serialize_rows = fun _ctx ->
  let rows = [ [ "a"; "b" ]; [ "c"; "d" ] ] in
  let str = Csv.to_string rows in
  if String.contains str "\n" then
    Ok ()
  else
    Error "Multiple rows should be separated by newlines"

let test_roundtrip = fun _ctx ->
  let original = [ [ "a"; "b"; "c" ]; [ "1"; "2"; "3" ] ] in
  let str = Csv.to_string original in
  let iter = Csv.of_string str in
  let parsed = Iter.MutIterator.to_list iter |> List.filter_map Result.to_option in
  if parsed = original then
    Ok ()
  else
    Error "Roundtrip failed"

let test_headers = fun _ctx ->
  let iter = Csv.of_string "name,age\nAlice,30\nBob,25" in
  let header = Iter.MutIterator.next iter in
  match header with
  | Some (Ok ["name";"age"]) -> Ok ()
  | _ -> Error "Failed to read headers"

let tests =
  Test.[
    case "parse simple row" test_parse_simple_row;
    case "parse empty string" test_parse_empty_string;
    case "parse single field" test_parse_single_field;
    case "parse multiple rows" test_parse_multiple_rows;
    case "parse quoted field" test_parse_quoted_field;
    case "parse quoted comma" test_parse_quoted_comma;
    case "parse escaped quote" test_parse_escaped_quote;
    case "parse empty fields" test_parse_empty_fields;
    case "parse trailing comma" test_parse_trailing_comma;
    case "parse with newlines" test_parse_with_newlines;
    case "custom delimiter" test_custom_delimiter;
    case "TSV parsing" test_tsv_parsing;
    case "trim fields" test_trim_fields;
    case "serialize simple row" test_serialize_simple_row;
    case "serialize with comma" test_serialize_with_comma;
    case "serialize with quote" test_serialize_with_quote;
    case "serialize empty field" test_serialize_empty_field;
    case "serialize rows" test_serialize_rows;
    case "roundtrip" test_roundtrip;
    case "headers" test_headers;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"csv" ~tests ~args) ~args:Env.args ()
