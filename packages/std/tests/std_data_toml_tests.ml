open Std
open Std.Data
open Std.Collections

(* Helper to extract values *)

let get_string = function
  | Toml.String s -> Some s
  | _ -> None

let get_int = function
  | Toml.Int i -> Some i
  | _ -> None

let get_table = function
  | Toml.Table t -> Some t
  | _ -> None

let get_array = function
  | Toml.Array a -> Some a
  | _ -> None

let get_bool = function
  | Toml.Bool b -> Some b
  | _ -> None

(* === BASIC VALUE TESTS === *)

let test_simple_string = Test.case "parse simple string value" @@ fun _ctx ->
  let input = {|name = "hello"|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match List.assoc_opt "name" items with
      | Some (Toml.String "hello") -> Ok ()
      | Some _ -> Error "Expected string value"
      | None -> Error "Missing 'name' key"
    )
  | _ -> Error "Parse failed"

let test_quoted_string_with_escapes = Test.case "parse string with escapes" @@ fun _ctx ->
  let input = {|text = "hello\nworld\t\"quoted\""|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_string (List.assoc "text" items) with
      | Some s when String.contains s "\n" && String.contains s "\t" -> Ok ()
      | Some s -> Error ("String escapes not handled: " ^ s)
      | None -> Error "Expected string"
    )
  | _ -> Error "Parse failed"

let test_boolean_true = Test.case "parse boolean true" @@ fun _ctx ->
  let input = {|enabled = true|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_bool (List.assoc "enabled" items) with
      | Some true -> Ok ()
      | Some false -> Error "Got false, expected true"
      | None -> Error "Expected boolean"
    )
  | _ -> Error "Parse failed"

let test_boolean_false = Test.case "parse boolean false" @@ fun _ctx ->
  let input = {|enabled = false|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_bool (List.assoc "enabled" items) with
      | Some false -> Ok ()
      | Some true -> Error "Got true, expected false"
      | None -> Error "Expected boolean"
    )
  | _ -> Error "Parse failed"

let test_integer_positive = Test.case "parse positive integer" @@ fun _ctx ->
  let input = {|port = 2112|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_int (List.assoc "port" items) with
      | Some 2_112 -> Ok ()
      | Some i -> Error ("Got " ^ string_of_int i ^ ", expected 2112")
      | None -> Error "Expected integer"
    )
  | _ -> Error "Parse failed"

let test_integer_negative = Test.case "parse negative integer" @@ fun _ctx ->
  let input = {|offset = -42|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_int (List.assoc "offset" items) with
      | Some (-42) -> Ok ()
      | Some i -> Error ("Got " ^ string_of_int i ^ ", expected -42")
      | None -> Error "Expected integer"
    )
  | _ -> Error "Parse failed"

let test_integer_zero = Test.case "parse zero" @@ fun _ctx ->
  let input = {|count = 0|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_int (List.assoc "count" items) with
      | Some 0 -> Ok ()
      | Some i -> Error ("Got " ^ string_of_int i ^ ", expected 0")
      | None -> Error "Expected integer"
    )
  | _ -> Error "Parse failed"

let test_integer_in_array = Test.case "parse array of integers" @@ fun _ctx ->
  let input = {|ports = [8080, 8081, 8082]|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_array (List.assoc "ports" items) with
      | Some [Toml.Int 8_080;Toml.Int 8_081;Toml.Int 8_082] -> Ok ()
      | Some arr -> Error ("Got array with " ^ string_of_int (List.length arr) ^ " items")
      | None -> Error "Expected array"
    )
  | _ -> Error "Parse failed"

let test_bare_string = Test.case "parse bare string value" @@ fun _ctx ->
  let input = {|version = release-1|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_string (List.assoc "version" items) with
      | Some "release-1" -> Ok ()
      | Some s -> Error ("Got '" ^ s ^ "', expected 'release-1'")
      | None -> Error "Expected string"
    )
  | _ -> Error "Parse failed"

(* === ARRAY TESTS === *)

let test_simple_array = Test.case "parse simple array" @@ fun _ctx ->
  let input = {|numbers = [1, 2, 3]|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_array (List.assoc "numbers" items) with
      | Some arr when List.length arr = 3 -> Ok ()
      | Some arr -> Error ("Expected 3 items, got " ^ Int.to_string (List.length arr))
      | None -> Error "Expected array"
    )
  | _ -> Error "Parse failed"

let test_string_array = Test.case "parse string array" @@ fun _ctx ->
  let input = {|tags = ["foo", "bar", "baz"]|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_array (List.assoc "tags" items) with
      | Some [Toml.String "foo";Toml.String "bar";Toml.String "baz"] -> Ok ()
      | Some _ -> Error "Array contents don't match"
      | None -> Error "Expected array"
    )
  | _ -> Error "Parse failed"

let test_empty_array = Test.case "parse empty array" @@ fun _ctx ->
  let input = {|empty = []|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_array (List.assoc "empty" items) with
      | Some [] -> Ok ()
      | Some arr -> Error ("Expected empty array, got " ^ Int.to_string (List.length arr) ^ " items")
      | None -> Error "Expected array"
    )
  | _ -> Error "Parse failed"

let test_nested_array = Test.case "parse nested array" @@ fun _ctx ->
  let input = {|matrix = [[1, 2], [3, 4]]|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_array (List.assoc "matrix" items) with
      | Some [Toml.Array _;Toml.Array _] -> Ok ()
      | Some _ -> Error "Expected nested arrays"
      | None -> Error "Expected array"
    )
  | _ -> Error "Parse failed"

(* === INLINE TABLE TESTS === *)

let test_simple_inline_table = Test.case "parse simple inline table" @@ fun _ctx ->
  let input = {|
[dependencies]
std = { path = "../std" }
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      match get_table (List.assoc "dependencies" sections) with
      | Some deps -> (
          match get_table (List.assoc "std" deps) with
          | Some std_attrs -> (
              match get_string (List.assoc "path" std_attrs) with
              | Some "../std" -> Ok ()
              | Some s -> Error ("Wrong path: " ^ s)
              | None -> Error "Missing path"
            )
          | None -> Error "std is not a table"
        )
      | None -> Error "dependencies is not a table"
    )
  | _ -> Error "Parse failed"

let test_multiple_inline_tables = Test.case "parse multiple inline tables" @@ fun _ctx ->
  let input = {|
[dependencies]
std = { path = "../std" }
kernel = { path = "../kernel" }
miniriot = { path = "../miniriot" }
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      match get_table (List.assoc "dependencies" sections) with
      | Some deps when List.length deps = 3 -> Ok ()
      | Some deps -> Error ("Expected 3 deps, got " ^ Int.to_string (List.length deps))
      | None -> Error "dependencies is not a table"
    )
  | _ -> Error "Parse failed"

let test_inline_table_multiple_keys = Test.case "parse inline table with multiple keys" @@ fun _ctx ->
  let input = {|person = { name = "John", age = "30", city = "NYC" }|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_table (List.assoc "person" items) with
      | Some attrs when List.length attrs = 3 -> (
          match (
            get_string (List.assoc "name" attrs),
            get_string (List.assoc "age" attrs),
            get_string (List.assoc "city" attrs)
          ) with
          | Some "John", Some "30", Some "NYC" -> Ok ()
          | _ -> Error "Values don't match"
        )
      | Some attrs ->
          Error ("Expected 3 keys, got " ^ Int.to_string (List.length attrs))
      | None ->
          Error "person is not a table"
    )
  | _ -> Error "Parse failed"

let test_inline_table_with_bool = Test.case "parse inline table with boolean" @@ fun _ctx ->
  let input = {|config = { enabled = true, debug = false }|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_table (List.assoc "config" items) with
      | Some attrs -> (
          match (get_bool (List.assoc "enabled" attrs), get_bool (List.assoc "debug" attrs)) with
          | Some true, Some false -> Ok ()
          | _ -> Error "Boolean values don't match"
        )
      | None -> Error "config is not a table"
    )
  | _ -> Error "Parse failed"

let test_nested_inline_tables = Test.case "parse nested inline tables" @@ fun _ctx ->
  let input = {|server = { host = { ip = "127.0.0.1", port = "8080" } }|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_table (List.assoc "server" items) with
      | Some server -> (
          match get_table (List.assoc "host" server) with
          | Some host -> (
              match get_string (List.assoc "ip" host) with
              | Some "127.0.0.1" -> Ok ()
              | _ -> Error "Nested value doesn't match"
            )
          | None -> Error "host is not a table"
        )
      | None -> Error "server is not a table"
    )
  | _ -> Error "Parse failed"

(* === SECTION TESTS === *)

let test_simple_section = Test.case "parse simple section" @@ fun _ctx ->
  let input = {|
[package]
name = "test"
version = "1.0.0"
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      match get_table (List.assoc "package" sections) with
      | Some pkg when List.length pkg = 2 -> Ok ()
      | Some pkg -> Error ("Expected 2 keys, got " ^ Int.to_string (List.length pkg))
      | None -> Error "package is not a table"
    )
  | _ -> Error "Parse failed"

let test_multiple_sections = Test.case "parse multiple sections" @@ fun _ctx ->
  let input = {|
[package]
name = "test"

[dependencies]
std = { path = "../std" }
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) when List.length sections = 2 -> Ok ()
  | Ok (Toml.Table sections) -> Error ("Expected 2 sections, got "
  ^ Int.to_string (List.length sections))
  | _ -> Error "Parse failed"

let test_nested_section_names = Test.case "parse nested section names" @@ fun _ctx ->
  let input = {|
[server.config]
port = "8080"
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      match List.assoc_opt "server" sections with
      | None -> Error "Nested section root not found"
      | Some server -> (
          match get_table server with
          | None -> Error "server is not a table"
          | Some server_fields -> (
              match List.assoc_opt "config" server_fields with
              | None -> Error "config table not found"
              | Some config -> (
                  match get_table config with
                  | Some fields -> (
                      match get_string (List.assoc "port" fields) with
                      | Some "8080" -> Ok ()
                      | _ -> Error "Nested port value doesn't match"
                    )
                  | None -> Error "config is not a table"
                )
            )
        )
    )
  | _ -> Error "Parse failed"

(* === ARRAY OF TABLES TESTS === *)

let test_array_of_tables_simple = Test.case "parse simple array of tables" @@ fun _ctx ->
  let input = {|
[[bin]]
name = "tusk"
path = "src/main.ml"

[[bin]]
name = "tusk-build"
path = "src/server.ml"
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      match List.assoc_opt "bin" sections with
      | Some value -> (
          match get_array value with
          | Some bins when List.length bins = 2 -> (
              match bins with
              | [Toml.Table bin1;Toml.Table bin2] -> (
                  match (get_string (List.assoc "name" bin1), get_string (List.assoc "path" bin1)) with
                  | Some "tusk", Some "src/main.ml" -> Ok ()
                  | Some n, Some p -> Error ("First binary values wrong: name=" ^ n ^ " path=" ^ p)
                  | _ -> Error "Missing name or path in first binary"
                )
              | _ -> Error "Expected array of 2 tables, got something else"
            )
          | Some bins ->
              Error ("Expected 2 bins, got " ^ Int.to_string (List.length bins))
          | None ->
              Error "bin value is not an array"
        )
      | None -> Error "bin key not found in parsed TOML"
    )
  | Ok _ ->
      Error "Expected Table at top level"
  | Error err ->
      Error ("Parse failed: " ^ (Toml.error_to_string err))

let test_array_of_tables_empty = Test.case "parse empty array of tables" @@ fun _ctx ->
  let input = {|[[empty]]|} in
  match Toml.parse input with
  | Ok _ -> Ok ()
  | Error _ -> Error "Should parse empty array of tables"

let test_array_of_tables_multiple_keys = Test.case "parse array of tables with multiple keys"
@@ fun _ctx ->
  let input = {|
[[fruits]]
name = "apple"
color = "red"
tasty = true

[[fruits]]
name = "banana"
color = "yellow"
tasty = true
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      match get_array (List.assoc "fruits" sections) with
      | Some [Toml.Table f1;Toml.Table f2] when List.length f1 = 3 && List.length f2 = 3 -> Ok ()
      | Some arr -> Error ("Array structure wrong: " ^ Int.to_string (List.length arr) ^ " items")
      | None -> Error "fruits is not an array"
    )
  | _ -> Error "Parse failed"

let test_array_of_tables_dotted_path = Test.case "parse array of tables with dotted path"
@@ fun _ctx ->
  let input = {|
[[log.handler]]
type = "stdout"
format = "full"

[[log.handler]]
type = "file"
path = "./app.log"
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      (* Should create nested structure: { "log": { "handler": [...] } } *)
      match List.assoc_opt "log" sections with
      | None -> Error "No 'log' key found (bug: dotted paths not nested)"
      | Some log_value -> (
          match get_table log_value with
          | None -> Error "'log' is not a table"
          | Some log_fields -> (
              match List.assoc_opt "handler" log_fields with
              | None -> Error "No 'handler' key in 'log' table"
              | Some handler_value -> (
                  match get_array handler_value with
                  | None -> Error "'handler' is not an array"
                  | Some [Toml.Table h1;Toml.Table h2] when List.length h1 = 2 && List.length h2 = 2 -> Ok ()
                  | Some arr -> Error ("Expected 2 handlers, got " ^ Int.to_string (List.length arr))
                )
            )
        )
    )
  | _ -> Error "Parse failed"

(* === COMMENT TESTS === *)

let test_line_comment = Test.case "parse with line comment" @@ fun _ctx ->
  let input = {|
# This is a comment
name = "test"
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_string (List.assoc "name" items) with
      | Some "test" -> Ok ()
      | _ -> Error "Value doesn't match"
    )
  | _ -> Error "Parse failed"

let test_inline_comment = Test.case "parse with inline comment" @@ fun _ctx ->
  let input = {|name = "test" # inline comment|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_string (List.assoc "name" items) with
      | Some "test" -> Ok ()
      | _ -> Error "Value doesn't match"
    )
  | _ -> Error "Parse failed"

let test_comment_in_section = Test.case "parse section with comment" @@ fun _ctx ->
  let input = {|
[package] # package section
name = "test" # package name
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      match get_table (List.assoc "package" sections) with
      | Some pkg -> (
          match get_string (List.assoc "name" pkg) with
          | Some "test" -> Ok ()
          | _ -> Error "Value doesn't match"
        )
      | None -> Error "package is not a table"
    )
  | _ -> Error "Parse failed"

(* === WHITESPACE TESTS === *)

let test_leading_whitespace = Test.case "parse with leading whitespace" @@ fun _ctx ->
  let input = {|  name = "test"|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_string (List.assoc "name" items) with
      | Some "test" -> Ok ()
      | _ -> Error "Value doesn't match"
    )
  | _ -> Error "Parse failed"

let test_trailing_whitespace = Test.case "parse with trailing whitespace" @@ fun _ctx ->
  let input = {|name = "test"   |} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_string (List.assoc "name" items) with
      | Some "test" -> Ok ()
      | _ -> Error "Value doesn't match"
    )
  | _ -> Error "Parse failed"

let test_whitespace_around_equals = Test.case "parse with whitespace around =" @@ fun _ctx ->
  let input = {|name   =   "test"|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_string (List.assoc "name" items) with
      | Some "test" -> Ok ()
      | _ -> Error "Value doesn't match"
    )
  | _ -> Error "Parse failed"

let test_empty_lines = Test.case "parse with empty lines" @@ fun _ctx ->
  let input = {|

name = "test"


version = "1.0"

|}
  in
  match Toml.parse input with
  | Ok (Toml.Table items) when List.length items = 2 -> Ok ()
  | Ok (Toml.Table items) -> Error ("Expected 2 items, got " ^ Int.to_string (List.length items))
  | _ -> Error "Parse failed"

(* === EDGE CASE TESTS === *)

let test_empty_string = Test.case "parse empty string value" @@ fun _ctx ->
  let input = {|text = ""|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_string (List.assoc "text" items) with
      | Some "" -> Ok ()
      | Some s -> Error ("Expected empty string, got '" ^ s ^ "'")
      | None -> Error "Expected string"
    )
  | _ -> Error "Parse failed"

let test_empty_inline_table = Test.case "parse empty inline table" @@ fun _ctx ->
  let input = {|empty = {}|} in
  match Toml.parse input with
  | Ok (Toml.Table items) -> (
      match get_table (List.assoc "empty" items) with
      | Some [] -> Ok ()
      | Some t -> Error ("Expected empty table, got " ^ Int.to_string (List.length t) ^ " items")
      | None -> Error "Expected table"
    )
  | _ -> Error "Parse failed"

let test_empty_document = Test.case "parse empty document" @@ fun _ctx ->
  let input = "" in
  match Toml.parse input with
  | Ok (Toml.Table []) -> Ok ()
  | Ok (Toml.Table items) -> Error ("Expected empty, got " ^ Int.to_string (List.length items) ^ " items")
  | _ -> Error "Parse failed"

let test_only_comments = Test.case "parse document with only comments" @@ fun _ctx ->
  let input = {|
# Comment 1
# Comment 2
# Comment 3
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table []) -> Ok ()
  | Ok (Toml.Table items) -> Error ("Expected empty, got " ^ Int.to_string (List.length items) ^ " items")
  | _ -> Error "Parse failed"

(* === COMPLEX REAL-WORLD TESTS === *)

let test_real_tusk_toml = Test.case "parse actual tusk.toml structure" @@ fun _ctx ->
  let content = {|[package]
name = "tusk"
version = "0.0.1"

[[bin]]
name = "tusk"
path = "src/main.ml"

[dependencies]
jsonrpc = { path = "../jsonrpc" }
mcp = { path = "../mcp" }
miniriot = { path = "../miniriot" }
std = { path = "../std" }
|}
  in
  match Toml.parse content with
  | Ok (Toml.Table sections) -> (
      match List.assoc_opt "bin" sections with
      | Some (Toml.Array bins) when List.length bins = 1 -> Ok ()
      | Some (Toml.Array bins) -> Error ("Expected 1 bin, got " ^ Int.to_string (List.length bins))
      | Some _ -> Error "bin is not an array"
      | None -> Error "No bin section found in tusk.toml"
    )
  | Ok _ ->
      Error "Expected Table at top level"
  | Error err ->
      Error ("Parse error: " ^ (Toml.error_to_string err))

let test_typical_package_toml = Test.case "parse typical package.toml" @@ fun _ctx ->
  let input = {|
[package]
name = "myapp"
version = "0.1.0"

[[bin]]
name = "myapp"
path = "src/main.ml"

[dependencies]
std = { path = "../std" }
kernel = { path = "../kernel" }
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) when List.length sections = 3 -> Ok ()
  | Ok (Toml.Table sections) -> Error ("Expected 3 sections, got "
  ^ Int.to_string (List.length sections))
  | _ -> Error "Parse failed"

let test_workspace_toml = Test.case "parse workspace.toml" @@ fun _ctx ->
  let input = {|
[workspace]
members = ["packages/a", "packages/b", "packages/c"]

[dependencies]
std = { path = "../riot/packages/std" }
kernel = { path = "../riot/packages/kernel" }
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      match get_table (List.assoc "workspace" sections) with
      | Some ws -> (
          match get_array (List.assoc "members" ws) with
          | Some members when List.length members = 3 -> Ok ()
          | Some members -> Error ("Expected 3 members, got " ^ Int.to_string (List.length members))
          | None -> Error "members is not an array"
        )
      | None -> Error "workspace is not a table"
    )
  | _ -> Error "Parse failed"

let test_mixed_inline_and_section_tables = Test.case "parse mixed inline and section tables"
@@ fun _ctx ->
  let input = {|
inline = { a = "1", b = "2" }

[section]
c = "3"
d = "4"
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table items) when List.length items = 2 -> Ok ()
  | Ok (Toml.Table items) -> Error ("Expected 2 items, got " ^ Int.to_string (List.length items))
  | _ -> Error "Parse failed"

(* === ERROR HANDLING TESTS === *)

let test_unterminated_string = Test.case "detect unterminated string" @@ fun _ctx ->
  let input = {|name = "unterminated|} in
  match Toml.parse input with
  | Error _ -> Ok ()
  | Ok _ -> Error "Should have failed on unterminated string"

let test_unterminated_array = Test.case "detect unterminated array" @@ fun _ctx ->
  let input = {|arr = [1, 2, 3|} in
  match Toml.parse input with
  | Error _ -> Ok ()
  | Ok _ -> Error "Should have failed on unterminated array"

let test_unterminated_inline_table = Test.case "detect unterminated inline table" @@ fun _ctx ->
  let input = {|tbl = { a = "1", b = "2"|} in
  match Toml.parse input with
  | Error _ -> Ok ()
  | Ok _ -> Error "Should have failed on unterminated inline table"

let test_missing_equals = Test.case "detect missing equals" @@ fun _ctx ->
  let input = {|name "value"|} in
  match Toml.parse input with
  | Ok (Toml.Table []) -> Ok ()
  | Ok _ -> Error "Malformed line should be ignored"
  | Error _ -> Error "Parser should currently ignore malformed lines"

let test_duplicate_keys_in_section = Test.case "parse duplicate keys in section (last wins)"
@@ fun _ctx ->
  let input = {|
[config]
port = "8080"
port = "9090"
|}
  in
  match Toml.parse input with
  | Ok (Toml.Table sections) -> (
      match get_table (List.assoc "config" sections) with
      | Some cfg -> (
          match get_string (List.assoc "port" cfg) with
          | Some "9090" -> Ok ()
          | Some p -> Error ("Expected last value '9090', got '" ^ p ^ "'")
          | None -> Error "Missing port"
        )
      | None -> Error "config is not a table"
    )
  | _ -> Error "Parse failed"

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let all_tests = [
        test_simple_string;
        test_quoted_string_with_escapes;
        test_boolean_true;
        test_boolean_false;
        test_integer_positive;
        test_integer_negative;
        test_integer_zero;
        test_integer_in_array;
        test_bare_string;
        test_simple_array;
        test_string_array;
        test_empty_array;
        test_nested_array;
        test_simple_inline_table;
        test_multiple_inline_tables;
        test_inline_table_multiple_keys;
        test_inline_table_with_bool;
        test_nested_inline_tables;
        test_simple_section;
        test_multiple_sections;
        test_nested_section_names;
        test_array_of_tables_simple;
        test_array_of_tables_empty;
        test_array_of_tables_multiple_keys;
        test_array_of_tables_dotted_path;
        test_line_comment;
        test_inline_comment;
        test_comment_in_section;
        test_leading_whitespace;
        test_trailing_whitespace;
        test_whitespace_around_equals;
        test_empty_lines;
        test_empty_string;
        test_empty_inline_table;
        test_empty_document;
        test_only_comments;
        test_real_tusk_toml;
        test_typical_package_toml;
        test_workspace_toml;
        test_mixed_inline_and_section_tables;
        test_unterminated_string;
        test_unterminated_array;
        test_unterminated_inline_table;
        test_missing_equals;
        test_duplicate_keys_in_section;
      ]
      in
      Test.Cli.main ~name:"toml" ~tests:all_tests ~args)
    ~args:Env.args
    ()
