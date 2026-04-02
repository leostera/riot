open Std
open Std.Data
open Std.Result
open Std.Collections

let test_parse_atom = fun _ctx ->
  match Sexp.of_string "hello" with
  | Ok (Sexp.Atom "hello") -> Ok ()
  | _ -> Error "Failed to parse atom"

let test_parse_atom_with_numbers = fun _ctx ->
  match Sexp.of_string "abc123" with
  | Ok (Sexp.Atom "abc123") -> Ok ()
  | _ -> Error "Failed to parse atom with numbers"

let test_parse_empty_list = fun _ctx ->
  match Sexp.of_string "()" with
  | Ok (Sexp.List []) -> Ok ()
  | _ -> Error "Failed to parse empty list"

let test_parse_list_with_atoms = fun _ctx ->
  match Sexp.of_string "(hello world)" with
  | Ok (Sexp.List [Sexp.Atom "hello";Sexp.Atom "world"]) -> Ok ()
  | _ -> Error "Failed to parse list with atoms"

let test_parse_nested_list = fun _ctx ->
  match Sexp.of_string "((inner))" with
  | Ok (Sexp.List [ Sexp.List [ Sexp.Atom "inner" ] ]) -> Ok ()
  | _ -> Error "Failed to parse nested list"

let test_parse_complex_nested = fun _ctx ->
  match Sexp.of_string "(outer (inner value))" with
  | Ok (Sexp.List [Sexp.Atom "outer";Sexp.List _]) -> Ok ()
  | _ -> Error "Failed to parse complex nested list"

let test_parse_multiple_sexps = fun _ctx ->
  match Sexp.parse_many "(first) (second) (third)" with
  | Ok sexps when List.length sexps = 3 -> Ok ()
  | _ -> Error "Failed to parse multiple s-expressions"

let test_parse_whitespace = fun _ctx ->
  match Sexp.of_string "  ( hello   world  )  " with
  | Ok (Sexp.List [Sexp.Atom "hello";Sexp.Atom "world"]) -> Ok ()
  | _ -> Error "Failed to parse with whitespace"

let test_create_atom = fun _ctx ->
  match Sexp.atom "test" with
  | Sexp.Atom "test" -> Ok ()
  | _ -> Error "Failed to create atom"

let test_create_list = fun _ctx ->
  match Sexp.list [ Sexp.atom "a"; Sexp.atom "b" ] with
  | Sexp.List [Sexp.Atom "a";Sexp.Atom "b"] -> Ok ()
  | _ -> Error "Failed to create list"

let test_to_string_atom = fun _ctx ->
  let sexp = Sexp.atom "hello" in
  if Sexp.to_string sexp = "hello" then
    Ok ()
  else
    Error "Failed to serialize atom"

let test_to_string_list = fun _ctx ->
  let sexp = Sexp.list [ Sexp.atom "a"; Sexp.atom "b" ] in
  if Sexp.to_string sexp = "(a b)" then
    Ok ()
  else
    Error "Failed to serialize list"

let test_to_string_nested = fun _ctx ->
  let sexp = Sexp.list [ Sexp.atom "outer"; Sexp.list [ Sexp.atom "inner" ] ] in
  if Sexp.to_string sexp = "(outer (inner))" then
    Ok ()
  else
    Error "Failed to serialize nested list"

let test_roundtrip = fun _ctx ->
  let original = Sexp.list [ Sexp.atom "test"; Sexp.list [ Sexp.atom "nested" ] ] in
  let serialized = Sexp.to_string original in
  match Sexp.of_string serialized with
  | Ok parsed when parsed = original -> Ok ()
  | _ -> Error "Roundtrip failed"

let test_is_atom = fun _ctx ->
  if Sexp.is_atom (Sexp.atom "test") && not (Sexp.is_atom (Sexp.list [])) then
    Ok ()
  else
    Error "is_atom check failed"

let test_is_list = fun _ctx ->
  if Sexp.is_list (Sexp.list []) && not (Sexp.is_list (Sexp.atom "test")) then
    Ok ()
  else
    Error "is_list check failed"

let test_to_atom = fun _ctx ->
  match Sexp.to_atom (Sexp.atom "hello") with
  | Some "hello" -> Ok ()
  | _ -> Error "to_atom failed"

let test_to_list = fun _ctx ->
  let sexp = Sexp.list [ Sexp.atom "a"; Sexp.atom "b" ] in
  match Sexp.to_list sexp with
  | Some [Sexp.Atom "a";Sexp.Atom "b"] -> Ok ()
  | _ -> Error "to_list failed"

let test_assoc = fun _ctx ->
  let pairs = [
    Sexp.list [ Sexp.atom "host"; Sexp.atom "localhost" ];
    Sexp.list [ Sexp.atom "port"; Sexp.atom "8080" ];
  ] in
  match Sexp.assoc "port" pairs with
  | Some (Sexp.Atom "8080") -> Ok ()
  | _ -> Error "assoc failed"

let test_csexp_atom = fun _ctx ->
  let sexp = Sexp.atom "hello" in
  if Sexp.Csexp.to_string sexp = "5:hello" then
    Ok ()
  else
    Error "Csexp atom serialization failed"

let test_csexp_list = fun _ctx ->
  let sexp = Sexp.list [ Sexp.atom "a"; Sexp.atom "b" ] in
  if Sexp.Csexp.to_string sexp = "(1:a1:b)" then
    Ok ()
  else
    Error "Csexp list serialization failed"

let test_csexp_parse_atom = fun _ctx ->
  match Sexp.Csexp.of_string "5:hello" with
  | Ok (Sexp.Atom "hello") -> Ok ()
  | _ -> Error "Csexp atom parsing failed"

let test_csexp_roundtrip = fun _ctx ->
  let original = Sexp.list [ Sexp.atom "test"; Sexp.atom "data" ] in
  let serialized = Sexp.Csexp.to_string original in
  match Sexp.Csexp.of_string serialized with
  | Ok parsed when parsed = original -> Ok ()
  | _ -> Error "Csexp roundtrip failed"

let tests =
  Test.[
    case "parse atom" test_parse_atom;
    case "parse atom with numbers" test_parse_atom_with_numbers;
    case "parse empty list" test_parse_empty_list;
    case "parse list with atoms" test_parse_list_with_atoms;
    case "parse nested list" test_parse_nested_list;
    case "parse complex nested list" test_parse_complex_nested;
    case "parse multiple s-expressions" test_parse_multiple_sexps;
    case "parse with whitespace" test_parse_whitespace;
    case "create atom" test_create_atom;
    case "create list" test_create_list;
    case "serialize atom" test_to_string_atom;
    case "serialize list" test_to_string_list;
    case "serialize nested list" test_to_string_nested;
    case "roundtrip" test_roundtrip;
    case "is_atom check" test_is_atom;
    case "is_list check" test_is_list;
    case "to_atom extractor" test_to_atom;
    case "to_list extractor" test_to_list;
    case "assoc lookup" test_assoc;
    case "csexp serialize atom" test_csexp_atom;
    case "csexp serialize list" test_csexp_list;
    case "csexp parse atom" test_csexp_parse_atom;
    case "csexp roundtrip" test_csexp_roundtrip;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"sexp" ~tests ~args) ~args:Env.args ()
