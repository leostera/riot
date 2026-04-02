open Std
open Macro

let sample_file = Path.v "sample.ml"

let normalize_generated_names = fun source ->
  let prefix = "__riot_macro_format_buffer_" in
  let prefix_len = String.length prefix in
  let source_len = String.length source in
  let buffer = IO.Buffer.create source_len in
  let rec has_prefix_at index offset =
    if offset = prefix_len then
      true
    else if index + offset >= source_len then
      false
    else if source.[index + offset] = prefix.[offset] then
      has_prefix_at index (offset + 1)
    else
      false
  in
  let rec consume_suffix index =
    if index >= source_len then
      index
    else
      match source.[index] with
      | '0' .. '9'
      | '_' -> consume_suffix (index + 1)
      | _ -> index
  in
  let rec loop index =
    if index >= source_len then
      IO.Buffer.contents buffer
    else if has_prefix_at index 0 then
      let next_index = consume_suffix (index + prefix_len) in
      let () = IO.Buffer.add_string buffer "__riot_macro_format_buffer" in
      loop next_index
    else (
      IO.Buffer.add_char buffer source.[index];
      loop (index + 1)
    )
  in
  loop 0

let assert_expansion = fun ~source ~expected ->
  match expand_source ~filename:sample_file source with
  | Error err -> Error ("expected expansion to succeed: " ^ error_message err)
  | Ok result ->
      Test.assert_true result.changed;
      Test.assert_equal ~expected ~actual:(normalize_generated_names result.source);
      Ok ()

let assert_error_contains = fun ~source ~expected_substring ->
  match expand_source ~filename:sample_file source with
  | Ok _ -> Error "expected expansion to fail"
  | Error err ->
      if String.contains err.message expected_substring then
        Ok ()
      else
        Error ("expected error containing '" ^ expected_substring ^ "', got '" ^ err.message ^ "'")

let assert_validator_error = fun ~source ~expected_substring ->
  match Validator.validate_source ~filename:sample_file source with
  | Ok () -> Error "expected validation to fail"
  | Error err ->
      if String.contains err.message expected_substring then
        Ok ()
      else
        Error ("expected error containing '" ^ expected_substring ^ "', got '" ^ err.message ^ "'")

let assert_expansion_reparses = fun ~source ->
  match expand_source ~filename:sample_file source with
  | Error err -> Error ("expected expansion to succeed: " ^ error_message err)
  | Ok result ->
      let reparsed = Syn.parse ~filename:sample_file result.source in
      if reparsed.diagnostics = [] then
        Ok ()
      else
        Error
          ("expected reparsed expansion to be clean, got: "
          ^ Syn.Diagnostic.main_message (List.hd reparsed.diagnostics))

let tests = [
  Test.case "Macro.format! lowers a bare {} placeholder to a buffer builder"
    (fun _ctx ->
      assert_expansion
        ~source:"let msg = Macro.format! \"hello {}\" name\n"
        ~expected:
          "let msg = (let __riot_macro_format_buffer = Stdlib.Buffer.create 10 in Stdlib.Buffer.add_string __riot_macro_format_buffer \"hello \"; Stdlib.Buffer.add_string __riot_macro_format_buffer (name); Stdlib.Buffer.contents __riot_macro_format_buffer)\n");
  Test.case "format! lowers named captures without consuming explicit arguments"
    (fun _ctx ->
      assert_expansion
        ~source:"let msg = Macro.format! \"hello {name}\"\n"
        ~expected:
          "let msg = (let __riot_macro_format_buffer = Stdlib.Buffer.create 14 in Stdlib.Buffer.add_string __riot_macro_format_buffer \"hello \"; Stdlib.Buffer.add_string __riot_macro_format_buffer (name); Stdlib.Buffer.contents __riot_macro_format_buffer)\n");
  Test.case "format! preserves escaped braces in literal segments"
    (fun _ctx ->
      assert_expansion
        ~source:"let msg = Macro.format! \"{{}} 100%\"\n"
        ~expected:
          "let msg = (let __riot_macro_format_buffer = Stdlib.Buffer.create 11 in Stdlib.Buffer.add_string __riot_macro_format_buffer \"{} 100%\"; Stdlib.Buffer.contents __riot_macro_format_buffer)\n");
  Test.case "format! expands recursively when another macro invocation appears in an argument"
    (fun _ctx ->
      assert_expansion
        ~source:"let msg = Macro.format! \"{}!\" (Macro.format! \"hello {}\" name)\n"
        ~expected:
          "let msg = (let __riot_macro_format_buffer = Stdlib.Buffer.create 5 in Stdlib.Buffer.add_string __riot_macro_format_buffer ((let __riot_macro_format_buffer = Stdlib.Buffer.create 10 in Stdlib.Buffer.add_string __riot_macro_format_buffer \"hello \"; Stdlib.Buffer.add_string __riot_macro_format_buffer (name); Stdlib.Buffer.contents __riot_macro_format_buffer)); Stdlib.Buffer.add_string __riot_macro_format_buffer \"!\"; Stdlib.Buffer.contents __riot_macro_format_buffer)\n");
  Test.case "successful expansions stay parse-clean after rewriting"
    (fun _ctx ->
      assert_expansion_reparses
        ~source:"let msg = Macro.format! \"hello {}\" (if ready then name else fallback)\n");
  Test.case "format! rejects non-literal format strings for the first prototype"
    (fun _ctx ->
      assert_error_contains
        ~source:"let msg = Macro.format! template name\n"
        ~expected_substring:"string literal");
  Test.case "format! rejects unsupported placeholder forms for the first prototype"
    (fun _ctx ->
      assert_error_contains
        ~source:"let msg = Macro.format! \"{:?}\" name\n"
        ~expected_substring:"{} and {name} placeholders");
  Test.case "format! still accepts a parenthesized body while parsing the new macro form"
    (fun _ctx ->
      assert_expansion
        ~source:"let msg = Macro.format!(\"hello {}\", name)\n"
        ~expected:
          "let msg = (let __riot_macro_format_buffer = Stdlib.Buffer.create 10 in Stdlib.Buffer.add_string __riot_macro_format_buffer \"hello \"; Stdlib.Buffer.add_string __riot_macro_format_buffer (name); Stdlib.Buffer.contents __riot_macro_format_buffer)\n");
  Test.case "bare format! still expands when the provider name is unambiguous"
    (fun _ctx ->
      assert_expansion
        ~source:"let msg = format! \"hello {}\" name\n"
        ~expected:
          "let msg = (let __riot_macro_format_buffer = Stdlib.Buffer.create 10 in Stdlib.Buffer.add_string __riot_macro_format_buffer \"hello \"; Stdlib.Buffer.add_string __riot_macro_format_buffer (name); Stdlib.Buffer.contents __riot_macro_format_buffer)\n");
  Test.case "files without macro syntax are left unchanged"
    (fun _ctx ->
      match expand_source ~filename:sample_file "let msg = format ! name\n" with
      | Error err -> Error ("expected unchanged source, got error: " ^ error_message err)
      | Ok result ->
          Test.assert_false result.changed;
          Test.assert_equal
            ~expected:"let msg = format ! name\n"
            ~actual:result.source;
          Ok ());
  Test.case "validator rejects invalid rewritten OCaml before compile"
    (fun _ctx ->
      assert_validator_error
        ~source:"let msg =\n"
        ~expected_substring:"macro expansion produced invalid OCaml");
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"macro:expansion" ~tests ~args) ~args:Env.args ()
