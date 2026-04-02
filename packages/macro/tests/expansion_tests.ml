open Std
open Macro

let sample_file = Path.v "sample.ml"

let assert_expansion = fun ~source ~expected ->
  match expand_source ~filename:sample_file source with
  | Error err -> Error ("expected expansion to succeed: " ^ error_message err)
  | Ok result ->
      Test.assert_true result.changed;
      Test.assert_equal ~expected ~actual:result.source;
      Ok ()

let assert_error_contains = fun ~source ~expected_substring ->
  match expand_source ~filename:sample_file source with
  | Ok _ -> Error "expected expansion to fail"
  | Error err ->
      if String.contains err.message expected_substring then
        Ok ()
      else
        Error ("expected error containing '" ^ expected_substring ^ "', got '" ^ err.message ^ "'")

let tests = [
  Test.case "format! lowers a bare {} placeholder to Stdlib.Printf.sprintf"
    (fun () ->
      assert_expansion
        ~source:"let msg = format! \"hello {}\" name\n"
        ~expected:"let msg = (Stdlib.Printf.sprintf \"hello %s\" (name))\n");
  Test.case "format! preserves escaped braces while escaping percent signs for Stdlib.Printf"
    (fun () ->
      assert_expansion
        ~source:"let msg = format! \"{{}} 100%\"\n"
        ~expected:"let msg = (Stdlib.Printf.sprintf \"{} 100%%\")\n");
  Test.case "format! expands recursively when another macro invocation appears in an argument"
    (fun () ->
      assert_expansion
        ~source:"let msg = format! \"{}!\" (format! \"hello {}\" name)\n"
        ~expected:
          "let msg = (Stdlib.Printf.sprintf \"%s!\" ((Stdlib.Printf.sprintf \"hello %s\" (name))))\n");
  Test.case "format! rejects non-literal format strings for the first prototype"
    (fun () ->
      assert_error_contains
        ~source:"let msg = format! template name\n"
        ~expected_substring:"string literal");
  Test.case "format! rejects unsupported placeholder forms for the first prototype"
    (fun () ->
      assert_error_contains
        ~source:"let msg = format! \"{:?}\" name\n"
        ~expected_substring:"bare {} placeholders");
  Test.case "format! still accepts a parenthesized body while parsing the new macro form"
    (fun () ->
      assert_expansion
        ~source:"let msg = format!(\"hello {}\", name)\n"
        ~expected:"let msg = (Stdlib.Printf.sprintf \"hello %s\" (name))\n");
  Test.case "files without macro syntax are left unchanged"
    (fun () ->
      match expand_source ~filename:sample_file "let msg = format ! name\n" with
      | Error err -> Error ("expected unchanged source, got error: " ^ error_message err)
      | Ok result ->
          Test.assert_false result.changed;
          Test.assert_equal
            ~expected:"let msg = format ! name\n"
            ~actual:result.source;
          Ok ());
]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"macro:expansion" ~tests ~args) ~args:Env.args ()
