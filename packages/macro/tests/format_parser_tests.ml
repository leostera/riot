open Std
open Macro

let span_of_literal = fun literal_text ->
  Syn.Ceibo.Span.make ~start:0 ~end_:(String.length literal_text)

let render_item = function
  | Format_parser.String text -> "  String " ^ "\"" ^ text ^ "\""
  | Format_parser.Hole Format_parser.Next_arg_to_string ->
      "  NextArgToString"
  | Format_parser.Hole (Format_parser.Var_to_string name) ->
      "  VarToString " ^ "\"" ^ name ^ "\""

let render_format = fun format_items ->
  match format_items with
  | [] -> "Format []"
  | items ->
      "Format [\n"
      ^ String.concat "\n" (List.map render_item items)
      ^ "\n]"

let assert_parse_snapshot = fun ~ctx ~literal_text ~expected ->
  match Format_parser.parse_literal ~literal_text ~span:(span_of_literal literal_text) with
  | Error err -> Error ("expected format parser to succeed: " ^ Error.message err)
  | Ok format_items ->
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual:(render_format format_items)
        ~expected

let assert_parse_error = fun ~literal_text ~expected_substring ->
  match Format_parser.parse_literal ~literal_text ~span:(span_of_literal literal_text) with
  | Ok _ -> Error "expected format parser to fail"
  | Error err ->
      if String.contains (Error.message err) expected_substring then
        Ok ()
      else
        Error
          ("expected error containing '" ^ expected_substring ^ "', got '" ^ Error.message err ^ "'")

let tests = [
  Test.case "format parser captures positional placeholders as explicit nodes"
    (fun ctx ->
      assert_parse_snapshot
        ~ctx
        ~literal_text:"\"hello {}!\""
        ~expected:
          "Format [\n  String \"hello \"\n  NextArgToString\n  String \"!\"\n]");
  Test.case "format parser captures named placeholders without consuming arguments"
    (fun ctx ->
      assert_parse_snapshot
        ~ctx
        ~literal_text:"\"is this good? {x}\""
        ~expected:
          "Format [\n  String \"is this good? \"\n  VarToString \"x\"\n]");
  Test.case "format parser keeps escaped braces inside literal segments"
    (fun ctx ->
      assert_parse_snapshot
        ~ctx
        ~literal_text:"\"{{}} 100%\""
        ~expected:
          "Format [\n  String \"{} 100%\"\n]");
  Test.case "format parser accepts dotted capture paths for future formatter dispatch"
    (fun ctx ->
      assert_parse_snapshot
        ~ctx
        ~literal_text:"\"hello {User.name}\""
        ~expected:
          "Format [\n  String \"hello \"\n  VarToString \"User.name\"\n]");
  Test.case "format parser rejects unsupported formatter specs for the first prototype"
    (fun _ctx ->
      assert_parse_error
        ~literal_text:"\"{x:?}\""
        ~expected_substring:"{} and {name} placeholders");
  Test.case "format parser rejects unmatched opening braces"
    (fun _ctx ->
      assert_parse_error
        ~literal_text:"\"hello {name\""
        ~expected_substring:"unmatched '{'");
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"macro:format_parser" ~tests ~args) ~args:Env.args ()
