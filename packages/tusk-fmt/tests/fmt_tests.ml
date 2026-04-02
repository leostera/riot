open Std
module Test = Std.Test

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let parse_fmt = fun args ->
  match ArgParser.get_matches Tusk_fmt.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_fmt_accepts_multiple_paths = fun _ctx ->
  match parse_fmt [ "fmt"; "packages/blink/src/connection.ml"; "packages/syn/src/parser.ml" ] with
  | Error err -> Error ("expected fmt args to parse: " ^ err)
  | Ok matches ->
      let actual = ArgParser.get_many matches "path" in
      Test.assert_equal
        ~expected:[ "packages/blink/src/connection.ml"; "packages/syn/src/parser.ml" ]
        ~actual;
      Ok ()

let test_fmt_usage_shows_variadic_paths = fun _ctx ->
  let usage = ArgParser.usage_string Tusk_fmt.command in
  if String.contains usage "path..." then
    Ok ()
  else
    Error ("expected variadic path usage, got: " ^ usage)

let test_fmt_formats_only_explicit_file = fun _ctx ->
  with_tempdir "tusk_fmt_explicit_file"
    (fun tmpdir ->
      let needs = Path.(tmpdir / Path.v "needs.ml") in
      let untouched = Path.(tmpdir / Path.v "untouched.ml") in
      Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs |> Result.expect ~msg:"write needs";
      Fs.write "let y = 3 + 4\nlet g y = y + 1\n" untouched |> Result.expect ~msg:"write untouched";
      let matches = parse_fmt [ "fmt"; Path.to_string needs ] |> Result.expect ~msg:"parse fmt args" in
      Tusk_fmt.run matches |> Result.expect ~msg:"format explicit target";
      let formatted = Fs.read needs |> Result.expect ~msg:"read formatted file" in
      let untouched_source = Fs.read untouched |> Result.expect ~msg:"read untouched file" in
      Test.assert_equal ~expected:"let x = 1 + 2\n\nlet f x = x + 1\n" ~actual:formatted;
      Test.assert_equal ~expected:"let y = 3 + 4\nlet g y = y + 1\n" ~actual:untouched_source;
      Ok ())

let tests =
  Test.[
    case "fmt: accept multiple path arguments" test_fmt_accepts_multiple_paths;
    case "fmt: usage shows variadic paths" test_fmt_usage_shows_variadic_paths;
    case "fmt: format rewrites only the explicit file" test_fmt_formats_only_explicit_file;
  ]

let name = "Tusk Fmt Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
