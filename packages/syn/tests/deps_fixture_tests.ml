open Std
open Std.Data
open Syn

let fixture_root = Path.v "packages/syn/tests/deps_fixtures"

let has_expected = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" ->
      `keep
  | _ -> `skip

let render_actual = fun ~fixture_path ->
  let source = Fs.read fixture_path |> Result.expect ~msg:"failed to read deps fixture" in
  let parse_result = Syn.parse ~filename:fixture_path source in
  match Syn.Deps.of_parse_result parse_result with
  | Ok deps -> Json.to_string_pretty (Syn.Deps.to_json deps) ^ "\n"
  | Error (Syn.Deps.Parse_diagnostics diagnostics) ->
      "parse diagnostics:\n" ^ String.concat "\n" (List.map Diagnostic.to_string diagnostics) ^ "\n"
  | Error (Syn.Deps.Cst_builder_error err) ->
      "cst builder error: "
      ^ err.Syn.CstBuilder.message
      ^ " @ "
      ^ Syn.SyntaxKind.to_string err.Syn.CstBuilder.syntax_kind
      ^ "\n"

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun value -> value)
    ~actual:(render_actual ~fixture_path:ctx.fixture_path)

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixture_root
          ~filter:has_expected
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"syn-deps-fixtures" ~tests ~args)
    ~args:Env.args
    ()
