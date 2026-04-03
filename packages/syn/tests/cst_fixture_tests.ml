open Std
open Std.Data
open Syn

let fixture_root = Path.v "packages/syn/tests/fixtures"

let cst_snapshot_path = fun path ->
  Path.join (Path.dirname path) (Path.v (Path.basename path ^ ".expected_cst.json"))

let has_cst_snapshot = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" ->
      let snapshot_path = cst_snapshot_path path in
      let exists = Fs.exists snapshot_path |> Result.unwrap_or ~default:false in
      if exists then
        `keep
      else
        `skip
  | _ -> `skip

let cst_result_json = fun ~fixture_path ~source parse_result ->
  if parse_result.Parser.diagnostics != [] then
    Json.Object [
      ("status", Json.String "parse_error");
      ("diagnostics", Json.Array (List.map Diagnostic.to_json parse_result.Parser.diagnostics))
    ]
  else
    let kind =
      if Path.extension fixture_path = Some ".mli" then
        `Interface
      else
        `Implementation
    in
    Syn.CstBuilder.create_from_ceibo
      ~kind
      ~source
      ~tokens:parse_result.Parser.tokens
      parse_result.Parser.tree
    |> Syn.CstJson.of_result

let test_cst_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source = Fs.read ctx.fixture_path |> Result.expect ~msg:"Failed to read CST fixture" in
  let parse_result = Syn.parse ~filename:ctx.fixture_path source in
  let actual_json = cst_result_json ~fixture_path:ctx.fixture_path ~source parse_result in
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun json -> Json.to_string_pretty json ^ "\n")
    ~actual:actual_json

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixture_root
          ~filter:has_cst_snapshot
          ~snapshot_path:(fun path -> Some (cst_snapshot_path path))
          ~run:(fun ctx -> test_cst_fixture ~ctx)
      in
      Test.Cli.main ~name:"syn-cst-fixtures" ~tests ~args)
    ~args:Env.args
    ()
