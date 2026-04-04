open Std
open Typ

let fixtures_dir = Path.v "packages/typ/tests/fixtures"

let append_snapshot_suffix = fun path suffix ->
  Path.to_string path ^ suffix
  |> Path.of_string
  |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let approved_snapshot_path = fun path ->
  append_snapshot_suffix path ".expected"

let fixture_filter = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" ->
      `keep
  | _ ->
      `skip

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source = Fs.read ctx.fixture_path |> Result.expect ~msg:"fixture should exist" in
  let report = Check.check_source ~filename:ctx.fixture_path source in
  Test.Snapshot.assert_json ~ctx:ctx.test ~actual:(Report.to_json report)

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:fixture_filter
          ~snapshot_path:(fun path -> Some (approved_snapshot_path path))
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"typ:prototype_fixtures" ~tests ~args)
    ~args:Env.args
    ()
