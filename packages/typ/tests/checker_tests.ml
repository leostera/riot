open Std

let ( let* ) = fun result fn -> Result.and_then result ~fn

let name = "typ:checker"

let fixtures_dir = Path.v "packages/typ/tests/fixtures/corpus"

let fixture_filter = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> Test.FixtureRunner.Keep
  | _ -> Test.FixtureRunner.Skip

let source_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create checker test source slice"

let checker_test = fun (ctx: Test.FixtureRunner.ctx) ->
  let* file = Fs.read ctx.fixture_path |> Result.map_err ~fn:IO.error_message in
  let parse_result = Syn.parse ~filename:ctx.fixture_path (source_slice file) in
  let source = Typ.Model.Source.make ~text:file in
  let typings = Typ.Check.check ~source parse_result in
  let* json_text = Serde_json.to_string Typ.Check.Typings.serializer typings
  |> Result.map_err ~fn:Serde.Error.to_string in
  let* json = Data.Json.of_string json_text |> Result.map_err ~fn:Data.Json.error_to_string in
  Test.Snapshot.assert_json ~ctx:ctx.test ~actual:json

let tests =
  Test.FixtureRunner.cases
    ()
    ~dir:fixtures_dir
    ~filter:fixture_filter
    ~snapshot_path:(fun path -> Some (Path.add_extension path ~ext:"checker.expected"))
    ~run:checker_test

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
