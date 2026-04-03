open Std
open Std.Data
open Typ

let diagnostics_dir = Path.v "packages/typ/tests/diagnostics"

let diagnostic_marker_path = fun path ->
  match Path.extension path with
  | Some ext -> Path.add_extension path ~ext:(ext ^ ".diagnostic")
  | None -> Path.add_extension path ~ext:"diagnostic"

let filter_diagnostic_fixture = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" ->
      let marker_path = diagnostic_marker_path path in
      let exists = Fs.exists marker_path |> Result.unwrap_or ~default:false in
      if exists then
        `keep
      else
        `skip
  | _ ->
      `skip

let canonicalize_json =
  let rec loop = function
    | Json.Object fields ->
        Json.Object (
          fields
          |> List.map (fun (key, value) -> (key, loop value))
          |> List.sort (fun (left, _) (right, _) -> String.compare left right)
        )
    | Json.Array items ->
        Json.Array (List.map loop items)
    | other ->
        other
  in
  loop

let diagnostics_to_json = fun (report: Check_result.t) ->
  Json.Object [
    ("parse_diagnostics", Json.Array (List.map Syn.Diagnostic.to_json report.parse_diagnostics));
    ("lowering_diagnostics", Json.Array (List.map Diagnostic.to_json report.lowering_diagnostics));
    ("typing_diagnostics", Json.Array (List.map Diagnostic.to_json report.typing_diagnostics));
  ]
  |> canonicalize_json

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source = Fs.read ctx.fixture_path |> Result.expect ~msg:"fixture should exist" in
  let report = Check.check_source ~filename:ctx.fixture_path source in
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun json -> Json.to_string_pretty json ^ "\n")
    ~actual:(diagnostics_to_json report)

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:diagnostics_dir
          ~filter:filter_diagnostic_fixture
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"typ:diagnostics" ~tests ~args)
    ~args:Env.args
    ()
