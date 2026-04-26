open Std
open Std.Collections
open Std.Result.Syntax

let name = "typ:infer"

let fixtures_dir = Path.v "packages/typ/tests/fixtures/corpus"

let infer_snapshot_path = fun path -> Path.add_extension path ~ext:"infer.expected"

let has_infer_snapshot = fun path ->
  Fs.exists (infer_snapshot_path path)
  |> Result.unwrap_or ~default:false

let fixture_filter = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" when has_infer_snapshot path -> `keep
  | _ -> `skip

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create infer test source slice"

let render_diagnostic = function
  | Typ.Diagnostics.Diagnostic.UnsupportedSyntax { summary; _ } -> "unsupported syntax: " ^ summary
  | Typ.Diagnostics.Diagnostic.UnsupportedType { summary; _ } -> "unsupported type: " ^ summary
  | diagnostic -> Typ.Diagnostics.Diagnostic.to_string diagnostic

let render_diagnostics = fun diagnostics ->
  diagnostics
  |> List.map ~fn:render_diagnostic
  |> String.concat "\n"

let render_infer_diagnostics = fun (diagnostics: Typ.Diagnostics.t) ->
  diagnostics.items
  |> Vector.iter
  |> Iter.Iterator.to_list
  |> render_diagnostics

let render_interface = fun (intf: Typ.Infer.ModuleInterface.t) ->
  intf
  |> Typ.Infer.ModuleInterface.values
  |> Typ.SignatureGenerator.from_values

let render_result = fun (result: Typ.Infer.infer_result) ->
  let diagnostics = render_infer_diagnostics result.diagnostics in
  if String.equal diagnostics "" then
    render_interface result.intf
  else
    diagnostics

let validate_interface_source = fun source ->
  let parse_result =
    source
    |> source_slice
    |> Syn.parse_interface
  in
  if Vector.is_empty parse_result.diagnostics then
    Ok source
  else
    Error "generated interface did not parse cleanly"

let infer_test = fun (ctx: Test.FixtureRunner.ctx) ->
  let* file =
    Fs.read ctx.fixture_path
    |> Result.map_err ~fn:IO.error_message
  in
  let parse_result = Syn.parse ~filename:ctx.fixture_path (source_slice file) in
  let source = Typ.Model.Source.make ~text:file in
  let* ast =
    Typ.Ast.from_parse_result ~source parse_result
    |> Result.map_err ~fn:render_diagnostics
  in
  let actual =
    Typ.Infer.check ast
    |> render_result
  in
  let* actual = validate_interface_source actual in
  Test.Snapshot.assert_text ~ctx:ctx.test ~actual

let tests =
  Test.FixtureRunner.cases
    ()
    ~dir:fixtures_dir
    ~filter:fixture_filter
    ~snapshot_path:(fun path -> Some (infer_snapshot_path path))
    ~run:infer_test

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
