open Std
open Macro

let with_temp_workspace_result = fun ~prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let write_file = fun path contents ->
  Fs.write contents path |> Std.Result.expect ~msg:(("write failed: " ^ Path.to_string path))

let write_macro_workspace = fun ~tmpdir ~helper_source ~provider_source ->
  let workspace_toml = Path.(tmpdir / Path.v "riot.toml") in
  let helper_root = Path.(tmpdir / Path.v "packages" / Path.v "helper") in
  let macro_root = Path.(tmpdir / Path.v "packages" / Path.v "macro-demo") in
  let helper_src_dir = Path.(helper_root / Path.v "src") in
  let macro_src_dir = Path.(macro_root / Path.v "src") in
  let _ = Fs.create_dir_all helper_src_dir |> Std.Result.expect ~msg:"create helper src dir failed" in
  let _ = Fs.create_dir_all macro_src_dir |> Std.Result.expect ~msg:"create macro src dir failed" in
  let _ = write_file
    workspace_toml
    {|
[workspace]
members = [
  "packages/helper",
  "packages/macro-demo",
]
|} in
  let _ = write_file
    Path.(helper_root / Path.v "riot.toml")
    {|
[package]
name = "helper"
version = "0.0.1"

[lib]
path = "src/helper.ml"
|} in
  let _ = write_file
    Path.(macro_root / Path.v "riot.toml")
    {|
[package]
name = "macro-demo"
version = "0.0.1"

[lib]
path = "src/macro.ml"

[riot.macro.provider]
path = "src/macro.ml"
module_path = "MacroDemo"
macros = ["demo"]

[build-dependencies]
helper = { path = "../helper", version = "*" }
|} in
  let _ = write_file Path.(helper_src_dir / Path.v "helper.ml") helper_source in
  let _ = write_file Path.(macro_src_dir / Path.v "macro.ml") provider_source in
  [
    Riot_model.Macro_provider.make
      ~package_name:"macro-demo"
      ~package_path:macro_root
      ~source_path:(Path.(macro_root / Path.v "src" / Path.v "macro.ml"))
      ~module_path:[ "MacroDemo" ]
      ~macros:[ "demo" ]
      ();
  ]

let test_provider_hash_tracks_dependency_closure_sources = Test.case
  "runner hash tracks dependency closure source changes"
  (fun _ctx ->
    with_temp_workspace_result
      ~prefix:"macro_runner_hash"
      (fun tmpdir ->
        let providers =
          write_macro_workspace
            ~tmpdir
            ~helper_source:"let version = \"one\"\n"
            ~provider_source:"let provider () = Macro.Provider.v ~module_path:[ \"MacroDemo\" ] [ Macro.Provider.fn \"demo\" (fun tokens -> { Macro.Result.output = tokens; diagnostics = [] }) ]\n" in
        let first_hash = Runner.providers_hash ~workspace_root:tmpdir providers in
        let _ = Fs.write
          "let version = \"two\"\n"
          Path.(tmpdir / Path.v "packages" / Path.v "helper" / Path.v "src" / Path.v "helper.ml")
        |> Std.Result.expect ~msg:"rewrite helper source failed" in
        let second_hash = Runner.providers_hash ~workspace_root:tmpdir providers in
        if String.equal first_hash second_hash then
          Error "expected provider hash to change when dependency closure sources change"
        else
          Ok ()))

let test_runner_materialize_reuses_existing_workspace = Test.case
  "runner materialize reuses existing workspace for identical inputs"
  (fun _ctx ->
    with_temp_workspace_result
      ~prefix:"macro_runner_reuse"
      (fun tmpdir ->
        let providers =
          write_macro_workspace
            ~tmpdir
            ~helper_source:"let version = \"one\"\n"
            ~provider_source:"let provider () = Macro.Provider.v ~module_path:[ \"MacroDemo\" ] [ Macro.Provider.fn \"demo\" (fun tokens -> { Macro.Result.output = tokens; diagnostics = [] }) ]\n" in
        let target_dir_root = Path.(tmpdir / Path.v "target") in
        let plan = Runner.materialize ~workspace_root:tmpdir ~target_dir_root providers in
        let sentinel = Path.(Runner.workspace_root plan / Path.v "sentinel.txt") in
        let _ = Fs.write "keep me\n" sentinel |> Std.Result.expect ~msg:"write sentinel failed" in
        let _ = Runner.materialize ~workspace_root:tmpdir ~target_dir_root providers in
        match Fs.read sentinel with
        | Ok "keep me\n" -> Ok ()
        | Ok _ -> Error "expected materialize reuse to preserve the existing generated workspace"
        | Error err -> Error ("expected sentinel to survive workspace reuse: " ^ IO.error_message err)))

let tests = [
  test_provider_hash_tracks_dependency_closure_sources;
  test_runner_materialize_reuses_existing_workspace;
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"macro:runner" ~tests ~args) ~args:Env.args ()
