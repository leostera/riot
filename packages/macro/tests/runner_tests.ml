open Std
open Macro

let with_temp_workspace_result = fun ~prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let workspace_dependency = fun name ->
  Riot_model.Package.{
    name;
    source = {
      workspace = true;
      builtin = false;
      path = None;
      source_locator = None;
      ref_ = None;
      version = None;
    };
  }

let write_macro_workspace = fun ~tmpdir ~helper_source ~provider_source ->
  let helper_root = Path.(tmpdir / Path.v "packages" / Path.v "helper") in
  let macro_root = Path.(tmpdir / Path.v "packages" / Path.v "macro-demo") in
  let helper_src_dir = Path.(helper_root / Path.v "src") in
  let macro_src_dir = Path.(macro_root / Path.v "src") in
  let _ = Fs.create_dir_all helper_src_dir |> Result.expect ~msg:"create helper src dir failed" in
  let _ = Fs.create_dir_all macro_src_dir |> Result.expect ~msg:"create macro src dir failed" in
  let _ = Fs.write helper_source Path.(helper_src_dir / Path.v "helper.ml")
  |> Result.expect ~msg:"write helper source failed" in
  let _ = Fs.write provider_source Path.(macro_src_dir / Path.v "macro.ml")
  |> Result.expect ~msg:"write macro provider source failed" in
  let helper_package =
    Riot_model.Package.{
      name = "helper";
      path = helper_root;
      relative_path = Path.v "packages/helper";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = Some { path = Path.v "src/helper.ml"; kind = Riot_model.Package.Runtime };
      sources = {
        src = [ Path.v "src/helper.ml" ];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
      publish = { version = None; description = None; license = None; is_public = None };
    }
  in
  let macro_package =
    Riot_model.Package.{
      name = "macro-demo";
      path = macro_root;
      relative_path = Path.v "packages/macro-demo";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [ workspace_dependency "helper" ];
      foreign_dependencies = [];
      binaries = [];
      library = Some { path = Path.v "src/macro.ml"; kind = Riot_model.Package.Macro };
      sources = {
        src = [ Path.v "src/macro.ml" ];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
      publish = { version = None; description = None; license = None; is_public = None };
    }
  in
  let workspace =
    Riot_model.Workspace.make ~root:tmpdir ~packages:[ helper_package; macro_package ] () in
  let providers = Riot_model.Workspace.discover_macro_providers workspace in
  (workspace, providers)

let test_provider_hash_tracks_dependency_closure_sources = Test.case
  "runner hash tracks dependency closure source changes"
  (fun _ctx ->
    with_temp_workspace_result
      ~prefix:"macro_runner_hash"
      (fun tmpdir ->
        let _workspace, providers =
          write_macro_workspace
            ~tmpdir
            ~helper_source:"let version = \"one\"\n"
            ~provider_source:"let provider () = Macro.Provider.v ~module_path:[ \"MacroDemo\" ] []\n" in
        let first_hash = Runner.providers_hash ~workspace_root:tmpdir providers in
        let _ = Fs.write
          "let version = \"two\"\n"
          Path.(tmpdir / Path.v "packages" / Path.v "helper" / Path.v "src" / Path.v "helper.ml")
        |> Result.expect ~msg:"rewrite helper source failed" in
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
        let _workspace, providers =
          write_macro_workspace
            ~tmpdir
            ~helper_source:"let version = \"one\"\n"
            ~provider_source:"let provider () = Macro.Provider.v ~module_path:[ \"MacroDemo\" ] []\n" in
        let target_dir_root = Path.(tmpdir / Path.v "target") in
        let plan = Runner.materialize ~workspace_root:tmpdir ~target_dir_root providers in
        let sentinel = Path.(plan.workspace_root / Path.v "sentinel.txt") in
        let _ = Fs.write "keep me\n" sentinel |> Result.expect ~msg:"write sentinel failed" in
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
