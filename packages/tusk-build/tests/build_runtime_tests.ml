open Std
module Test = Std.Test

let make_broken_workspace = fun tmpdir ->
  let pkg_dir = Path.(tmpdir / Path.v "demo") in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed" in
  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ = Fs.write "let broken =" ml_file |> Result.expect ~msg:"Write ml failed" in
  let tusk_file = Path.(pkg_dir / Path.v "tusk.toml") in
  let tusk_content = "[package]\nname = \"demo\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n" in
  let _ = Fs.write tusk_content tusk_file |> Result.expect ~msg:"Write tusk.toml failed" in
  let package =
    Tusk_model.Package.{
      name = "demo";
      path = pkg_dir;
      relative_path = Path.v "demo";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = Some { path = Path.v "src/lib.ml" };
      sources =
        {
          src = [ Path.v "src/lib.ml" ];
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
  Tusk_model.Workspace.make ~root:tmpdir ~packages:[ package ] ()

let make_valid_workspace = fun tmpdir ->
  let pkg_dir = Path.(tmpdir / Path.v "demo") in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed" in
  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ = Fs.write "let value = 42\n" ml_file |> Result.expect ~msg:"Write ml failed" in
  let tusk_file = Path.(pkg_dir / Path.v "tusk.toml") in
  let tusk_content = "[package]\nname = \"demo\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n" in
  let _ = Fs.write tusk_content tusk_file |> Result.expect ~msg:"Write tusk.toml failed" in
  let package =
    Tusk_model.Package.{
      name = "demo";
      path = pkg_dir;
      relative_path = Path.v "demo";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = Some { path = Path.v "src/lib.ml" };
      sources =
        {
          src = [ Path.v "src/lib.ml" ];
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
  Tusk_model.Workspace.make ~root:tmpdir ~packages:[ package ] ()

let test_build_surfaces_failed_builds = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"tusk_build_runtime"
      (fun tmpdir ->
        let workspace = make_broken_workspace tmpdir in
        match
          Tusk_build.build
            {
              workspace;
              packages = [ "demo" ];
              targets = Tusk_build.Host;
              scope = Tusk_build.Runtime;
              profile = "debug";
            }
        with
        | Error (Tusk_build.ClientError (Tusk_build.Client.BuildFailed { errors })) ->
            if List.length errors > 0 then
              Ok ()
            else
              Error "expected at least one build error"
        | Error err -> Error ("expected build failure, got: " ^ Tusk_build.build_error_message err)
        | Ok () -> Error "expected broken package build to fail")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_release_uses_release_lane = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"tusk_build_release_runtime"
      (fun tmpdir ->
        let workspace = make_valid_workspace tmpdir in
        let host_target = Tusk_model.Tusk_dirs.host_target () in
        let release_package_dir = Tusk_model.Tusk_dirs.out_dir_with_target
          ~workspace_root:workspace.root
          ~profile:"release"
          ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo") in
        let debug_package_dir = Tusk_model.Tusk_dirs.out_dir_with_target
          ~workspace_root:workspace.root
          ~profile:"debug"
          ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo") in
        match
          Tusk_build.build
            {
              workspace;
              packages = [ "demo" ];
              targets = Tusk_build.Host;
              scope = Tusk_build.Runtime;
              profile = "release";
            }
        with
        | Error err -> Error ("expected release build to succeed, got: "
        ^ Tusk_build.build_error_message err)
        | Ok () ->
            if not
                (Fs.exists release_package_dir |> Result.unwrap_or ~default:false) then
              Error ("expected release output under " ^ Path.to_string release_package_dir)
            else if Fs.exists debug_package_dir |> Result.unwrap_or ~default:false then
              Error ("did not expect debug output under " ^ Path.to_string debug_package_dir)
            else
              Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests =
  let open Test in [
    case "build runtime: failed builds surface as errors" test_build_surfaces_failed_builds;
    case "build runtime: release builds use the release lane" test_build_release_uses_release_lane;
  ]

let name = "Tusk Build Runtime Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
