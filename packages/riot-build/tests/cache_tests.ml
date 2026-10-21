open Std
open Riot_build

module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let make_test_workspace = fun tmpdir packages ->
  Riot_model.Workspace.make_realized
    ~root:tmpdir
    ~packages
    ~target_dir:(Path.v "target")
    ()

let build_package = fun ~workspace package ->
  let request =
    Riot_build.Request.make
      ~workspace
      ~packages:[ package.Riot_model.Package.name ]
      ~targets:Riot_model.Target.Host
      ~scope:Riot_build.Request.Runtime
      ~profile:Riot_model.Profile.debug
      ()
  in
  match Riot_build.build request with
  | Error err -> Error (Riot_build.error_message err)
  | Ok result ->
      match Riot_build.Build_result.find_package result package.name with
      | Some package_result -> Ok package_result
      | None ->
          Error ("expected package result for " ^ Riot_model.Package_name.to_string package.name)

let make_package = fun tmpdir name content ->
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let package_name = package_name name in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ =
    Fs.create_dir_all src_dir
    |> Result.expect ~msg:"Create src failed"
  in
  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ =
    Fs.write content ml_file
    |> Result.expect ~msg:"Write ml failed"
  in
  let riot_file = Path.(pkg_dir / Path.v "riot.toml") in
  let riot_content =
    "[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n"
  in
  let _ =
    Fs.write riot_content riot_file
    |> Result.expect ~msg:"Write riot.toml"
  in
  Riot_model.Package.make
    ~name:package_name
    ~path:pkg_dir
    ~relative_path:(Path.v name)
    ~library:{ path = Path.v "src/lib.ml" }
    ~sources:{
      src = [ Path.v "src/lib.ml" ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let test_fresh_build_no_cache = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"cache_test"
    (fun tmpdir ->
      let package = make_package tmpdir "test-pkg" "let x = 42" in
      let workspace = make_test_workspace tmpdir [ package ] in
      match build_package ~workspace package with
      | Error err -> Error ("Build failed: " ^ err)
      | Ok package_result ->
          match Riot_build.Build_result.package_status package_result with
          | Riot_build.Build_result.Built _ -> Ok ()
          | Cached _ -> Error "Fresh build should not be cached"
          | Skipped reason -> Error ("Build skipped: " ^ reason)
          | Failed reason -> Error ("Build failed: " ^ reason)) with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_second_build_reuses_action_cache_path = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"cache_test"
    (fun tmpdir ->
      let package = make_package tmpdir "test-pkg" "let x = 42" in
      let workspace = make_test_workspace tmpdir [ package ] in
      match build_package ~workspace package with
      | Ok first_build -> (
          match Riot_build.Build_result.package_status first_build with
          | Riot_build.Build_result.Built _ -> (
              match build_package ~workspace package with
              | Error err -> Error ("Second build failed: " ^ err)
              | Ok second_build -> (
                  match Riot_build.Build_result.package_status second_build with
                  | Built _
                  | Cached _ -> Ok ()
                  | Skipped reason -> Error ("Second build skipped: " ^ reason)
                  | Failed reason -> Error ("Second build failed: " ^ reason)
                )
            )
          | Skipped reason -> Error ("First build skipped: " ^ reason)
          | Cached _ -> Error "First build should not be cached"
          | Failed reason -> Error ("First build failed: " ^ reason)
        )
      | Error err -> Error ("First build failed: " ^ err)) with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests = let open Test in
[
  case ~size:Large "cache: fresh build, no cache" test_fresh_build_no_cache;
  case
    ~size:Large
    "cache: second build, action cache path"
    test_second_build_reuses_action_cache_path;
]

let name = "Cache Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
