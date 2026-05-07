open Std

module Test = Std.Test
module HashSet = Std.Collections.HashSet

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let builtin_dependency = fun name ->
  Riot_model.Package.{
    name = package_name name;
    source =
      {
        workspace = false;
        builtin = true;
        path = None;
        source_locator = None;
        ref_ = None;
        version = None;
      };
  }

let version = fun value ->
  Std.Version.parse value
  |> Result.expect ~msg:("invalid version: " ^ value)

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target: " ^ value)

let empty_sources =
  Riot_model.Package.{
    src = [];
    native = [];
    tests = [];
    examples = [];
    bench = [];
  }

let make_package = fun
  ?(workspace_member = true) ?version ?(sources = empty_sources) ?(binaries = []) name ->
  let relative_path =
    if workspace_member then
      Path.v name
    else
      Path.v ("../registry/" ^ name)
  in
  Riot_model.Package.make
    ~name:(package_name name)
    ~path:(Path.v ("/tmp/" ^ name))
    ~relative_path
    ?publish:(
      match version with
      | None -> None
      | Some version ->
          Some Riot_model.Package.{
            version = Some version;
            description = None;
            license = None;
            is_public = Some true;
          }
    )
    ~binaries
    ~sources
    ()

let parse_build = fun args ->
  match ArgParser.get_matches Riot_cli.Build.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_run = fun args ->
  match ArgParser.get_matches Riot_cli.Run.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_trace = fun args ->
  match ArgParser.get_matches Riot_cli.Trace_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_test = fun args ->
  match ArgParser.get_matches Riot_cli.Test_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_bench = fun args ->
  match ArgParser.get_matches Riot_cli.Bench_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_install = fun args ->
  match ArgParser.get_matches Riot_cli.Install.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_info = fun args ->
  match ArgParser.get_matches Riot_cli.Info_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_clean = fun args ->
  match ArgParser.get_matches Riot_cli.Clean.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let error_line = fun message -> "error " ^ message

let write_workspace_manifest = fun ~root ~members ->
  let members =
    members
    |> List.map ~fn:(fun member -> "  \"" ^ Path.to_string member ^ "\"")
    |> String.concat ",\n"
  in
  let content = "[workspace]\nmembers = [\n" ^ members ^ "\n]\n" in
  Fs.write content Path.(root / Path.v "riot.toml")
  |> Result.expect ~msg:"Write workspace riot.toml failed"

let make_valid_workspace = fun ?target_dir tmpdir ->
  let pkg_dir = Path.(tmpdir / Path.v "demo") in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ =
    Fs.create_dir_all src_dir
    |> Result.expect ~msg:"Create src failed"
  in
  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ =
    Fs.write "let value = 42\n" ml_file
    |> Result.expect ~msg:"Write ml failed"
  in
  let riot_file = Path.(pkg_dir / Path.v "riot.toml") in
  let riot_content =
    "[package]\nname = \"demo\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n\n[dependencies]\nstdlib = \"*\"\n"
  in
  let _ =
    Fs.write riot_content riot_file
    |> Result.expect ~msg:"Write riot.toml failed"
  in
  let _ = write_workspace_manifest ~root:tmpdir ~members:[ Path.v "demo" ] in
  let package =
    Riot_model.Package.make
      ~name:(package_name "demo")
      ~path:pkg_dir
      ~relative_path:(Path.v "demo")
      ~library:{ path = Path.v "src/lib.ml" }
      ~sources:{
        src = [ Path.v "src/lib.ml" ];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      }
      ()
  in
  Riot_model.Workspace.make_realized ~root:tmpdir ?target_dir ~packages:[ package ] ()

let make_workspace_with_dev_binaries = fun ?target_dir tmpdir ->
  let pkg_dir = Path.(tmpdir / Path.v "demo") in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let tests_dir = Path.(pkg_dir / Path.v "tests") in
  let examples_dir = Path.(pkg_dir / Path.v "examples") in
  let bench_dir = Path.(pkg_dir / Path.v "bench") in
  let _ =
    Fs.create_dir_all src_dir
    |> Result.expect ~msg:"Create src failed"
  in
  let _ =
    Fs.create_dir_all tests_dir
    |> Result.expect ~msg:"Create tests failed"
  in
  let _ =
    Fs.create_dir_all examples_dir
    |> Result.expect ~msg:"Create examples failed"
  in
  let _ =
    Fs.create_dir_all bench_dir
    |> Result.expect ~msg:"Create bench failed"
  in
  let _ =
    Fs.write "let value = 42\n" Path.(src_dir / Path.v "lib.ml")
    |> Result.expect ~msg:"Write lib failed"
  in
  let _ =
    Fs.write "let main ~args:_ = Stdlib.Ok ()\n" Path.(tests_dir / Path.v "demo_tests.ml")
    |> Result.expect ~msg:"Write test binary failed"
  in
  let _ =
    Fs.write "let main ~args:_ = Stdlib.Ok ()\n" Path.(examples_dir / Path.v "demo_example.ml")
    |> Result.expect ~msg:"Write example binary failed"
  in
  let _ =
    Fs.write "let main ~args:_ = Stdlib.Ok ()\n" Path.(bench_dir / Path.v "demo_bench.ml")
    |> Result.expect ~msg:"Write bench binary failed"
  in
  let riot_file = Path.(pkg_dir / Path.v "riot.toml") in
  let riot_content =
    "[package]\nname = \"demo\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n"
  in
  let _ =
    Fs.write riot_content riot_file
    |> Result.expect ~msg:"Write riot.toml failed"
  in
  let _ = write_workspace_manifest ~root:tmpdir ~members:[ Path.v "demo" ] in
  let package =
    Riot_model.Package.make
      ~name:(package_name "demo")
      ~path:pkg_dir
      ~relative_path:(Path.v "demo")
      ~library:{ path = Path.v "src/lib.ml" }
      ~binaries:[
        Riot_model.Package.{ name = "demo_tests"; path = Path.v "tests/demo_tests.ml" };
        Riot_model.Package.{ name = "demo_example"; path = Path.v "examples/demo_example.ml" };
        Riot_model.Package.{ name = "demo_bench"; path = Path.v "bench/demo_bench.ml" };
      ]
      ~dependencies:[ builtin_dependency "stdlib" ]
      ~sources:{
        src = [ Path.v "src/lib.ml" ];
        native = [];
        tests = [ Path.v "tests/demo_tests.ml" ];
        examples = [ Path.v "examples/demo_example.ml" ];
        bench = [ Path.v "bench/demo_bench.ml" ];
      }
      ()
  in
  Riot_model.Workspace.make_realized ~root:tmpdir ?target_dir ~packages:[ package ] ()

let host_out_package_dir = fun (workspace: Riot_model.Workspace.t) ->
  let host_target = Riot_model.Riot_dirs.host_target () in
  Riot_model.Riot_dirs.out_dir_in_workspace ~workspace ~profile:"debug" ~target:host_target
  |> fun out_dir -> Path.(out_dir / Path.v "demo")

let dev_binary_paths = fun (workspace: Riot_model.Workspace.t) ->
  let package_out_dir = host_out_package_dir workspace in
  (
    Path.(package_out_dir / Path.v "demo_tests"),
    Path.(package_out_dir / Path.v "demo_example"),
    Path.(package_out_dir / Path.v "demo_bench")
  )

let assert_path_exists = fun path ~message ->
  if Fs.exists path
  |> Result.unwrap_or ~default:false then
    Ok ()
  else
    Error message

let assert_path_missing = fun path ~message ->
  if Fs.exists path
  |> Result.unwrap_or ~default:false then
    Error message
  else
    Ok ()

let test_build_accepts_multiple_packages = fun _ctx ->
  match parse_build [ "build"; "-p"; "syn"; "-p"; "krasny"; "-p"; "riot-cli"; ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      let actual = ArgParser.get_many matches "package" in
      Test.assert_equal ~expected:[ "syn"; "krasny"; "riot-cli" ] ~actual;
      Ok ()

let test_display_package_name_keeps_workspace_package_bare = fun _ctx ->
  let package = make_package "demo" in
  Test.assert_equal ~expected:"demo" ~actual:(Riot_cli.Build.display_package_name package);
  Ok ()

let test_display_package_name_shows_debug_profile = fun _ctx ->
  let package = make_package "demo" in
  Test.assert_equal
    ~expected:"demo (debug)"
    ~actual:(Riot_cli.Build.display_package_name ~profile:"debug" package);
  Ok ()

let test_display_package_name_shows_non_debug_profile = fun _ctx ->
  let package = make_package "demo" in
  Test.assert_equal
    ~expected:"demo (fuzz)"
    ~actual:(Riot_cli.Build.display_package_name ~profile:"fuzz" package);
  Ok ()

let test_display_package_name_shows_external_package_version = fun _ctx ->
  let package = make_package ~workspace_member:false ~version:(version "1.2.3") "serde-json" in
  Test.assert_equal
    ~expected:"serde-json (1.2.3)"
    ~actual:(Riot_cli.Build.display_package_name package);
  Ok ()

let test_display_package_name_shows_external_package_version_and_target = fun _ctx ->
  let package = make_package ~workspace_member:false ~version:(version "1.2.3") "serde-json" in
  Test.assert_equal
    ~expected:"serde-json (1.2.3, aarch64-apple-darwin)"
    ~actual:(Riot_cli.Build.display_package_name
      ~build_target:(target "aarch64-apple-darwin")
      ~show_target:true
      package);
  Ok ()

let test_display_package_name_shows_workspace_test_and_target = fun _ctx ->
  let package =
    make_package
      ~sources:Riot_model.Package.{
        empty_sources with
        tests = [ Path.v "tests/serde_json_tests.ml" ];
      }
      "serde-json"
  in
  Test.assert_equal
    ~expected:"serde-json (fuzz, test, aarch64-apple-darwin)"
    ~actual:(Riot_cli.Build.display_package_name
      ~profile:"fuzz"
      ~build_target:(target "aarch64-apple-darwin")
      ~show_target:true
      package);
  Ok ()

let test_display_package_name_shows_workspace_bench = fun _ctx ->
  let package =
    make_package
      ~binaries:[
        Riot_model.Package.{
          name = "serde_json_bench";
          path = Path.v "bench/serde_json_bench.ml";
        };
      ]
      "serde-json"
  in
  Test.assert_equal
    ~expected:"serde-json (bench)"
    ~actual:(Riot_cli.Build.display_package_name package);
  Ok ()

let test_planning_error_lines_describe_internal_module_violation = fun _ctx ->
  let lines =
    Riot_cli.Build.planning_error_lines
      (
        Riot_planner.Planning_error.TargetDependsOnInternalLibraryModule {
          target_name = "main";
          source = Path.v "src/main.ml";
          requested_module = "A";
          internal_module = "Demo__A";
          public_module = "Demo";
        }
      )
  in
  Test.assert_equal
    ~expected:[
      error_line "target main imports private module A";
      "The target source reaches Demo__A, which is internal to this package library.";
      "target: main";
      "source: src/main.ml";
      "requested module: A";
      "internal module: Demo__A";
      "public module: Demo";
      "examples:";
      "  - use Demo.A instead";
      "  - move shared target code behind Demo or a shared helper module";
    ]
    ~actual:lines;
  Ok ()

let test_planning_error_lines_describe_undeclared_package_module = fun _ctx ->
  let lines =
    Riot_cli.Build.planning_error_lines
      (
        Riot_planner.Planning_error.SourceDependsOnUndeclaredPackageModule {
          package_name = "demo";
          source = Path.v "src/main.ml";
          requested_module = "Kernel";
          allowed_modules = [ "Demo"; "Std" ];
          suggested_modules = [];
        }
      )
  in
  Test.assert_equal
    ~expected:[
      error_line "Kernel is not available to package demo";
      "The source file imports Kernel, but Riot only exposes modules from this package and its direct dependencies.";
      "package: demo";
      "source: src/main.ml";
      "requested module: Kernel";
      "available direct modules: Demo, Std";
      "examples:";
      "  - add the package that provides Kernel to [dependencies]";
      "  - or depend through one of the exposed modules above if that is the public API you meant";
    ]
    ~actual:lines;
  Ok ()

let test_planning_error_lines_include_module_name_suggestions = fun _ctx ->
  let lines =
    Riot_cli.Build.planning_error_lines
      (
        Riot_planner.Planning_error.SourceDependsOnUndeclaredPackageModule {
          package_name = "typ";
          source = Path.v "src/check.ml";
          requested_module = "SurfacePath";
          allowed_modules = [ "Std"; "Typ" ];
          suggested_modules = [ "Surface_path" ];
        }
      )
  in
  Test.assert_equal
    ~expected:[
      error_line "SurfacePath is not available to package typ";
      "The source file imports SurfacePath, but Riot only exposes modules from this package and its direct dependencies.";
      "package: typ";
      "source: src/check.ml";
      "requested module: SurfacePath";
      "available direct modules: Std, Typ";
      "did you mean: Surface_path";
      "examples:";
      "  - add the package that provides SurfacePath to [dependencies]";
      "  - or depend through one of the exposed modules above if that is the public API you meant";
    ]
    ~actual:lines;
  Ok ()

let test_planning_error_lines_describe_invalid_executable_main = fun _ctx ->
  let lines =
    Riot_cli.Build.planning_error_lines
      (
        Riot_planner.Planning_error.InvalidExecutableMain {
          package_name = "riot-fix";
          target_name = "riot-fix";
          source = Path.v "src/main.ml";
          file = Path.v "packages/riot-fix/src/main.ml";
          error = Riot_planner.Planning_error.MissingMain;
        }
      )
  in
  Test.assert_equal
    ~expected:[
      error_line "`riot-fix` has no executable entry point";
      "";
      "Riot is building this target as an executable:";
      "";
      "    package: riot-fix";
      "    target:  riot-fix";
      "    file:    ./packages/riot-fix/src/main.ml";
      "";
      "To start the program, Riot needs this file to define a top-level";
      "`main` function with this shape:";
      "";
      "    let main ~args =";
      "      ...";
      "      Ok ()";
      "";
      "But we could not find one.";
    ]
    ~actual:lines;
  Ok ()

let test_workspace_planning_error_lines_describe_missing_dependencies = fun _ctx ->
  let lines =
    Riot_cli.Build.build_unit_planning_error_lines
      (Riot_build.Internal.Build_unit_plan.MissingPackages {
        missing = [
          Riot_planner.Build_unit_graph.Dependency {
            package = package_name "demo";
            dependency = package_name "std";
          };
          Dependency {
            package = package_name "demo";
            dependency = package_name "tty";
          };
        ];
      })
  in
  Test.assert_equal
    ~expected:[
      error_line "missing package dependencies";
      "Riot found package dependency edges that do not point at a loaded workspace or resolved package.";
      "missing: demo -> std";
      "missing: demo -> tty";
      "examples:";
      "  - add the missing package to the workspace";
      "  - add a registry, path, or workspace dependency entry for the missing package";
    ]
    ~actual:lines;
  Ok ()

let test_planning_error_lines_indent_multiline_reasons = fun _ctx ->
  let lines =
    Riot_cli.Build.planning_error_lines
      (Riot_planner.Planning_error.DependencyAnalysisFailed {
        reason = "failed to parse tests/solver_tests.ml\n\nhint: add )";
      })
  in
  Test.assert_equal
    ~expected:[
      error_line "dependency analysis failed";
      "Riot could not parse or analyze a source file while discovering module dependencies.";
      "reason: failed to parse tests/solver_tests.ml";
      "";
      "  hint: add )";
    ]
    ~actual:lines;
  Ok ()

let test_build_failure_detail_lines_render_planning_errors = fun _ctx ->
  let failure: Riot_build.Build_result.failure = {
    package_name = package_name "demo";
    unit_key =
      ({
        package = package_name "demo";
        artifact = Riot_planner.Build_unit.Library;
        target = Riot_model.Target.host ();
        profile = Riot_model.Profile.debug;
      }:Riot_planner.Build_unit.key);
    reason = Riot_build.Build_result.PackagePlanningFailed (Riot_planner.Planning_error.DependencyAnalysisFailed {
      reason = "failed to parse src/demo.ml\nhint: add end";
    });
    message = "raw fallback should not be rendered";
    ocamlc_warnings = [];
    duration_ms = 12;
  }
  in
  Test.assert_equal
    ~expected:[
      error_line "dependency analysis failed";
      "Riot could not parse or analyze a source file while discovering module dependencies.";
      "reason: failed to parse src/demo.ml";
      "  hint: add end";
    ]
    ~actual:(Riot_cli.Build.build_failure_detail_lines failure);
  Ok ()

let test_build_usage_shows_repeated_package_flag = fun _ctx ->
  let usage = ArgParser.usage_string Riot_cli.Build.command in
  if String.equal usage "Usage: build [OPTIONS]" then
    Ok ()
  else
    Error ("expected repeated package flag usage, got: " ^ usage)

let test_build_rejects_positional_package_args = fun _ctx ->
  match parse_build [ "build"; "syn" ] with
  | Error _ -> Ok ()
  | Ok _ -> Error "expected positional package arguments to be rejected"

let test_build_accepts_json_flag = fun _ctx ->
  match parse_build [ "build"; "--json"; "-p"; "syn"; ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected --json flag to be parsed"

let test_build_accepts_release_flag = fun _ctx ->
  match parse_build [ "build"; "--release"; "-p"; "syn"; ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "release" then
        Ok ()
      else
        Error "expected --release flag to be parsed"

let test_build_accepts_jobs_flag = fun _ctx ->
  match parse_build [ "build"; "--jobs"; "4"; "-p"; "syn"; ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      match ArgParser.get_one matches "jobs" with
      | Some jobs ->
          if String.equal jobs "4" then
            Ok ()
          else
            Error ("unexpected --jobs value: " ^ jobs)
      | None -> Error "expected --jobs flag to be parsed"

let test_build_accepts_target_dir_flag = fun _ctx ->
  match parse_build [ "build"; "--target-dir"; "_build_bin"; "-p"; "syn"; ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      match ArgParser.get_one matches "target-dir" with
      | Some target_dir ->
          if String.equal target_dir "_build_bin" then
            Ok ()
          else
            Error ("unexpected --target-dir value: " ^ target_dir)
      | None -> Error "expected --target-dir flag to be parsed"

let test_build_accepts_tests_flag = fun _ctx ->
  match parse_build [ "build"; "--tests"; "-p"; "syn"; ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "tests" then
        Ok ()
      else
        Error "expected --tests flag to be parsed"

let test_build_accepts_examples_flag = fun _ctx ->
  match parse_build [ "build"; "--examples"; "-p"; "syn"; ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "examples" then
        Ok ()
      else
        Error "expected --examples flag to be parsed"

let test_build_accepts_benches_flag = fun _ctx ->
  match parse_build [ "build"; "--benches"; "-p"; "syn"; ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "benches" then
        Ok ()
      else
        Error "expected --benches flag to be parsed"

let test_build_accepts_all_flag = fun _ctx ->
  match parse_build [ "build"; "--all"; "-p"; "syn"; ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "all" then
        Ok ()
      else
        Error "expected --all flag to be parsed"

let test_build_rejects_invalid_jobs_flag = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_invalid_jobs"
    (fun tmpdir ->
      let workspace = make_valid_workspace tmpdir in
      match parse_build [ "build"; "--jobs"; "abc"; "-p"; "demo"; ] with
      | Error err -> Error ("expected build args to parse: " ^ err)
      | Ok matches ->
          match Riot_cli.Build.run ~workspace matches with
          | Ok () -> Error "expected invalid --jobs value to fail"
          | Error (Failure message) ->
              if String.equal message "invalid --jobs value: abc" then
                Ok ()
              else
                Error ("unexpected jobs parse error: " ^ message)
          | Error err -> Error ("expected failure to be Failure: " ^ Exception.to_string err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_accepts_jobs_flag_in_run = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_jobs_runtime"
    (fun tmpdir ->
      let workspace = make_valid_workspace tmpdir in
      match parse_build [ "build"; "--jobs"; "2"; "-p"; "demo"; ] with
      | Error err -> Error ("expected build args to parse: " ^ err)
      | Ok matches ->
          match Riot_cli.Build.run ~workspace matches with
          | Ok () -> Ok ()
          | Error err -> Error ("expected build success: " ^ Exception.to_string err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_rejects_zero_jobs_runtime = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_zero_jobs"
    (fun tmpdir ->
      let workspace = make_valid_workspace tmpdir in
      match parse_build [ "build"; "--jobs"; "0"; "-p"; "demo"; ] with
      | Error err -> Error ("expected build args to parse: " ^ err)
      | Ok matches ->
          match Riot_cli.Build.run ~workspace matches with
          | Error (Failure message) when String.equal
            message
            "invalid requested parallelism (0): jobs must be >= 1" -> Ok ()
          | Error (Failure message) -> Error ("unexpected failure message: " ^ message)
          | Ok () -> Error "expected zero jobs to be rejected"
          | Error err -> Error ("expected Failure: " ^ Exception.to_string err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_rejects_negative_jobs_runtime = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_negative_jobs"
    (fun tmpdir ->
      let workspace = make_valid_workspace tmpdir in
      match parse_build [ "build"; "--jobs"; "-1"; "-p"; "demo"; ] with
      | Error err -> Error ("expected build args to parse: " ^ err)
      | Ok matches ->
          match Riot_cli.Build.run ~workspace matches with
          | Error (Failure message) when String.equal
            message
            "invalid requested parallelism (-1): jobs must be >= 1" -> Ok ()
          | Error (Failure message) -> Error ("unexpected failure message: " ^ message)
          | Ok () -> Error "expected negative jobs to be rejected"
          | Error err -> Error ("expected Failure: " ^ Exception.to_string err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_command_accepts_workspace = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_command"
    (fun tmpdir ->
      let workspace = make_valid_workspace tmpdir in
      Riot_cli.Build.build_command
        ~workspace
        ~show_finished_summary:false
        ~mode:Riot_cli.Build.Human
        (Some (package_name "demo"))
        None) with
  | Ok (Ok ()) -> Ok ()
  | Ok (Error err) ->
      Error ("expected workspace build command to succeed: " ^ Exception.to_string err)
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_run_accepts_workspace = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_command"
    (fun tmpdir ->
      let workspace = make_valid_workspace tmpdir in
      let matches =
        parse_build [ "build"; "-p"; "demo" ]
        |> Result.expect ~msg:"expected build args to parse"
      in
      Riot_cli.Build.run ~workspace matches) with
  | Ok (Ok ()) -> Ok ()
  | Ok (Error err) -> Error ("expected workspace build run to succeed: " ^ Exception.to_string err)
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_tests_compile_only_test_binaries = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_tests_scope"
    (fun tmpdir ->
      let workspace = make_workspace_with_dev_binaries tmpdir in
      let (test_binary, example_binary, bench_binary) = dev_binary_paths workspace in
      let tests_matches =
        parse_build [ "build"; "--tests"; "-p"; "demo"; ]
        |> Result.expect ~msg:"expected test build args to parse"
      in
      let open Std.Result.Syntax in
      let* () =
        match Riot_cli.Build.run ~workspace tests_matches with
        | Ok () -> Ok ()
        | Error err -> Error ("expected test-scoped build success: " ^ Exception.to_string err)
      in
      let* () =
        assert_path_exists test_binary ~message:"expected --tests to materialize test binary"
      in
      let* () =
        assert_path_missing
          example_binary
          ~message:"did not expect --tests to materialize example binary"
      in
      assert_path_missing bench_binary ~message:"did not expect --tests to materialize bench binary") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_examples_compile_only_example_binaries = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_examples_scope"
    (fun tmpdir ->
      let workspace = make_workspace_with_dev_binaries tmpdir in
      let (test_binary, example_binary, bench_binary) = dev_binary_paths workspace in
      let example_matches =
        parse_build [ "build"; "--examples"; "-p"; "demo"; ]
        |> Result.expect ~msg:"expected example build args to parse"
      in
      let open Std.Result.Syntax in
      let* () =
        match Riot_cli.Build.run ~workspace example_matches with
        | Ok () -> Ok ()
        | Error err -> Error ("expected example-scoped build success: " ^ Exception.to_string err)
      in
      let* () =
        assert_path_missing
          test_binary
          ~message:"did not expect --examples to materialize test binary"
      in
      let* () =
        assert_path_exists
          example_binary
          ~message:"expected --examples to materialize example binary"
      in
      assert_path_missing
        bench_binary
        ~message:"did not expect --examples to materialize bench binary") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_benches_compile_only_bench_binaries = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_benches_scope"
    (fun tmpdir ->
      let workspace = make_workspace_with_dev_binaries tmpdir in
      let (test_binary, example_binary, bench_binary) = dev_binary_paths workspace in
      let bench_matches =
        parse_build [ "build"; "--benches"; "-p"; "demo"; ]
        |> Result.expect ~msg:"expected bench build args to parse"
      in
      let open Std.Result.Syntax in
      let* () =
        match Riot_cli.Build.run ~workspace bench_matches with
        | Ok () -> Ok ()
        | Error err -> Error ("expected bench-scoped build success: " ^ Exception.to_string err)
      in
      let* () =
        assert_path_missing
          test_binary
          ~message:"did not expect --benches to materialize test binary"
      in
      let* () =
        assert_path_missing
          example_binary
          ~message:"did not expect --benches to materialize example binary"
      in
      assert_path_exists bench_binary ~message:"expected --benches to materialize bench binary") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_all_compiles_all_dev_binaries = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_cli_build_all_scope"
    (fun tmpdir ->
      let workspace = make_workspace_with_dev_binaries tmpdir in
      let (test_binary, example_binary, bench_binary) = dev_binary_paths workspace in
      let all_matches =
        parse_build [ "build"; "--all"; "-p"; "demo"; ]
        |> Result.expect ~msg:"expected all build args to parse"
      in
      let open Std.Result.Syntax in
      let* () =
        match Riot_cli.Build.run ~workspace all_matches with
        | Ok () -> Ok ()
        | Error err -> Error ("expected all-artifacts build success: " ^ Exception.to_string err)
      in
      let* () = assert_path_exists test_binary ~message:"expected --all to materialize test binary" in
      let* () =
        assert_path_exists example_binary ~message:"expected --all to materialize example binary"
      in
      assert_path_exists bench_binary ~message:"expected --all to materialize bench binary") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_test_accepts_json_flag = fun _ctx ->
  match parse_test [ "test"; "--json"; "-p"; "riot-build"; ] with
  | Error err -> Error ("expected test args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected test --json flag to be parsed"

let test_test_accepts_release_flag = fun _ctx ->
  match parse_test [ "test"; "--release"; "-p"; "riot-build"; ] with
  | Error err -> Error ("expected test args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "release" then
        Ok ()
      else
        Error "expected test --release flag to be parsed"

let test_test_accepts_list_flag = fun _ctx ->
  match parse_test [ "test"; "--list"; "-p"; "riot-build"; ] with
  | Error err -> Error ("expected test args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "list" then
        Ok ()
      else
        Error "expected test --list flag to be parsed"

let test_bench_accepts_json_flag = fun _ctx ->
  match parse_bench [ "bench"; "--json"; "-p"; "std"; ] with
  | Error err -> Error ("expected bench args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected bench --json flag to be parsed"

let test_bench_accepts_release_flag = fun _ctx ->
  match parse_bench [ "bench"; "--release"; "-p"; "std"; ] with
  | Error err -> Error ("expected bench args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "release" then
        Ok ()
      else
        Error "expected bench --release flag to be parsed"

let test_bench_accepts_list_flag = fun _ctx ->
  match parse_bench [ "bench"; "--list"; "-p"; "std"; ] with
  | Error err -> Error ("expected bench args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "list" then
        Ok ()
      else
        Error "expected bench --list flag to be parsed"

let test_bench_accepts_iterations_flag = fun _ctx ->
  match parse_bench [ "bench"; "--iterations"; "500"; "-p"; "std"; ] with
  | Error err -> Error ("expected bench args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some 500) ~actual:(ArgParser.get_int matches "iterations");
      Ok ()

let test_bench_accepts_warmup_flag = fun _ctx ->
  match parse_bench [ "bench"; "--warmup"; "25"; "-p"; "std"; ] with
  | Error err -> Error ("expected bench args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some 25) ~actual:(ArgParser.get_int matches "warmup");
      Ok ()

let test_bench_invocation_args_keep_top_level_flags_after_forwarded_args = fun _ctx ->
  let invocation =
    Riot_cli.Bench_cmd.bench_invocation_args
      [
        "bench";
        "-p";
        "serde-json";
        "-f";
        "manual decode from parsed tree";
        "--compare";
        "3";
        "--iterations";
        "500";
        "--warmup";
        "25";
        "--release";
        "--json";
      ]
  in
  Test.assert_equal
    ~expected:[
      "bench";
      "-p";
      "serde-json";
      "-f";
      "manual decode from parsed tree";
      "--compare";
      "3";
      "--iterations";
      "500";
      "--warmup";
      "25";
      "--release";
      "--json";
    ]
    ~actual:invocation.parsed;
  Test.assert_equal ~expected:[] ~actual:invocation.trailing;
  Ok ()

let test_run_accepts_missing_name = fun _ctx ->
  match parse_run [ "run" ] with
  | Error err -> Error ("expected run args to parse without a name: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:None ~actual:(ArgParser.get_one matches "name");
      Ok ()

let test_run_accepts_list_flag = fun _ctx ->
  match parse_run [ "run"; "--list" ] with
  | Error err -> Error ("expected run args to parse with --list: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "list" then
        Ok ()
      else
        Error "expected run --list flag to be parsed"

let test_run_accepts_list_json_flag = fun _ctx ->
  match parse_run [ "run"; "--list"; "--json" ] with
  | Error err -> Error ("expected run args to parse with --list --json: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "list" && ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected run --list --json flags to be parsed"

let test_run_accepts_release_flag = fun _ctx ->
  match parse_run [ "run"; "--release"; "riot" ] with
  | Error err -> Error ("expected run args to parse with --release: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "release" then
        Ok ()
      else
        Error "expected run --release flag to be parsed"

let test_run_accepts_update_flag = fun _ctx ->
  match parse_run [ "run"; "--update"; "leostera/hello-world" ] with
  | Error err -> Error ("expected run args to parse with --update: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "update" then
        Ok ()
      else
        Error "expected run --update flag to be parsed"

let test_run_rejects_removed_trace_flag = fun _ctx ->
  match Riot_cli.Run.run_with_workspace_info
    ~workspace:None
    ~workspace_error:None
    (
      parse_run [ "run"; "--trace"; "out.trace"; "syn" ]
      |> Result.expect ~msg:"expected legacy trace args to parse as forwarded args"
    ) with
  | Ok () -> Error "expected riot run --trace to fail with migration guidance"
  | Error (Failure message) ->
      if String.contains message "riot run no longer accepts --trace" then
        Ok ()
      else
        Error ("unexpected riot run --trace error: " ^ message)
  | Error err -> Error ("unexpected error kind: " ^ Exception.to_string err)

let test_trace_accepts_missing_name = fun _ctx ->
  match parse_trace [ "trace" ] with
  | Error err -> Error ("expected trace args to parse without a name: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:None ~actual:(ArgParser.get_one matches "name");
      Ok ()

let test_trace_accepts_output_option = fun _ctx ->
  match parse_trace [ "trace"; "--output"; "out.trace"; "riot" ] with
  | Error err -> Error ("expected trace args to parse with --output: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "out.trace") ~actual:(ArgParser.get_one matches "output");
      Ok ()

let test_trace_accepts_profiler_option = fun _ctx ->
  match parse_trace [ "trace"; "--profiler"; "xctrace"; "riot" ] with
  | Error err -> Error ("expected trace args to parse with --profiler: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "xctrace") ~actual:(ArgParser.get_one matches "profiler");
      Ok ()

let test_trace_accepts_output_policy_options = fun _ctx ->
  match parse_trace [ "trace"; "--output"; "out.trace"; "--force"; "riot" ] with
  | Error err -> Error ("expected trace args to parse with --force: " ^ err)
  | Ok matches ->
      if not (ArgParser.get_flag matches "force") then
        Error "expected trace --force flag to be parsed"
      else
        match parse_trace [ "trace"; "--output"; "out.trace"; "--append"; "riot" ] with
        | Error err -> Error ("expected trace args to parse with --append: " ^ err)
        | Ok matches ->
            if ArgParser.get_flag matches "append" then
              Ok ()
            else
              Error "expected trace --append flag to be parsed"

let test_trace_accepts_sampling_options = fun _ctx ->
  match
    parse_trace [
      "trace";
      "--sample-rate";
      "997";
      "--time-limit";
      "5s";
      "--window";
      "1s";
      "--xctrace-template";
      "Time Profiler";
      "--perf-call-graph";
      "dwarf";
      "--perf-call-graph-stack-size";
      "8192";
      "riot";
    ]
  with
  | Error err -> Error ("expected trace args to parse sampling options: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "997") ~actual:(ArgParser.get_one matches "sample-rate");
      Test.assert_equal ~expected:(Some "5s") ~actual:(ArgParser.get_one matches "time-limit");
      Test.assert_equal ~expected:(Some "1s") ~actual:(ArgParser.get_one matches "window");
      Test.assert_equal
        ~expected:(Some "Time Profiler")
        ~actual:(ArgParser.get_one matches "xctrace-template");
      Test.assert_equal
        ~expected:(Some "dwarf")
        ~actual:(ArgParser.get_one matches "perf-call-graph");
      Test.assert_equal
        ~expected:(Some "8192")
        ~actual:(ArgParser.get_one matches "perf-call-graph-stack-size");
      Ok ()

let test_trace_accepts_summary_json = fun _ctx ->
  match parse_trace [ "trace"; "summary"; "--json"; "out.trace" ] with
  | Error err -> Error ("expected trace summary args to parse with --json: " ^ err)
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("summary", summary_matches) ->
          if ArgParser.get_flag summary_matches "json" then
            Ok ()
          else
            Error "expected trace summary --json flag to be parsed"
      | _ -> Error "expected trace summary subcommand to be parsed"
    )

let test_trace_accepts_summary_filter = fun _ctx ->
  match parse_trace [ "trace"; "summary"; "-f"; "*Prelude*"; "out.trace" ] with
  | Error err -> Error ("expected trace summary args to parse with -f: " ^ err)
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("summary", summary_matches) ->
          Test.assert_equal
            ~expected:(Some "*Prelude*")
            ~actual:(ArgParser.get_one summary_matches "filter");
          Ok ()
      | _ -> Error "expected trace summary subcommand to be parsed"
    )

let test_trace_accepts_call_tree_filter = fun _ctx ->
  match parse_trace [ "trace"; "call-tree"; "--filter"; "*Prelude*"; "out.trace" ] with
  | Error err -> Error ("expected trace call-tree args to parse with --filter: " ^ err)
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("call-tree", call_tree_matches) ->
          Test.assert_equal
            ~expected:(Some "*Prelude*")
            ~actual:(ArgParser.get_one call_tree_matches "filter");
          Ok ()
      | _ -> Error "expected trace call-tree subcommand to be parsed"
    )

let test_install_accepts_update_flag = fun _ctx ->
  match parse_install [ "install"; "--update"; "leostera/hello-world" ] with
  | Error err -> Error ("expected install args to parse with --update: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "update" then
        Ok ()
      else
        Error "expected install --update flag to be parsed"

let test_install_accepts_missing_name = fun _ctx ->
  match parse_install [ "install" ] with
  | Error err -> Error ("expected install args to parse without a name: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:None ~actual:(ArgParser.get_one matches "name");
      Ok ()

let test_install_accepts_package_flag = fun _ctx ->
  match parse_install [ "install"; "--package"; "riot-cli"; "riot"; ] with
  | Error err -> Error ("expected install args to parse with --package: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "riot-cli") ~actual:(ArgParser.get_one matches "package");
      Ok ()

let test_info_accepts_json_flag = fun _ctx ->
  match parse_info [ "info"; "--json" ] with
  | Error err -> Error ("expected info args to parse with --json: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected info --json flag to be parsed"

let test_info_accepts_workspace_target = fun _ctx ->
  match parse_info [ "info"; "workspace" ] with
  | Error err -> Error ("expected info workspace target to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal
        ~expected:Riot_cli.Info_cmd.Workspace_target
        ~actual:(Riot_cli.Info_cmd.target_of_matches matches);
      Ok ()

let test_info_accepts_package_target = fun _ctx ->
  match parse_info [ "info"; "serde-json@1.0.0" ] with
  | Error err -> Error ("expected info package target to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal
        ~expected:(Riot_cli.Info_cmd.Package_target "serde-json@1.0.0")
        ~actual:(Riot_cli.Info_cmd.target_of_matches matches);
      Ok ()

let test_clean_accepts_json_flag = fun _ctx ->
  match parse_clean [ "clean"; "--json" ] with
  | Error err -> Error ("expected clean args to parse with --json: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected clean --json flag to be parsed"

let test_clean_accepts_force_flag = fun _ctx ->
  match parse_clean [ "clean"; "--force" ] with
  | Error err -> Error ("expected clean args to parse with --force: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "force" then
        Ok ()
      else
        Error "expected clean --force flag to be parsed"

let test_clean_usage_explains_cache_gc_and_force = fun _ctx ->
  let usage = ArgParser.usage_string Riot_cli.Clean.command in
  if String.equal usage "Usage: clean [OPTIONS]" then
    Ok ()
  else
    Error ("expected clean usage to explain cache GC and --force, got: " ^ usage)

let test_run_defaults_remote_binary_to_repo_name = fun _ctx ->
  Test.assert_equal
    ~expected:"hello-world"
    ~actual:(Riot_cli.Run.default_remote_binary_name "leostera/hello-world");
  Test.assert_equal
    ~expected:"hello-world"
    ~actual:(Riot_cli.Run.default_remote_binary_name "github.com/leostera/hello-world/packages/demo");
  Ok ()

let test_run_rejects_trailing_remote_binary_separator = fun _ctx ->
  match Riot_cli.Run.run_with_workspace_info
    ~workspace:None
    ~workspace_error:None
    (
      parse_run [ "run"; "leostera/hello-world@" ]
      |> Result.expect ~msg:"expected run args to parse"
    ) with
  | Ok () -> Error "expected trailing @ remote target to fail"
  | Error (Failure message) ->
      if
        String.equal
          message
          "invalid remote target 'leostera/hello-world@': expected binary name after @"
      then
        Ok ()
      else
        Error ("unexpected trailing @ error: " ^ message)
  | Error err -> Error ("unexpected error kind: " ^ Exception.to_string err)

let make_workspace = fun binaries ->
  let package =
    Riot_model.Package.make
      ~name:(package_name "demo")
      ~path:(Path.v "/workspace/packages/demo")
      ~relative_path:(Path.v "packages/demo")
      ~binaries
      ()
  in
  Riot_model.Workspace.make_realized ~root:(Path.v "/workspace") ~packages:[ package ] ()

let make_workspace_with_packages = fun packages ->
  Riot_model.Workspace.make_realized
    ~root:(Path.v "/workspace")
    ~packages
    ()

let make_fix_provider = fun package_name ->
  Riot_model.Fix_provider.{
    name = Riot_model.Package_name.to_string package_name;
    package_name;
    package_path = Path.(Path.v "/workspace"
    / Path.v "packages"
    / Path.v (Riot_model.Package_name.to_string package_name));
    source_path = Path.(Path.v "/workspace"
    / Path.v "packages"
    / Path.v (Riot_model.Package_name.to_string package_name)
    / Path.v "fix"
    / Path.v "riot_fix_rules.ml");
    rules = [ Riot_model.Package_name.to_string package_name ^ ":demo-rule" ];
  }

let test_build_fix_providers_ignore_dependency_packages = fun _ctx ->
  let app_name = package_name "app" in
  let std_name = package_name "std" in
  let app =
    Riot_model.Package.make
      ~name:app_name
      ~path:(Path.v "/workspace/packages/app")
      ~relative_path:(Path.v "packages/app")
      ~fix_providers:[ make_fix_provider app_name ]
      ()
  in
  let std =
    Riot_model.Package.make
      ~name:std_name
      ~path:(Path.v "/registry/std")
      ~relative_path:(Path.v "../registry/std")
      ~fix_providers:[ make_fix_provider std_name ]
      ()
  in
  let providers =
    Riot_cli.Build.workspace_fix_providers (make_workspace_with_packages [ app; std ])
  in
  let provider_names =
    providers
    |> List.map
      ~fn:(fun (provider: Riot_model.Fix_provider.t) ->
        Riot_model.Package_name.to_string
          provider.package_name)
  in
  Test.assert_equal ~expected:[ "app" ] ~actual:provider_names;
  Ok ()

let test_run_build_scope_uses_runtime_for_runtime_binaries = fun _ctx ->
  let workspace =
    make_workspace [ Riot_model.Package.{ name = "demo"; path = Path.v "src/demo.ml" } ]
  in
  Test.assert_equal
    ~expected:Riot_cli.Build.Runtime
    ~actual:(Riot_cli.Run.build_scope_for_binary
      workspace
      ~package_name:(package_name "demo")
      ~binary_name:"demo");
  Ok ()

let test_run_build_scope_uses_dev_for_test_binaries = fun _ctx ->
  let workspace =
    make_workspace [ Riot_model.Package.{ name = "pm_tests"; path = Path.v "tests/pm_tests.ml" } ]
  in
  Test.assert_equal
    ~expected:Riot_cli.Build.Dev
    ~actual:(Riot_cli.Run.build_scope_for_binary
      workspace
      ~package_name:(package_name "demo")
      ~binary_name:"pm_tests");
  Ok ()

let test_run_build_scope_defaults_to_runtime_when_binary_is_missing = fun _ctx ->
  let workspace = make_workspace [] in
  Test.assert_equal
    ~expected:Riot_cli.Build.Runtime
    ~actual:(Riot_cli.Run.build_scope_for_binary
      workspace
      ~package_name:(package_name "demo")
      ~binary_name:"missing");
  Ok ()

let test_run_resolves_single_implicit_binary = fun _ctx ->
  let workspace =
    make_workspace
      [ Riot_model.Package.{ name = "hello-world"; path = Path.v "src/hello_world.ml" } ]
  in
  match Riot_cli.Run.resolve_implicit_local_target workspace with
  | Ok { package_name = resolved_package_name; binary_name } ->
      Test.assert_equal ~expected:(package_name "demo") ~actual:resolved_package_name;
      Test.assert_equal ~expected:"hello-world" ~actual:binary_name;
      Ok ()
  | Error err -> Error ("expected single implicit binary to resolve: " ^ err)

let test_run_resolves_single_implicit_binary_in_package = fun _ctx ->
  let demo =
    Riot_model.Package.make
      ~name:(package_name "demo")
      ~path:(Path.v "/workspace/packages/demo")
      ~relative_path:(Path.v "packages/demo")
      ~binaries:[ Riot_model.Package.{ name = "demo"; path = Path.v "src/demo.ml" } ]
      ()
  in
  let util =
    Riot_model.Package.make
      ~name:(package_name "util")
      ~path:(Path.v "/workspace/packages/util")
      ~relative_path:(Path.v "packages/util")
      ~binaries:[ Riot_model.Package.{ name = "util"; path = Path.v "src/util.ml" } ]
      ()
  in
  let workspace = make_workspace_with_packages [ demo; util ] in
  match Riot_cli.Run.resolve_implicit_local_target ~package_filter:(package_name "util") workspace with
  | Ok { package_name = resolved_package_name; binary_name } ->
      Test.assert_equal ~expected:(package_name "util") ~actual:resolved_package_name;
      Test.assert_equal ~expected:"util" ~actual:binary_name;
      Ok ()
  | Error err -> Error ("expected package-filtered implicit binary to resolve: " ^ err)

let test_run_rejects_ambiguous_implicit_binary = fun _ctx ->
  let demo =
    Riot_model.Package.make
      ~name:(package_name "demo")
      ~path:(Path.v "/workspace/packages/demo")
      ~relative_path:(Path.v "packages/demo")
      ~binaries:[ Riot_model.Package.{ name = "demo"; path = Path.v "src/demo.ml" } ]
      ()
  in
  let util =
    Riot_model.Package.make
      ~name:(package_name "util")
      ~path:(Path.v "/workspace/packages/util")
      ~relative_path:(Path.v "packages/util")
      ~binaries:[ Riot_model.Package.{ name = "util"; path = Path.v "src/util.ml" } ]
      ()
  in
  let workspace = make_workspace_with_packages [ demo; util ] in
  match Riot_cli.Run.resolve_implicit_local_target workspace with
  | Ok _ -> Error "expected implicit run target resolution to reject multiple binaries"
  | Error err ->
      if String.contains err "multiple runnable binaries found" then
        Ok ()
      else
        Error ("expected ambiguity error, got: " ^ err)

let test_run_reports_no_binaries_with_creation_hint = fun _ctx ->
  let workspace = make_workspace [] in
  match Riot_cli.Run.resolve_implicit_local_target workspace with
  | Ok _ -> Error "expected implicit run target resolution to reject missing binaries"
  | Error err ->
      if
        String.equal
          err
          "no runnable binaries found; pass a binary name or create one with `riot new --bin ./packages/my-binary`"
      then
        Ok ()
      else
        Error ("expected no-binaries hint, got: " ^ err)

let test_run_reports_package_without_binaries_with_creation_hint = fun _ctx ->
  let workspace = make_workspace [] in
  match Riot_cli.Run.resolve_implicit_local_target ~package_filter:(package_name "demo") workspace with
  | Ok _ ->
      Error "expected package-filtered implicit run target resolution to reject missing binaries"
  | Error err ->
      if
        String.equal
          err
          "package 'demo' has no runnable binaries; create one with `riot new --bin ./packages/my-binary`"
      then
        Ok ()
      else
        Error ("expected package no-binaries hint, got: " ^ err)

let test_pm_event_hides_workspace_resolved_packages = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual =
    Riot_cli.Build.format_pm_event
      ~seen_registry_updates
      (
        Riot_model.Event.PackageResolvedForBuild {
          package = package_name "create-riot-app";
          version = None;
          path = "/workspace";
          workspace = true;
        }
      )
  in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_hides_materialization_started = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual =
    Riot_cli.Build.format_pm_event
      ~seen_registry_updates
      (Riot_model.Event.PackageMaterializationStarted {
        package = package_name "std";
        version = "0.1.0";
        path = "/cache/std";
      })
  in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_hides_manifest_fetch_chatter = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual =
    Riot_cli.Build.format_pm_event
      ~seen_registry_updates
      (Riot_model.Event.PackageManifestFetchStarted {
        package = package_name "std";
        version = "0.1.0";
      })
  in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_hides_download_skipped = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual =
    Riot_cli.Build.format_pm_event
      ~seen_registry_updates
      (
        Riot_model.Event.PackageDownloadSkipped {
          package = package_name "std";
          version = "0.1.0";
          path = "/cache/std";
          reason = "already materialized";
        }
      )
  in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_shows_installing_with_padding = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual =
    Riot_cli.Build.format_pm_event
      ~seen_registry_updates
      (Riot_model.Event.SourceDependencyMaterializationStarted {
        source_locator = "leostera/hello-world";
        ref_ = None;
      })
  in
  Test.assert_equal ~expected:(Some "run installing leostera/hello-world") ~actual;
  Ok ()

let test_pm_event_shows_locked_package = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual =
    Riot_cli.Build.format_pm_event
      ~seen_registry_updates
      (Riot_model.Event.PackageVersionLocked { package = package_name "std"; version = "0.2.0" })
  in
  Test.assert_equal ~expected:(Some "ok locked std (0.2.0)") ~actual;
  Ok ()

let test_pm_event_shows_up_to_date = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual =
    Riot_cli.Build.format_pm_event
      ~seen_registry_updates
      (Riot_model.Event.PackageVersionsUnchanged { packages = 3 })
  in
  Test.assert_equal ~expected:(Some "ok dependencies are already up to date") ~actual;
  Ok ()

let tests =
  Test.[
    case
      "build: workspace package labels stay bare"
      test_display_package_name_keeps_workspace_package_bare;
    case
      "build: debug profile package labels stay bare"
      test_display_package_name_shows_debug_profile;
    case
      "build: non-debug profile package labels are explicit"
      test_display_package_name_shows_non_debug_profile;
    case
      "build: external package labels show version"
      test_display_package_name_shows_external_package_version;
    case
      "build: external package labels show version and target"
      test_display_package_name_shows_external_package_version_and_target;
    case
      "build: workspace test package labels show artifact and target"
      test_display_package_name_shows_workspace_test_and_target;
    case
      "build: workspace bench package labels show artifact"
      test_display_package_name_shows_workspace_bench;
    case
      "build: planning error lines explain internal module violations"
      test_planning_error_lines_describe_internal_module_violation;
    case
      "build: planning error lines explain undeclared package modules"
      test_planning_error_lines_describe_undeclared_package_module;
    case
      "build: planning error lines include module name suggestions"
      test_planning_error_lines_include_module_name_suggestions;
    case
      "build: planning error lines explain invalid executable main"
      test_planning_error_lines_describe_invalid_executable_main;
    case
      "build: workspace planning error lines explain missing dependencies"
      test_workspace_planning_error_lines_describe_missing_dependencies;
    case
      "build: planning error lines indent multiline reasons"
      test_planning_error_lines_indent_multiline_reasons;
    case
      "build: final failure lines render planning errors"
      test_build_failure_detail_lines_render_planning_errors;
    case "build: accept multiple package arguments" test_build_accepts_multiple_packages;
    case "build: usage shows repeated package flag" test_build_usage_shows_repeated_package_flag;
    case "build: reject positional package args" test_build_rejects_positional_package_args;
    case "build: parse --json flag" test_build_accepts_json_flag;
    case "build: parse --release flag" test_build_accepts_release_flag;
    case "build: parse --tests flag" test_build_accepts_tests_flag;
    case "build: parse --examples flag" test_build_accepts_examples_flag;
    case "build: parse --benches flag" test_build_accepts_benches_flag;
    case "build: parse --all flag" test_build_accepts_all_flag;
    case "build: parse --jobs flag" test_build_accepts_jobs_flag;
    case "build: parse --target-dir flag" test_build_accepts_target_dir_flag;
    case "build: reject invalid --jobs flag" test_build_rejects_invalid_jobs_flag;
    case "build: accept --jobs flag at runtime" test_build_accepts_jobs_flag_in_run;
    case "build: reject zero --jobs at runtime" test_build_rejects_zero_jobs_runtime;
    case "build: reject negative --jobs at runtime" test_build_rejects_negative_jobs_runtime;
    case "build: command accepts workspace" test_build_command_accepts_workspace;
    case "build: run accepts workspace" test_build_run_accepts_workspace;
    case
      ~size:Large
      "build: --tests compiles only test binaries"
      test_build_tests_compile_only_test_binaries;
    case
      ~size:Large
      "build: --examples compiles only example binaries"
      test_build_examples_compile_only_example_binaries;
    case
      ~size:Large
      "build: --benches compiles only bench binaries"
      test_build_benches_compile_only_bench_binaries;
    case
      ~size:Large
      "build: --all compiles all dev binaries"
      test_build_all_compiles_all_dev_binaries;
    case "test: parse --json flag" test_test_accepts_json_flag;
    case "test: parse --release flag" test_test_accepts_release_flag;
    case "test: parse --list flag" test_test_accepts_list_flag;
    case "bench: parse --json flag" test_bench_accepts_json_flag;
    case "bench: parse --release flag" test_bench_accepts_release_flag;
    case "bench: parse --list flag" test_bench_accepts_list_flag;
    case "bench: parse --iterations flag" test_bench_accepts_iterations_flag;
    case "bench: parse --warmup flag" test_bench_accepts_warmup_flag;
    case
      "bench: keep top-level flags after forwarded args"
      test_bench_invocation_args_keep_top_level_flags_after_forwarded_args;
    case "run: parse missing name" test_run_accepts_missing_name;
    case "run: parse --list flag" test_run_accepts_list_flag;
    case "run: parse --list --json flags" test_run_accepts_list_json_flag;
    case "run: parse --release flag" test_run_accepts_release_flag;
    case "run: parse --update flag" test_run_accepts_update_flag;
    case "run: parse rejects removed --trace flag" test_run_rejects_removed_trace_flag;
    case "trace: parse missing name" test_trace_accepts_missing_name;
    case "trace: parse --output option" test_trace_accepts_output_option;
    case "trace: parse --profiler option" test_trace_accepts_profiler_option;
    case "trace: parse output policy options" test_trace_accepts_output_policy_options;
    case "trace: parse sampling options" test_trace_accepts_sampling_options;
    case "trace: parse summary --json" test_trace_accepts_summary_json;
    case "trace: parse summary -f" test_trace_accepts_summary_filter;
    case "trace: parse call-tree -f" test_trace_accepts_call_tree_filter;
    case "install: parse missing name" test_install_accepts_missing_name;
    case "install: parse --update flag" test_install_accepts_update_flag;
    case "install: parse --package flag" test_install_accepts_package_flag;
    case "info: parse --json flag" test_info_accepts_json_flag;
    case "info: parse workspace target" test_info_accepts_workspace_target;
    case "info: parse package target" test_info_accepts_package_target;
    case "clean: parse --json flag" test_clean_accepts_json_flag;
    case "clean: parse --force flag" test_clean_accepts_force_flag;
    case "clean: usage explains cache gc and --force" test_clean_usage_explains_cache_gc_and_force;
    case
      "run: remote source defaults binary to repo name"
      test_run_defaults_remote_binary_to_repo_name;
    case
      "run: trailing @ in remote target is rejected"
      test_run_rejects_trailing_remote_binary_separator;
    case
      "run: runtime binaries use runtime scope"
      test_run_build_scope_uses_runtime_for_runtime_binaries;
    case "run: test binaries use dev scope" test_run_build_scope_uses_dev_for_test_binaries;
    case
      "run: missing binaries default to runtime scope"
      test_run_build_scope_defaults_to_runtime_when_binary_is_missing;
    case "run: single implicit binary resolves" test_run_resolves_single_implicit_binary;
    case
      "run: package-filtered implicit binary resolves"
      test_run_resolves_single_implicit_binary_in_package;
    case "run: ambiguous implicit binary is rejected" test_run_rejects_ambiguous_implicit_binary;
    case
      "run: missing binaries suggest creating one"
      test_run_reports_no_binaries_with_creation_hint;
    case
      "run: package with no binaries suggests creating one"
      test_run_reports_package_without_binaries_with_creation_hint;
    case
      "build: fix providers ignore dependency packages"
      test_build_fix_providers_ignore_dependency_packages;
    case
      "build: pm events hide workspace resolved packages"
      test_pm_event_hides_workspace_resolved_packages;
    case "build: pm materialization start is hidden" test_pm_event_hides_materialization_started;
    case "build: pm manifest fetch chatter is hidden" test_pm_event_hides_manifest_fetch_chatter;
    case "build: pm download skipped is hidden" test_pm_event_hides_download_skipped;
    case
      "build: pm installing source is shown with padding"
      test_pm_event_shows_installing_with_padding;
    case "build: pm locked package is shown" test_pm_event_shows_locked_package;
    case "build: pm no-op update is shown" test_pm_event_shows_up_to_date;
  ]

let name = "Riot CLI Build Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
