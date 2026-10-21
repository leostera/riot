open Std
open Riot_build

module Test = Std.Test
module Package_builder = Riot_build.Internal.Package_builder

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let target_set_of_strings = fun values ->
  values
  |> List.map ~fn:target
  |> Riot_model.Target.Set.from_list

let write_workspace_manifest = fun ~root ~members ->
  let members =
    members
    |> List.map ~fn:(fun member -> "  \"" ^ Path.to_string member ^ "\"")
    |> String.concat ",\n"
  in
  let content = "[workspace]\nmembers = [\n" ^ members ^ "\n]\n" in
  Fs.write content Path.(root / Path.v "riot.toml")
  |> Result.expect ~msg:"Write workspace riot.toml failed"

let write_toolchain_config = fun ~root ~targets ->
  let target_lines =
    targets
    |> List.map ~fn:(fun target -> "\"" ^ target ^ "\"")
    |> String.concat ", "
  in
  Fs.write
    ("[toolchain]\nversion = \""
    ^ Riot_model.Toolchain_config.default_ocaml_version
    ^ "\"\ntargets = ["
    ^ target_lines
    ^ "]\n")
    Path.(root / Path.v "ocaml-toolchain.toml")
  |> Result.expect ~msg:"Write ocaml-toolchain.toml failed"

let make_package = fun ~root ~name ~value ->
  let pkg_dir = Path.(root / Path.v name) in
  let package_name = package_name name in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"Create src failed";
  Fs.write ("let value = " ^ value ^ "\n") Path.(src_dir / Path.v "lib.ml")
  |> Result.expect ~msg:"Write ml failed";
  Fs.write
    ("[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n")
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"Write riot.toml failed";
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

let make_external_package = fun ~root ~name ~value ->
  let pkg_dir = Path.(root / Path.v "_deps" / Path.v name) in
  let package_name = package_name name in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"Create external src failed";
  Fs.write ("let value = " ^ value ^ "\n") Path.(src_dir / Path.v "lib.ml")
  |> Result.expect ~msg:"Write external ml failed";
  Riot_model.Package.make
    ~name:package_name
    ~path:pkg_dir
    ~relative_path:(Path.v ("../_deps/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ~sources:{
      src = [ Path.v "src/lib.ml" ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let map_with_index = fun items ~fn ->
  let rec loop index acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> acc
    | item :: rest -> loop (index + 1) (acc @ [ fn index item ]) rest
  in
  loop 0 [] items

let make_valid_workspace = fun ?(package_names = ["demo"]) ?toolchain_targets tmpdir ->
  let packages =
    map_with_index
      package_names
      ~fn:(fun index name ->
        make_package ~root:tmpdir ~name ~value:(Int.to_string (index + 1)))
  in
  write_workspace_manifest ~root:tmpdir ~members:(List.map package_names ~fn:Path.v);
  (
    match toolchain_targets with
    | Some targets -> write_toolchain_config ~root:tmpdir ~targets
    | None -> ()
  );
  Riot_model.Workspace.make_realized ~root:tmpdir ~packages ()

let make_workspace_with_sources = fun ?toolchain_targets ~root ~packages () ->
  let packages = List.map packages ~fn:(fun (name, value) -> make_package ~root ~name ~value) in
  write_workspace_manifest
    ~root
    ~members:(List.map packages ~fn:(fun (pkg: Riot_model.Package.t) -> pkg.relative_path));
  (
    match toolchain_targets with
    | Some targets -> write_toolchain_config ~root ~targets
    | None -> ()
  );
  Riot_model.Workspace.make_realized ~root ~packages ()

let make_two_targets = fun () ->
  let host_target = Riot_model.Target.current in
  let secondary_target =
    if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
      target "aarch64-unknown-linux-gnu"
    else
      target "x86_64-unknown-linux-gnu"
  in
  Riot_model.Target.Set.from_list [ host_target; secondary_target ]

let make_broken_workspace = fun ?target_dir tmpdir ->
  let pkg_dir = Path.(tmpdir / Path.v "demo") in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"Create src failed";
  Fs.write "let broken =" Path.(src_dir / Path.v "lib.ml")
  |> Result.expect ~msg:"Write broken ml failed";
  Fs.write
    "[package]\nname = \"demo\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n"
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"Write riot.toml failed";
  write_workspace_manifest ~root:tmpdir ~members:[ Path.v "demo" ];
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

let make_request = fun
  ~workspace
  ?(packages = [package_name "demo"])
  ?(targets = Riot_model.Target.Host)
  ?(scope = Riot_build.Request.Runtime)
  ?(profile = Riot_model.Profile.debug)
  ?(requested_parallelism = None)
  () ->
  Riot_build.Request.make
    ~workspace
    ~packages
    ~targets
    ~scope
    ~profile
    ~requested_parallelism
    ()

let build_request = fun ?on_event request -> Riot_build.build ?on_event request

let package_names = fun output ->
  Riot_build.Build_result.packages output
  |> List.map ~fn:Riot_build.Build_result.package_name

let sort_package_names = fun package_names ->
  List.sort
    package_names
    ~compare:Riot_model.Package_name.compare

let expect_int = fun ~label ~expected ~actual ->
  if Int.equal expected actual then
    Ok ()
  else
    Error (label ^ ": expected " ^ Int.to_string expected ^ ", got " ^ Int.to_string actual)

let expect_string_list = fun ~label ~expected ~actual ->
  let rec equal left right =
    match (left, right) with
    | ([], []) -> true
    | (left :: left_rest, right :: right_rest) ->
        String.equal left right && equal left_rest right_rest
    | (_, _) -> false
  in
  if equal expected actual then
    Ok ()
  else
    Error (label
    ^ ": expected ["
    ^ String.concat ", " expected
    ^ "], got ["
    ^ String.concat ", " actual
    ^ "]")

let phase_name = fun __tmp1 ->
  match __tmp1 with
  | Riot_build.Event.TargetsResolved _ -> "targets_resolved"
  | Riot_build.Event.ToolchainsEnsured _ -> "toolchains_ensured"
  | Riot_build.Event.ToolchainsValidated _ -> "toolchains_validated"
  | Riot_build.Event.RuntimeStarting -> "runtime_starting"
  | Riot_build.Event.RuntimeStarted -> "runtime_started"
  | Riot_build.Event.BuildLockWaiting _ -> "build_lock_waiting"
  | Riot_build.Event.BuildLanesPreparationStarted _ -> "build_lanes_preparation_started"
  | Riot_build.Event.BuildLanesPreparationFinished _ -> "build_lanes_preparation_finished"
  | Riot_build.Event.BuildUnitPlanCreated _ -> "build_unit_plan_created"
  | Riot_build.Event.BuildLanePreparationStarted _ -> "build_lane_preparation_started"
  | Riot_build.Event.BuildLaneLockAcquired _ -> "build_lane_lock_acquired"
  | Riot_build.Event.BuildLaneToolchainInitialized _ -> "build_lane_toolchain_initialized"
  | Riot_build.Event.BuildLaneStoreCreated _ -> "build_lane_store_created"
  | Riot_build.Event.BuildLanePreparationFinished _ -> "build_lane_preparation_finished"
  | Riot_build.Event.PackagePlanningStarted _ -> "package_planning_started"
  | Riot_build.Event.PackagePlanStarted _ -> "package_plan_started"
  | Riot_build.Event.PackagePlanSourceStarted _ -> "package_plan_source_started"
  | Riot_build.Event.PackagePlanFinished _ -> "package_plan_finished"
  | Riot_build.Event.PackagePlanningFinished _ -> "package_planning_finished"
  | Riot_build.Event.PackageActionGraphPlanned _ -> "package_action_graph_planned"
  | Riot_build.Event.PackageExecutionStarted _ -> "package_execution_started"
  | Riot_build.Event.PackageExecutionFinished _ -> "package_execution_finished"
  | Riot_build.Event.TargetBuildStarted _ -> "target_build_started"
  | Riot_build.Event.TargetBuildFinished _ -> "target_build_finished"
  | Riot_build.Event.CacheGenerationRecordingStarted _ -> "cache_generation_recording_started"
  | Riot_build.Event.CacheGenerationRecorded _ -> "cache_generation_recorded"
  | Riot_build.Event.ReturningResults _ -> "returning_results"

let build_phase = fun (event: Riot_build.Event.t) ->
  match event.kind with
  | Riot_model.Event.Build (Riot_model.Event.BuildPhase phase) -> Some phase
  | _ -> None

let build_event = fun (event: Riot_build.Event.t) ->
  match event.kind with
  | Riot_model.Event.Build event -> Some event
  | _ -> None

let expect_subsequence = fun ~haystack ~needle ->
  let rec loop haystack needle =
    match (haystack, needle) with
    | (_, []) -> Ok ()
    | ([], _) ->
        Error ("expected phase subsequence "
        ^ String.concat " -> " needle
        ^ " in "
        ^ String.concat " -> " haystack)
    | (actual :: haystack_rest, expected :: needle_rest) ->
        if String.equal actual expected then
          loop haystack_rest needle_rest
        else
          loop haystack_rest needle
  in
  loop haystack needle

let make_artifact = fun ?(exports = []) name ->
  {
    Riot_store.Artifact.input_hash = Crypto.hash_string (name ^ ":input");
    output_hash = Crypto.hash_string (name ^ ":output");
    size_bytes = 0L;
    files = [
      Riot_store.Manifest.{
        path = Path.v (name ^ ".cmx");
        hash = name ^ "-file-hash";
        size = 0;
      };
    ];
    ocamlc_warnings = [];
    exports;
  }

let make_build_result = fun ~scope ~(package:Riot_model.Package.t) ~status ->
  let artifact =
    match scope with
    | "build" -> Riot_planner.Build_unit.SyntheticTool { name = "build-core-test-tool" }
    | "dev" -> TestBinary { name = "build_core_tests" }
    | _ -> Library
  in
  {
    Package_builder.unit_key = ({
      package = package.name;
      artifact;
      target = Riot_model.Target.host ();
      profile = Riot_model.Profile.debug;
    }: Riot_planner.Build_unit.key);
    package;
    status;
    depset = [];
    ocamlc_warnings = [];
    duration = Time.Duration.zero;
  }

let make_runtime_build_result = fun ~package ~status ->
  make_build_result
    ~scope:"runtime"
    ~package
    ~status

let test_output_maps_build_result_statuses = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_output_statuses"
    (fun tmpdir ->
      let built_pkg = make_package ~root:tmpdir ~name:"built" ~value:"1" in
      let cached_pkg = make_package ~root:tmpdir ~name:"cached" ~value:"2" in
      let skipped_pkg = make_package ~root:tmpdir ~name:"skipped" ~value:"3" in
      let failed_pkg = make_package ~root:tmpdir ~name:"failed" ~value:"4" in
      let output =
        Riot_build.Build_result.from_build_results
          [
            make_build_result
              ~scope:"runtime"
              ~package:built_pkg
              ~status:(Package_builder.Built (make_artifact "built"));
            make_build_result
              ~scope:"runtime"
              ~package:cached_pkg
              ~status:(Package_builder.Cached (make_artifact "cached"));
            make_build_result
              ~scope:"runtime"
              ~package:skipped_pkg
              ~status:(Package_builder.Skipped { reason = "not requested" });
            make_build_result
              ~scope:"runtime"
              ~package:failed_pkg
              ~status:(Package_builder.Failed (Package_builder.ExecutionFailed { message = "boom" }));
          ]
      in
      let open Std.Result.Syntax in
      let expect_package name =
        match Riot_build.Build_result.find_package output (package_name name) with
        | Some package_output -> Ok package_output
        | None -> Error ("missing package output: " ^ name)
      in
      let* built = expect_package "built" in
      let* cached = expect_package "cached" in
      let* skipped = expect_package "skipped" in
      let* failed = expect_package "failed" in
      let* () =
        match Riot_build.Build_result.package_status built with
        | Riot_build.Build_result.Built _ -> Ok ()
        | _ -> Error "expected built package output"
      in
      let* () =
        match Riot_build.Build_result.package_status cached with
        | Riot_build.Build_result.Cached _ -> Ok ()
        | _ -> Error "expected cached package output"
      in
      let* () =
        match Riot_build.Build_result.package_status skipped with
        | Riot_build.Build_result.Skipped "not requested" -> Ok ()
        | _ -> Error "expected skipped package reason to be preserved"
      in
      match Riot_build.Build_result.package_status failed with
      | Riot_build.Build_result.Failed message when String.contains message "boom" -> Ok ()
      | _ -> Error "expected failed package message to be preserved") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_output_exposes_artifacts_and_exports = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_output_exports"
    (fun tmpdir ->
      let package = make_package ~root:tmpdir ~name:"demo" ~value:"1" in
      let artifact = {
        Riot_store.Artifact.input_hash = Crypto.hash_string "demo-input";
        output_hash = Crypto.hash_string "demo-output";
        size_bytes = 0L;
        files = [
          Riot_store.Manifest.{ path = Path.v "demo.exe"; hash = "demo-file-hash"; size = 0 };
        ];
        ocamlc_warnings = [];
        exports = [
          { Riot_store.Manifest.name = "demo"; path = "demo.exe"; action_hash = "abc123" };
        ];
      }
      in
      let output =
        Riot_build.Build_result.from_build_results
          [ make_runtime_build_result ~package ~status:(Package_builder.Built artifact) ]
      in
      match Riot_build.Build_result.find_package output (package_name "demo") with
      | None -> Error "missing package output: demo"
      | Some package_output ->
          match Riot_build.Build_result.package_artifact package_output with
          | None -> Error "expected built package artifact to be preserved"
          | Some found_artifact ->
              if
                not
                  (String.equal
                    (Crypto.Digest.hex found_artifact.input_hash)
                    (Crypto.Digest.hex artifact.input_hash))
              then
                Error "expected built package artifact hash to be preserved"
              else
                match Riot_build.Build_result.find_export package_output "demo" with
                | None -> Error "expected built package export to be preserved"
                | Some export_entry when String.equal export_entry.path "demo.exe" -> Ok ()
                | Some _ -> Error "expected built package export path to be preserved") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_output_prefers_dev_scope_and_merges_exports = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_output_scope_merge"
    (fun tmpdir ->
      let package = make_package ~root:tmpdir ~name:"demo" ~value:"1" in
      let build_artifact = make_artifact "demo_build" in
      let runtime_artifact = make_artifact "demo_runtime" in
      let dev_artifact =
        make_artifact
          ~exports:[
            {
              Riot_store.Manifest.name = "build_core_tests";
              path = "build_core_tests";
              action_hash = "dev-export";
            };
          ]
          "demo_dev"
      in
      let output =
        Riot_build.Build_result.from_build_results
          [
            make_build_result ~scope:"build" ~package ~status:(Package_builder.Built build_artifact);
            make_build_result
              ~scope:"runtime"
              ~package
              ~status:(Package_builder.Cached runtime_artifact);
            make_build_result ~scope:"dev" ~package ~status:(Package_builder.Built dev_artifact);
          ]
      in
      let package_outputs = Riot_build.Build_result.packages output in
      Test.assert_equal ~expected:1 ~actual:(List.length package_outputs);
      match Riot_build.Build_result.find_package output (package_name "demo") with
      | None -> Error "missing merged package output: demo"
      | Some package_output ->
          let open Std.Result.Syntax in
          let* () =
            match Riot_build.Build_result.package_status package_output with
            | Riot_build.Build_result.Built artifact when Crypto.Hash.equal
              artifact.input_hash
              dev_artifact.input_hash -> Ok ()
            | _ -> Error "expected dev-scoped artifact to win merged package status"
          in
          match Riot_build.Build_result.find_export package_output "build_core_tests" with
          | Some export_entry when String.equal export_entry.path "build_core_tests" -> Ok ()
          | Some _ -> Error "expected merged package exports to preserve dev suite binary"
          | None -> Error "expected merged package exports to include dev suite binary") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_returns_successful_output = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_success"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      match build_request (make_request ~workspace:prepared_workspace ()) with
      | Error err -> Error ("expected build to succeed, got: " ^ Riot_build.error_message err)
      | Ok output -> (
          match Riot_build.Build_result.find_package output (package_name "demo") with
          | Some package_output -> (
              match Riot_build.Build_result.package_status package_output with
              | Riot_build.Build_result.Built _
              | Riot_build.Build_result.Cached _ -> Ok ()
              | Riot_build.Build_result.Skipped reason ->
                  Error ("expected successful package output, got skipped: " ^ reason)
              | Riot_build.Build_result.Failed message ->
                  Error ("expected successful package output, got failure: " ^ message)
            )
          | None -> Error "expected output for package demo"
        )) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_uses_all_packages_when_none_are_requested = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_all_packages"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace ~package_names:[ "util"; "demo" ] tmpdir in
      match build_request (make_request ~workspace:prepared_workspace ~packages:[] ()) with
      | Error err -> Error ("expected build to succeed, got: " ^ Riot_build.error_message err)
      | Ok output ->
          Test.assert_equal
            ~expected:(sort_package_names [ package_name "demo"; package_name "util" ])
            ~actual:(
              package_names output
              |> sort_package_names
            );
          Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_default_build_resolution_targets_workspace_members_only = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_resolve_workspace_members"
    (fun tmpdir ->
      let app = make_package ~root:tmpdir ~name:"app" ~value:"1" in
      let std = make_external_package ~root:tmpdir ~name:"std" ~value:"2" in
      let workspace = Riot_model.Workspace.make_realized ~root:tmpdir ~packages:[ app; std ] () in
      let request = make_request ~workspace ~packages:[] ~scope:Riot_build.Request.Dev () in
      let context =
        Riot_build.Internal.Build_context.make request
        |> Result.expect ~msg:"expected valid build context"
      in
      let resolved =
        Riot_build.Internal.Resolved_build.resolve context request
        |> Result.expect ~msg:"expected build request to resolve"
      in
      Test.assert_equal
        ~expected:[ package_name "app" ]
        ~actual:(Riot_build.Internal.Resolved_build.package_names resolved);
      Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_returns_outputs_for_requested_packages = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_requested_packages"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace ~package_names:[ "demo"; "util" ] tmpdir in
      match build_request
        (make_request ~workspace:prepared_workspace ~packages:[ package_name "util" ] ()) with
      | Error err -> Error ("expected build to succeed, got: " ^ Riot_build.error_message err)
      | Ok output ->
          Test.assert_equal ~expected:[ package_name "util" ] ~actual:(package_names output);
          Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_reports_missing_single_package = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_missing_package"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace ~package_names:[ "demo"; "util" ] tmpdir in
      match build_request
        (make_request ~workspace:prepared_workspace ~packages:[ package_name "missing" ] ()) with
      | Error (
        Riot_build.PackageNotFound { package_name = missing_package; available_packages }
      ) ->
          Test.assert_equal ~expected:(package_name "missing") ~actual:missing_package;
          Test.assert_equal
            ~expected:(sort_package_names [ package_name "demo"; package_name "util" ])
            ~actual:(sort_package_names available_packages);
          Ok ()
      | Error err ->
          Error ("expected package-not-found error, got: " ^ Riot_build.error_message err)
      | Ok _ -> Error "expected missing package build to fail") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_reports_missing_multiple_packages = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_missing_packages"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace ~package_names:[ "demo"; "util" ] tmpdir in
      match build_request
        (make_request
          ~workspace:prepared_workspace
          ~packages:[ package_name "demo"; package_name "missing"; package_name "other" ]
          ()) with
      | Error (
        Riot_build.PackagesNotFound { package_names = missing_packages; available_packages }
      ) ->
          Test.assert_equal
            ~expected:(sort_package_names [ package_name "missing"; package_name "other" ])
            ~actual:(sort_package_names missing_packages);
          Test.assert_equal
            ~expected:(sort_package_names [ package_name "demo"; package_name "util" ])
            ~actual:(sort_package_names available_packages);
          Ok ()
      | Error err ->
          Error ("expected packages-not-found error, got: " ^ Riot_build.error_message err)
      | Ok _ -> Error "expected missing packages build to fail") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_reports_target_selection_failures_before_execution = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_target_miss"
    (fun tmpdir ->
      let prepared_workspace =
        make_valid_workspace ~toolchain_targets:[ "aarch64-apple-darwin" ] tmpdir
      in
      match build_request
        (make_request ~workspace:prepared_workspace ~targets:(Riot_model.Target.parse "linux") ()) with
      | Error (Riot_build.TargetSelectionFailed { pattern; _ }) when String.equal pattern "linux" ->
          Ok ()
      | Error err -> Error ("expected target-selection error, got: " ^ Riot_build.error_message err)
      | Ok _ -> Error "expected target selection to fail before execution") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_failure_surfaces_compiler_errors = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_failure"
    (fun tmpdir ->
      let prepared_workspace = make_broken_workspace tmpdir in
      match build_request (make_request ~workspace:prepared_workspace ()) with
      | Error (Riot_build.BuildFailed { errors }) ->
          if List.length errors > 0 then
            Ok ()
          else
            Error "expected at least one build error"
      | Error err -> Error ("expected compiler failure, got: " ^ Riot_build.error_message err)
      | Ok _ -> Error "expected broken package build to fail") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_can_return_cached_outputs_on_repeat_builds = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_cached_output"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      let request = make_request ~workspace:prepared_workspace () in
      let _ =
        build_request request
        |> Result.expect ~msg:"expected first build to succeed"
      in
      match build_request request with
      | Error err ->
          Error ("expected second build to succeed, got: " ^ Riot_build.error_message err)
      | Ok output -> (
          match Riot_build.Build_result.find_package output (package_name "demo") with
          | Some package_output -> (
              match Riot_build.Build_result.package_status package_output with
              | Riot_build.Build_result.Cached _ -> Ok ()
              | Riot_build.Build_result.Built _ -> Error "expected repeated build to be cached"
              | Riot_build.Build_result.Skipped reason ->
                  Error ("expected cached package output, got skipped: " ^ reason)
              | Riot_build.Build_result.Failed message ->
                  Error ("expected cached package output, got failure: " ^ message)
            )
          | None -> Error "expected build output for package demo"
        )) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_cached_build_does_not_emit_generation_recording_events = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_cached_generation"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      let request = make_request ~workspace:prepared_workspace () in
      let _ =
        build_request request
        |> Result.expect ~msg:"expected first build to succeed"
      in
      let saw_generation_event = ref false in
      match build_request
        ~on_event:(fun __tmp1 ->
          match build_phase __tmp1 with
          | Some (Riot_build.Event.CacheGenerationRecordingStarted _
          | Riot_build.Event.CacheGenerationRecorded _) ->
              saw_generation_event := true
          | _ -> ())
        request with
      | Error err ->
          Error ("expected second build to succeed, got: " ^ Riot_build.error_message err)
      | Ok _ ->
          Test.assert_false !saw_generation_event;
          Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_emits_runtime_phases_in_order = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_phase_order"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      let seen = ref [] in
      match build_request
        ~on_event:(fun __tmp1 ->
          match build_phase __tmp1 with
          | Some phase -> seen := !seen @ [ phase_name phase ]
          | _ -> ())
        (make_request ~workspace:prepared_workspace ()) with
      | Error err -> Error ("expected build to succeed, got: " ^ Riot_build.error_message err)
      | Ok _ ->
          expect_subsequence
            ~haystack:!seen
            ~needle:[
              "targets_resolved";
              "toolchains_ensured";
              "toolchains_validated";
              "runtime_starting";
              "runtime_started";
              "build_lanes_preparation_started";
              "build_unit_plan_created";
              "build_lane_preparation_started";
              "build_lane_lock_acquired";
              "build_lane_toolchain_initialized";
              "build_lane_store_created";
              "build_lane_preparation_finished";
              "build_lanes_preparation_finished";
              "target_build_started";
              "package_planning_started";
              "package_planning_finished";
              "package_execution_started";
              "package_execution_finished";
              "target_build_finished";
              "returning_results";
            ]) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_emits_detailed_build_telemetry = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_telemetry"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      let seen = ref [] in
      match build_request
        ~on_event:(fun __tmp1 ->
          match build_event __tmp1 with
          | Some (Riot_model.Event.BuildPackageCompilationStarted _) ->
              seen := !seen @ [ "compilation_started" ]
          | Some (Riot_model.Event.BuildPackageFinished _) -> seen := !seen @ [ "build_completed" ]
          | _ -> ())
        (make_request ~workspace:prepared_workspace ()) with
      | Error err -> Error ("expected build to succeed, got: " ^ Riot_build.error_message err)
      | Ok _ ->
          expect_subsequence ~haystack:!seen ~needle:[ "compilation_started"; "build_completed" ]) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_preserves_exact_target_subset = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_exact_targets"
    (fun tmpdir ->
      let requested_targets = make_two_targets () in
      let expected_targets = Riot_model.Target.Set.to_list requested_targets in
      let expected_target_count = List.length expected_targets in
      let prepared_workspace =
        make_valid_workspace
          ~toolchain_targets:(
            expected_targets
            |> List.map ~fn:(fun target -> Riot_model.Target.to_string target)
            |> List.sort ~compare:String.compare
          )
          tmpdir
      in
      let target_count = ref None in
      let started_count = ref 0 in
      let finished_count = ref 0 in
      match build_request
        ~on_event:(fun __tmp1 ->
          match build_phase __tmp1 with
          | Some (Riot_build.Event.TargetsResolved { target_count = count }) ->
              target_count := Some count
          | Some (Riot_build.Event.TargetBuildStarted _) -> started_count := !started_count + 1
          | Some (Riot_build.Event.TargetBuildFinished _) -> finished_count := !finished_count + 1
          | _ -> ())
        (make_request
          ~workspace:prepared_workspace
          ~targets:(Riot_model.Target.Exact requested_targets)
          ()) with
      | Error err ->
          Error ("expected exact target build to succeed, got: " ^ Riot_build.error_message err)
      | Ok _ ->
          match !target_count with
          | Some count ->
              let open Std.Result.Syntax in
              let* () =
                expect_int
                  ~label:"resolved target count"
                  ~expected:expected_target_count
                  ~actual:count
              in
              let* () =
                expect_int
                  ~label:"started target count"
                  ~expected:expected_target_count
                  ~actual:!started_count
              in
              expect_int
                ~label:"finished target count"
                ~expected:expected_target_count
                ~actual:!finished_count
          | None -> Error "expected targets_resolved event") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_multi_target_outputs_and_events = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_multi_target_runtime"
    (fun tmpdir ->
      let host_target = Riot_model.Target.current in
      let secondary_target =
        if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
          target "aarch64-unknown-linux-gnu"
        else
          target "x86_64-unknown-linux-gnu"
      in
      let requested_targets = Riot_model.Target.Set.from_list [ host_target; secondary_target ] in
      let expected_targets = Riot_model.Target.Set.to_list requested_targets in
      let expected_target_count = List.length expected_targets in
      let expected_target_names =
        List.map expected_targets ~fn:(fun target -> Riot_model.Target.to_string target)
        |> List.sort ~compare:String.compare
      in
      let expected_profile = Riot_model.Profile.debug in
      let prepared_workspace =
        make_valid_workspace ~toolchain_targets:expected_target_names tmpdir
      in
      let seen_target_count = ref None in
      let started_targets = ref [] in
      let finished_targets = ref [] in
      let finished_counts = ref [] in
      let any_partial_failure = ref false in
      match build_request
        ~on_event:(fun __tmp1 ->
          match build_phase __tmp1 with
          | Some (Riot_build.Event.TargetsResolved { target_count }) ->
              seen_target_count := Some target_count
          | Some (Riot_build.Event.TargetBuildStarted { target }) ->
              started_targets := Riot_model.Target.to_string target :: !started_targets
          | Some (
            Riot_build.Event.TargetBuildFinished { target; result_count; had_partial_failure }
          ) ->
              finished_targets := Riot_model.Target.to_string target :: !finished_targets;
              finished_counts := result_count :: !finished_counts;
              any_partial_failure := !any_partial_failure || had_partial_failure
          | _ -> ())
        (make_request
          ~workspace:prepared_workspace
          ~targets:(Riot_model.Target.Exact requested_targets)
          ()) with
      | Error err ->
          Error ("expected multi-target build to succeed, got: " ^ Riot_build.error_message err)
      | Ok output ->
          let sort_target_names = List.sort ~compare:String.compare in
          match !seen_target_count with
          | None -> Error "expected targets_resolved event"
          | Some count ->
              let open Std.Result.Syntax in
              let* () =
                expect_int
                  ~label:"resolved target count"
                  ~expected:expected_target_count
                  ~actual:count
              in
              let* () =
                expect_int
                  ~label:"started target count"
                  ~expected:expected_target_count
                  ~actual:(List.length !started_targets)
              in
              let* () =
                expect_int
                  ~label:"finished target count"
                  ~expected:expected_target_count
                  ~actual:(List.length !finished_targets)
              in
              let started_sorted = sort_target_names !started_targets in
              let finished_sorted = sort_target_names !finished_targets in
              let* () =
                expect_string_list
                  ~label:"started targets"
                  ~expected:expected_target_names
                  ~actual:started_sorted
              in
              let* () =
                expect_string_list
                  ~label:"finished targets"
                  ~expected:expected_target_names
                  ~actual:finished_sorted
              in
              Test.assert_false !any_partial_failure;
              let build_result_status =
                if not (List.all !finished_counts ~fn:(fun result_count -> result_count = 1)) then
                  Error ("expected each target lane to report one package result, got "
                  ^ String.concat ", " (List.map !finished_counts ~fn:Int.to_string))
                else
                  (
                    match Riot_build.Build_result.find_package output (package_name "demo") with
                    | None -> Error "expected output for package demo"
                    | Some package_output ->
                        match Riot_build.Build_result.package_status package_output with
                        | Riot_build.Build_result.Built _
                        | Riot_build.Build_result.Cached _ -> Ok ()
                        | Riot_build.Build_result.Skipped reason ->
                            Error ("expected demo package to be built, got skipped: " ^ reason)
                        | Riot_build.Build_result.Failed message ->
                            Error ("expected demo package to be built, got failed: " ^ message)
                  )
              in
              match build_result_status with
              | Error err -> Error err
              | Ok () ->
                  let validate_output_dirs =
                    List.map
                      expected_targets
                      ~fn:(fun target ->
                        Riot_model.Riot_dirs.out_dir_in_workspace
                          ~workspace:prepared_workspace
                          ~profile:expected_profile.name
                          ~target
                        |> fun out_dir -> Path.(out_dir / Path.v "demo"))
                  in
                  let missing_dirs =
                    List.filter
                      validate_output_dirs
                      ~fn:(fun output_dir ->
                        not
                          (
                            Fs.exists output_dir
                            |> Result.unwrap_or ~default:false
                          ))
                  in
                  if not (List.is_empty missing_dirs) then
                    Error ("expected package output directories to exist for each target: "
                    ^ String.concat ", " (List.map missing_dirs ~fn:Path.to_string))
                  else
                    Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_multi_target_partial_failures_fail_by_default = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_multi_target_partial_fail_default"
    (fun tmpdir ->
      let requested_targets = make_two_targets () in
      let expected_target_count = Riot_model.Target.Set.length requested_targets in
      let expected_targets =
        Riot_model.Target.Set.to_list requested_targets
        |> List.map ~fn:(fun target -> Riot_model.Target.to_string target)
        |> List.sort ~compare:String.compare
      in
      let prepared_workspace =
        make_workspace_with_sources
          ~root:tmpdir
          ~toolchain_targets:expected_targets
          ~packages:[ ("good", "let value = 2\n"); ("bad", "let broken ="); ]
          ()
      in
      let seen_target_count = ref None in
      let started_targets = ref [] in
      let finished_targets = ref [] in
      let partial_failure_flags = ref [] in
      let saw_returning_results = ref false in
      match build_request
        ~on_event:(fun __tmp1 ->
          match build_phase __tmp1 with
          | Some (Riot_build.Event.TargetsResolved { target_count }) ->
              seen_target_count := Some target_count
          | Some (Riot_build.Event.TargetBuildStarted { target }) ->
              started_targets := Riot_model.Target.to_string target :: !started_targets
          | Some (Riot_build.Event.TargetBuildFinished { had_partial_failure; target; _ }) ->
              finished_targets := Riot_model.Target.to_string target :: !finished_targets;
              partial_failure_flags := had_partial_failure :: !partial_failure_flags
          | Some (Riot_build.Event.ReturningResults _) -> saw_returning_results := true
          | _ -> ())
        (make_request
          ~workspace:prepared_workspace
          ~targets:(Riot_model.Target.Exact requested_targets)
          ~packages:[ package_name "good"; package_name "bad" ]
          ()) with
      | Ok _ -> Error "expected partial failure to make build fail by default"
      | Error (Riot_build.BuildFailed _) -> (
          match !seen_target_count with
          | None -> Error "expected targets_resolved event"
          | Some count ->
              let open Std.Result.Syntax in
              let* () =
                expect_int
                  ~label:"resolved target count"
                  ~expected:expected_target_count
                  ~actual:count
              in
              let* () =
                expect_int
                  ~label:"started target count"
                  ~expected:expected_target_count
                  ~actual:(List.length !started_targets)
              in
              let* () =
                expect_int
                  ~label:"finished target count"
                  ~expected:expected_target_count
                  ~actual:(List.length !finished_targets)
              in
              let* () =
                expect_int
                  ~label:"partial failure flag count"
                  ~expected:expected_target_count
                  ~actual:(List.length !partial_failure_flags)
              in
              if not (List.all !partial_failure_flags ~fn:(fun partial -> partial)) then
                Error "expected each target build to report partial failures"
              else if !saw_returning_results then
                Error "expected no returning_results event when default multi-target build fails"
              else
                let sort_target_names = List.sort ~compare:String.compare in
                let* () =
                  expect_string_list
                    ~label:"started targets"
                    ~expected:expected_targets
                    ~actual:(sort_target_names !started_targets)
                in
                expect_string_list
                  ~label:"finished targets"
                  ~expected:expected_targets
                  ~actual:(sort_target_names !finished_targets)
        )
      | Error err ->
          Error ("expected build to fail with BuildFailed, got: " ^ Riot_build.error_message err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_rejects_zero_jobs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_zero_jobs"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      match build_request
        (make_request ~workspace:prepared_workspace ~requested_parallelism:(Some 0) ()) with
      | Ok _ -> Error "expected zero jobs request to fail"
      | Error (Riot_build.InvalidRequestedParallelism _) -> Ok ()
      | Error err ->
          Error ("expected InvalidRequestedParallelism, got: " ^ Riot_build.error_message err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_rejects_zero_jobs_with_message = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_zero_jobs_message"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      match build_request
        (make_request ~workspace:prepared_workspace ~requested_parallelism:(Some 0) ()) with
      | Ok _ -> Error "expected zero jobs request to fail"
      | Error (Riot_build.InvalidRequestedParallelism _ as err) when String.equal
        (Riot_build.error_message err)
        "invalid requested parallelism (0): jobs must be >= 1" -> Ok ()
      | Error err ->
          Error ("expected specific message for invalid jobs, got: " ^ Riot_build.error_message err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_rejects_negative_jobs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_negative_jobs"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      match build_request
        (make_request ~workspace:prepared_workspace ~requested_parallelism:(Some (-1)) ()) with
      | Ok _ -> Error "expected negative jobs request to fail"
      | Error (Riot_build.InvalidRequestedParallelism _) -> Ok ()
      | Error err ->
          Error ("expected InvalidRequestedParallelism, got: " ^ Riot_build.error_message err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_rejects_negative_jobs_with_message = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_negative_jobs_message"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      match build_request
        (make_request ~workspace:prepared_workspace ~requested_parallelism:(Some (-1)) ()) with
      | Ok _ -> Error "expected negative jobs request to fail"
      | Error (Riot_build.InvalidRequestedParallelism _ as err) when String.equal
        (Riot_build.error_message err)
        "invalid requested parallelism (-1): jobs must be >= 1" -> Ok ()
      | Error err ->
          Error ("expected specific message for invalid jobs, got: " ^ Riot_build.error_message err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_accepts_positive_jobs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_high_jobs"
    (fun tmpdir ->
      let prepared_workspace = make_valid_workspace tmpdir in
      let requested_parallelism = Thread.available_parallelism * 2 + 3 in
      match build_request
        (make_request
          ~workspace:prepared_workspace
          ~requested_parallelism:(Some requested_parallelism)
          ()) with
      | Ok _ -> Ok ()
      | Error err ->
          Error ("expected high jobs request to be accepted, got: " ^ Riot_build.error_message err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests = let open Test in
[
  case "build core: output maps build result statuses" test_output_maps_build_result_statuses;
  case
    "build core: output preserves artifacts and exports"
    test_output_exposes_artifacts_and_exports;
  case
    "build core: output prefers dev scope and merges exports"
    test_output_prefers_dev_scope_and_merges_exports;
  case
    ~size:Large
    "build core: build returns successful output"
    test_build_returns_successful_output;
  case
    ~size:Large
    "build core: build uses all packages when none are requested"
    test_build_uses_all_packages_when_none_are_requested;
  case
    "build core: default build resolution targets workspace members only"
    test_default_build_resolution_targets_workspace_members_only;
  case
    ~size:Large
    "build core: build returns outputs for requested packages"
    test_build_returns_outputs_for_requested_packages;
  case "build core: build reports missing single package" test_build_reports_missing_single_package;
  case
    "build core: build reports missing multiple packages"
    test_build_reports_missing_multiple_packages;
  case
    "build core: build reports target selection failures before execution"
    test_build_reports_target_selection_failures_before_execution;
  case
    "build core: build failure surfaces compiler errors"
    test_build_failure_surfaces_compiler_errors;
  case
    ~size:Large
    "build core: repeated builds return cached outputs"
    test_build_can_return_cached_outputs_on_repeat_builds;
  case
    ~size:Large
    "build core: cached builds do not emit generation recording events"
    test_cached_build_does_not_emit_generation_recording_events;
  case
    ~size:Large
    "build core: build emits runtime phases in order"
    test_build_emits_runtime_phases_in_order;
  case
    ~size:Large
    "build core: build emits detailed telemetry events"
    test_build_emits_detailed_build_telemetry;
  case
    ~size:Large
    "build core: build preserves exact target subsets"
    test_build_preserves_exact_target_subset;
  case
    ~size:Large
    "build core: build emits multi-target lane outputs and events"
    test_build_multi_target_outputs_and_events;
  case
    ~size:Large
    "build core: default partial failure defaults to failing in multi-target builds"
    test_build_multi_target_partial_failures_fail_by_default;
  case "build core: rejects zero jobs requests" test_build_rejects_zero_jobs;
  case
    "build core: rejects zero jobs requests with clear message"
    test_build_rejects_zero_jobs_with_message;
  case "build core: rejects negative jobs requests" test_build_rejects_negative_jobs;
  case
    "build core: rejects negative jobs requests with clear message"
    test_build_rejects_negative_jobs_with_message;
  case ~size:Large "build core: accepts positive jobs requests" test_build_accepts_positive_jobs;
]

let name = "Riot Build Core Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
