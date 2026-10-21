open Std

module Test = Std.Test
module Build_runtime = Riot_build.Internal.Build_runtime
module Build_context = Riot_build.Internal.Build_context
module Resolved_build = Riot_build.Internal.Resolved_build
module Package_builder = Riot_build.Internal.Package_builder

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let build_phase = fun (event: Riot_build.Event.t) ->
  match event.kind with
  | Riot_model.Event.Build (Riot_model.Event.BuildPhase phase) -> Some phase
  | _ -> None

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

let make_package = fun ~root ~name ~source ->
  let pkg_dir = Path.(root / Path.v name) in
  let package_name = package_name name in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"Create src failed";
  Fs.write source Path.(src_dir / Path.v "lib.ml")
  |> Result.expect ~msg:"Write source failed";
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

let make_workspace = fun ?target_dir ?toolchain_targets ~root ~packages () ->
  write_workspace_manifest
    ~root
    ~members:(List.map packages ~fn:(fun (pkg: Riot_model.Package.t) -> pkg.relative_path));
  (
    match toolchain_targets with
    | Some targets -> write_toolchain_config ~root ~targets
    | None -> ()
  );
  Riot_model.Workspace.make_realized ~root ?target_dir ~packages ()

let make_valid_workspace = fun ?target_dir ?toolchain_targets tmpdir ->
  let package = make_package ~root:tmpdir ~name:"demo" ~source:"let value = 42\n" in
  make_workspace ?target_dir ?toolchain_targets ~root:tmpdir ~packages:[ package ] ()

let make_workspace_with_sources = fun ?toolchain_targets ~root ~packages () ->
  let packages = List.map packages ~fn:(fun (name, source) -> make_package ~root ~name ~source) in
  make_workspace ?toolchain_targets ~root ~packages ()

let load_prepared_workspace = fun ~root ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan workspace_manager root with
  | Error err ->
      Error ("workspace scan failed: " ^ Riot_model.Workspace_manager.scan_error_message err)
  | Ok (workspace, load_errors) ->
      if not (List.is_empty load_errors) then
        Error "workspace scan produced load errors"
      else
        let registry =
          Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:"pkgs.ml" ()
          |> Result.expect ~msg:"registry init failed"
        in
        Riot_deps.ensure_workspace
          ~workspace_manager
          ~mode:Riot_deps.Dep_solver.Refresh
          ~registry
          ~workspace
          ()
        |> Result.map_err ~fn:Riot_model.Pm_error.message

let write_path_dependency_package = fun ~root ~dir_name ~package_name ->
  let pkg_dir = Path.(root / Path.v dir_name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"Create path dependency src failed";
  Fs.write "let value = 42\n" Path.(src_dir / Path.v (package_name ^ ".ml"))
  |> Result.expect ~msg:"Write path dependency source failed";
  Fs.write
    ("[package]\nname = \""
    ^ package_name
    ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/"
    ^ package_name
    ^ ".ml\"\n")
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"Write path dependency riot.toml failed"

let write_app_package_with_path_dependency = fun ~root ~dep_path ->
  let pkg_dir = Path.(root / Path.v "packages" / Path.v "app") in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"Create app src failed";
  Fs.write "let value = Dep.value\n" Path.(src_dir / Path.v "app.ml")
  |> Result.expect ~msg:"Write app source failed";
  Fs.write
    ("[package]\nname = \"app\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/app.ml\"\n\n[dependencies]\n"
    ^ "dep = { path = \""
    ^ dep_path
    ^ "\" }\n")
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"Write app riot.toml failed"

let write_app_package_with_build_dependency = fun ~root ~dep_path ->
  let pkg_dir = Path.(root / Path.v "packages" / Path.v "app") in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"Create app src failed";
  Fs.write "let value = 42\n" Path.(src_dir / Path.v "app.ml")
  |> Result.expect ~msg:"Write app source failed";
  Fs.write
    ("[package]\nname = \"app\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/app.ml\"\n\n[build-dependencies]\n"
    ^ "dep = { path = \""
    ^ dep_path
    ^ "\" }\n")
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"Write app riot.toml failed"

let make_request = fun ~workspace ?(profile = Riot_model.Profile.debug) () ->
  Riot_build.Request.make
    ~workspace
    ~packages:[ package_name "demo" ]
    ~targets:Riot_model.Target.Host
    ~scope:Riot_build.Request.Runtime
    ~profile
    ()

let make_runtime_request = fun
  ~workspace
  ~package_names
  ~targets
  ?(scope = Riot_build.Request.Runtime)
  ?(profile = Riot_model.Profile.debug)
  ?(requested_parallelism = None)
  () ->
  Riot_build.Request.make
    ~workspace
    ~packages:package_names
    ~targets:(Riot_model.Target.Exact targets)
    ~scope
    ~profile
    ~requested_parallelism
    ()

let make_runtime_inputs = fun
  ?on_event ~workspace ~package_names ~targets ?scope ?profile ?requested_parallelism () ->
  let request =
    make_runtime_request
      ~workspace
      ~package_names
      ~targets
      ?scope
      ?profile
      ?requested_parallelism
      ()
  in
  let context =
    Build_context.make ?on_event request
    |> Result.expect ~msg:"expected build context creation to succeed"
  in
  let resolved =
    Resolved_build.resolve context request
    |> Result.expect ~msg:"expected build intent resolution to succeed"
  in
  (context, resolved)

let build_request = fun request -> Riot_build.build request

let write_nested_udp_workspace = fun ~root ~creation_order ->
  let pkg_dir = Path.(root / Path.v "demo") in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let net_dir = Path.(src_dir / Path.v "net") in
  Fs.create_dir_all net_dir
  |> Result.expect ~msg:"Create nested src failed";
  write_workspace_manifest ~root ~members:[ Path.v "demo" ];
  Fs.write
    "[package]\nname = \"demo\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/demo.ml\"\n"
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"Write nested riot.toml failed";
  let file_for_key = fun __tmp1 ->
    match __tmp1 with
    | "demo_ml" -> (Path.(src_dir / Path.v "demo.ml"), "module Net = Net\n")
    | "net_ml" -> (
      Path.(net_dir / Path.v "net.ml"),
      "module Udp_socket = Udp_socket\nmodule Udp_server = Udp_server\n"
    )
    | "udp_socket_mli" -> (Path.(net_dir / Path.v "udp_socket.mli"), "type t\n")
    | "udp_socket_ml" -> (Path.(net_dir / Path.v "udp_socket.ml"), "type t = unit\n")
    | "udp_server_mli" -> (
      Path.(net_dir / Path.v "udp_server.mli"),
      "type handler = socket:Udp_socket.t -> bytes -> unit\nval run : handler -> unit\n"
    )
    | "udp_server_ml" -> (
      Path.(net_dir / Path.v "udp_server.ml"),
      "type handler = socket:Udp_socket.t -> bytes -> unit\nlet run _ = ()\n"
    )
    | key -> panic ("unknown nested udp workspace file key: " ^ key)
  in
  List.for_each
    creation_order
    ~fn:(fun key ->
      let (path, contents) = file_for_key key in
      Fs.write contents path
      |> Result.expect ~msg:("Write nested source failed: " ^ key))

let test_release_build_uses_release_lane = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_release_runtime"
    (fun tmpdir ->
      let workspace = make_valid_workspace tmpdir in
      let host_target = Riot_model.Riot_dirs.host_target () in
      let release_package_dir =
        Riot_model.Riot_dirs.out_dir_in_workspace ~workspace ~profile:"release" ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo")
      in
      let debug_package_dir =
        Riot_model.Riot_dirs.out_dir_in_workspace ~workspace ~profile:"debug" ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo")
      in
      match build_request (make_request ~workspace ~profile:Riot_model.Profile.release ()) with
      | Error err ->
          Error ("expected release build to succeed, got: " ^ Riot_build.error_message err)
      | Ok _ ->
          if not
            (
              Fs.exists release_package_dir
              |> Result.unwrap_or ~default:false
            ) then
            Error ("expected release output under " ^ Path.to_string release_package_dir)
          else if Fs.exists debug_package_dir
          |> Result.unwrap_or ~default:false then
            Error ("did not expect debug output under " ^ Path.to_string debug_package_dir)
          else
            Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_build_respects_custom_target_dir = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_custom_target_runtime"
    (fun tmpdir ->
      let workspace = make_valid_workspace ~target_dir:(Path.v "build-out") tmpdir in
      let host_target = Riot_model.Riot_dirs.host_target () in
      let release_package_dir =
        Riot_model.Riot_dirs.out_dir_in_workspace ~workspace ~profile:"release" ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo")
      in
      let default_release_dir =
        Riot_model.Riot_dirs.out_dir_with_target
          ~workspace_root:workspace.root
          ~profile:"release"
          ~target:host_target
        |> fun out_dir -> Path.(out_dir / Path.v "demo")
      in
      match build_request (make_request ~workspace ~profile:Riot_model.Profile.release ()) with
      | Error err ->
          Error ("expected custom-target build to succeed, got: " ^ Riot_build.error_message err)
      | Ok _ ->
          if not
            (
              Fs.exists release_package_dir
              |> Result.unwrap_or ~default:false
            ) then
            Error ("expected release output under custom target dir "
            ^ Path.to_string release_package_dir)
          else if Fs.exists default_release_dir
          |> Result.unwrap_or ~default:false then
            Error ("did not expect output under default build dir "
            ^ Path.to_string default_release_dir)
          else
            Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_nested_udp_workspace_builds_across_file_creation_orders = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_nested_udp_runtime"
    (fun tmpdir ->
      let orders = [
        (
          "canonical",
          [
            "demo_ml";
            "net_ml";
            "udp_socket_mli";
            "udp_socket_ml";
            "udp_server_mli";
            "udp_server_ml";
          ]
        );
        (
          "server_first",
          [
            "udp_server_mli";
            "udp_server_ml";
            "udp_socket_mli";
            "udp_socket_ml";
            "net_ml";
            "demo_ml";
          ]
        );
        (
          "socket_impl_first",
          [
            "udp_socket_ml";
            "net_ml";
            "demo_ml";
            "udp_server_ml";
            "udp_socket_mli";
            "udp_server_mli";
          ]
        );
        (
          "mixed",
          [
            "net_ml";
            "udp_server_mli";
            "demo_ml";
            "udp_socket_mli";
            "udp_server_ml";
            "udp_socket_ml";
          ]
        );
      ]
      in
      let rec run = fun __tmp1 ->
        match __tmp1 with
        | [] -> Ok ()
        | (order_name, creation_order) :: rest ->
            let root = Path.(tmpdir / Path.v order_name) in
            Fs.create_dir_all root
            |> Result.expect ~msg:"Create order root failed";
            write_nested_udp_workspace ~root ~creation_order;
            let workspace_manager = Riot_model.Workspace_manager.create () in
            begin
              match Riot_model.Workspace_manager.scan workspace_manager root with
              | Error err ->
                  Error ("workspace scan failed for creation order "
                  ^ order_name
                  ^ ": "
                  ^ Riot_model.Workspace_manager.scan_error_message err)
              | Ok (workspace, load_errors) ->
                  if not (List.is_empty load_errors) then
                    Error ("workspace scan had load errors for creation order " ^ order_name)
                  else
                    let registry =
                      Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:"pkgs.ml" ()
                      |> Result.expect ~msg:"registry init failed"
                    in
                    let prepared_workspace =
                      Riot_deps.ensure_workspace
                        ~workspace_manager
                        ~mode:Riot_deps.Dep_solver.Refresh
                        ~registry
                        ~workspace
                        ()
                      |> Result.expect ~msg:"workspace prepare failed"
                    in
                    match build_request (make_request ~workspace:prepared_workspace ()) with
                    | Ok _ -> run rest
                    | Error err ->
                        Error ("nested udp build failed for creation order "
                        ^ order_name
                        ^ ": "
                        ^ Riot_build.error_message err)
            end
      in
      run orders) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_manifest_path_dependency_change_invalidates_package_cache = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_dep_path_runtime"
    (fun tmpdir ->
      write_workspace_manifest ~root:tmpdir ~members:[ Path.v "packages/app" ];
      write_path_dependency_package ~root:tmpdir ~dir_name:"dep-one" ~package_name:"dep";
      write_path_dependency_package ~root:tmpdir ~dir_name:"dep-two" ~package_name:"dep";
      write_app_package_with_path_dependency ~root:tmpdir ~dep_path:"../../dep-one";
      let prepared_workspace =
        load_prepared_workspace ~root:tmpdir
        |> Result.expect ~msg:"expected initial workspace preparation to succeed"
      in
      let request =
        make_runtime_request
          ~workspace:prepared_workspace
          ~package_names:[ package_name "app" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ()
      in
      let _ =
        build_request request
        |> Result.expect ~msg:"expected initial build to succeed"
      in
      write_app_package_with_path_dependency ~root:tmpdir ~dep_path:"../../dep-two";
      let prepared_workspace =
        load_prepared_workspace ~root:tmpdir
        |> Result.expect ~msg:"expected refreshed workspace preparation to succeed"
      in
      let request =
        make_runtime_request
          ~workspace:prepared_workspace
          ~package_names:[ package_name "app" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ()
      in
      match build_request request with
      | Error err ->
          Error ("expected second build to succeed, got: " ^ Riot_build.error_message err)
      | Ok output ->
          match Riot_build.Build_result.find_package output (package_name "app") with
          | None -> Error "expected second build output for package app"
          | Some package_output ->
              match Riot_build.Build_result.package_status package_output with
              | Riot_build.Build_result.Built _ -> Ok ()
              | Riot_build.Build_result.Cached _ ->
                  Error "expected path dependency manifest change to invalidate the app package cache"
              | Riot_build.Build_result.Skipped reason ->
                  Error ("expected app package to rebuild, got skipped: " ^ reason)
              | Riot_build.Build_result.Failed message ->
                  Error ("expected app package to rebuild, got failure: " ^ message)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_manifest_build_dependency_path_change_invalidates_package_cache = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_build_dep_path_runtime"
    (fun tmpdir ->
      write_workspace_manifest ~root:tmpdir ~members:[ Path.v "packages/app" ];
      write_path_dependency_package ~root:tmpdir ~dir_name:"dep-one" ~package_name:"dep";
      write_path_dependency_package ~root:tmpdir ~dir_name:"dep-two" ~package_name:"dep";
      write_app_package_with_build_dependency ~root:tmpdir ~dep_path:"../../dep-one";
      let prepared_workspace =
        load_prepared_workspace ~root:tmpdir
        |> Result.expect ~msg:"expected initial workspace preparation to succeed"
      in
      let request =
        make_runtime_request
          ~workspace:prepared_workspace
          ~package_names:[ package_name "app" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ()
      in
      let _ =
        build_request request
        |> Result.expect ~msg:"expected initial build to succeed"
      in
      write_app_package_with_build_dependency ~root:tmpdir ~dep_path:"../../dep-two";
      let prepared_workspace =
        load_prepared_workspace ~root:tmpdir
        |> Result.expect ~msg:"expected refreshed workspace preparation to succeed"
      in
      let request =
        make_runtime_request
          ~workspace:prepared_workspace
          ~package_names:[ package_name "app" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ()
      in
      match build_request request with
      | Error err ->
          Error ("expected second build to succeed, got: " ^ Riot_build.error_message err)
      | Ok output ->
          match Riot_build.Build_result.find_package output (package_name "app") with
          | None -> Error "expected second build output for package app"
          | Some package_output ->
              match Riot_build.Build_result.package_status package_output with
              | Riot_build.Build_result.Built _ -> Ok ()
              | Riot_build.Build_result.Cached _ ->
                  Error "expected build dependency path change to invalidate the app package cache"
              | Riot_build.Build_result.Skipped reason ->
                  Error ("expected app package to rebuild, got skipped: " ^ reason)
              | Riot_build.Build_result.Failed message ->
                  Error ("expected app package to rebuild, got failure: " ^ message)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_rejects_invalid_parallelism = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_invalid_parallelism_runtime"
    (fun tmpdir ->
      let workspace = make_valid_workspace tmpdir in
      let request =
        make_runtime_request
          ~workspace
          ~package_names:[ package_name "demo" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ~scope:Riot_build.Request.Runtime
          ~profile:Riot_model.Profile.debug
          ~requested_parallelism:(Some 0)
          ()
      in
      match Build_context.make request with
      | Error (Build_context.InvalidRequestedParallelism 0) -> Ok ()
      | Error err -> (
          match err with
          | Build_context.InvalidRequestedParallelism requested ->
              Error ("expected invalid parallelism 0, got " ^ Int.to_string requested)
        )
      | Ok _ -> Error "expected invalid parallelism to reject context creation") with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_does_not_record_cache_generation_when_disabled = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_no_cache_recording"
    (fun tmpdir ->
      let workspace = make_valid_workspace tmpdir in
      let saw_cache_event = ref false in
      let (context, spec) =
        make_runtime_inputs
          ~workspace
          ~package_names:[ package_name "demo" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ~on_event:(fun event ->
            match build_phase event with
            | Some (Riot_build.Event.CacheGenerationRecordingStarted _
            | Riot_build.Event.CacheGenerationRecorded _) ->
                saw_cache_event := true
            | _ -> ())
          ()
      in
      match Riot_build.Internal.Build_runtime.execute ~record_cache_generation:false context spec with
      | Error err -> Error ("expected build to succeed, got: " ^ Build_runtime.error_message err)
      | Ok _ ->
          Test.assert_false !saw_cache_event;
          Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_partial_failures_by_default = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_default_partial_failure"
    (fun tmpdir ->
      let workspace =
        make_workspace_with_sources
          ~root:tmpdir
          ~packages:[ ("good", "let value = 1\n"); ("bad", "let broken ="); ]
          ()
      in
      let bad = package_name "bad" in
      let saw_returning_results = ref false in
      let saw_partial_failure = ref false in
      let (context, spec) =
        make_runtime_inputs
          ~workspace
          ~package_names:[ package_name "good"; package_name "bad" ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ~requested_parallelism:(Some 1)
          ~on_event:(fun event ->
            match build_phase event with
            | Some (Riot_build.Event.TargetBuildFinished { had_partial_failure }) ->
                saw_partial_failure := had_partial_failure
            | Some (Riot_build.Event.ReturningResults _) -> saw_returning_results := true
            | _ -> ())
          ()
      in
      match Riot_build.Internal.Build_runtime.execute context spec with
      | Ok _ -> Error "expected default build to fail when any package fails"
      | Error (Build_runtime.BuildFailed { errors }) ->
          let output = Riot_build.Build_result.from_build_results errors in
          let bad_output = Riot_build.Build_result.find_package output bad in
          let has_bad_failure =
            match bad_output with
            | Some bad_package -> (
                match Riot_build.Build_result.package_status bad_package with
                | Riot_build.Build_result.Failed _ -> true
                | _ -> false
              )
            | None -> false
          in
          if not has_bad_failure then
            Error "expected failure output to include failed bad package"
          else if not !saw_partial_failure then
            Error "expected target build finished event to report partial failure"
          else if !saw_returning_results then
            Error "expected returning results event not to fire on build failure"
          else
            Ok ()
      | Error err -> Error ("expected BuildFailed, got: " ^ Build_runtime.error_message err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_allows_partial_failures = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_allow_partial_failures"
    (fun tmpdir ->
      let workspace =
        make_workspace_with_sources
          ~root:tmpdir
          ~packages:[ ("good", "let value = 2\n"); ("bad", "let broken ="); ]
          ()
      in
      let good = package_name "good" in
      let bad = package_name "bad" in
      let (context, spec) =
        make_runtime_inputs
          ~workspace
          ~package_names:[ good; bad ]
          ~targets:(Riot_model.Target.make_set [ Riot_model.Target.current ])
          ~requested_parallelism:(Some 1)
          ~on_event:(fun _ -> ())
          ()
      in
      let result =
        Riot_build.Internal.Build_runtime.execute ~allow_partial_failures:true context spec
      in
      (
        match result with
        | Error err ->
            Error ("expected partial failures to be returned, got: "
            ^ Build_runtime.error_message err)
        | Ok results ->
            let build_output = Riot_build.Build_result.from_build_results results in
            let good_result = Riot_build.Build_result.find_package build_output good in
            let bad_result = Riot_build.Build_result.find_package build_output bad in
            (
              match good_result with
              | None -> Error "expected good package result"
              | Some good_result -> (
                  match Riot_build.Build_result.package_status good_result with
                  | Riot_build.Build_result.Built _
                  | Riot_build.Build_result.Cached _ -> (
                      match bad_result with
                      | None -> Error "expected bad package result"
                      | Some bad_result -> (
                          match Riot_build.Build_result.package_status bad_result with
                          | Riot_build.Build_result.Failed _ -> Ok ()
                          | _ ->
                              Error "expected bad package result to be failed with allow_partial_failures"
                        )
                    )
                  | Riot_build.Build_result.Skipped _
                  | Riot_build.Build_result.Failed _ ->
                      Error "expected good package result to be successful"
                )
            )
      )) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_allows_multi_target_partial_failures = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_allow_partial_failures_multi_target"
    (fun tmpdir ->
      let host_target = Riot_model.Target.current in
      let secondary_target =
        if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
          target "aarch64-unknown-linux-gnu"
        else
          target "x86_64-unknown-linux-gnu"
      in
      let requested_targets = Riot_model.Target.Set.from_list [ host_target; secondary_target ] in
      let expected_targets =
        requested_targets
        |> Riot_model.Target.Set.to_list
        |> List.map ~fn:Riot_model.Target.to_string
        |> List.sort ~compare:String.compare
      in
      let expected_target_count = List.length expected_targets in
      let workspace =
        make_workspace_with_sources
          ~root:tmpdir
          ~toolchain_targets:expected_targets
          ~packages:[ ("good", "let value = 2\n"); ("bad", "let broken ="); ]
          ()
      in
      let good = package_name "good" in
      let bad = package_name "bad" in
      let started_targets = ref [] in
      let finished_targets = ref [] in
      let finished_counts = ref [] in
      let partial_flags = ref [] in
      let (context, spec) =
        make_runtime_inputs
          ~workspace
          ~package_names:[ good; bad ]
          ~targets:requested_targets
          ~requested_parallelism:(Some 1)
          ~on_event:(fun event ->
            match build_phase event with
            | Some (Riot_build.Event.TargetBuildStarted { target }) ->
                started_targets := Riot_model.Target.to_string target :: !started_targets
            | Some (
              Riot_build.Event.TargetBuildFinished {
                target;
                result_count;
                had_partial_failure = partial;
              }
            ) ->
                finished_targets := Riot_model.Target.to_string target :: !finished_targets;
                finished_counts := result_count :: !finished_counts;
                partial_flags := partial :: !partial_flags
            | _ -> ())
          ()
      in
      match Riot_build.Internal.Build_runtime.execute ~allow_partial_failures:true context spec with
      | Error err ->
          Error ("expected partial failures to be returned, got: " ^ Build_runtime.error_message err)
      | Ok results ->
          if not (Int.equal (List.length !started_targets) expected_target_count) then
            Error ("expected " ^ Int.to_string expected_target_count ^ " target builds started")
          else if not (Int.equal (List.length !finished_targets) expected_target_count) then
            Error ("expected " ^ Int.to_string expected_target_count ^ " target builds finished")
          else if not (Int.equal (List.length !finished_counts) expected_target_count) then
            Error ("expected " ^ Int.to_string expected_target_count ^ " target result_count events")
          else if not (List.all !finished_counts ~fn:(fun count -> count = 2)) then
            Error ("expected each lane to report two package results, got "
            ^ String.concat ", " (List.map !finished_counts ~fn:Int.to_string))
          else if not (List.all !partial_flags ~fn:(fun partial -> partial)) then
            Error "expected each lane to report partial failures"
          else
            let sort_target_names = List.sort ~compare:String.compare in
            let started_sorted = sort_target_names !started_targets in
            let finished_sorted = sort_target_names !finished_targets in
            let rec equal_target_lists left right =
              match (left, right) with
              | ([], []) -> true
              | (left :: left_rest, right :: right_rest) ->
                  String.equal left right && equal_target_lists left_rest right_rest
              | (_, _) -> false
            in
            if not (equal_target_lists expected_targets started_sorted) then
              Error ("expected target starts to include " ^ String.concat ", " expected_targets)
            else if not (equal_target_lists expected_targets finished_sorted) then
              Error ("expected target finishes to include " ^ String.concat ", " expected_targets)
            else
              let build_output = Riot_build.Build_result.from_build_results results in
              let good_output = Riot_build.Build_result.find_package build_output good in
              let bad_output = Riot_build.Build_result.find_package build_output bad in
              let good_ok =
                match good_output with
                | Some package_output -> (
                    match Riot_build.Build_result.package_status package_output with
                    | Riot_build.Build_result.Built _
                    | Riot_build.Build_result.Cached _ -> true
                    | _ -> false
                  )
                | None -> false
              in
              let bad_ok =
                match bad_output with
                | Some package_output -> (
                    match Riot_build.Build_result.package_status package_output with
                    | Riot_build.Build_result.Failed _ -> true
                    | _ -> false
                  )
                | None -> false
              in
              if not good_ok then
                Error "expected merged output to include successful good package"
              else if not bad_ok then
                Error "expected merged output to include failed bad package"
              else
                Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_multi_target_reports_global_returning_results = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_multi_target_returning_results"
    (fun tmpdir ->
      let host_target = Riot_model.Target.current in
      let secondary_target =
        if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
          target "aarch64-unknown-linux-gnu"
        else
          target "x86_64-unknown-linux-gnu"
      in
      let requested_targets = Riot_model.Target.Set.from_list [ host_target; secondary_target ] in
      let target_count = List.length (Riot_model.Target.Set.to_list requested_targets) in
      let expected_return_count = target_count * 2 in
      let returning_event = ref None in
      let workspace =
        make_workspace_with_sources
          ~root:tmpdir
          ~toolchain_targets:(
            Riot_model.Target.Set.to_list requested_targets
            |> List.map ~fn:Riot_model.Target.to_string
          )
          ~packages:[ ("good", "let value = 2\n"); ("bad", "let broken ="); ]
          ()
      in
      let (context, spec) =
        make_runtime_inputs
          ~workspace
          ~package_names:[ package_name "good"; package_name "bad" ]
          ~targets:requested_targets
          ~requested_parallelism:(Some 1)
          ~on_event:(fun event ->
            match build_phase event with
            | Some (Riot_build.Event.ReturningResults { result_count; had_partial_failure }) ->
                returning_event := Some (result_count, had_partial_failure)
            | _ -> ())
          ()
      in
      match Riot_build.Internal.Build_runtime.execute ~allow_partial_failures:true context spec with
      | Error err ->
          Error ("expected partial failures to be returned, got: " ^ Build_runtime.error_message err)
      | Ok _ ->
          match !returning_event with
          | None -> Error "expected returning results event"
          | Some (result_count, had_partial_failure) ->
              if not (Int.equal result_count expected_return_count) then
                Error ("expected returning result count "
                ^ Int.to_string expected_return_count
                ^ ", got "
                ^ Int.to_string result_count)
              else if not had_partial_failure then
                Error "expected returning results to report partial failure"
              else
                Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_multi_target_all_success_reports_aggregated_results = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_multi_target_success_returning"
    (fun tmpdir ->
      let host_target = Riot_model.Target.current in
      let secondary_target =
        if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
          target "aarch64-unknown-linux-gnu"
        else
          target "x86_64-unknown-linux-gnu"
      in
      let requested_targets = Riot_model.Target.Set.from_list [ host_target; secondary_target ] in
      let expected_return_count =
        (List.length (Riot_model.Target.Set.to_list requested_targets)) * 2
      in
      let returning_event = ref None in
      let workspace =
        make_workspace_with_sources
          ~root:tmpdir
          ~toolchain_targets:(
            Riot_model.Target.Set.to_list requested_targets
            |> List.map ~fn:Riot_model.Target.to_string
          )
          ~packages:[ ("good", "let value = 2\n"); ("nice", "let answer = 42\n"); ]
          ()
      in
      let (context, spec) =
        make_runtime_inputs
          ~workspace
          ~package_names:[ package_name "good"; package_name "nice" ]
          ~targets:requested_targets
          ~requested_parallelism:(Some 1)
          ~on_event:(fun event ->
            match build_phase event with
            | Some (Riot_build.Event.ReturningResults { result_count; had_partial_failure }) ->
                returning_event := Some (result_count, had_partial_failure)
            | _ -> ())
          ()
      in
      match Riot_build.Internal.Build_runtime.execute context spec with
      | Error err -> Error ("expected build to succeed, got: " ^ Build_runtime.error_message err)
      | Ok _ ->
          match !returning_event with
          | None -> Error "expected returning results event"
          | Some (result_count, had_partial_failure) ->
              if not (Int.equal result_count expected_return_count) then
                Error ("expected returning result count "
                ^ Int.to_string expected_return_count
                ^ ", got "
                ^ Int.to_string result_count)
              else if had_partial_failure then
                Error "expected returning results to report no partial failure"
              else
                Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_multi_target_partial_failures_skip_cache_recording = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_multi_target_partial_skip_cache"
    (fun tmpdir ->
      let host_target = Riot_model.Target.current in
      let secondary_target =
        if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
          target "aarch64-unknown-linux-gnu"
        else
          target "x86_64-unknown-linux-gnu"
      in
      let requested_targets = Riot_model.Target.Set.from_list [ host_target; secondary_target ] in
      let workspace =
        make_workspace_with_sources
          ~root:tmpdir
          ~toolchain_targets:(
            Riot_model.Target.Set.to_list requested_targets
            |> List.map ~fn:Riot_model.Target.to_string
          )
          ~packages:[ ("good", "let value = 2\n"); ("bad", "let broken ="); ]
          ()
      in
      let saw_cache_event = ref false in
      let (context, spec) =
        make_runtime_inputs
          ~workspace
          ~package_names:[ package_name "good"; package_name "bad" ]
          ~targets:requested_targets
          ~requested_parallelism:(Some 1)
          ~on_event:(fun event ->
            match build_phase event with
            | Some (Riot_build.Event.CacheGenerationRecordingStarted _
            | Riot_build.Event.CacheGenerationRecorded _) ->
                saw_cache_event := true
            | _ -> ())
          ()
      in
      match Riot_build.Internal.Build_runtime.execute ~allow_partial_failures:true context spec with
      | Error err ->
          Error ("expected build to succeed with allow flag, got: "
          ^ Build_runtime.error_message err)
      | Ok _ ->
          if !saw_cache_event then
            Error "expected partial failures to skip cache generation recording"
          else
            Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_execute_multi_target_success_records_cache_generation = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_multi_target_cache_generation"
    (fun tmpdir ->
      let host_target = Riot_model.Target.current in
      let secondary_target =
        if String.equal (Riot_model.Target.to_string host_target) "x86_64-unknown-linux-gnu" then
          target "aarch64-unknown-linux-gnu"
        else
          target "x86_64-unknown-linux-gnu"
      in
      let requested_targets = Riot_model.Target.Set.from_list [ host_target; secondary_target ] in
      let workspace =
        make_workspace_with_sources
          ~root:tmpdir
          ~toolchain_targets:(
            Riot_model.Target.Set.to_list requested_targets
            |> List.map ~fn:Riot_model.Target.to_string
          )
          ~packages:[ ("good", "let value = 2\n"); ("nice", "let answer = 42\n"); ]
          ()
      in
      let recording_started = ref false in
      let recording_recorded = ref false in
      let (context, spec) =
        make_runtime_inputs
          ~workspace
          ~package_names:[ package_name "good"; package_name "nice" ]
          ~targets:requested_targets
          ~requested_parallelism:(Some 1)
          ~on_event:(fun event ->
            match build_phase event with
            | Some (Riot_build.Event.CacheGenerationRecordingStarted _) -> recording_started := true
            | Some (Riot_build.Event.CacheGenerationRecorded _) -> recording_recorded := true
            | _ -> ())
          ()
      in
      match Riot_build.Internal.Build_runtime.execute context spec with
      | Error err -> Error ("expected successful build, got: " ^ Build_runtime.error_message err)
      | Ok _ ->
          if not !recording_started then
            Error "expected cache generation recording to start"
          else if not !recording_recorded then
            Error "expected cache generation recorded event"
          else
            Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests = let open Test in
[
  case
    ~size:Large
    "build runtime: release builds use the release lane"
    test_release_build_uses_release_lane;
  case
    ~size:Large
    "build runtime: custom target_dir is respected"
    test_build_respects_custom_target_dir;
  case
    ~size:Large
    "build runtime: nested udp workspace succeeds across file creation orders"
    test_nested_udp_workspace_builds_across_file_creation_orders;
  case
    ~size:Large
    "build runtime: manifest path dependency change invalidates package cache"
    test_manifest_path_dependency_change_invalidates_package_cache;
  case
    ~size:Large
    "build runtime: manifest build dependency path change invalidates package cache"
    test_manifest_build_dependency_path_change_invalidates_package_cache;
  case "build runtime: rejects invalid parallelism" test_execute_rejects_invalid_parallelism;
  case
    ~size:Large
    "build runtime: execute does not record cache generation when disabled"
    test_execute_does_not_record_cache_generation_when_disabled;
  case
    ~size:Large
    "build runtime: partial failures fail by default"
    test_execute_partial_failures_by_default;
  case
    ~size:Large
    "build runtime: allow partial failures returns partial results"
    test_execute_allows_partial_failures;
  case
    ~size:Large
    "build runtime: multi-target partial failures can succeed with allow flag"
    test_execute_allows_multi_target_partial_failures;
  case
    ~size:Large
    "build runtime: multi-target partial build returns aggregated returning results"
    test_execute_multi_target_reports_global_returning_results;
  case
    ~size:Large
    "build runtime: multi-target successful build returns aggregated returning results"
    test_execute_multi_target_all_success_reports_aggregated_results;
  case
    ~size:Large
    "build runtime: multi-target partial failures skip cache recording"
    test_execute_multi_target_partial_failures_skip_cache_recording;
  case
    ~size:Large
    "build runtime: multi-target success records cache generation"
    test_execute_multi_target_success_records_cache_generation;
]

let name = "Riot Build Runtime Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
