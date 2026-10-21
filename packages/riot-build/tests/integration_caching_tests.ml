open Std
open Riot_build
open Std.Collections
open Riot_model

module Action_scheduler = Riot_build.Internal.Action_scheduler
module Sandbox = Riot_build.Internal.Sandbox
module Test = Std.Test

let package_name = fun value ->
  Package_name.from_string value
  |> Result.expect ~msg:("expected valid package name: " ^ value)

let test_toolchain = fun () ->
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"failed to initialize toolchain"

let test_build_target = Riot_model.Target.current

let make_workspace = fun ?(packages = []) root ->
  Riot_model.Workspace.make_realized
    ~root
    ~packages
    ~target_dir:(Path.v "target")
    ()

let workspace_dependency = fun name ->
  Riot_model.Package.{
    name = package_name name;
    source =
      {
        workspace = true;
        builtin = false;
        path = None;
        source_locator = None;
        ref_ = None;
        version = None;
      };
  }

let make_package = fun ~root ~name ->
  let package_name = package_name name in
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Riot_model.Package.make
    ~name:package_name
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_library_package = fun ~root ~name ?interface ?(dependencies = []) implementation ->
  let package_name = package_name name in
  let path = Path.(root / Path.v "packages" / Path.v name) in
  let src_dir = Path.(path / Path.v "src") in
  let _ =
    Fs.create_dir_all src_dir
    |> Result.expect ~msg:"failed to create src dir"
  in
  let impl_rel = Path.v ("src/" ^ name ^ ".ml") in
  let impl_path = Path.(path / impl_rel) in
  let _ =
    Fs.write implementation impl_path
    |> Result.expect ~msg:"failed to write impl"
  in
  let srcs =
    match interface with
    | None -> [ impl_rel ]
    | Some signature ->
        let intf_rel = Path.v ("src/" ^ name ^ ".mli") in
        let intf_path = Path.(path / intf_rel) in
        let _ =
          Fs.write signature intf_path
          |> Result.expect ~msg:"failed to write interface"
        in
        [ intf_rel; impl_rel ]
  in
  Riot_model.Package.make
    ~name:package_name
    ~path
    ~relative_path:(Path.v ("packages/" ^ name))
    ~dependencies:(List.map dependencies ~fn:workspace_dependency)
    ~library:{ path = impl_rel }
    ~sources:{
      src = srcs;
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let make_graph_with_write = fun ~package ~content ->
  let graph = Riot_planner.Action_graph.create () in
  let spec =
    Riot_planner.Action_node.make
      ~actions:[ Riot_planner.Action.WriteFile { destination = Path.v "out.txt"; content } ]
      ~outs:[ Path.v "out.txt" ]
      ~srcs:[]
      ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  let node = Riot_planner.Action_graph.add_node graph spec in
  (graph, node)

let execute_graph = fun ~workspace ~store ~package ~graph ->
  let sandbox = Sandbox.create ~workspace () ~package_name:package.Riot_model.Package.name in
  let result =
    Action_scheduler.run
      ~action_graph:graph
      ~sandbox
      ~store
      ~session_id:(Riot_model.Session_id.make ())
      ~build_target:test_build_target
      ~on_event:(fun _ -> ())
      (test_toolchain ())
      ~concurrency:1
  in
  let output = Path.(Sandbox.get_dir sandbox / Path.v "out.txt") in
  let output_content = Fs.read_to_string output in
  let output_exists =
    Fs.exists output
    |> Result.unwrap_or ~default:false
  in
  let sandbox_dir = Sandbox.get_dir sandbox in
  let _ = Sandbox.cleanup sandbox in
  (result, output_exists, output_content, sandbox_dir)

let build_package_artifact = fun ~workspace package ->
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
      | None ->
          Error ("expected package result for " ^ Riot_model.Package_name.to_string package.name)
      | Some package_result ->
          match Riot_build.Build_result.package_artifact package_result with
          | Some artifact -> Ok artifact
          | None -> Error "expected package result to carry an artifact"

let test_execute_reuses_cache_for_equivalent_graph = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"integration_cache_equivalent"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let (graph1, node1) = make_graph_with_write ~package ~content:"cached output" in
      let (result1, exists1, content1, _sandbox1) =
        execute_graph ~workspace ~store ~package ~graph:graph1
      in
      let (graph2, node2) = make_graph_with_write ~package ~content:"cached output" in
      let (result2, exists2, content2, _sandbox2) =
        execute_graph ~workspace ~store ~package ~graph:graph2
      in
      match (
        Action_scheduler.find_result result1 node1,
        Action_scheduler.find_result result2 node2,
        content1,
        content2
      ) with
      | (
          Some { status = Action_scheduler.Executed _; _ },
          Some { status = Action_scheduler.Cached _; _ },
          Ok first_content,
          Ok second_content
        ) ->
          if
            exists1
            && exists2
            && String.equal first_content "cached output"
            && String.equal second_content "cached output"
          then
            Ok ()
          else
            Error "expected both sandboxes to materialize identical cached output"
      | _ -> Error "expected first run executed and second run cached") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_cache_misses_when_action_changes = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"integration_cache_changed"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let (graph1, node1) = make_graph_with_write ~package ~content:"v1" in
      let (result1, exists1, _, _sandbox1) =
        execute_graph ~workspace ~store ~package ~graph:graph1
      in
      let (graph2, node2) = make_graph_with_write ~package ~content:"v2" in
      let (result2, exists2, content2, _sandbox2) =
        execute_graph ~workspace ~store ~package ~graph:graph2
      in
      match (
        Action_scheduler.find_result result1 node1,
        Action_scheduler.find_result result2 node2,
        content2
      ) with
      | (
          Some { status = Action_scheduler.Executed _; _ },
          Some { status = Action_scheduler.Executed _; _ },
          Ok second_content
        ) ->
          if exists1 && exists2 && String.equal second_content "v2" then
            Ok ()
          else
            Error "expected changed action to execute freshly with new output"
      | _ -> Error "expected changed action to miss cache") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dependency_change_invalidates_cached_compile_actions = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"integration_dep_cache_invalidates"
    (fun tmpdir ->
      let dep =
        make_library_package
          ~root:tmpdir
          ~name:"dep"
          ~interface:"val value : int\n"
          "let value = 1\n"
      in
      let app =
        make_library_package
          ~root:tmpdir
          ~name:"app"
          ~interface:"val value : int\n"
          ~dependencies:[ "dep" ]
          "let value = Dep.value\n"
      in
      let workspace = make_workspace ~packages:[ dep; app ] tmpdir in
      match build_package_artifact ~workspace app with
      | Error err -> Error ("first app build failed: " ^ err)
      | Ok first_app_artifact ->
          let dep_source = Path.(dep.path / Path.v "src" / Path.v "dep.ml") in
          let _ =
            Fs.write "let value = 2\n" dep_source
            |> Result.expect ~msg:"failed to rewrite dependency source"
          in
          match build_package_artifact ~workspace app with
          | Error err -> Error ("second app build failed: " ^ err)
          | Ok second_app_artifact ->
              if
                Crypto.Hash.equal first_app_artifact.input_hash second_app_artifact.input_hash
              then
                Error "expected dependency change to invalidate dependent package cache"
              else
                Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_rebuilds_corrupted_cached_action = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"integration_cache_corrupted"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let store = Riot_store.Store.create ~workspace in
      let package = make_package ~root:tmpdir ~name:"pkg" in
      let (graph1, node1) = make_graph_with_write ~package ~content:"cached output" in
      let (result1, _, _, _) = execute_graph ~workspace ~store ~package ~graph:graph1 in
      match Action_scheduler.find_result result1 node1 with
      | Some { status = Action_scheduler.Executed artifact; _ } -> (
          (* Same-size tampering: only content verification can notice it. *)
          let cached_output =
            Path.(Riot_store.Store.action_hash_dir_of store artifact.input_hash / Path.v "out.txt")
          in
          let _ =
            Fs.write "tampered!!!!!" cached_output
            |> Result.expect ~msg:"failed to tamper with cached output"
          in
          let fresh_store = Riot_store.Store.create ~workspace in
          let (graph2, node2) = make_graph_with_write ~package ~content:"cached output" in
          let (result2, _, content2, _) =
            execute_graph ~workspace ~store:fresh_store ~package ~graph:graph2
          in
          match (Action_scheduler.find_result result2 node2, content2) with
          | (Some { status = Action_scheduler.Executed _; _ }, Ok "cached output") -> Ok ()
          | (Some { status = Action_scheduler.Cached _; _ }, _) ->
              Error "corrupted cached action must not be reported as a cache hit"
          | (_, Ok content) -> Error ("expected rebuilt output, got: " ^ content)
          | _ -> Error "expected corrupted cached action to be rebuilt"
        )
      | _ -> Error "expected first run to execute") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.[
    case
      "equivalent action graph reuses cached artifact"
      test_execute_reuses_cache_for_equivalent_graph;
    case "changed action graph misses cache" test_execute_cache_misses_when_action_changes;
    case "corrupted cached action is rebuilt" test_execute_rebuilds_corrupted_cached_action;
    case
      ~size:Large
      "dependency change invalidates cached compile actions"
      test_dependency_change_invalidates_cached_compile_actions;
  ]

let name = "riot-build:integration-caching"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
