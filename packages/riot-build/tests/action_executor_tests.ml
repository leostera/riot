open Std
open Riot_build
open Std.Collections
open Riot_model
module Test = Std.Test

let package_name = fun value ->
  Package_name.from_string value |> Result.expect ~msg:("expected valid package name: " ^ value)

let test_toolchain = fun () ->
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"failed to initialize toolchain"

let make_workspace = fun root ->
  Riot_model.Workspace.{
    name = None;
    root;
    target_dir_root =
      Path.(root / Path.v "target");
    packages = [];
    dependencies = [];
    dev_dependencies = [];
    build_dependencies = [];
    profile_overrides = [];
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

let make_node = fun ~package ?(dep_nodes = []) ~actions ~outs () ->
  let graph = Riot_planner.Action_graph.create () in
  let deps = List.map dep_nodes ~fn:(fun (node: Riot_planner.Action_node.t) -> node.id) in
  let spec =
    Riot_planner.Action_node.make
      ~actions
      ~outs
      ~srcs:[]
      ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps
  in
  let node = Riot_planner.Action_graph.add_node graph spec in
  let _ =
    List.for_each dep_nodes ~fn:(fun dep_node ->
      Riot_planner.Action_graph.add_dependency graph node ~depends_on:dep_node)
  in
  node

let node_id = fun (node: Riot_planner.Action_node.t) -> node.id

let failed_result = fun node_id ->
  let now = Time.Instant.now () in
  Action_executor.{
    node_id;
    status = Failed (ExecutionFailed { message = "boom" });
    ocamlc_warnings = [];
    duration = Time.Duration.zero;
    started_at = now;
    completed_at = now;
  }

let test_execute_node_writes_file = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"action_executor_write"
      (fun tmpdir ->
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox failed" in
        let workspace = make_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let package = make_package ~root:tmpdir ~name:"pkg" in
        let output = Path.v "out.txt" in
        let node = make_node
          ~package
          ~actions:[ Riot_planner.Action.WriteFile { destination = output; content = "hello" }; ]
          ~outs:[ output ]
          ()
        in
        let completed = HashMap.create () in
        let result = Action_executor.execute_node
          ~completed
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          (test_toolchain ())
          sandbox
          node
        in
        match result.status with
        | Action_executor.Executed -> (
            let output_path = Path.(sandbox / output) in
            match Fs.read_to_string output_path with
            | Ok content when String.equal content "hello" -> Ok ()
            | Ok content -> Error ("unexpected output content: " ^ content)
            | Error err -> Error ("failed to read output: " ^ IO.error_message err)
          )
        | Action_executor.Cached _
        | Action_executor.Failed _
        | Action_executor.Skipped -> Error "expected write action to execute")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_node_copies_file = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"action_executor_copy"
      (fun tmpdir ->
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox failed" in
        let source = Path.(sandbox / Path.v "source.txt") in
        let _ = Fs.write "copy me" source |> Result.expect ~msg:"write source failed" in
        let workspace = make_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let package = make_package ~root:tmpdir ~name:"pkg" in
        let destination = Path.v "copied.txt" in
        let node = make_node
          ~package
          ~actions:[ Riot_planner.Action.CopyFile {
            source = Path.v "source.txt";
            destination
          }; ]
          ~outs:[ destination ]
          ()
        in
        let completed = HashMap.create () in
        let result = Action_executor.execute_node
          ~completed
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          (test_toolchain ())
          sandbox
          node
        in
        match result.status with
        | Action_executor.Executed -> (
            let destination_path = Path.(sandbox / destination) in
            match Fs.read_to_string destination_path with
            | Ok content when String.equal content "copy me" -> Ok ()
            | Ok content -> Error ("unexpected copied content: " ^ content)
            | Error err -> Error ("failed to read copied file: " ^ IO.error_message err)
          )
        | Action_executor.Cached _
        | Action_executor.Failed _
        | Action_executor.Skipped -> Error "expected copy action to execute")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_node_fails_when_declared_output_is_missing = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"action_executor_missing_output"
      (fun tmpdir ->
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox failed" in
        let workspace = make_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let package = make_package ~root:tmpdir ~name:"pkg" in
        let declared_output = Path.v "declared.txt" in
        let node = make_node
          ~package
          ~actions:[ Riot_planner.Action.WriteFile {
            destination = Path.v "actual.txt";
            content = "hello"
          }; ]
          ~outs:[ declared_output ]
          ()
        in
        let completed = HashMap.create () in
        let result = Action_executor.execute_node
          ~completed
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          (test_toolchain ())
          sandbox
          node
        in
        match result.status with
        | Action_executor.Failed (OutputsNotCreated { missing }) ->
            let expected = Path.to_string Path.(sandbox / declared_output) in
            if List.any missing ~fn:(fun path -> String.equal (Path.to_string path) expected) then
              Ok ()
            else
              Error "expected missing declared output to be reported"
        | Action_executor.Failed _ -> Error "expected output verification failure"
        | Action_executor.Cached _
        | Action_executor.Executed
        | Action_executor.Skipped -> Error "expected missing output failure")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_node_skips_when_dependency_failed = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"action_executor_skip"
      (fun tmpdir ->
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox failed" in
        let workspace = make_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let package = make_package ~root:tmpdir ~name:"pkg" in
        let dependency = make_node
          ~package
          ~actions:[ Riot_planner.Action.WriteFile {
            destination = Path.v "dep.txt";
            content = "dependency"
          }; ]
          ~outs:[ Path.v "dep.txt" ]
          ()
        in
        let node = make_node
          ~package
          ~dep_nodes:[ dependency ]
          ~actions:[ Riot_planner.Action.WriteFile {
            destination = Path.v "out.txt";
            content = "child"
          }; ]
          ~outs:[ Path.v "out.txt" ]
          ()
        in
        let completed = HashMap.create () in
        let _ = HashMap.insert
          completed
          ~key:(node_id dependency)
          ~value:(failed_result (node_id dependency))
        in
        let result = Action_executor.execute_node
          ~completed
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          (test_toolchain ())
          sandbox
          node
        in
        match result.status with
        | Action_executor.Skipped -> Ok ()
        | Action_executor.Cached _
        | Action_executor.Executed
        | Action_executor.Failed _ -> Error "expected node to be skipped")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_cached_promotion_failure_returns_failed_result = fun _ctx ->
  match Fs.with_tempdir ~prefix:"action_executor_cached_promotion_failure"
    (fun root ->
      let store = Riot_store.Store.create ~workspace:(make_workspace root) in
      let sandbox = Path.(root / Path.v "sandbox") in
      let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox" in
      let output = Path.v "out.txt" in
      let node = make_node ~package:(make_package ~root ~name:"pkg")
        ~actions:[Riot_planner.Action.WriteFile { destination = output; content = "hello" }]
        ~outs:[output] () in
      let file = Path.(sandbox / output) in
      let _ = Fs.write "hello" file |> Result.expect ~msg:"write cached output" in
      let _ = Riot_store.Store.save store ~package:"pkg"
        ~hash:(Riot_planner.Action_node.get_hash node) ~sandbox_dir:sandbox ~outs:[file]
        |> Result.expect ~msg:"seed cache" in
      let blocked = Path.(root / Path.v "blocked") in
      let _ = Fs.write "not a directory" blocked |> Result.expect ~msg:"block promotion destination" in
      try
        let result = Action_executor.execute_node ~completed:(HashMap.create ()) ~store
          ~session_id:(Session_id.make ()) (test_toolchain ()) blocked node in
        match result.status with
        | Action_executor.Failed (ExecutionFailed { message }) when not (String.equal message "") -> Ok ()
        | _ -> Error "cached promotion failure did not return a failed execution result"
      with
      | _ -> Error "cached promotion failure raised instead of returning a failed execution result")
  with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let tests =
  Test.[
    case "execute_node writes declared output" test_execute_node_writes_file;
    case "execute_node copies file action outputs" test_execute_node_copies_file;
    case "execute_node fails when declared output is missing" test_execute_node_fails_when_declared_output_is_missing;
    case "execute_node skips when dependency already failed" test_execute_node_skips_when_dependency_failed;
    case "cached promotion failure returns failed result" test_cached_promotion_failure_returns_failed_result;
  ]

let name = "riot-build:action-executor"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
