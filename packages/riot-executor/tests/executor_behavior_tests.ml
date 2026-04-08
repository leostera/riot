(*
open Std
open Std.Collections

module Test = Std.Test
module G = Graph.SimpleGraph

let test_toolchain =
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"Failed to initialize test toolchain"

let make_test_workspace tmpdir =
  Riot_model.Workspace.
    {
      root = tmpdir;
      target_dir_root = Path.(tmpdir / Path.v "target");
      packages = [];
      profile_overrides = [];
    }

let make_test_package () =
  Riot_model.Package.
    {
      name = "test";
      path = Path.v ".";
      relative_path = Path.v ".";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources = { src = []; native = []; tests = []; examples = [] };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
      macro_providers = [];
      publish = { version = None; description = None; license = None; is_public = None };
    }

let make_action_spec ?(actions = []) ?(outs = []) ?(srcs = []) () =
  {
    Riot_planner.Action_node.actions;
    outs;
    srcs;
    package = make_test_package ();
    toolchain = test_toolchain;
    hash = Crypto.hash_string "test";
  }

let test_empty_graph_completes_immediately () =
  match
    Fs.with_tempdir ~prefix:"empty_graph_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let toolchain = test_toolchain in

        let action_graph = Riot_planner.Action_graph.create () in

        Riot_executor.Sandbox.with_sandbox ~workspace ~inputs:[]
          ~expected_outputs:[] (fun sandbox ->
            let start = Time.Instant.now () in
            let result =
              Riot_executor.Action_executor.execute ~action_graph ~sandbox
                ~store toolchain ~concurrency:4
            in
            let duration =
              Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
            in
            let completed_count =
              HashMap.to_list result.completed |> List.length
            in

            if completed_count = 0 && Time.Duration.to_millis duration < 100
            then Ok ()
            else
              Error
                ("Expected 0 completed in <100ms, got " ^ Int.to_string completed_count ^ " in " ^ Int.to_string (Time.Duration.to_millis duration) ^ "ms")))
  with
  | Ok (Ok ()) -> Ok ()
  | Ok (Error e) -> Error e
  | Error _ -> Error "Tempdir creation failed"

let test_independent_actions_continue_on_failure () =
  match
    Fs.with_tempdir ~prefix:"independent_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let toolchain = test_toolchain in

        let out_success = Path.v "success.txt" in
        let out_fail = Path.v "fail.txt" in

        let action_graph = Riot_planner.Action_graph.create () in

        let success_spec =
          make_action_spec
            ~actions:
              [
                Riot_planner.Action.WriteFile
                  { destination = out_success; content = "ok" };
              ]
            ~outs:[ out_success ] ()
        in
        let fail_spec =
          make_action_spec
            ~actions:
              [
                Riot_planner.Action.WriteFile
                  {
                    destination = Path.v "nonexistent_dir/fail.txt";
                    content = "fail";
                  };
              ]
            ~outs:[ out_fail ] ()
        in

        let _ = Riot_planner.Action_graph.add_node action_graph success_spec in
        let _ = Riot_planner.Action_graph.add_node action_graph fail_spec in

        Riot_executor.Sandbox.with_sandbox ~workspace ~inputs:[]
          ~expected_outputs:[] (fun sandbox ->
            let result =
              Riot_executor.Action_executor.execute ~action_graph ~sandbox
                ~store toolchain ~concurrency:2
            in
            let completed = HashMap.to_list result.completed in

            if List.length completed = 2 then Ok ()
            else
              Error
                ("Expected 2 completed, got " ^ Int.to_string (List.length completed))))
  with
  | Ok (Ok ()) -> Ok ()
  | Ok (Error e) -> Error e
  | Error _ -> Error "Tempdir creation failed"

let test_dependent_actions_not_executed_on_failure () =
  match
    Fs.with_tempdir ~prefix:"dependent_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let toolchain = test_toolchain in

        let out1 = Path.v "out1.txt" in
        let out2 = Path.v "out2.txt" in

        let action_graph = Riot_planner.Action_graph.create () in

        let fail_spec =
          make_action_spec
            ~actions:
              [
                Riot_planner.Action.WriteFile
                  {
                    destination = Path.v "nonexistent_dir/fail.txt";
                    content = "fail";
                  };
              ]
            ~outs:[ out1 ] ()
        in
        let dependent_spec =
          make_action_spec
            ~actions:
              [
                Riot_planner.Action.WriteFile
                  { destination = out2; content = "should not run" };
              ]
            ~outs:[ out2 ] ()
        in

        let fail_node =
          Riot_planner.Action_graph.add_node action_graph fail_spec
        in
        let dep_node =
          Riot_planner.Action_graph.add_node action_graph dependent_spec
        in
        Riot_planner.Action_graph.add_dependency action_graph dep_node
          ~depends_on:fail_node;

        Riot_executor.Sandbox.with_sandbox ~workspace ~inputs:[]
          ~expected_outputs:[] (fun sandbox ->
            let result =
              Riot_executor.Action_executor.execute ~action_graph ~sandbox
                ~store toolchain ~concurrency:2
            in
            let completed = HashMap.to_list result.completed in
            let skipped_count =
              List.filter
                (fun (_, r) ->
                  match r.Riot_executor.Action_executor.status with
                  | Skipped -> true
                  | _ -> false)
                completed
              |> List.length
            in

            if List.length completed = 2 && skipped_count = 1 then Ok ()
            else
              Error
                (format
                   "Expected 2 completed (1 failed, 1 skipped), got %d (%d \
                    skipped)"
                   (List.length completed) skipped_count)))
  with
  | Ok (Ok ()) -> Ok ()
  | Ok (Error e) -> Error e
  | Error _ -> Error "Tempdir creation failed"

let test_parallel_execution_timing () =
  match
    Fs.with_tempdir ~prefix:"parallel_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let toolchain = test_toolchain in

        let out1 = Path.v "out1.txt" in
        let out2 = Path.v "out2.txt" in
        let out3 = Path.v "out3.txt" in
        let out4 = Path.v "out4.txt" in

        let content = String.make 100000 'x' in

        let make_actions out =
          let rec make_n n acc =
            if n = 0 then acc
            else
              make_n (n - 1)
                (Riot_planner.Action.WriteFile { destination = out; content }
                :: acc)
          in
          make_n 10 []
        in

        let action_graph = Riot_planner.Action_graph.create () in

        let spec1 =
          make_action_spec ~actions:(make_actions out1) ~outs:[ out1 ] ()
        in
        let spec2 =
          make_action_spec ~actions:(make_actions out2) ~outs:[ out2 ] ()
        in
        let spec3 =
          make_action_spec ~actions:(make_actions out3) ~outs:[ out3 ] ()
        in
        let spec4 =
          make_action_spec ~actions:(make_actions out4) ~outs:[ out4 ] ()
        in

        let _ = Riot_planner.Action_graph.add_node action_graph spec1 in
        let _ = Riot_planner.Action_graph.add_node action_graph spec2 in
        let _ = Riot_planner.Action_graph.add_node action_graph spec3 in
        let _ = Riot_planner.Action_graph.add_node action_graph spec4 in

        Riot_executor.Sandbox.with_sandbox ~workspace ~inputs:[]
          ~expected_outputs:[] (fun sandbox ->
            let result =
              Riot_executor.Action_executor.execute ~action_graph ~sandbox
                ~store toolchain ~concurrency:4
            in
            let completed = HashMap.to_list result.completed in

            if List.length completed = 4 then Ok ()
            else
              Error
                ("Expected 4 completed, got " ^ Int.to_string (List.length completed))))
  with
  | Ok (Ok ()) -> Ok ()
  | Ok (Error e) -> Error e
  | Error _ -> Error "Tempdir creation failed"

let tests =
  Test.
    [
      case "empty graph completes immediately"
        test_empty_graph_completes_immediately;
      case "independent actions continue on failure"
        test_independent_actions_continue_on_failure;
      case "dependent actions not executed on failure"
        test_dependent_actions_not_executed_on_failure;
      case "parallel execution timing" test_parallel_execution_timing;
    ]

let name = "Executor Behavior Tests"
let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
*)
