open Std

module Test = Std.Test
module G = Graph.SimpleGraph

let test_toolchain =
  lazy
    (Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
    |> Result.expect ~msg:"failed to initialize toolchain")

let make_test_package name =
  Tusk_model.Package.
    {
      name;
      path = Path.v ("packages/" ^ name);
      relative_path = Path.v ("packages/" ^ name);
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = Some { path = Path.v "src/lib.ml" };
      sources = { src = []; native = []; tests = []; examples = []; bench = [] };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
    }

let make_action_node ?(deps = []) ?(outs = []) ?(actions = []) package_name =
  let package = make_test_package package_name in
  Tusk_planner.Action_node.make ~actions ~outs ~srcs:[] ~package
    ~toolchain:(Lazy.force test_toolchain)
    ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
    ~deps

let executed_result node_id =
  let now = Time.Instant.now () in
  Tusk_executor.Action_queue.
    {
      node_id;
      status = Executed;
      duration = Time.Duration.zero;
      started_at = now;
      completed_at = now;
    }

let failed_result node_id =
  let now = Time.Instant.now () in
  Tusk_executor.Action_queue.
    {
      node_id;
      status = Failed (ExecutionFailed { message = "boom" });
      duration = Time.Duration.zero;
      started_at = now;
      completed_at = now;
    }

let queue_respects_dependency_order () =
  let queue = Tusk_executor.Action_queue.create () in
  let dep_node =
    make_action_node "kernel"
      ~actions:
        [
          Tusk_planner.Action.WriteFile
            { destination = Path.v "kernel.txt"; content = "kernel" };
        ]
  in
  let app_node =
    make_action_node "app" ~deps:[ dep_node.id ]
      ~actions:
        [
          Tusk_planner.Action.WriteFile
            { destination = Path.v "app.txt"; content = "app" };
        ]
  in
  Tusk_executor.Action_queue.queue queue app_node;
  Tusk_executor.Action_queue.queue queue dep_node;
  match Tusk_executor.Action_queue.next queue with
  | None -> Error "expected dependency node first"
  | Some first ->
      Test.assert_true (G.Node_id.eq first.id dep_node.id);
      Tusk_executor.Action_queue.mark_completed queue (executed_result dep_node.id);
      (match Tusk_executor.Action_queue.next queue with
      | None -> Error "expected dependent node after dependency completion"
      | Some second ->
          Test.assert_true (G.Node_id.eq second.id app_node.id);
          Ok ())

let queue_marks_dependents_skipped_after_failure () =
  let queue = Tusk_executor.Action_queue.create () in
  let dep_node = make_action_node "std" in
  let dependent_node = make_action_node "tusk-model" ~deps:[ dep_node.id ] in
  Tusk_executor.Action_queue.queue queue dependent_node;
  Tusk_executor.Action_queue.queue queue dep_node;
  ignore (Tusk_executor.Action_queue.next queue);
  Tusk_executor.Action_queue.mark_completed queue (failed_result dep_node.id);
  ignore (Tusk_executor.Action_queue.next queue);
  match Tusk_executor.Action_queue.get_result queue dependent_node.id with
  | Some { status = Skipped; _ } -> Ok ()
  | Some _ -> Error "expected dependent action to be skipped"
  | None -> Error "missing dependent result"

let requeue_with_deps_moves_blocked_node_and_queues_missing_dependency () =
  let queue = Tusk_executor.Action_queue.create () in
  let missing_dep = make_action_node "kernel" in
  let blocked = make_action_node "std" ~deps:[ missing_dep.id ] in
  Tusk_executor.Action_queue.queue queue blocked;
  ignore (Tusk_executor.Action_queue.next queue);
  Tusk_executor.Action_queue.requeue_with_deps queue blocked
    ~missing_deps:[ missing_dep.id ] ~all_nodes:[ blocked; missing_dep ];
  match Tusk_executor.Action_queue.next queue with
  | Some ready -> Test.assert_true (G.Node_id.eq ready.id missing_dep.id); Ok ()
  | None -> Error "expected missing dependency node to be queued"

let is_complete_checks_all_nodes_accounted_for () =
  let queue = Tusk_executor.Action_queue.create () in
  let node_a = make_action_node "a" in
  let node_b = make_action_node "b" in
  Tusk_executor.Action_queue.queue queue node_a;
  Tusk_executor.Action_queue.queue queue node_b;
  ignore (Tusk_executor.Action_queue.next queue);
  Tusk_executor.Action_queue.mark_completed queue (executed_result node_a.id);
  ignore (Tusk_executor.Action_queue.next queue);
  Tusk_executor.Action_queue.mark_completed queue (executed_result node_b.id);
  if Tusk_executor.Action_queue.is_complete queue ~total_nodes:2 then Ok ()
  else Error "queue should be complete after both nodes finish"

let tests =
  Test.
    [
      case "queue respects dependency order" queue_respects_dependency_order;
      case "queue marks dependents skipped after failure"
        queue_marks_dependents_skipped_after_failure;
      case "requeue_with_deps moves blocked node and queues missing dependency"
        requeue_with_deps_moves_blocked_node_and_queues_missing_dependency;
      case "is_complete checks all nodes accounted for"
        is_complete_checks_all_nodes_accounted_for;
    ]

let name = "tusk-executor:action-queue-workspace-graph"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
