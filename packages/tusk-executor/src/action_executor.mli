open Std
open Std.Collections
open Tusk_planner
module G = Graph.SimpleGraph

(** Errors that can occur during action execution *)
type action_error =
  | ExecutionFailed of { message : string }
  | OutputsNotCreated of { missing : Path.t list }
  | DependenciesFailed of { failed : G.Node_id.t list }

(** Status of an executed action *)
type action_status =
  | Cached of Crypto.hash
  | Executed
  | Failed of action_error
  | Skipped

(** Result of executing a single action *)
type execution_result = {
  node_id : G.Node_id.t;
  status : action_status;
  duration : Time.Duration.t;
  started_at : Time.Instant.t;
  completed_at : Time.Instant.t;
}

(** Collection of execution results *)
type t = { completed : (G.Node_id.t, execution_result) HashMap.t }

(** Execute an action graph with dependency-aware parallelism.

    Passing `concurrency = 1` uses the same code path with serialized
    execution.

    The executor performs cache lookup/save per action node hash and emits
    action telemetry events scoped to the provided `session_id`. *)
val execute :
  action_graph:Action_graph.t ->
  sandbox:Sandbox.t ->
  store:Tusk_store.Store.t ->
  session_id:Tusk_model.Session_id.t ->
  Tusk_toolchain.t ->
  concurrency:int ->
  t
