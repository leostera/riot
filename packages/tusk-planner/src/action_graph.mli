open Std
open Tusk_model

module G = Std.Graph.SimpleGraph

type t

val create : unit -> t
(** Create an empty action graph *)

val hash_action_node : t -> Action_node.t -> Crypto.hash
(** Compute content-based hash of an action node as part of a Merkle graph.
    
    If two action nodes have the same hash, they will produce identical outputs.
    This enables caching: if we find a cached node with matching hash, we can
    skip execution and copy its outputs from the cache.
    
    The hash includes (forming a Merkle graph):
    1. All actions in the node (sources, flags, includes, etc.)
    2. Source file contents (hashed)
    3. Expected outputs (paths only, since contents don't exist yet)
    4. Hashes of all dependency nodes (recursive Merkle property)
    
    This means if ANY source file changes OR any dependency changes, the hash
    changes, invalidating the cache for this node and all downstream nodes.
*)

val from_module_graph : 
  package:Package.t ->
  profile:Profile.t ->
  ctx:Build_ctx.t ->
  toolchain:Tusk_toolchain.t ->
  store:Tusk_store.Store.t ->
  depset:Dependency.t list ->
  needs_unix:bool ->
  needs_dynlink:bool ->
  Module_node.t G.t -> 
  t * Path.t list
(** Map a module graph to an action graph and collect all outputs.
    
    Each module node is transformed into an action node containing the 
    actions needed to build that module. The graph structure (dependencies)
    is preserved, so the action graph mirrors the module graph.
    
    Returns (action_graph, all_outputs) where all_outputs is the complete
    list of files produced by all actions.
    
    This enables parallelization analysis: nodes without dependencies 
    between them can be built in parallel.
*)

val add_node : t -> Action_node.action_spec -> Action_node.t
val add_dependency : t -> Action_node.t -> depends_on:Action_node.t -> unit
val topo_sort : t -> Action_node.t list
val nodes : t -> Action_node.t list
(** Returns all nodes in deterministic topological order *)
val graph : t -> Action_node.action_spec G.t
val to_action_list : t -> Action.t list
val to_json : t -> Data.Json.t
(** Convert action graph to JSON with sorted nodes for deterministic output.

    The JSON representation is used as a persisted plan artifact for warm builds.
    It includes node-level package path context and the precomputed node hash so
    cache-key identity is stable across process boundaries. *)

val from_json : Data.Json.t -> (t, string) Result.t
(** Reconstruct action graph from JSON for warm-build execution.

    This function preserves each node's serialized hash and package path
    metadata rather than recomputing them. Recomputing can diverge from the
    original Merkle graph and break action-cache lookup. *)

val equal : t -> t -> bool
(** Compare two action graphs structurally by comparing topologically sorted nodes *)
