(** Package Planner - Plans individual packages with dependency-aware hashing *)

open Std
open Tusk_model

type plan_result =
  | Planned of {
      package_key : Package.key;
      package : Package.t;
      module_graph : Module_node.t Graph.SimpleGraph.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
      depset : Dependency.t list;
    }
  | MissingDependencies of { package : Package.t; missing : Package.t list }
  | FailedDependencies of { package : Package.t; failed : Package.t list }

val plan_package :
  workspace:Workspace.t ->
  toolchain:Tusk_toolchain.t ->
  store:Tusk_store.Store.t ->
  package_graph:Package_graph.t ->
  package_key:Package.key ->
  package:Package.t ->
  build_ctx:Build_ctx.t ->
  (plan_result, Planning_error.t) result
(** Plan a package:

    1. Check if all package dependencies are planned in the package_graph 2. If
    not → return MissingDependencies 3. Compute package input hash 4. Try to
    load a cached plan bundle by that hash 5. On miss, build module/action
    graphs and persist the bundle 6. Return Planned

    The hash includes:
    - Package metadata (name, dependencies, binaries)
    - Build profile and target context
    - Source-level package metadata
    - Hashes of all direct dependencies (transitive cache invalidation) *)
