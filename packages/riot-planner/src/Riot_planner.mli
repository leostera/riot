open Std
open Riot_model

(** Top-level planner interface *)
type build_target = Workspace_planner.target
type plan_error = Workspace_planner.plan_error
type workspace_plan_result = Workspace_planner.package_plan
type module_plan_result = Module_planner.plan_result
(** Plan the workspace - returns ordered packages to build and package graph.
    Does NOT plan module/action graphs - that's done on-demand per package. *)
type package_plan_result = Package_planner.plan_result
val plan_workspace:
  workspace:Workspace.t ->
  target:build_target ->
  scope:Package_graph.build_scope ->
  load_errors:Workspace_manager.load_error list ->
  (workspace_plan_result, plan_error) result

(** Plan a single package with dependency-aware hashing. Checks if all
    dependencies are planned first. Returns MissingDependencies if deps not
    ready, or Planned with hash. *)
val plan_package_with_graph:
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  package_graph:Package_graph.t ->
  package_key:Package.key ->
  package:Package.t ->
  build_ctx:Build_ctx.t ->
  (package_plan_result, Planning_error.t) result

(* Legacy/testing function - commented out, use plan_package_with_graph instead *)

(* val plan_package :
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  package:Package.t ->
  (module_plan_result, Planning_error.t) result *)

(** Plan a single package on-demand - builds module graph and action graph.
    Called by executor when actually building a package (after cache miss). This
    is the old interface without dependency hash tracking. *)

(** Sub-modules *)
module Action = Action

module Action_graph = Action_graph

module Action_node = Action_node

module Alias_module = Alias_module

module Compilation_pipeline = Compilation_pipeline

module Dependency = Dependency

module Module_graph = Module_graph

module Library_definition = Library_definition

module Library_interface = Library_interface

module Module_node = Module_node

module Module_registry = Module_registry

module Module_scanner = Module_scanner

module Planning_error = Planning_error

module Package_graph = Package_graph

module Workspace_planner = Workspace_planner

module Module_planner = Module_planner

module Package_planner = Package_planner
