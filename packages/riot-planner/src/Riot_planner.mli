open Std
open Riot_model

(** Top-level planner interface *)
type module_plan_result = Module_planner.plan_result
(**
   Plan a single build unit with dependency-aware hashing.
*)
type package_plan_result = Package_planner.plan_result

val plan_build_unit:
  on_source_analyzed:(Module_graph.source_analysis_progress -> unit) ->
  workspace:Workspace.t ->
  toolchain:Riot_toolchain.t ->
  store:Riot_store.Store.t ->
  unit:Build_unit.t ->
  depset:Dependency.t list ->
  build_ctx:Build_ctx.t ->
  (package_plan_result, Planning_error.t) result

(** Sub-modules *)
module Action = Action

module Action_graph = Action_graph

module Action_node = Action_node

module Alias_module = Alias_module

module Dependency = Dependency

module Package_namespace = Package_namespace

module Module_graph = Module_graph

module Library_definition = Library_definition

module Library_interface = Library_interface

module Module_node = Module_node

module Module_registry = Module_registry

module Module_scanner = Module_scanner

module Planning_error = Planning_error

module Package_layout_validator = Package_layout_validator

module Build_unit = Build_unit

module Build_unit_graph = Build_unit_graph

module Module_planner = Module_planner

module Package_planner = Package_planner
