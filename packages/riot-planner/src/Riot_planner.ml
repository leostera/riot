open Std
open Riot_model

type build_target = Workspace_planner.target

type plan_error = Workspace_planner.plan_error

type workspace_plan_result = Workspace_planner.package_plan

type module_plan_result = Module_planner.plan_result

type package_plan_result = Package_planner.plan_result

let plan_workspace = fun ~workspace ~target ~scope ~load_errors ->
  Workspace_planner.plan_workspace ~workspace ~target ~scope ~load_errors

let plan_package_with_graph = fun ~workspace ~toolchain ~store ~package_graph ~package_key ~package ~build_ctx ->
  Package_planner.plan_package ~workspace ~toolchain ~store ~package_graph ~package_key ~package ~build_ctx

(* Legacy/testing function - not used in production builds.
   Use plan_package_with_graph instead. *)

(* let plan_package ~workspace ~toolchain ~package =
  let planning_root = Path.v "src" in
  let depset = [] in
  let store = Riot_store.Store.create ~workspace in
  (* Use debug profile as default for standalone package planning *)
  let profile = Profile.debug in
  let session_id = Session_id.create () in
  let ctx = Build_ctx.make ~session_id ~profile () in
  let plan_input =
    {
      Module_planner.package;
      profile;
      ctx;
      toolchain;
      workspace;
      planning_root;
      depset;
      store;
    }
  in
  Module_planner.plan_node plan_input *)

module Action = Action
module Action_node = Action_node
module Action_graph = Action_graph
module Module_node = Module_node
module Module_registry = Module_registry
module Module_scanner = Module_scanner
module Module_graph = Module_graph
module Alias_module = Alias_module
module Compilation_pipeline = Compilation_pipeline
module Library_interface = Library_interface
module Library_definition = Library_definition
module Dependency = Dependency
module Planning_error = Planning_error
module Package_graph = Package_graph
module Workspace_planner = Workspace_planner
module Module_planner = Module_planner
module Package_planner = Package_planner
