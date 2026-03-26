(** Build Planner - Orchestrates build graph creation, wiring, and action
    generation *)

open Std
open Std.Collections
open Tusk_model
module G = Graph.SimpleGraph

type plan_input = {
  package : Package.t;
  profile : Profile.t;
  ctx : Build_ctx.t;
  toolchain : Tusk_toolchain.t;
  workspace : Workspace.t;
  planning_root : Path.t;
  depset : Dependency.t list;
  store : Tusk_store.Store.t;
}

type plan_result = {
  sources : Path.t list;
  module_graph : Module_node.t G.t;
  action_graph : Action_graph.t;
}

let plan_node input =
  let namespace = String.capitalize_ascii input.package.name in

  let config =
    Module_graph.
      {
        root = input.package.path;
        source_dir = input.planning_root;
        allowed_source_files = input.package.sources.src;
        namespace;
        package = input.package;
        toolchain = input.toolchain;
        workspace = input.workspace;
      }
  in

  try
    let graph_builder = Module_graph.create config in

    (match input.package.sources.native with
    | [] -> ()
    | files ->
        let native_node = Module_node.make_native ~files in
        let _ = G.add_node (Module_graph.graph graph_builder) native_node in
        ());

    let sandbox_dir = Path.(input.package.path / input.planning_root) in
    Module_graph.wire_dependencies graph_builder sandbox_dir;

    (match input.package.library with
    | Some _lib ->
        Module_graph.add_library_node graph_builder ~name:input.package.name
          ~includes:[]
    | None -> ());

    (* Direct deps are used for linking, but compile-time includes need the
       full transitive closure because package alias/interface modules can
       reference transitive package modules. *)
    let all_deps = input.depset in
    let transitive_deps = Dependency.transitive_closure all_deps in

    (* Check if any package (including our own) needs unix *)
    let needs_unix =
      let check_pkg (pkg : Package.t) =
        List.exists
          (fun (d : Package.dependency) -> d.name = "unix")
          (Package.build_graph_dependencies pkg)
      in
      check_pkg input.package
      || List.exists
           (fun (dep : Dependency.t) -> check_pkg dep.package)
           transitive_deps
    in

    (* Check if any package (including our own) needs dynlink *)
    let needs_dynlink =
      let check_pkg (pkg : Package.t) =
        List.exists
          (fun (d : Package.dependency) -> d.name = "dynlink")
          (Package.build_graph_dependencies pkg)
      in
      check_pkg input.package
      || List.exists
           (fun (dep : Dependency.t) -> check_pkg dep.package)
           transitive_deps
    in

    let binary_libraries =
      (* Binaries and commands need the full transitive runtime library
         closure, not just direct deps, because a direct package library
         like Std can reference modules provided by transitive deps such as
         Kernel and Miniriot. Dev-only packages have no local library, but
         their test/example binaries still need the dependency closure. *)
      let unix_lib = if needs_unix then [ Path.v "unix.cmxa" ] else [] in
      let dynlink_lib = if needs_dynlink then [ Path.v "dynlink.cmxa" ] else [] in
      let dep_libs = List.map Dependency.library_cmxa transitive_deps in
      let own_lib =
        match input.package.library with
        | Some _ ->
            [ Module_name.(of_string input.package.name |> cmxa) ]
        | None -> []
      in
      List.unique (unix_lib @ dynlink_lib @ dep_libs @ own_lib)
    in

    (* Add cache directories from dependencies to includes *)
    let dep_cache_dirs =
      List.map
        (fun (dep : Dependency.t) -> dep.artifact_dir)
        transitive_deps
    in

    let binary_includes =
      let unix_includes = if needs_unix then [ Path.v "+unix" ] else [] in
      let dynlink_includes = if needs_dynlink then [ Path.v "+dynlink" ] else [] in
      (* Include current package directory so binaries can access the library *)
      let own_package_dir = [ Path.v "." ] in
      unix_includes @ dynlink_includes @ own_package_dir @ dep_cache_dirs
    in

    List.iter
      (fun (bin : Package.binary) ->
        Module_graph.add_binary_node graph_builder ~name:bin.name
          ~source:bin.path ~libraries:binary_libraries ~includes:binary_includes)
      input.package.binaries;

    (* Add command nodes for package commands *)
    (* Commands are regular binaries - link all libraries like regular binaries *)
    Log.debug ("[MODULE_PLANNER] Command includes for " ^ input.package.name ^ ":");
    List.iter (fun inc -> Log.debug ("  " ^ Path.to_string inc)) binary_includes;
    List.iter
      (fun (cmd : Package_command.t) ->
        Module_graph.add_command_node graph_builder ~name:cmd.name
          ~source:cmd.command_source ~libraries:binary_libraries ~includes:binary_includes)
      input.package.commands;

    let main_library_node_id : G.Node_id.t option =
      match input.package.library with
      | Some _lib ->
          let result = ref None in
          G.iter (Module_graph.graph graph_builder) ~fn:(fun node_id node ->
              match node.value.Module_node.kind with
              | Module_node.Library _ when !result = None ->
                  result := Some node_id
              | _ -> ());
          !result
      | None -> None
    in

    let module_graph = Module_graph.graph graph_builder in

    (match G.topo_sort module_graph with
    | Error cycle_ids ->
        let cycle =
          List.filter_map
            (fun node_id ->
              match G.get_node module_graph node_id with
              | None -> None
              | Some node ->
                  Some (match node.value.kind with
                  | Module_node.ML m | Module_node.MLI m ->
                      Module.module_name m |> Module_name.to_string
                  | Module_node.Library { name; _ } -> "Library(" ^ name ^ ")"
                  | Module_node.Binary { name; _ } -> "Binary(" ^ name ^ ")"
                  | Module_node.Native _ -> "Native"
                  | Module_node.C -> "C"
                  | Module_node.H -> "H"
                  | Module_node.Root -> "Root"
                  | Module_node.Other s -> "Other(" ^ s ^ ")"))
            cycle_ids
          |> List.rev  (* Reverse to show A -> B means A uses/opens B *)
        in
        Error (Planning_error.CyclicDependency { cycle })
    | Ok sorted_modules -> (
        let action_graph, _outputs =
          Action_graph.from_module_graph ~package:input.package ~profile:input.profile
            ~ctx:input.ctx ~toolchain:input.toolchain ~store:input.store ~depset:input.depset
            ~needs_unix ~needs_dynlink
            module_graph
        in

        let sources =
          sorted_modules
          |> List.concat_map (fun (node : Module_node.t G.node) ->
              match node.value.kind with
              | Native { files } ->
                  List.map
                    (fun path ->
                      if Path.is_absolute path then path
                      else Path.(input.package.path / path))
                    files
              | _ -> (
                  match node.value.file with
                  | Concrete path when Path.to_string path != "" ->
                      let abs_path =
                        if Path.is_absolute path then path
                        else Path.(input.package.path / path)
                      in
                      [ abs_path ]
                  | _ -> []))
        in

        Ok { sources; module_graph; action_graph }))
  with exn -> Error (Planning_error.Exception { exn })
