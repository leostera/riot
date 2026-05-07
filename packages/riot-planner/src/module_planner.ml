(**
   Build Planner - Orchestrates build graph creation, wiring, and action
   generation
*)
open Std
open Std.Collections
open Riot_model

module G = Graph.SimpleGraph

type plan_input = {
  package: Package.t;
  profile: Profile.t;
  ctx: Build_ctx.t;
  toolchain: Riot_toolchain.t;
  workspace: Workspace.t;
  source_groups: Module_graph.source_group list;
  depset: Dependency.t list;
  dependency_packages: Package.t list;
  store: Riot_store.Store.t;
  on_source_analyzed: Module_graph.source_analysis_progress -> unit;
}

type plan_result = {
  sources: Path.t list;
  module_graph: Module_node.t G.t;
  analyzed_modules: (G.Node_id.t * Module_graph.analyzed_module) list;
  action_graph: Action_graph.t;
}

type direct_dependency_root = {
  package_name: Package_name.t;
  root_module: string;
  compiled_root_module: string;
  package: Package.t option;
}

let root_module_name_of_package_name = fun package_name ->
  Module_name.(from_string (Package_name.to_string package_name)
  |> to_string)

let direct_dependency_package_by_name = fun depset package_name ->
  depset
  |> List.find ~fn:(fun (dep: Dependency.t) -> Package_name.equal dep.package.name package_name)
  |> Option.map ~fn:(fun (dep: Dependency.t) -> dep.package)

let transitive_dependency_package_by_name = fun depset package_name ->
  Dependency.transitive_closure depset
  |> List.find ~fn:(fun (dep: Dependency.t) -> Package_name.equal dep.package.name package_name)
  |> Option.map ~fn:(fun (dep: Dependency.t) -> dep.package)

let workspace_dependency_package_by_name = fun (input: plan_input) package_name ->
  input.workspace.packages
  |> List.find
    ~fn:(fun (manifest: Package_manifest.t) ->
      Package_name.equal manifest.name package_name)
  |> Option.map ~fn:(Workspace.realize_package ~intent:Package.Runtime input.workspace)

let input_dependency_package_by_name = fun (input: plan_input) package_name ->
  match direct_dependency_package_by_name input.depset package_name with
  | Some package -> Some package
  | None ->
      match transitive_dependency_package_by_name input.depset package_name with
      | Some package -> Some package
      | None -> (
          match List.find
            input.dependency_packages
            ~fn:(fun (package: Package.t) -> Package_name.equal package.name package_name) with
          | Some package -> Some package
          | None -> workspace_dependency_package_by_name input package_name
        )

let direct_dependency_roots = fun (input: plan_input) ->
  let seen = HashSet.create () in
  let roots = ref [] in
  let add ~package_name ~root_module ~compiled_root_module ~package =
    if HashSet.insert seen ~value:root_module then
      roots := { package_name; root_module; compiled_root_module; package } :: !roots
    else
      ()
  in
  Package.build_graph_dependencies input.package
  |> List.for_each
    ~fn:(fun (dep: Package.dependency) ->
      let package =
        if Package.is_builtin_dependency dep then
          None
        else
          input_dependency_package_by_name input dep.name
      in
      let (root_module, compiled_root_module) =
        match package with
        | Some package ->
            (Package_namespace.public_root package, Package_namespace.compiled_root package)
        | None ->
            let root = root_module_name_of_package_name dep.name in
            (root, root)
      in
      add ~package_name:dep.name ~root_module ~compiled_root_module ~package);
  let () =
    if Option.is_none input.package.library then
      match Dependency.transitive_closure input.depset
      |> List.find
        ~fn:(fun (dep: Dependency.t) ->
          Package_name.equal dep.package.name input.package.name
          && Option.is_some dep.package.library) with
      | None -> ()
      | Some (dep: Dependency.t) ->
          add
            ~package_name:dep.package.name
            ~root_module:(Package_namespace.public_root dep.package)
            ~compiled_root_module:(Package_namespace.compiled_root dep.package)
            ~package:(Some dep.package)
    else
      ()
  in
  List.reverse !roots

let plan_node = fun (input: plan_input) ->
  let config =
    Module_graph.{
      root = input.package.path;
      source_groups = input.source_groups;
      package = input.package;
      toolchain = input.toolchain;
      workspace = input.workspace;
    }
  in
  try
    let graph_builder = Module_graph.create config in
    let direct_dependency_roots = direct_dependency_roots input in
    List.for_each
      direct_dependency_roots
      ~fn:(fun direct_dependency ->
        match direct_dependency.package with
        | Some package -> Module_graph.add_direct_dependency_package graph_builder package
        | None ->
            Module_graph.add_direct_dependency_root
              graph_builder
              ~package_name:direct_dependency.package_name
              ~root_module:direct_dependency.root_module);
    Module_graph.add_relocation_aliases
      graph_builder
      ~aliases:(List.filter_map
        direct_dependency_roots
        ~fn:(fun direct_dependency ->
          if String.equal direct_dependency.root_module direct_dependency.compiled_root_module then
            None
          else
            Some (direct_dependency.root_module, direct_dependency.compiled_root_module)));
    Module_graph.add_self_relocation_alias graph_builder;
    (
      match input.package.sources.native with
      | [] -> ()
      | files ->
          let native_node = Module_node.make_native ~files in
          let _ = G.add_node (Module_graph.graph graph_builder) native_node in
          ()
    );
    match Module_graph.wire_dependencies ~on_source_analyzed:input.on_source_analyzed graph_builder with
    | Error _ as error -> error
    | Ok () ->
        (
          match input.package.library with
          | Some _lib ->
              Module_graph.add_library_node
                graph_builder
                ~name:(Package_namespace.planning_library_name input.package)
                ~includes:[]
          | None -> ()
        );
        (* Direct deps are used for linking, but compile-time includes need the
           full transitive closure because package alias/interface modules can
           reference transitive package modules.
        *)
        let all_deps = input.depset in
        let transitive_deps = Dependency.transitive_closure all_deps in
        (* Check if any package (including our own) needs unix *)
        let needs_unix =
          let check_pkg (pkg: Package.t) =
            List.any
              (Package.build_graph_dependencies pkg)
              ~fn:(fun (d: Package.dependency) ->
                Package_name.equal
                  d.name
                  (
                    Package_name.from_string "unix"
                    |> Result.expect ~msg:"expected valid package name"
                  ))
          in
          check_pkg input.package
          || List.any transitive_deps ~fn:(fun (dep: Dependency.t) -> check_pkg dep.package)
        in
        (* Check if any package (including our own) needs dynlink *)
        let needs_dynlink =
          let check_pkg (pkg: Package.t) =
            List.any
              (Package.build_graph_dependencies pkg)
              ~fn:(fun (d: Package.dependency) ->
                Package_name.equal
                  d.name
                  (
                    Package_name.from_string "dynlink"
                    |> Result.expect ~msg:"expected valid package name"
                  ))
          in
          check_pkg input.package
          || List.any transitive_deps ~fn:(fun (dep: Dependency.t) -> check_pkg dep.package)
        in
        let binary_libraries =
          (* Binaries and commands need the full transitive runtime library
             closure, not just direct deps, because a direct package library
             like Std can reference modules provided by transitive deps such as
             Kernel and Runtime. Dev-only packages have no local library, but
             their test/example binaries still need the dependency closure.
          *)
          let unix_lib =
            if needs_unix then
              [ Path.v "unix.cmxa" ]
            else
              []
          in
          let dynlink_lib =
            if needs_dynlink then
              [ Path.v "dynlink.cmxa" ]
            else
              []
          in
          let dep_libs =
            List.filter_map
              transitive_deps
              ~fn:(fun (dep: Dependency.t) ->
                match dep.package.Package.library with
                | Some _ -> Some (Dependency.library_cmxa dep)
                | None -> None)
          in
          let own_lib =
            match input.package.library with
            | Some _ -> [ Package_namespace.library_cmxa input.package ]
            | None -> []
          in
          let seen_libraries = HashSet.create () in
          List.filter_map
            (((unix_lib @ dynlink_lib) @ dep_libs) @ own_lib)
            ~fn:(fun library_path ->
              if HashSet.insert seen_libraries ~value:library_path then
                Some library_path
              else
                None)
        in
        (* Add cache directories from dependencies to includes *)
        let dep_cache_dirs =
          List.map transitive_deps ~fn:(fun (dep: Dependency.t) -> dep.artifact_dir)
        in
        let binary_includes =
          let unix_includes =
            if needs_unix then
              [ Path.v "+unix" ]
            else
              []
          in
          let dynlink_includes =
            if needs_dynlink then
              [ Path.v "+dynlink" ]
            else
              []
          in
          (* Include current package directory so binaries can access the library *)
          let own_package_dir = [ Path.v "." ] in
          ((unix_includes @ dynlink_includes) @ own_package_dir) @ dep_cache_dirs
        in
        List.for_each
          input.package.binaries
          ~fn:(fun (bin: Package.binary) ->
            Module_graph.add_binary_node
              graph_builder
              ~name:bin.name
              ~source:bin.path
              ~libraries:binary_libraries
              ~includes:binary_includes);
        (* Add command nodes for package commands *)
        (* Commands are regular binaries - link all libraries like regular binaries *)
        Log.debug
          ("[MODULE_PLANNER] Command includes for "
          ^ Package_name.to_string input.package.name
          ^ ":");
        List.for_each binary_includes ~fn:(fun inc -> Log.debug ("  " ^ Path.to_string inc));
        List.for_each
          input.package.commands
          ~fn:(fun (cmd: Package_command.t) ->
            Module_graph.add_command_node
              graph_builder
              ~name:cmd.name
              ~source:cmd.command_source
              ~libraries:binary_libraries
              ~includes:binary_includes);
        let module_graph = Module_graph.graph graph_builder in
        let analyzed_modules = Module_graph.analyzed_modules graph_builder in
        (
          match Package_layout_validator.validate
            ~direct_dependency_modules:(List.map
              direct_dependency_roots
              ~fn:(fun root -> root.root_module))
            ~package:input.package
            ~module_graph
            ~analyzed_modules with
          | Error _ as err -> err
          | Ok () -> (
              match G.topo_sort module_graph with
              | Error cycle_ids ->
                  let cycle =
                    List.filter_map
                      cycle_ids
                      ~fn:(fun node_id ->
                        match G.get_node module_graph node_id with
                        | None -> None
                        | Some node ->
                            Some (
                              match (G.value node).kind with
                              | Module_node.ML m
                              | Module_node.MLI m ->
                                  Module.module_name m
                                  |> Module_name.to_string
                              | Module_node.Library { name; _ } -> "Library(" ^ name ^ ")"
                              | Module_node.Binary { name; _ } -> "Binary(" ^ name ^ ")"
                              | Module_node.Native _ -> "Native"
                              | Module_node.PackageDependency { root_module; _ } ->
                                  "PackageDependency(" ^ root_module ^ ")"
                              | Module_node.C -> "C"
                              | Module_node.H -> "H"
                              | Module_node.Root -> "Root"
                              | Module_node.Other s -> "Other(" ^ s ^ ")"
                            ))
                    |> List.reverse
                  in
                  Error (Planning_error.CyclicDependency { cycle })
              | Ok sorted_modules -> (
                  let (action_graph, _outputs) =
                    Action_graph.from_module_graph
                      ~analyzed_modules
                      ~package:input.package
                      ~profile:input.profile
                      ~ctx:input.ctx
                      ~toolchain:input.toolchain
                      ~store:input.store
                      ~depset:input.depset
                      ~needs_unix
                      ~needs_dynlink
                      module_graph
                  in
                  let sources =
                    sorted_modules
                    |> List.map
                      ~fn:(fun (node: Module_node.t G.node) ->
                        match (G.value node).kind with
                        | Native { files } ->
                            List.map
                              files
                              ~fn:(fun path ->
                                if Path.is_absolute path then
                                  path
                                else
                                  Path.(input.package.path / path))
                        | _ -> (
                            match (G.value node).file with
                            | Concrete path when Path.to_string path != "" ->
                                let abs_path =
                                  if Path.is_absolute path then
                                    path
                                  else
                                    Path.(input.package.path / path)
                                in
                                [ abs_path ]
                            | _ -> []
                          ))
                    |> List.concat
                  in
                  Ok {
                    sources;
                    module_graph;
                    analyzed_modules;
                    action_graph;
                  }
                )
            )
        )
  with
  | exn -> Error (Planning_error.Exception { exn })
