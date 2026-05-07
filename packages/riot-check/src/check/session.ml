open Std
open Std.Collections

module Check_event = Event
module Check_error = Error

open Riot_model

module Typ_check_result = Typ.Analysis.Check_result
module Typ_diagnostic = Typ.Model.Diagnostic
module Typ_file_summary = Typ.Model.FileSummary
module Typ_loaded_modules = Typ.Model.LoadedModules
module Typ_module_typings = Typ.Model.ModuleTypings
module Typ_source = Typ.Model.Source
module Typ_source_id = Typ.Model.SourceId
module Typ_module_surface = Typ.ModuleSurface
module Typ_source_analysis = Typ.SourceAnalysis
module Typ_local_modules = Typ.Model.LocalModules

type package_typ_source = {
  internal_module_name: string;
  local_module_name: string;
  public_module_name: string option;
  display_path: Path.t;
  source_id: Typ_source_id.t;
  source: Typ_source.t;
}

type planned_typ_source = {
  generated: bool;
  source: package_typ_source;
}

type checked_group = {
  checked_files: State.checked_file list;
  module_typings: Typ_module_typings.t list;
}

type cached_package_typings = {
  fingerprint: Crypto.hash;
  typings: Typ_loaded_modules.t;
}

type source_group_typ_context = {
  config: Typ.Config.t;
  dependency_packages: cached_package_typings list;
}

let emit_event = fun ?on_event event ->
  match on_event with
  | Some callback -> callback event
  | None -> ()

let with_typ_event_sink = fun ?on_event config ->
  match on_event with
  | None -> config
  | Some on_event ->
      config
      |> Typ.Config.with_on_event ~on_event:(fun event -> on_event (Check_event.Typ { event }))

let default_registry_name = "pkgs.ml"

let workspace_manifest_path = fun (workspace: Workspace.t) ->
  Path.(workspace.root / Path.v "riot.toml")

let workspace_can_be_prepared = fun (workspace: Workspace.t) ->
  match Fs.exists (workspace_manifest_path workspace) with
  | Ok true -> true
  | Ok false
  | Error _ -> false

let prepare_workspace = fun (workspace: Workspace.t) ->
  if not (workspace_can_be_prepared workspace) then
    Ok workspace
  else
    match Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:default_registry_name () with
    | Error error ->
        Error (Check_error.RegistryInitializationFailed {
          registry = default_registry_name;
          error = Pkgs_ml.Registry_cache.create_error_message error;
        })
    | Ok registry ->
        Riot_deps.ensure_workspace ~mode:Riot_deps.Dep_solver.Refresh ~registry ~workspace ()
        |> Result.map_error
          (fun error ->
            Check_error.WorkspacePreparationFailed {
              path = workspace.root;
              error = Riot_model.Pm_error.message error;
            })

let report_of_analysis = fun path (analysis: Typ_source_analysis.t) ->
  let (item_tree, body_arena, origin_map) =
    match analysis.semantic_tree with
    | Some semantic_tree -> (
      Some semantic_tree.item_tree,
      Some semantic_tree.body_arena,
      Some semantic_tree.origin_map
    )
    | None -> (None, None, None)
  in
  {
    Typ_check_result.source_id = analysis.source.source_id;
    filename = path;
    parse_diagnostics = analysis.parse_diagnostics;
    item_tree;
    body_arena;
    origin_map;
    semantic_tree = analysis.semantic_tree;
    lowering_diagnostics = analysis.lowering_diagnostics;
    typing_diagnostics = analysis.typing_diagnostics;
    file_summary = analysis.file_summary;
    type_index = analysis.type_index;
    exports =
      Typ_source_analysis.exports analysis
      |> List.map (fun (name, scheme) -> (Typ.Model.SurfacePath.to_string name, scheme));
    item_traces = analysis.item_traces;
    expr_traces = analysis.expr_traces;
  }

let checked_file_of_analysis = fun path (analysis: Typ_source_analysis.t) ->
  let report = report_of_analysis path analysis in
  let diagnostics = Diagnostic.from_report report in
  State.Typed { path; report; diagnostics }

let typ_check_prepared_source_of_package_source = fun (source: package_typ_source) ->
  {
    Typ.Check.display_path = source.display_path;
    internal_module_name = Typ_local_modules.InternalName.from_string source.internal_module_name;
    local_module_name = Typ_local_modules.AmbientName.from_string source.local_module_name;
    public_module_name =
      source.public_module_name
      |> Option.map Typ_local_modules.AmbientName.from_string;
    source = source.source;
  }

let workspace_package_by_name = fun (workspace: Workspace.t) package_name ->
  workspace.packages
  |> List.find_opt (fun (pkg: Package.t) -> String.equal pkg.name package_name)

let package_typ_source_files = fun ?(include_dev = false) (pkg: Package.t) ->
  let sources =
    if
      List.is_empty pkg.sources.src
      && List.is_empty pkg.sources.tests
      && List.is_empty pkg.sources.examples
      && List.is_empty pkg.sources.bench
    then
      Package.scan_sources ~package_path:pkg.path ()
    else
      pkg.sources
  in
  let scoped_sources =
    if include_dev then
      ((sources.src @ sources.tests) @ sources.examples) @ sources.bench
    else
      sources.src
  in
  scoped_sources
  |> List.filter Scope.is_supported_source_file
  |> List.map (fun relative -> Path.(pkg.path / relative))
  |> Scope.dedupe_paths

let package_library_typ_source_files = fun (pkg: Package.t) ->
  match pkg.library with
  | None -> package_typ_source_files pkg
  | Some { path = library_path } ->
      let interface_path = Path.(add_extension (remove_extension library_path) ~ext:"mli") in
      let has_interface =
        match Fs.exists interface_path with
        | Ok true -> true
        | Ok false
        | Error _ -> false
      in
      let files =
        if has_interface then
          [ interface_path; library_path ]
        else
          [ library_path ]
      in
      files
      |> List.filter Scope.is_supported_source_file
      |> Scope.dedupe_paths

let package_typ_support_source_files = fun (pkg: Package.t) ->
  package_typ_source_files
    ~include_dev:false
    pkg

let planner_package_for_typing = fun (pkg: Package.t) ~(sources:Package.sources) ->
  Package.make
    ~name:pkg.name
    ~path:pkg.path
    ~relative_path:pkg.relative_path
    ~dependencies:pkg.dependencies
    ~dev_dependencies:pkg.dev_dependencies
    ~build_dependencies:pkg.build_dependencies
    ~foreign_dependencies:pkg.foreign_dependencies
    ~binaries:[]
    ?library:pkg.library
    ~sources
    ~compiler:pkg.compiler
    ~commands:pkg.commands
    ~fix_providers:pkg.fix_providers
    ~publish:pkg.publish
    ()

let planner_source_groups_for_package = fun ?(include_dev = false) (pkg: Package.t) ->
  let sources =
    if
      List.is_empty pkg.sources.src
      && List.is_empty pkg.sources.tests
      && List.is_empty pkg.sources.examples
      && List.is_empty pkg.sources.bench
    then
      Package.scan_sources ~package_path:pkg.path ()
    else
      pkg.sources
  in
  let groups = [
    (Path.v "src", sources.src);
    (Path.v "tests", if include_dev then
      sources.tests
    else
      []);
    (Path.v "examples", if include_dev then
      sources.examples
    else
      []);
    (Path.v "bench", if include_dev then
      sources.bench
    else
      []);
  ]
  in
  groups
  |> List.filter_map
    (fun (planning_root, allowed_source_files) ->
      if List.is_empty allowed_source_files then
        None
      else
        Some (planning_root, allowed_source_files))

let planner_source_group = fun (pkg: Package.t) planning_root allowed_source_files ->
  let root_mode =
    if Path.equal planning_root (Path.v "src") then
      Riot_planner.Module_graph.Library_root {
        library_name = Riot_planner.Package_namespace.planning_library_name pkg;
        public_root_name = Riot_planner.Package_namespace.public_root pkg;
      }
    else
      Riot_planner.Module_graph.Loose_sources
  in
  let namespace =
    if Path.equal planning_root (Path.v "src") then
      Namespace.empty
    else
      Path.to_string planning_root
      |> String.split ~by:"/"
      |> List.filter ~fn:(fun part -> not (String.is_empty part))
      |> List.map ~fn:String.capitalize_ascii
      |> Namespace.from_list
  in
  Riot_planner.Module_graph.{
    source_dir = planning_root;
    allowed_source_files;
    root_mode;
    namespace;
  }

let merge_module_exports = fun preferred fallback ->
  let rec loop seen acc remaining =
    match remaining with
    | [] -> List.rev acc
    | ((name, _) as export) :: tail ->
        if List.exists (Typ.Model.SurfacePath.equal name) seen then
          loop seen acc tail
        else
          loop (name :: seen) (export :: acc) tail
  in
  loop [] [] (preferred @ fallback)

let type_decl_key = fun (type_decl: Typ_file_summary.type_decl) ->
  if Typ.Model.SurfacePath.is_empty type_decl.scope_path then
    type_decl.declaration.type_name
  else
    Typ.Model.SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name
    |> Typ.Model.SurfacePath.to_string

let merge_module_type_decls = fun preferred fallback ->
  let rec loop seen acc remaining =
    match remaining with
    | [] -> List.rev acc
    | ((type_decl: Typ_file_summary.type_decl) as candidate) :: tail ->
        let key = type_decl_key candidate in
        if List.mem key seen then
          loop seen acc tail
        else
          loop (key :: seen) (candidate :: acc) tail
  in
  loop [] [] (preferred @ fallback)

let typings_with_payload = fun template ~source_hash ~exports ~type_decls ->
  let module_name = Typ_module_typings.module_name template in
  match (Typ_module_typings.export_result template, exports) with
  | (Typ_file_summary.TrustedExport _, _) ->
      Typ_module_typings.trusted ~module_name ~source_hash ~type_decls exports
  | (Typ_file_summary.ErroredExport _, _) ->
      Typ_module_typings.errored ~module_name ~source_hash ~type_decls exports
  | (Typ_file_summary.NoExport, []) ->
      Typ_module_typings.missing ~module_name ~source_hash ~type_decls ()
  | (Typ_file_summary.NoExport, _) ->
      Typ_module_typings.errored ~module_name ~source_hash ~type_decls exports

let merge_module_typings = fun preferred fallback ->
  let module_name = Typ_module_typings.module_name preferred in
  let exports =
    merge_module_exports
      (Typ_module_typings.exports preferred)
      (Typ_module_typings.exports fallback)
  in
  let type_decls =
    merge_module_type_decls
      (Typ_module_typings.type_decls preferred)
      (Typ_module_typings.type_decls fallback)
  in
  let template =
    match (Typ_module_typings.export_result preferred, exports) with
    | (Typ_file_summary.NoExport, _ :: _) -> fallback
    | _ -> preferred
  in
  let source_hash =
    Typ_module_typings.synthetic_source_hash
      ~module_name
      ~export_result:(Typ_module_typings.export_result template)
      ~type_decls
      ()
  in
  typings_with_payload template ~source_hash ~exports ~type_decls

let merge_loaded_module_typings = fun preferred fallback ->
  Typ_loaded_modules.merge
    ~preferred:(Typ_loaded_modules.from_list preferred)
    ~fallback:(Typ_loaded_modules.from_list fallback)
    ~combine:merge_module_typings

let merge_loaded_module_index = fun preferred fallback ->
  Typ_loaded_modules.merge
    ~preferred:(Typ_loaded_modules.from_list preferred)
    ~fallback
    ~combine:merge_module_typings

let default_typ_loaded_modules = Typ.Config.default.loaded_modules

let default_typ_config =
  Typ.Config.default
  |> Typ.Config.with_loaded_module_index ~loaded_modules:default_typ_loaded_modules

let rebind_module_typings_name = fun ~module_name (typings: Typ_module_typings.t) ->
  let type_decls =
    Typ_module_surface.qualify_signature_type_decls
      ~module_name
      (Typ_module_typings.type_decls typings)
  in
  let value_definitions = Typ_module_typings.value_definitions typings in
  match Typ_module_typings.export_result typings with
  | Typ_file_summary.TrustedExport { exports } ->
      let exports = Typ_module_surface.qualify_signature_exports ~module_name ~type_decls exports in
      let source_hash =
        Typ_module_typings.synthetic_source_hash
          ~module_name
          ~export_result:(Typ_file_summary.TrustedExport { exports })
          ~type_decls
          ~value_definitions
          ()
      in
      Typ_module_typings.trusted ~module_name ~source_hash ~type_decls ~value_definitions exports
  | Typ_file_summary.ErroredExport { exports } ->
      let exports = Typ_module_surface.qualify_signature_exports ~module_name ~type_decls exports in
      let source_hash =
        Typ_module_typings.synthetic_source_hash
          ~module_name
          ~export_result:(Typ_file_summary.ErroredExport { exports })
          ~type_decls
          ~value_definitions
          ()
      in
      Typ_module_typings.errored ~module_name ~source_hash ~type_decls ~value_definitions exports
  | Typ_file_summary.NoExport ->
      let source_hash =
        Typ_module_typings.synthetic_source_hash
          ~module_name
          ~export_result:Typ_file_summary.NoExport
          ~type_decls
          ~value_definitions
          ()
      in
      Typ_module_typings.missing ~module_name ~source_hash ~type_decls ~value_definitions ()

let hash_equal = fun left right -> String.equal (Crypto.Digest.hex left) (Crypto.Digest.hex right)

let package_typings_of_cached_packages = fun packages ->
  packages
  |> List.fold_left
    (fun loaded_modules (entry: cached_package_typings) ->
      Typ_loaded_modules.merge
        ~preferred:loaded_modules
        ~fallback:entry.typings
        ~combine:merge_module_typings)
    Typ_loaded_modules.empty

let package_fingerprint_of_typ_sources = fun ordered_sources dependency_packages ->
  let module H = Crypto.Sha256 in
  let state = H.create () in
  let () = H.write state "typ-package\x1f" in
  let () =
    dependency_packages
    |> List.iter
      (fun (dependency: cached_package_typings) ->
        H.write state (Crypto.Digest.hex dependency.fingerprint);
        H.write state "\x1f")
  in
  let () =
    ordered_sources
    |> List.iter
      (fun (source: package_typ_source) ->
        H.write state source.internal_module_name;
        H.write state "\x1f";
        H.write state source.local_module_name;
        H.write state "\x1f";
        (
          match source.public_module_name with
          | Some module_name -> H.write state module_name
          | None -> H.write state "-"
        );
        H.write state "\x1f";
        H.write state (Crypto.Digest.hex (Typ_source.input_hash source.source));
        H.write state "\x1f")
  in
  H.finish state

let workspace_typ_store_root = fun (workspace: Workspace.t) ->
  Path.(workspace.target_dir_root / Path.v "typ-cache")

let workspace_typ_store = fun (workspace: Workspace.t) ->
  let contentstore =
    Contentstore.create
      ~root:(workspace_typ_store_root workspace)
      ~policy:Contentstore.Policy.default
      ()
  in
  Typ.Store.create contentstore ()

let workspace_build_store = fun (workspace: Workspace.t) ~profile ~target ->
  Riot_store.Store.create_for_lane
    ~workspace
    ~profile
    ~target

let resolve_typ_profile = fun ~(workspace:Workspace.t) ~(pkg:Package.t) ->
  let base_profile = Profile.apply_overrides Profile.debug workspace.profile_overrides in
  let profile = Profile.apply_overrides base_profile pkg.compiler.profile_overrides in
  let target_platform =
    Build_ctx.make ~session_id:(Session_id.make ()) ~profile ()
    |> Build_ctx.target_platform_name
  in
  match List.assoc_opt target_platform pkg.compiler.target_overrides with
  | Some { Package.profile_override = Some override } -> Profile.apply_override profile override
  | Some { Package.profile_override = None }
  | None -> profile

let workspace_typ_toolchain = fun (workspace: Workspace.t) ->
  Riot_toolchain.init
    ~config:(Toolchain_config.from_root ~root:workspace.Workspace.root)

let local_module_segments_of_module = fun (pkg: Package.t) (mod_: Riot_model.Module.t) ->
  let package_namespace = Package.root_module_name pkg in
  let module_name = Riot_model.Module.module_name mod_ in
  let namespace =
    module_name
    |> Riot_model.Module_name.namespace
    |> Riot_model.Namespace.to_list
  in
  let simple_name = Riot_model.Module_name.to_string module_name in
  let segments =
    match namespace with
    | root :: rest when String.equal root package_namespace -> rest @ [ simple_name ]
    | _ -> namespace @ [ simple_name ]
  in
  let rec collapse_adjacent_duplicates acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.rev acc
    | segment :: rest -> (
        match acc with
        | previous :: _ when String.equal previous segment -> collapse_adjacent_duplicates acc rest
        | _ -> collapse_adjacent_duplicates (segment :: acc) rest
      )
  in
  collapse_adjacent_duplicates [] segments

let public_module_name_of_module = fun (pkg: Package.t) (mod_: Riot_model.Module.t) ->
  match local_module_segments_of_module pkg mod_ with
  | [ module_name ] -> Some module_name
  | _ -> None

let local_module_name_of_module = fun (pkg: Package.t) (mod_: Riot_model.Module.t) ->
  local_module_segments_of_module pkg mod_
  |> String.concat "."

let package_typ_sources_from_planner = fun
  ~on_event ~include_dev ~(workspace:Workspace.t) ~(pkg:Package.t) ->
  let () =
    emit_event
      ?on_event
      (Check_event.PackagePlanningStarted { package_name = pkg.name; include_dev })
  in
  match workspace_typ_toolchain workspace with
  | Error _ -> None
  | Ok toolchain ->
      let profile = resolve_typ_profile ~workspace ~pkg in
      let build_ctx = Build_ctx.make ~session_id:(Session_id.make ()) ~profile () in
      let store =
        workspace_build_store
          workspace
          ~profile:profile.name
          ~target:(Riot_model.Target.to_string (Riot_dirs.host_target ()))
      in
      let next_source_id = ref 0 in
      let sources =
        let effective_sources =
          if
            List.is_empty pkg.sources.src
            && List.is_empty pkg.sources.tests
            && List.is_empty pkg.sources.examples
            && List.is_empty pkg.sources.bench
          then
            Package.scan_sources ~package_path:pkg.path ()
          else
            pkg.sources
        in
        let planner_pkg = planner_package_for_typing pkg ~sources:effective_sources in
        let source_groups = planner_source_groups_for_package ~include_dev pkg in
        let () =
          emit_event
            ?on_event
            (
              Check_event.PackagePlanningFinished {
                package_name = pkg.name;
                include_dev;
                group_count = List.length source_groups;
                allowed_source_count =
                  source_groups
                  |> List.map
                    (fun (_planning_root, allowed_source_files) -> List.length allowed_source_files)
                  |> List.fold_left Int.add 0;
              }
            )
        in
        source_groups
        |> List.concat_map
          (fun (planning_root, allowed_source_files) ->
            let () =
              emit_event
                ?on_event
                (
                  Check_event.PackageSourcePreparationStarted {
                    package_name = pkg.name;
                    planning_root;
                    allowed_source_count = List.length allowed_source_files;
                    include_dev;
                  }
                )
            in
            let input =
              Riot_planner.Module_planner.{
                package = planner_pkg;
                profile;
                ctx = build_ctx;
                toolchain;
                workspace;
                source_groups = [
                  planner_source_group planner_pkg planning_root allowed_source_files;
                ];
                depset = [];
                dependency_packages = [];
                store;
                on_source_analyzed = (fun _ -> ());
              }
            in
            match Riot_planner.Module_planner.plan_node input with
            | Error err ->
                let () =
                  emit_event
                    ?on_event
                    (Check_event.PackageSourcePreparationFailed {
                      package_name = pkg.name;
                      planning_root;
                      reason = Riot_planner.Planning_error.to_string err;
                    })
                in
                []
            | Ok plan -> (
                let analyzed_modules = map plan.analyzed_modules in
                match Std.Graph.SimpleGraph.topo_sort plan.module_graph with
                | Error _ ->
                    let () =
                      emit_event
                        ?on_event
                        (Check_event.PackageSourcePreparationFailed {
                          package_name = pkg.name;
                          planning_root;
                          reason = "module graph contains a cycle";
                        })
                    in
                    []
                | Ok nodes ->
                    let produced_sources =
                      nodes
                      |> List.filter_map
                        (fun (node: Riot_planner.Module_node.t Std.Graph.SimpleGraph.node) ->
                          let node_value = Std.Graph.SimpleGraph.value node in
                          let node_id = Std.Graph.SimpleGraph.id node in
                          match node_value.kind with
                          | Riot_planner.Module_node.ML mod_
                          | Riot_planner.Module_node.MLI mod_ -> (
                              match HashMap.get analyzed_modules node_id with
                              | None -> None
                              | Some analyzed -> (
                                  match analyzed.cst with
                                  | Error _ -> None
                                  | Ok cst ->
                                      let source_id = Typ_source_id.from_int !next_source_id in
                                      let () =
                                        next_source_id := !next_source_id + 1
                                      in
                                      let internal_module_name =
                                        mod_
                                        |> Riot_model.Module.module_name
                                        |> Riot_model.Module_name.qualified_name
                                      in
                                      let source =
                                        let implicit_opens =
                                          analyzed.implicit_opens
                                          |> List.map Typ.Model.SurfacePath.from_string
                                        in
                                        Typ_source.make_prepared
                                          ~source_id
                                          ~kind:(
                                            match node_value.file with
                                            | Riot_planner.Module_node.Generated _ ->
                                                Typ_source.Generated
                                            | Riot_planner.Module_node.Concrete _ -> Typ_source.File
                                          )
                                          ~module_name:internal_module_name
                                          ~implicit_opens
                                          ~origin:(Typ_source.Path analyzed.display_path)
                                          ~revision:0
                                          ~source_hash:analyzed.source_hash
                                          ~parse_result:analyzed.parse_result
                                          ~cst
                                      in
                                      Some {
                                        generated =
                                          (
                                            match node_value.file with
                                            | Riot_planner.Module_node.Generated _ -> true
                                            | Riot_planner.Module_node.Concrete _ -> false
                                          );
                                        source =
                                          {
                                            internal_module_name;
                                            local_module_name = local_module_name_of_module pkg mod_;
                                            public_module_name = public_module_name_of_module
                                              pkg
                                              mod_;
                                            display_path = analyzed.display_path;
                                            source_id;
                                            source;
                                          };
                                      }
                                )
                            )
                          | Riot_planner.Module_node.C
                          | Riot_planner.Module_node.H
                          | Riot_planner.Module_node.Other _
                          | Riot_planner.Module_node.Root
                          | Riot_planner.Module_node.Native _
                          | Riot_planner.Module_node.Library _
                          | Riot_planner.Module_node.Binary _ -> None)
                    in
                    let () =
                      emit_event
                        ?on_event
                        (
                          Check_event.PackageSourcePreparationFinished {
                            package_name = pkg.name;
                            planning_root;
                            produced_source_count = List.length produced_sources;
                            generated_source_count =
                              produced_sources
                              |> List.filter
                                (fun (planned: planned_typ_source) -> planned.generated)
                              |> List.length;
                          }
                        )
                    in
                    produced_sources
              ))
      in
      let concrete_module_names =
        sources
        |> List.filter_map
          (fun planned ->
            if planned.generated then
              None
            else
              Some planned.source.internal_module_name)
        |> List.sort_uniq String.compare
      in
      sources
      |> List.filter
        (fun planned ->
          (not planned.generated)
          || not (List.mem planned.source.internal_module_name concrete_module_names))
      |> List.map (fun planned -> planned.source)
      |> Option.some

let qualify_typings_exports = fun module_name type_decls exports ->
  Typ_module_surface.qualify_exports
    ~module_name
    ~type_decls
    exports

let qualify_typings_type_decls = fun module_name type_decls ->
  Typ_module_surface.qualify_type_decls
    ~module_name
    type_decls

let relative_module_name = fun ~current_local_module_name module_name ->
  let module_path = Typ.Model.SurfacePath.from_string module_name in
  let current_module_path = Typ.Model.SurfacePath.from_string current_local_module_name in
  let current_scope_path =
    current_module_path
    |> Typ.Model.SurfacePath.split_last
    |> Option.map fst
  in
  let prefixes = [
    current_module_path;
    (
      match current_scope_path with
      | Some scope_path when not (Typ.Model.SurfacePath.is_empty scope_path) -> scope_path
      | _ -> Typ.Model.SurfacePath.empty
    );
  ]
  in
  prefixes
  |> List.find_map
    (fun prefix ->
      if Typ.Model.SurfacePath.is_empty prefix then
        None
      else
        module_path
        |> Typ.Model.SurfacePath.strip_prefix ~prefix
        |> Option.filter (fun relative -> not (Typ.Model.SurfacePath.is_empty relative))
        |> Option.map Typ.Model.SurfacePath.to_string)

let relative_ambient_exports_for_loaded_modules = fun
  ?exclude_module_name ~current_local_module_name loaded_modules ->
  Typ_loaded_modules.fold
    (fun _required_name typings ambient ->
      let module_name = Typ_module_typings.module_name typings in
      if match exclude_module_name with
      | Some excluded_module_name -> String.equal module_name excluded_module_name
      | None -> false then
        ambient
      else
        match relative_module_name ~current_local_module_name module_name with
        | Some relative_module_name ->
            ambient
            @ qualify_typings_exports
              relative_module_name
              (Typ_module_typings.type_decls typings)
              (Typ_module_typings.exports typings)
        | None -> ambient)
    loaded_modules
    []

let relative_ambient_type_decls_for_loaded_modules = fun
  ?exclude_module_name ~current_local_module_name loaded_modules ->
  Typ_loaded_modules.fold
    (fun _required_name typings ambient ->
      let module_name = Typ_module_typings.module_name typings in
      if match exclude_module_name with
      | Some excluded_module_name -> String.equal module_name excluded_module_name
      | None -> false then
        ambient
      else
        match relative_module_name ~current_local_module_name module_name with
        | Some relative_module_name ->
            ambient
            @ qualify_typings_type_decls
              relative_module_name
              (Typ_module_typings.type_decls typings)
        | None -> ambient)
    loaded_modules
    []

let ambient_env_for_loaded_modules = fun
  ~current_module_name ~current_local_module_name loaded_modules ->
  let base_ambient =
    Typ_loaded_modules.fold
      (fun _required_name typings ambient ->
        let module_name = Typ_module_typings.module_name typings in
        if String.equal current_module_name module_name then
          ambient
        else
          ambient
          @ qualify_typings_exports
            module_name
            (Typ_module_typings.type_decls typings)
            (Typ_module_typings.exports typings))
      loaded_modules
      []
  in
  base_ambient
  @ relative_ambient_exports_for_loaded_modules
    ~exclude_module_name:current_module_name
    ~current_local_module_name
    loaded_modules

let ambient_type_decls_for_loaded_modules = fun
  ~current_module_name ~current_local_module_name loaded_modules ->
  let base_ambient =
    Typ_loaded_modules.fold
      (fun _required_name typings ambient ->
        let module_name = Typ_module_typings.module_name typings in
        if String.equal current_module_name module_name then
          ambient
        else
          ambient @ qualify_typings_type_decls module_name (Typ_module_typings.type_decls typings))
      loaded_modules
      []
  in
  base_ambient
  @ relative_ambient_type_decls_for_loaded_modules
    ~exclude_module_name:current_module_name
    ~current_local_module_name
    loaded_modules

let missing_requirements_reason = fun missing ->
  let details =
    Typ.Session.MissingRequirements.requirements missing
    |> List.map
      (fun __tmp1 ->
        match __tmp1 with
        | Typ.Session.MissingRequirements.MissingRootSource { source_id } ->
            "root:" ^ Int.to_string (Typ.Model.SourceId.to_int source_id)
        | Typ.Session.MissingRequirements.MissingModuleSummary { module_name; _ } ->
            "module:" ^ module_name
        | Typ.Session.MissingRequirements.LocalModuleCycle { module_names; _ } ->
            "cycle:" ^ String.concat " -> " module_names)
    |> String.concat ", "
  in
  if String.equal details "" then
    "missing type requirements"
  else
    "missing type requirements: " ^ details

let incremental_check_error_reason = fun __tmp1 ->
  match __tmp1 with
  | Typ.Check.MissingRequirements { module_name; requirements } ->
      "while checking "
      ^ Typ_local_modules.InternalName.to_string module_name
      ^ ": "
      ^ missing_requirements_reason requirements
  | Typ.Check.MissingModuleTypings { module_name } ->
      "missing authoritative module typings for "
      ^ Typ_local_modules.InternalName.to_string module_name
  | Typ.Check.MissingAnalysis { module_name; path } ->
      "missing checked analysis for "
      ^ Typ_local_modules.InternalName.to_string module_name
      ^ " at "
      ^ Path.to_string path
  | Typ.Check.StoreFailure { module_name; reason } ->
      "while persisting module typings for "
      ^ Typ_local_modules.InternalName.to_string module_name
      ^ ": "
      ^ reason
  | Typ.Check.PackageStoreFailure { package_name; reason } ->
      "while persisting package bundle for " ^ package_name ^ ": " ^ reason

let load_package_module_typings_from_store = fun store (pkg: Package.t) ->
  match Typ.Store.load_package_bundle store ~package_name:pkg.name with
  | Some ({ typings = []; _ } as bundle) ->
      Some { fingerprint = bundle.fingerprint; typings = Typ_loaded_modules.empty }
  | Some bundle ->
      Some {
        fingerprint = bundle.fingerprint;
        typings = Typ_loaded_modules.from_list bundle.typings;
      }
  | None -> None

let workspace_dependency_packages = fun ~include_dev (workspace: Workspace.t) (pkg: Package.t) ->
  let dependencies =
    if include_dev then
      Package.build_graph_dependencies pkg
    else
      pkg.dependencies
  in
  dependencies
  |> List.filter_map
    (fun (dependency: Package.dependency) -> workspace_package_by_name workspace dependency.name)
  |> List.sort_uniq
    (fun (left: Package.t) (right: Package.t) -> String.compare left.name right.name)

let workspace_module_typings_for_package =
  let rec load
    cache
    typ_store
    (workspace: Workspace.t)
    ?on_event
    ?(visiting = [])
    (pkg: Package.t) =
    match List.assoc_opt pkg.name !cache with
    | Some entry -> entry
    | None when List.mem pkg.name visiting ->
        { fingerprint = Crypto.hash_string ""; typings = Typ_loaded_modules.empty }
    | None ->
        let dependency_packages =
          workspace_dependency_packages ~include_dev:false workspace pkg
          |> List.map
            (fun dependency_pkg ->
              load
                cache
                typ_store
                workspace
                ?on_event
                ~visiting:(pkg.name :: visiting)
                dependency_pkg)
        in
        let dependency_typings = package_typings_of_cached_packages dependency_packages in
        let ordered_sources =
          match package_typ_sources_from_planner ~on_event ~include_dev:false ~workspace ~pkg with
          | Some ordered_sources -> ordered_sources
          | None -> []
        in
        let package_fingerprint =
          package_fingerprint_of_typ_sources ordered_sources dependency_packages
        in
        let compute_package_typings () =
          let () =
            emit_event
              ?on_event
              (Check_event.PackageEngineSelected {
                package_name = pkg.name;
                engine = Check_event.AuthoritativePackageEngine;
              })
          in
          let loaded_modules =
            Typ_loaded_modules.merge
              ~preferred:dependency_typings
              ~fallback:default_typ_loaded_modules
              ~combine:merge_module_typings
          in
          let config =
            default_typ_config
            |> Typ.Config.with_loaded_module_index ~loaded_modules
            |> Typ.Config.with_store ~store:(Some typ_store)
            |> Typ.Config.with_capture_traces ~capture_traces:false
            |> with_typ_event_sink ?on_event
          in
          match Typ.Check.fold_package_sources
            ~package_name:pkg.name
            ~package_fingerprint
            ~config
            ~ordered_sources:(List.map typ_check_prepared_source_of_package_source ordered_sources)
            ~init:()
            ~f:(fun () (_finished_group: Typ.Check.finished_group) -> ())
            () with
          | Ok result -> result.public_module_typings
          | Error error -> raise (Failure (incremental_check_error_reason error))
        in
        let package_typings =
          match load_package_module_typings_from_store typ_store pkg with
          | Some cached when hash_equal cached.fingerprint package_fingerprint ->
              let () =
                emit_event ?on_event (Check_event.PackageCached { package_name = pkg.name })
              in
              cached.typings
          | None ->
              let () = emit_event ?on_event (Check_event.Package { package_name = pkg.name }) in
              if List.is_empty ordered_sources then
                Typ_loaded_modules.empty
              else
                compute_package_typings ()
          | Some _ ->
              let () = emit_event ?on_event (Check_event.Package { package_name = pkg.name }) in
              if List.is_empty ordered_sources then
                Typ_loaded_modules.empty
              else
                compute_package_typings ()
        in
        let typings =
          Typ_loaded_modules.merge
            ~preferred:package_typings
            ~fallback:dependency_typings
            ~combine:merge_module_typings
        in
        let entry = { fingerprint = package_fingerprint; typings } in
        let () =
          cache := (pkg.name, entry) :: !cache
        in
        entry
  in
  load

let typ_config_for_source_group = fun ~workspace ~summary_cache ~include_dev ?on_event paths ->
  match paths with
  | [] ->
      {
        config =
          default_typ_config
          |> Typ.Config.with_capture_traces ~capture_traces:false
          |> with_typ_event_sink ?on_event;
        dependency_packages = [];
      }
  | path :: _ -> (
      match Workspace.find_package_for_path workspace ~path with
      | None ->
          {
            config =
              default_typ_config
              |> Typ.Config.with_capture_traces ~capture_traces:false
              |> with_typ_event_sink ?on_event;
            dependency_packages = [];
          }
      | Some pkg ->
          let typ_store = workspace_typ_store workspace in
          let dependency_packages =
            workspace_dependency_packages ~include_dev workspace pkg
            |> List.map
              (fun dependency_pkg ->
                workspace_module_typings_for_package
                  summary_cache
                  typ_store
                  workspace
                  ?on_event
                  dependency_pkg)
          in
          let dependency_typings = package_typings_of_cached_packages dependency_packages in
          let loaded_modules =
            Typ_loaded_modules.merge
              ~preferred:dependency_typings
              ~fallback:default_typ_loaded_modules
              ~combine:merge_module_typings
          in
          {
            config =
              default_typ_config
              |> Typ.Config.with_loaded_module_index ~loaded_modules
              |> Typ.Config.with_store ~store:(Some typ_store)
              |> Typ.Config.with_capture_traces ~capture_traces:false
              |> with_typ_event_sink ?on_event;
            dependency_packages;
          }
    )

let path_key = fun path ->
  Path.normalize path
  |> Path.to_string

let checked_group_for_package_scan = fun
  ?on_event config ~package_name ~package_fingerprint ordered_sources target_paths ->
  let checked_files_by_path = ref [] in
  let record_checked_file path analysis =
    checked_files_by_path := (path_key path, checked_file_of_analysis path analysis)
    :: !checked_files_by_path
  in
  let () =
    emit_event
      ?on_event
      (Check_event.PackageEngineSelected {
        package_name;
        engine = Check_event.AuthoritativePackageEngine;
      })
  in
  match Typ.Check.fold_package_sources
    ~package_name
    ~package_fingerprint
    ~config
    ~ordered_sources:(List.map typ_check_prepared_source_of_package_source ordered_sources)
    ~init:()
    ~f:(fun () (finished_group: Typ.Check.finished_group) ->
      finished_group.checked_sources
      |> List.iter
        (fun (checked_source: Typ.Check.checked_source) ->
          record_checked_file
            checked_source.path
            checked_source.analysis))
    () with
  | Error error ->
      let reason = incremental_check_error_reason error in
      {
        checked_files =
          target_paths
          |> List.map
            (fun path ->
              match List.assoc_opt (path_key path) !checked_files_by_path with
              | Some checked_file -> checked_file
              | None -> State.Unreadable { path; reason });
        module_typings = [];
      }
  | Ok result ->
      {
        checked_files =
          target_paths
          |> List.map
            (fun path ->
              match List.assoc_opt (path_key path) !checked_files_by_path with
              | Some checked_file -> checked_file
              | None ->
                  State.Unreadable {
                    path;
                    reason = "missing checked result for " ^ Path.to_string path;
                  });
        module_typings = Typ_loaded_modules.values result.public_module_typings;
      }

let package_root_for_target = fun (workspace: Workspace.t) path ->
  Workspace.find_package_for_path workspace ~path
  |> Option.map (fun (pkg: Package.t) -> pkg.path)

let grouped_targets_for_session = fun ~workspace target_files ->
  target_files
  |> List.fold_left
    (fun groups path ->
      let key =
        match package_root_for_target workspace path with
        | Some package_root -> Path.to_string package_root
        | None -> Path.to_string (Path.dirname path)
      in
      let existing =
        match List.assoc_opt key groups with
        | Some existing -> existing
        | None -> []
      in
      (key, existing @ [ path ]) :: List.remove_assoc key groups)
    []
  |> List.rev

let check_target_files = fun
  ~workspace ~scan_mode:_ ~include_dev ?on_event ?on_result target_files ->
  let summary_cache = ref [] in
  let checked_by_path = ref [] in
  let emit checked_file =
    checked_by_path := (path_key (State.checked_file_path checked_file), checked_file)
    :: !checked_by_path;
    match on_result with
    | Some callback -> callback checked_file
    | None -> ()
  in
  let () =
    grouped_targets_for_session ~workspace target_files
    |> List.iter
      (fun (_, group_targets) ->
        let { config; dependency_packages } =
          typ_config_for_source_group ~workspace ~summary_cache ~include_dev ?on_event group_targets
        in
        let emit_unreadable reason =
          group_targets
          |> List.iter (fun path -> emit (State.Unreadable { path; reason }))
        in
        match group_targets with
        | [] -> ()
        | path :: _ -> (
            match Workspace.find_package_for_path workspace ~path with
            | None ->
                emit_unreadable "planner-backed source preparation requires a workspace package"
            | Some pkg -> (
                let () = emit_event ?on_event (Check_event.Package { package_name = pkg.name }) in
                match package_typ_sources_from_planner ~on_event ~include_dev ~workspace ~pkg with
                | None ->
                    emit_unreadable
                      ("failed to prepare planner-owned sources for package " ^ pkg.name)
                | Some ordered_sources ->
                    let package_fingerprint =
                      package_fingerprint_of_typ_sources ordered_sources dependency_packages
                    in
                    let checked_group =
                      checked_group_for_package_scan
                        ?on_event
                        config
                        ~package_name:pkg.name
                        ~package_fingerprint
                        ordered_sources
                        group_targets
                    in
                    let () =
                      emit_event
                        ?on_event
                        (Check_event.PackageCheckedGroupEmitStarted {
                          package_name = pkg.name;
                          checked_file_count = List.length checked_group.checked_files;
                        })
                    in
                    let () =
                      checked_group.checked_files
                      |> List.iter emit
                    in
                    let () =
                      emit_event
                        ?on_event
                        (Check_event.PackageCheckedGroupEmitFinished {
                          package_name = pkg.name;
                          checked_file_count = List.length checked_group.checked_files;
                        })
                    in
                    ()
              )
          ))
  in
  target_files
  |> List.map
    (fun path ->
      !checked_by_path
      |> List.assoc_opt (path_key path)
      |> Option.expect ~msg:("missing checked result for " ^ Path.to_string path))
