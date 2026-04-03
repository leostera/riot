(** Graph Builder - Constructs module dependency graphs for OCaml packages

    This module implements the core build graph construction algorithm that:

    1. SCANNING PHASE: Recursively scans source directories to build a forest of
    nodes
    - Creates nodes for ML/MLI/C/H files
    - Generates library interface modules for directories
    - Creates alias modules for namespace flattening
    - Adds structural edges (parent-child, ML->MLI, library dependencies)

    2. WIRING PHASE: Runs syntactic dependency analysis to discover
    module-level dependencies
    - Collects all concrete ML/MLI files from the graph
    - Parses each file in memory and extracts raw module references
    - Adds dependency edges based on `open` statements and module references
    - Skips MLI -> ML edges (interfaces shouldn't depend on implementations)

    Key edge cases handled:
    - Binary sources must be excluded from library compilation
    - Library interface files (foo/foo.ml) must not be included in their own
      children
    - Subdirectories create virtual modules if no corresponding file exists
    - Namespace management for nested libraries
    - Cycle prevention between library interfaces and sublibraries
    - MLI files cannot depend on ML files (enforced during wiring)

    The algorithm is a port of miniriot's dep_graph.ml with full support for:
    - Namespacing (Foo__Bar__Baz)
    - Directory-based libraries
    - Recursive sublibraries
    - Generated vs concrete library interface modules *)
open Std
open Std.Collections
open Riot_model
module G = Std.Graph.SimpleGraph

type config = {
  root: Path.t;
  source_dir: Path.t;
  allowed_source_files: Path.t list;
  namespace: string;
  package: Package.t;
  toolchain: Riot_toolchain.t;
  workspace: Workspace.t;
}

type t = {
  config: config;
  graph: Module_node.t G.t;
  registry: Module_registry.t;
  entries: Module_scanner.entry list;
}

type scan_ctx = {
  ns: Namespace.t;
  parent_intf: Module_node.t G.node;
  parent_impl: Module_node.t G.node;
  aliases: Module_node.t G.node list;
}

(** Scan context passed through recursive directory traversal.

    Fields:
    - ns: Current namespace (e.g., [] for top-level, ["Foo"; "Bar"] for
      Foo/Bar/)
    - parent_intf: Library interface node that interface files should depend on
    - parent_impl: Library implementation node that implementation files should
      depend on
    - aliases: Alias modules in scope that all files should open *)
let make_relative = fun ~base ~path ->
  let base_str = Path.to_string base in
  let path_str = Path.to_string path in
  let prefix = base_str ^ "/" in
  if String.starts_with ~prefix path_str then
    let len = String.length prefix in
    Path.v (String.sub path_str len (String.length path_str - len))
  else
    path

let rec filter_entries = fun ~allowed entries ->
  let allowed_strings = List.map Path.to_string allowed in
  List.filter_map
    (
      function
      | Module_scanner.ML (name, path) ->
          if List.mem (Path.to_string path) allowed_strings then
            Some (Module_scanner.ML (name, path))
          else
            None
      | Module_scanner.MLI (name, path) ->
          if List.mem (Path.to_string path) allowed_strings then
            Some (Module_scanner.MLI (name, path))
          else
            None
      | Module_scanner.C (name, path) ->
          if List.mem (Path.to_string path) allowed_strings then
            Some (Module_scanner.C (name, path))
          else
            None
      | Module_scanner.H (name, path) ->
          if List.mem (Path.to_string path) allowed_strings then
            Some (Module_scanner.H (name, path))
          else
            None
      | Module_scanner.Other _ ->
          None
      | Module_scanner.Dir (name, path, children) ->
          let children = filter_entries ~allowed children in
          if List.length children = 0 then
            None
          else
            Some (Module_scanner.Dir (name, path, children))
    )
    entries

(** Check if a path is a binary source file.

    Binary paths are stored as ABSOLUTE in Package.binary, while scanned paths
    are RELATIVE to package root. We must normalize both before comparing. *)
let is_binary = fun config path ->
  let bin_rel = make_relative ~base:config.package.path ~path in
  List.exists
    (fun (bin: Package.binary) ->
      let bin_abs_rel = make_relative ~base:config.package.path ~path:bin.path in
      Path.equal path bin_rel && Path.equal bin_rel bin_abs_rel)
    config.package.binaries

(** Recursively scan directory entries and build graph nodes.

    Processing order (entries are pre-sorted by Module_scanner): 1. MLI files
    (interfaces must compile before implementations) 2. ML files (but binaries
    are handled separately) 3. C files (compiled to .o objects) 4. H files
    (headers, no compilation needed) 5. Directories (create library interface
    modules, descend recursively)

    Binary detection uses a guard pattern to separate binary handling from
    regular module handling, keeping each function focused. *)
let rec do_scan = fun ~t ~ctx entries ->
  match entries with
  | [] -> ()
  | entry :: rest -> (
      match entry with
      | Module_scanner.ML (name, path) when is_binary t.config path ->
          handle_ocaml_binary ~t ~ctx path;
          do_scan ~t ~ctx rest
      | Module_scanner.ML (_, path)
      | Module_scanner.MLI (_, path) ->
          handle_ocaml_module ~t ~ctx path;
          do_scan ~t ~ctx rest
      | Module_scanner.C (_, _)
      | Module_scanner.H (_, _) ->
          (* C and H files are handled separately in action_graph.ml *)
          do_scan ~t ~ctx rest
      | Module_scanner.Other _ ->
          do_scan ~t ~ctx rest
      | Module_scanner.Dir (name, path, children) ->
          handle_library ~t ~ctx path name children;
          do_scan ~t ~ctx rest
    )

(** Handle a binary source file.

    Binaries are compiled separately from the library, so we do nothing during
    the library scan phase. Binary compilation will be handled later during
    action generation. *)
and handle_ocaml_binary = fun ~t ~ctx:_ _path -> ()

(** Handle a regular OCaml module file (.ml or .mli).

    Creates a graph node and adds structural edges: 1. ML implementation ->
    corresponding MLI interface (if it exists) 2. Module -> parent library
    interface 3. Module -> alias modules in scope (for namespace flattening)

    The registry tracks modules by name so we can later find the corresponding
    .mli when processing a .ml file. *)
and handle_ocaml_module = fun ~t ~ctx path ->
  let { ns; aliases; parent_impl; parent_intf } = ctx in
  let mod_ = Module.make ~namespace:ns ~filename:path in
  let file = Module_node.Concrete path in
  let node_val =
    match Module.kind mod_ with
    | `interface -> Module_node.make_mli mod_ file
    | `implementation -> Module_node.make_ml mod_ file
  in
  Module_node.set_open_modules node_val aliases;
  let node = G.add_node t.graph node_val in
  Module_registry.register t.registry mod_ node.id;
  (
    match Module.kind mod_ with
    | `implementation -> (
        let mod_name = Module.module_name mod_ |> Module_name.to_string in
        try
          let node_ids = Module_registry.get_by_name t.registry mod_name in
          List.iter
            (fun intf_node_id ->
              match G.get_node t.graph intf_node_id with
              | Some intf_node -> (
                  match intf_node.value.kind with
                  | MLI intf_mod when Module.module_name intf_mod |> Module_name.to_string = mod_name -> G.add_edge
                    node
                    ~depends_on:intf_node
                  | _ -> ()
                )
              | None -> ())
            node_ids
        with
        | Not_found -> ()
      )
    | `interface -> ()
  );
  let parent =
    match Module.kind mod_ with
    | `interface -> parent_intf
    | `implementation -> parent_impl
  in
  G.add_edge parent ~depends_on:node;
  List.iter (fun aliases_node -> G.add_edge node ~depends_on:aliases_node) aliases

(** Handle a directory as a library.

    This is the most complex part of the build graph construction. A directory
    becomes a library with the following structure:

    Given directory "foo/" with children [bar.ml, baz.ml, qux/]:

    1. Library interface modules (foo.ml, foo.mli):
    - May be concrete (user-written) or generated (auto-created)
    - Generated content: "module Bar = Foo__Bar\nmodule Baz = Foo__Baz\n..."

    2. Alias module (Foo__Aliases.ml):
    - Flattens namespace: "module Bar = Foo__Bar"
    - All files in this library implicitly open this

    3. Child modules in new namespace:
    - bar.ml becomes Foo__Bar
    - baz.ml becomes Foo__Baz
    - qux/ becomes a nested library (recursive)

    Edge structure:
    - foo.ml depends on foo.mli, Foo__Aliases, and all children
    - foo.mli depends on Foo__Aliases and children
    - All children depend on Foo__Aliases (implicit open)
    - Parent library depends on foo.ml/foo.mli

    Cycle prevention:
    - Concrete foo.ml only depends on child FILES, not subdirectories
    - Generated foo.ml depends on everything (safe because it's explicit) *)
and handle_library = fun ~t ~ctx dir name children ->
  let { ns; aliases; parent_impl; parent_intf } = ctx in
  let lib_module_name = Module_name.of_string name in
  let intf_file = Module_name.canonical_mli lib_module_name in
  let impl_file = Module_name.canonical_ml lib_module_name in
  let intf_mod = Module.make ~namespace:ns ~filename:intf_file in
  let impl_mod = Module.make ~namespace:ns ~filename:impl_file in
  let ns =
    let namespaced_lib = Module.module_name impl_mod in
    Namespace.append ns (Module_name.to_string namespaced_lib)
  in
  let lib_def = Library_definition.from_entries
    ~namespace:ns
    ~library_name:name
    ~package_path:t.config.package.path
    ~binaries:t.config.package.binaries
    children in
  let child_modules = Library_definition.child_modules lib_def in
  let children_without_lib = Library_definition.children_without_lib lib_def in
  (* Skip creating library interface nodes for libraries with no OCaml content at all.
     We still create them if:
     - There are child modules, OR
     - There are concrete library interface files (lib.ml/lib.mli exist)
  *)
  let has_ocaml_content =
    child_modules != []
    || Library_definition.has_concrete_ml lib_def
    || Library_definition.has_concrete_mli lib_def in
  if not has_ocaml_content then
    do_scan ~t ~ctx children_without_lib
  else
    let aliases_node =
      let node = Alias_module.make_node ns child_modules in
      G.add_node t.graph node
    in
    let lib_aliases =
      if Namespace.is_empty ns then
        [ aliases_node ]
      else
        aliases @ [ aliases_node ]
    in
    let intf_node =
      let intf = Library_interface.make_node
        intf_mod
        child_modules
        lib_aliases
        ~exists:(Library_definition.has_concrete_mli lib_def)
        ~actual_path:(Library_definition.concrete_mli_path lib_def) in
      G.add_node t.graph intf
    in
    Module_registry.register t.registry intf_mod intf_node.id;
    let impl_node =
      let impl = Library_interface.make_node
        impl_mod
        child_modules
        lib_aliases
        ~exists:(Library_definition.has_concrete_ml lib_def)
        ~actual_path:(Library_definition.concrete_ml_path lib_def) in
      G.add_node t.graph impl
    in
    Module_registry.register t.registry impl_mod impl_node.id;
    G.add_edge intf_node ~depends_on:aliases_node;
    G.add_edge impl_node ~depends_on:aliases_node;
    G.add_edge impl_node ~depends_on:intf_node;
    let ctx = {
      ns;
      aliases = aliases @ [ aliases_node ];
      parent_impl = impl_node;
      parent_intf = intf_node
    } in
    do_scan ~t ~ctx children_without_lib;
    let deps_for_library_interface = Library_definition.deps_for_library_interface lib_def in
    List.iter
      (fun child_mod ->
        try
          let child_node_ids = Module_registry.get_by_name t.registry
            (Module.module_name child_mod |> Module_name.to_string)
          in
          List.iter
            (fun child_node_id ->
              match G.get_node t.graph child_node_id with
              | Some child_node ->
                  G.add_edge intf_node ~depends_on:child_node;
                  G.add_edge impl_node ~depends_on:child_node
              | None -> ())
            child_node_ids
        with
        | Not_found -> ())
      deps_for_library_interface

let scan_sources = fun t (sources: Module_scanner.entry list) ->
  let root_node = Module_node.make_root () in
  let root = G.add_node t.graph root_node in
  let ctx = { ns = Namespace.empty; parent_impl = root; parent_intf = root; aliases = [] } in
  handle_library ~t ~ctx t.config.source_dir t.config.package.name sources

let create = fun config ->
  let entries = Module_scanner.scan ~root:config.root ~source_dir:config.source_dir
  |> filter_entries ~allowed:config.allowed_source_files in
  let graph = G.make () in
  let registry = Module_registry.create () in
  let t = { config; graph; registry; entries } in
  scan_sources t entries;
  t

(** Wire module dependencies using `Syn.Deps`.

    This function implements Phase 2 of graph construction by analyzing source
    files in memory to discover which modules reference which other modules
    (via `open` statements, direct module references, etc.).

    Algorithm: 1. Collect all concrete ML/MLI nodes (skip generated files and
    non-OCaml files) 2. Parse each file and extract raw module dependencies with
    `Syn.Deps` 3. Resolve dependency names using the file's namespace 4. Add
    graph edges to the referenced modules 5. Skip MLI -> ML edges (interfaces
    shouldn't depend on
    implementations)

    Edge cases:
    - Generated files are excluded (they have no `open` statements to analyze)
    - Missing dependencies are silently skipped (external modules, stdlib)
    - MLI -> ML dependencies are filtered out to maintain proper compilation
      order *)
let wire_dependencies = fun t ->
  let preferred_dependency_nodes dep_node_ids =
    let rec collect acc has_ml = function
      | [] -> (List.rev acc, has_ml)
      | dep_node_id :: rest -> (
          match G.get_node t.graph dep_node_id with
          | Some (dep_node: Module_node.t G.node) -> (
              match dep_node.value.kind with
              | Module_node.ML _ -> collect ((dep_node_id, dep_node) :: acc) true rest
              | _ -> collect ((dep_node_id, dep_node) :: acc) has_ml rest
            )
          | None -> collect acc has_ml rest
        )
    in
    let resolved_nodes, has_ml = collect [] false dep_node_ids in
    if has_ml then
      List.filter
        (fun ((_dep_node_id, (dep_node: Module_node.t G.node))) ->
          match dep_node.value.kind with
          | Module_node.ML _ -> true
          | _ -> false)
        resolved_nodes
    else
      resolved_nodes
  in
  let all_nodes =
    G.map t.graph ~fn:(fun ((node_id, node)) -> (node_id, node))
  in
  (* Sort nodes by ID to ensure deterministic ordering - G.map uses Hashtbl.to_seq which is non-deterministic *)
  let sorted_nodes =
    List.sort
      (fun ((id1, _)) ((id2, _)) ->
        Int.compare (G.Node_id.to_int id1) (G.Node_id.to_int id2))
      all_nodes
  in
  let files_with_nodes =
    List.filter_map
      (fun ((_node_id, (node: Module_node.t G.node))) ->
        let module_node = node.value in
        match module_node.kind with
        | Module_node.ML _
        | Module_node.MLI _ -> (
            match module_node.file with
            | Module_node.Concrete path -> Some (path, node)
            | Module_node.Generated _ -> None
          )
        | _ -> None)
      sorted_nodes
  in
  let files =
    List.map (fun ((path, _)) -> path) files_with_nodes
  in
  let namespace = Namespace.of_string t.config.namespace in
  let source_dir_prefix = Path.to_string t.config.source_dir ^ "/" in
  let stringify_dependency_error = fun path ->
    function
    | Syn.Deps.Parse_diagnostics diagnostics ->
        let messages = List.map Syn.Diagnostic.to_string diagnostics in
        "failed to parse "
        ^ Path.to_string path
        ^ " for dependency analysis: "
        ^ String.concat "; " messages
    | Syn.Deps.Cst_builder_error err -> "failed to build CST for "
    ^ Path.to_string path
    ^ " during dependency analysis: "
    ^ err.message
  in
  let file_namespace path =
    let file_str = Path.to_string path in
    let rel_path =
      if String.starts_with ~prefix:source_dir_prefix file_str then
        let len = String.length source_dir_prefix in
        String.sub file_str len (String.length file_str - len)
      else
        Path.basename path
    in
    let file_dir =
      match Path.parent (Path.v rel_path) with
      | Some p -> Path.to_string p
      | None -> "."
    in
    let subdir_parts =
      if file_dir = "." then
        []
      else
        String.split_on_char '/' file_dir |> List.map String.capitalize_ascii
    in
    List.fold_left Namespace.append namespace subdir_parts
  in
  let analyze_file path =
    let absolute_path =
      if Path.is_absolute path then
        path
      else
        Path.(t.config.root / path)
    in
    match Fs.read absolute_path with
    | Error err -> Error (Planning_error.DependencyAnalysisFailed {
      reason = "failed to read "
      ^ Path.to_string absolute_path
      ^ " for dependency analysis: "
      ^ IO.error_message err
    })
    | Ok source -> (
        let parse_result = Syn.parse ~filename:path source in
        match Syn.Deps.of_parse_result parse_result with
        | Ok deps ->
            let names = Syn.Deps.modules deps
            |> List.map
              (fun modname -> Module_name.of_string ~namespace:(file_namespace path) modname) in
            Ok names
        | Error err -> Error (Planning_error.DependencyAnalysisFailed {
          reason = stringify_dependency_error path err
        })
      )
  in
  (* Sort files deterministically to ensure consistent hashing *)
  let sorted_files =
    List.sort
      (fun a b ->
        String.compare (Path.to_string a) (Path.to_string b))
      files
  in
  let nodes_by_path = HashMap.with_capacity (List.length files_with_nodes) in
  List.iter
    (fun ((path, node)) ->
      let _ = HashMap.insert nodes_by_path (Path.to_string path) node in
      ())
    files_with_nodes;
  let deps =
    List.fold_left
      (fun acc path ->
        match acc with
        | Error _ as error -> error
        | Ok deps -> (
            match analyze_file path with
            | Error _ as error -> error
            | Ok module_deps -> (
                match HashMap.get nodes_by_path (Path.to_string path) with
                | Some node -> Ok ((node, module_deps) :: deps)
                | None -> Ok deps
              )
          ))
      (Ok [])
      sorted_files
  in
  match deps with
  | Error _ as error -> error
  | Ok deps ->
      List.iter
        (fun (((node: Module_node.t G.node), module_deps)) ->
          List.iter
            (fun dep_mod_name ->
              let dep_name = Module_name.to_string dep_mod_name in
              try
                let dep_node_ids = Module_registry.get_by_name t.registry dep_name in
                List.iter
                  (fun (dep_node_id, dep_node) ->
                    (* Skip self-references: a module can't depend on itself.
                       This happens when dependency analysis reports "A" as a dependency of A.ml,
                       which actually refers to a different module A (e.g., Bar.A when using 'open Bar'). *)
                    if G.Node_id.eq dep_node_id node.id then
                      ()
                    else
                      G.add_edge node ~depends_on:dep_node)
                  (preferred_dependency_nodes dep_node_ids)
              with
              | Not_found -> ())
            module_deps)
        deps;
      Ok ()

let add_library_node = fun t ~name ~includes ->
  let lib_node_value = Module_node.make_library ~name ~includes in
  let lib_node = G.add_node t.graph lib_node_value in
  (* Library archive depends on ALL ML/MLI/C modules.
     Unreachable modules will be filtered later in action_graph.ml based on
     what the library interface actually references.
     
     IMPORTANT: We iterate over topologically sorted nodes to preserve dependency order.
     This ensures that when we later collect objects from node.deps, they're in the
     correct order for linking. *)
  let sorted_nodes =
    match G.topo_sort t.graph with
    | Ok sorted -> sorted
    | Error _cycle_ids ->
        (* Cycle will be caught later in module planning *)
        []
  in
  (* Add edges in REVERSE topological order because add_edge prepends to deps list.
     This ensures lib_node.deps ends up in correct topological order. *)
  List.iter
    (fun (node: Module_node.t G.node) ->
      match node.value.kind with
      | Module_node.ML _
      | Module_node.MLI _
      | Module_node.C
      | Module_node.Native _ -> G.add_edge lib_node ~depends_on:node
      | _ -> ())
    (List.rev sorted_nodes)

let add_binary_node = fun t ~name ~source ~libraries ~includes ->
  let bin_node_value = Module_node.make_binary ~name ~source ~libraries ~includes in
  let bin_node = G.add_node t.graph bin_node_value in
  G.iter t.graph
    ~fn:(fun _node_id node ->
      match node.value.kind with
      | Module_node.Library _ -> G.add_edge bin_node ~depends_on:node
      | _ -> ())

(* Commands are just regular binaries *)

let add_command_node = add_binary_node

let graph = fun t -> t.graph

let registry = fun t -> t.registry

let entries = fun t -> t.entries
