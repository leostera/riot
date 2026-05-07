(**
   Graph Builder - Constructs module dependency graphs for OCaml packages

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
   - Generated vs concrete library interface modules
*)
open Std
open Std.Sync
open Std.Collections
open Std.Result.Syntax
open Riot_model

let iter_fold = fun fold value ~fn ->
  fold
    value
    ~init:()
    ~fn:(fun item () ->
      fn item;
      Syn.Ast.Continue ())

module G = Std.Graph.SimpleGraph

type root_mode =
  | Library_root of {
      library_name: string;
      public_root_name: string;
    }
  | Loose_sources

type source_group = {
  source_dir: Path.t;
  allowed_source_files: Path.t list;
  root_mode: root_mode;
  namespace: Namespace.t;
}

type config = {
  root: Path.t;
  source_groups: source_group list;
  package: Package.t;
  toolchain: Riot_toolchain.t;
  workspace: Workspace.t;
}

type analyzed_module = {
  display_path: Path.t;
  source_hash: Crypto.hash;
  implicit_opens: string list;
  parse_result: Syn.Parser.parse_result;
  deps: (Syn.Deps.t, Syn.Deps.parse_error) result;
  resolved_deps: Module_name.t list;
  resolved_dep_ids: G.Node_id.t list;
  unresolved_deps: string list;
}
type source_analysis_progress = {
  source: Path.t;
  source_index: int;
  source_count: int;
}

type root_export_source =
  | Export_from_ml of {
      public_root_name: string;
      source_path: Path.t;
    }
  | Export_from_mli of {
      public_root_name: string;
      source_path: Path.t;
    }

type t = {
  config: config;
  graph: Module_node.t G.t;
  registry: Module_registry.t;
  entries: Module_scanner.entry list;
  deps_env: Syn.Deps.Env.t Cell.t;
  root_export_sources: (string, root_export_source) HashMap.t;
  analyzed_modules: (G.Node_id.t, analyzed_module) HashMap.t;
}

type scan_ctx = {
  ns: Namespace.t;
  aliases: Module_node.t G.node list;
}

(**
   Scan context passed through recursive directory traversal.

   Fields:
   - ns: Current namespace (e.g., [] for top-level, ["Foo"; "Bar"] for
     Foo/Bar/)
   - aliases: Alias modules in scope that all files should open
*)
let make_relative = fun ~base ~path ->
  let base_str = Path.to_string base in
  let path_str = Path.to_string path in
  let prefix = base_str ^ "/" in
  if String.starts_with ~prefix path_str then
    let len = String.length prefix in
    Path.v (String.sub path_str ~offset:len ~len:(String.length path_str - len))
  else
    path

let sanitize_module_name = fun name ->
  String.map
    ~fn:(fun ch ->
      if ch = '-' then
        '_'
      else
        ch)
    name

let source_hash = fun ~implicit_opens ~source ->
  let module H = Crypto.Sha256 in
  let state = H.create () in
  let () = H.write state source in
  let () = H.write state "\x1f" in
  let () =
    implicit_opens
    |> List.for_each
      ~fn:(fun module_name ->
        H.write state module_name;
        H.write state "\x1f")
  in
  H.finish state

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create parser source slice: " ^ IO.IoVec.error_message error)

let rec filter_entries = fun ~allowed entries ->
  let allowed_strings = List.map allowed ~fn:Path.to_string in
  List.filter_map
    entries
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Module_scanner.ML (name, path) ->
          if List.contains allowed_strings ~value:(Path.to_string path) then
            Some (Module_scanner.ML (name, path))
          else
            None
      | Module_scanner.MLI (name, path) ->
          if List.contains allowed_strings ~value:(Path.to_string path) then
            Some (Module_scanner.MLI (name, path))
          else
            None
      | Module_scanner.C (name, path) ->
          if List.contains allowed_strings ~value:(Path.to_string path) then
            Some (Module_scanner.C (name, path))
          else
            None
      | Module_scanner.H (name, path) ->
          if List.contains allowed_strings ~value:(Path.to_string path) then
            Some (Module_scanner.H (name, path))
          else
            None
      | Module_scanner.Other _ -> None
      | Module_scanner.Dir (name, path, children) ->
          let children = filter_entries ~allowed children in
          if List.length children = 0 then
            None
          else
            Some (Module_scanner.Dir (name, path, children)))

(**
   Check if a path is a binary source file.

   Binary paths are stored as ABSOLUTE in Package.binary, while scanned paths
   are RELATIVE to package root. We must normalize both before comparing.
*)
let is_binary = fun config path ->
  let bin_rel = make_relative ~base:config.package.path ~path in
  List.any
    config.package.binaries
    ~fn:(fun (bin: Package.binary) ->
      let bin_abs_rel = make_relative ~base:config.package.path ~path:bin.path in
      Path.equal path bin_rel && Path.equal bin_rel bin_abs_rel)

let binary_for_path = fun config path ->
  let path_rel =
    make_relative ~base:config.package.path ~path
    |> Path.normalize
  in
  List.find
    config.package.binaries
    ~fn:(fun (bin: Package.binary) ->
      let bin_rel =
        make_relative ~base:config.package.path ~path:bin.path
        |> Path.normalize
      in
      Path.equal path_rel bin_rel)

let rec executable_pattern_to_string = fun pattern ->
  let module Ast = Syn.Ast in
  match Ast.Pattern.view pattern with
  | Ast.Pattern.Unit -> "<positional>"
  | Ast.Pattern.Ident { ident } ->
      Ast.Ident.last_segment ident
      |> Option.map ~fn:Ast.Token.text
      |> Option.unwrap_or ~default:(String.trim (Ast.Node.text (Ast.Pattern.as_node pattern)))
  | Ast.Pattern.Constraint { pattern; _ } -> executable_pattern_to_string pattern
  | Ast.Pattern.Alias { pattern; _ } -> executable_pattern_to_string pattern
  | _ ->
      let text = String.trim (Ast.Node.text (Ast.Pattern.as_node pattern)) in
      if String.is_empty text then
        "<positional>"
      else
        text

let executable_parameter_to_string = fun parameter ->
  let module Ast = Syn.Ast in
  match Ast.Parameter.view parameter with
  | Ast.Parameter.Param { label = Ast.Parameter.NoLabel; pattern = Some pattern } ->
      executable_pattern_to_string pattern
  | Ast.Parameter.Param { label = Ast.Parameter.Labeled { name = Some label }; _ } ->
      "~" ^ Ast.Token.text label
  | Ast.Parameter.Param { label = Ast.Parameter.Optional { name = Some label; _ }; _ } ->
      "?" ^ Ast.Token.text label
  | _ -> "<unknown>"

let rec is_labeled_args_parameter = fun (parameter: Syn.Ast.Parameter.t) ->
  let module Ast = Syn.Ast in
  match Ast.Parameter.view parameter with
  | Ast.Parameter.Param { label = Ast.Parameter.Labeled { name = Some label }; _ } ->
      String.equal (Ast.Token.text label) "args"
  | _ -> false

let rec pattern_binding_name = fun pattern ->
  let module Ast = Syn.Ast in
  match Ast.Pattern.view pattern with
  | Ast.Pattern.Ident { ident } ->
      Ast.Ident.last_segment ident
      |> Option.map ~fn:Ast.Token.text
  | Ast.Pattern.Constraint { pattern = inner; _ } -> pattern_binding_name inner
  | Ast.Pattern.Alias { pattern = inner; _ } -> pattern_binding_name inner
  | _ -> None

let let_binding_name = fun binding ->
  Syn.Ast.LetBinding.pattern binding
  |> Option.and_then ~fn:pattern_binding_name

let executable_main_bindings = fun (source_file: Syn.Ast.SourceFile.t) ->
  let module Ast = Syn.Ast in
  let bindings = Vector.with_capacity ~size:(Ast.SourceFile.structure_item_count source_file) in
  iter_fold
    Ast.SourceFile.fold_structure_item
    source_file
    ~fn:(fun item ->
      match Ast.StructureItem.view item with
      | Ast.StructureItem.Let decl ->
          iter_fold
            Ast.LetDeclaration.fold_binding
            decl
            ~fn:(fun binding ->
              match let_binding_name binding with
              | Some name when String.equal name "main" -> Vector.push bindings ~value:binding
              | _ -> ())
      | _ -> ());
  Vector.to_array bindings
  |> Array.to_list

let package_source_file = fun config source ->
  match Path.to_string config.package.relative_path with
  | "."
  | "" -> source
  | _ -> Path.(config.package.relative_path / source)

let validate_executable_main = fun ~package_name ~target_name ~source ~file source_file ->
  match executable_main_bindings source_file with
  | [] ->
      Error (
        Planning_error.InvalidExecutableMain {
          package_name;
          target_name;
          source;
          file;
          error = Planning_error.MissingMain;
        }
      )
  | [ binding ] ->
      let parameters = Vector.with_capacity ~size:(Syn.Ast.LetBinding.parameter_count binding) in
      iter_fold
        Syn.Ast.LetBinding.fold_parameter
        binding
        ~fn:(fun parameter -> Vector.push parameters ~value:parameter);
      let parameters =
        Vector.to_array parameters
        |> Array.to_list
      in
      (
        match parameters with
        | [ parameter ] when is_labeled_args_parameter parameter -> Ok ()
        | _ ->
            Error (
              Planning_error.InvalidExecutableMain {
                package_name;
                target_name;
                source;
                file;
                error = Planning_error.InvalidMainParameters {
                  parameters = List.map parameters ~fn:executable_parameter_to_string;
                };
              }
            )
      )
  | bindings ->
      Error (
        Planning_error.InvalidExecutableMain {
          package_name;
          target_name;
          source;
          file;
          error = Planning_error.MultipleMainDefinitions { count = List.length bindings };
        }
      )

(**
   Recursively scan directory entries and build graph nodes.

   Processing order (entries are pre-sorted by Module_scanner): 1. MLI files
   (interfaces must compile before implementations) 2. ML files 3. C files
   (compiled to .o objects) 4. H files (headers, no compilation needed) 5.
   Directories (create library interface modules, descend recursively)

   Binary sources are still analyzed as regular OCaml modules so target-specific
   reachability can follow their helper-module closure later on. The dedicated
   binary target node is still added separately after dependency wiring.
*)
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
      | Module_scanner.Other _ -> do_scan ~t ~ctx rest
      | Module_scanner.Dir (name, path, children) ->
          handle_library ~t ~ctx path name children;
          do_scan ~t ~ctx rest
    )

(**
   Handle a binary source file.

   Binary roots participate in dependency analysis like any other module. They
   are excluded from the library later by target-specific reachability, and
   the executable target node consumes their reachable closure during action
   planning.
*)
and handle_ocaml_binary = fun ~t ~ctx path ->
  let { ns; aliases } = ctx in
  let mod_ = Module.make ~namespace:ns ~filename:path in
  let file = Module_node.Concrete path in
  let node_val =
    match Module.kind mod_ with
    | `interface -> Module_node.make_mli mod_ file
    | `implementation -> Module_node.make_ml mod_ file
  in
  Module_node.set_open_modules node_val aliases;
  let node = G.add_node t.graph node_val in
  Module_registry.register t.registry mod_ (G.id node);
  (
    match Module.kind mod_ with
    | `implementation -> (
        let qualified_name =
          Module.module_name mod_
          |> Module_name.qualified_name
        in
        try
          let node_ids = Module_registry.get_by_qualified_name t.registry qualified_name in
          List.for_each
            node_ids
            ~fn:(fun intf_node_id ->
              match G.get_node t.graph intf_node_id with
              | Some intf_node -> (
                  match (G.value intf_node).kind with
                  | MLI intf_mod when (
                    Module.module_name intf_mod
                    |> Module_name.qualified_name
                  )
                  = qualified_name -> G.add_edge node ~depends_on:intf_node
                  | _ -> ()
                )
              | None -> ())
        with
        | Not_found -> ()
      )
    | `interface -> ()
  );
  List.for_each aliases ~fn:(fun aliases_node -> G.add_edge node ~depends_on:aliases_node)

(**
   Handle a regular OCaml module file (.ml or .mli).

   Creates a graph node and adds structural edges: 1. ML implementation ->
   corresponding MLI interface (if it exists) 2. Module -> parent library
   interface 3. Module -> alias modules in scope (for namespace flattening)

   The registry tracks modules by name so we can later find the corresponding
   .mli when processing a .ml file.
*)
and handle_ocaml_module = fun ~t ~ctx path ->
  let { ns; aliases } = ctx in
  let mod_ = Module.make ~namespace:ns ~filename:path in
  let file = Module_node.Concrete path in
  let node_val =
    match Module.kind mod_ with
    | `interface -> Module_node.make_mli mod_ file
    | `implementation -> Module_node.make_ml mod_ file
  in
  Module_node.set_open_modules node_val aliases;
  let node = G.add_node t.graph node_val in
  Module_registry.register t.registry mod_ (G.id node);
  (
    match Module.kind mod_ with
    | `implementation -> (
        let qualified_name =
          Module.module_name mod_
          |> Module_name.qualified_name
        in
        try
          let node_ids = Module_registry.get_by_qualified_name t.registry qualified_name in
          List.for_each
            node_ids
            ~fn:(fun intf_node_id ->
              match G.get_node t.graph intf_node_id with
              | Some intf_node -> (
                  match (G.value intf_node).kind with
                  | MLI intf_mod when (
                    Module.module_name intf_mod
                    |> Module_name.qualified_name
                  )
                  = qualified_name -> G.add_edge node ~depends_on:intf_node
                  | _ -> ()
                )
              | None -> ())
        with
        | Not_found -> ()
      )
    | `interface -> ()
  );
  List.for_each aliases ~fn:(fun aliases_node -> G.add_edge node ~depends_on:aliases_node)

(**
   Handle a directory as a library.

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
   - Generated foo.ml depends on everything (safe because it's explicit)
*)
and handle_library = fun ?public_root_name ~t ~ctx dir name children ->
  let { ns; aliases } = ctx in
  let lib_module_name = Module_name.from_string name in
  let intf_file = Module_name.canonical_mli lib_module_name in
  let impl_file = Module_name.canonical_ml lib_module_name in
  let intf_mod = Module.make ~namespace:ns ~filename:intf_file in
  let impl_mod = Module.make ~namespace:ns ~filename:impl_file in
  let ns =
    let namespaced_lib = Module.module_name impl_mod in
    Namespace.append ns (Module_name.to_string namespaced_lib)
  in
  let lib_def =
    Library_definition.from_entries
      ~namespace:ns
      ~library_name:name
      ~package_path:t.config.package.path
      ~concrete_library_path:(
        if Namespace.is_empty ctx.ns then
          Option.map t.config.package.library ~fn:(fun (library: Package.library) -> library.path)
        else
          None
      )
      ~binaries:t.config.package.binaries
      children
  in
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
    || Library_definition.has_concrete_mli lib_def
  in
  if not has_ocaml_content then
    do_scan ~t ~ctx children_without_lib
  else
    let aliases_node =
      let node_value = Alias_module.make_node ns child_modules in
      let node = G.add_node t.graph node_value in
      (
        match (G.value node).kind with
        | Module_node.ML mod_
        | Module_node.MLI mod_ -> Module_registry.register t.registry mod_ (G.id node)
        | _ -> ()
      );
      node
    in
    let lib_aliases =
      if Namespace.is_empty ns then
        [ aliases_node ]
      else
        aliases @ [ aliases_node ]
    in
    let intf_node =
      if
        Library_definition.has_concrete_ml lib_def
        && not (Library_definition.has_concrete_mli lib_def)
      then
        None
      else
        let intf =
          Library_interface.make_node
            intf_mod
            child_modules
            lib_aliases
            ~exists:(Library_definition.has_concrete_mli lib_def)
            ~actual_path:(Library_definition.concrete_mli_path lib_def)
        in
        Some (G.add_node t.graph intf)
    in
  let () =
    match intf_node with
    | Some intf_node ->
        Module_registry.register t.registry intf_mod (G.id intf_node);
        Option.for_each
          public_root_name
          ~fn:(fun public_root_name ->
            Module_registry.register_qualified_name t.registry public_root_name (G.id intf_node))
    | None -> ()
  in
    let impl_node =
      let impl =
        Library_interface.make_node
          impl_mod
          child_modules
          lib_aliases
          ~exists:(Library_definition.has_concrete_ml lib_def)
          ~actual_path:(Library_definition.concrete_ml_path lib_def)
      in
      G.add_node t.graph impl
    in
    Module_registry.register t.registry impl_mod (G.id impl_node);
  Option.for_each
    public_root_name
    ~fn:(fun public_root_name ->
      Module_registry.register_qualified_name t.registry public_root_name (G.id impl_node));
  (
    match intf_node with
    | Some intf_node ->
        List.for_each
          (List.reverse lib_aliases)
          ~fn:(fun alias_node -> G.add_edge intf_node ~depends_on:alias_node)
    | None -> ()
  );
  List.for_each
    (List.reverse lib_aliases)
    ~fn:(fun alias_node -> G.add_edge impl_node ~depends_on:alias_node);
  (
    match intf_node with
    | Some intf_node -> G.add_edge impl_node ~depends_on:intf_node
    | None -> ()
  );
  let ctx = { ns; aliases = aliases @ [ aliases_node ] } in
  do_scan ~t ~ctx children_without_lib;
  let deps_for_library_interface = Library_definition.deps_for_library_interface lib_def in
  let add_root_child_edges root_node =
    List.for_each
      deps_for_library_interface
      ~fn:(fun child_mod ->
        try
          let child_node_ids =
            Module_registry.get_by_qualified_name
              t.registry
              (
                Module.module_name child_mod
                |> Module_name.qualified_name
              )
          in
          List.for_each
            child_node_ids
            ~fn:(fun child_node_id ->
              match G.get_node t.graph child_node_id with
              | Some child_node -> G.add_edge root_node ~depends_on:child_node
              | None -> ())
        with
        | Not_found -> ())
  in
  (
    match intf_node with
    | Some intf_node -> add_root_child_edges intf_node
    | None -> ()
  );
  if not (Library_definition.has_concrete_ml lib_def) then
    add_root_child_edges impl_node
  else
    ()

let scan_sources = fun t (group: source_group) (sources: Module_scanner.entry list) ->
  let root_node = Module_node.make_root () in
  let _ = G.add_node t.graph root_node in
  let ctx = { ns = group.namespace; aliases = [] } in
  match group.root_mode with
  | Library_root { library_name; public_root_name } ->
      handle_library ~t ~ctx group.source_dir library_name sources ~public_root_name
  | Loose_sources -> do_scan ~t ~ctx sources

let module_name_segments = fun module_name ->
  (
    Module_name.namespace module_name
    |> Namespace.to_list
  ) @ [ Module_name.to_string module_name ]

let public_module_segments = fun ~public_root_name module_segments ->
  match module_segments with
  | [] -> []
  | _compiled_root :: rest -> public_root_name :: rest

let string_list_equal = fun left right ->
  List.length left = List.length right
  && List.all (List.zip left right) ~fn:(fun (left, right) -> String.equal left right)

let add_public_module_path = fun env ~module_segments ~public_root_name ->
  let public_segments = public_module_segments ~public_root_name module_segments in
  if string_list_equal module_segments public_segments then
    env
  else
    Syn.Deps.Env.add_path env ~path:public_segments ~free_names:[ public_root_name ]

let add_public_module_binding = fun env ~module_segments ~public_root_name ~exports ->
  let public_segments = public_module_segments ~public_root_name module_segments in
  if string_list_equal module_segments public_segments then
    env
  else
    Syn.Deps.Env.add_binding
      env
      ~path:public_segments
      ~free_names:[ public_root_name ]
      ~exports

let ocaml_stdlib_module_names = [
  "Arg";
  "Array";
  "ArrayLabels";
  "Atomic";
  "Bigarray";
  "Bool";
  "Buffer";
  "Bytes";
  "BytesLabels";
  "Callback";
  "Char";
  "Complex";
  "Digest";
  "Domain";
  "Effect";
  "Either";
  "Ephemeron";
  "Filename";
  "Float";
  "Format";
  "Fun";
  "Gc";
  "Hashtbl";
  "In_channel";
  "Int";
  "Int32";
  "Int64";
  "Lazy";
  "Lexing";
  "List";
  "ListLabels";
  "Map";
  "Marshal";
  "MoreLabels";
  "Mutex";
  "Nativeint";
  "Obj";
  "Oo";
  "Option";
  "Out_channel";
  "Parsing";
  "Printexc";
  "Printf";
  "Queue";
  "Random";
  "Result";
  "Scanf";
  "Semaphore";
  "Seq";
  "Set";
  "Stack";
  "StdLabels";
  "String";
  "StringLabels";
  "Sys";
  "Uchar";
  "Unit";
  "Weak";
]

let add_ocaml_stdlib_exports = fun env ~root_module ->
  if not (String.equal root_module "Stdlib") then
    env
  else
    let exports =
      List.fold_left
        ocaml_stdlib_module_names
        ~init:Syn.Deps.Env.empty
        ~fn:(fun exports module_name ->
          Syn.Deps.Env.add_path
            exports
            ~path:[ module_name ]
            ~free_names:[ root_module ])
    in
    let env =
      Syn.Deps.Env.add_binding env ~path:[ root_module ] ~free_names:[ root_module ] ~exports
    in
    List.fold_left
      ocaml_stdlib_module_names
      ~init:env
      ~fn:(fun env module_name ->
        Syn.Deps.Env.add_path
          env
          ~path:[ module_name ]
          ~free_names:[ root_module ])

let rec build_deps_env_for_library = fun
  (env, root_export_sources)
  ~package_path
  ~binaries
  ~namespace
  ~public_root_name
  ~library_name
  ~concrete_library_path
  children ->
  let lib_def =
    Library_definition.from_entries
      ~namespace
      ~library_name
      ~package_path
      ~concrete_library_path
      ~binaries
      children
  in
  let library_module_name = Module_name.from_string ~namespace library_name in
  let qualified_root_name = Module_name.qualified_name library_module_name in
  let library_module_segments = module_name_segments library_module_name in
  let env =
    Syn.Deps.Env.add_path
      env
      ~path:library_module_segments
      ~free_names:[ public_root_name ]
    |> add_public_module_path ~module_segments:library_module_segments ~public_root_name
  in
  let alias_namespace = Namespace.append namespace (Module_name.to_string library_module_name) in
  let alias_module_name =
    Namespace.to_string alias_namespace
    |> fun prefix -> prefix ^ "__Aliases"
  in
  let alias_module_segments = Namespace.to_list alias_namespace @ [ "Aliases" ] in
  let alias_exports =
    List.fold_left
      (Library_definition.child_modules lib_def)
      ~init:Syn.Deps.Env.empty
      ~fn:(fun exports child_mod ->
        let child_name =
          Module.module_name child_mod
          |> Module_name.to_string
        in
        Syn.Deps.Env.add_path exports ~path:[ child_name ] ~free_names:[ child_name ])
  in
  let alias_exports =
    if List.is_empty (Library_definition.child_modules lib_def) then
      alias_exports
    else
      Syn.Deps.Env.add_scoped_binding
        alias_exports
        ~path:[ "Super" ]
        ~free_names:[ alias_module_name ]
        ~exports:alias_exports
  in
  let env =
    Syn.Deps.Env.add_scoped_binding
      env
      ~path:alias_module_segments
      ~free_names:[ alias_module_name ]
      ~exports:alias_exports
  in
  let (env, root_export_sources) =
    if Library_definition.has_concrete_mli lib_def then
      match Library_definition.concrete_mli_path lib_def with
      | Some source_path ->
          let _ =
            HashMap.insert
              root_export_sources
              ~key:qualified_root_name
              ~value:(Export_from_mli { public_root_name; source_path })
          in
          (env, root_export_sources)
      | None -> (env, root_export_sources)
    else if Library_definition.has_concrete_ml lib_def then
      match Library_definition.concrete_ml_path lib_def with
      | Some source_path ->
          let _ =
            HashMap.insert
              root_export_sources
              ~key:qualified_root_name
              ~value:(Export_from_ml { public_root_name; source_path })
          in
          (env, root_export_sources)
      | None -> (env, root_export_sources)
    else
      (
        List.fold_left
          (Library_definition.child_modules lib_def)
          ~init:env
          ~fn:(fun env child_mod ->
            Syn.Deps.Env.add_path
              env
              ~path:(module_name_segments (Module.module_name child_mod))
              ~free_names:[ public_root_name ]),
        root_export_sources
      )
  in
  let child_namespace = Namespace.append namespace (Module_name.to_string library_module_name) in
  let child_dir_names =
    let names = HashSet.create () in
    let () =
      Library_definition.child_dirs lib_def
      |> List.for_each
        ~fn:(fun child_mod ->
          let _ =
            HashSet.insert
              names
              ~value:(
                Module.module_name child_mod
                |> Module_name.to_string
              )
          in
          ())
    in
    names
  in
  List.fold_left
    (Library_definition.children_without_lib lib_def)
    ~init:(env, root_export_sources)
    ~fn:(fun (env, root_export_sources) ->
      fun __tmp1 ->
        match __tmp1 with
        | Module_scanner.Dir (name, _, nested_children) ->
            let child_name =
              Module_name.from_string name
              |> Module_name.to_string
            in
            if HashSet.contains child_dir_names ~value:child_name then
              build_deps_env_for_library
                (env, root_export_sources)
                ~package_path
                ~binaries
                ~namespace:child_namespace
                ~public_root_name
                ~library_name:name
                ~concrete_library_path:None
                nested_children
            else
              (env, root_export_sources)
        | _ -> (env, root_export_sources))

let build_deps_env_for_group = fun
  config (env, root_export_sources) (group: source_group) group_entries ->
  match group.root_mode with
  | Loose_sources -> (env, root_export_sources)
  | Library_root { library_name; public_root_name } ->
      build_deps_env_for_library
        (env, root_export_sources)
        ~package_path:config.package.path
        ~binaries:config.package.binaries
        ~namespace:group.namespace
        ~public_root_name
        ~library_name
        ~concrete_library_path:(
          if Namespace.is_empty group.namespace then
            Option.map config.package.library ~fn:(fun (library: Package.library) -> library.path)
          else
            None
        )
        group_entries

let group_namespace = fun root ->
  if Path.equal root (Path.v "src") then
    Namespace.empty
  else
    Path.to_string root
    |> String.split ~by:"/"
    |> List.filter ~fn:(fun part -> not (String.is_empty part))
    |> List.map ~fn:String.capitalize_ascii
    |> Namespace.from_list

let dependency_source_groups = fun (package: Package.t) ->
  let groups = [
    (Path.v "src", package.sources.src);
    (Path.v "tests", package.sources.tests);
    (Path.v "examples", package.sources.examples);
    (Path.v "bench", package.sources.bench);
  ]
  in
  List.filter_map
    groups
    ~fn:(fun (source_dir, allowed_source_files) ->
      if List.is_empty allowed_source_files then
        None
      else
        let root_mode =
          if Path.equal source_dir (Path.v "src") then
            match package.library with
            | Some _ ->
                Library_root {
                  library_name = Package_namespace.compiled_root package;
                  public_root_name = Package_namespace.public_root package;
                }
            | None -> Loose_sources
          else
            Loose_sources
        in
        Some {
          source_dir;
          allowed_source_files;
          root_mode;
          namespace = group_namespace source_dir;
        })

let dependency_group_entries = fun ~root (group: source_group) ->
  Module_scanner.scan ~root ~source_dir:group.source_dir
  |> filter_entries ~allowed:group.allowed_source_files

let dependency_root_export_env = fun config env (group: source_group) group_entries ->
  match group.root_mode with
  | Loose_sources -> env
  | Library_root { library_name; public_root_name } ->
      let lib_def =
        Library_definition.from_entries
          ~namespace:group.namespace
          ~library_name
          ~package_path:config.package.path
          ~concrete_library_path:(
            if Namespace.is_empty group.namespace then
              Option.map config.package.library ~fn:(fun (library: Package.library) -> library.path)
            else
              None
          )
          ~binaries:config.package.binaries
          group_entries
      in
      let root_path =
        if Library_definition.has_concrete_mli lib_def then
          Library_definition.concrete_mli_path lib_def
        else if Library_definition.has_concrete_ml lib_def then
          Library_definition.concrete_ml_path lib_def
        else
          None
      in
      (
        match root_path with
        | None -> env
        | Some path ->
            let display_path =
              if Path.is_absolute path then
                path
              else
                Path.(config.package.path / path)
            in
            match Fs.read display_path with
            | Error _ -> env
            | Ok source ->
                let library_module_name =
                  Module_name.from_string ~namespace:group.namespace library_name
                in
                let alias_namespace =
                  Namespace.append group.namespace (Module_name.to_string library_module_name)
                in
                let alias_module_segments = Namespace.to_list alias_namespace @ [ "Aliases" ] in
                let parse_env = Syn.Deps.Env.open_path env ~path:alias_module_segments in
                let parse_result = Syn.parse ~filename:display_path (source_slice source) in
                match Syn.Deps.from_parse_result ~env:parse_env parse_result with
                | Error _ -> env
          | Ok deps ->
              let module_segments = module_name_segments library_module_name in
              Syn.Deps.Env.add_binding
                env
                ~path:module_segments
                ~free_names:[ public_root_name ]
                ~exports:(Syn.Deps.exports deps)
              |> add_public_module_binding
                ~module_segments
                ~public_root_name
                ~exports:(Syn.Deps.exports deps)
      )

let create = fun config ->
  let scanned_groups =
    List.map
      config.source_groups
      ~fn:(fun (group: source_group) ->
        let entries =
          Module_scanner.scan ~root:config.root ~source_dir:group.source_dir
          |> filter_entries ~allowed:group.allowed_source_files
        in
        (group, entries))
  in
  let entries =
    List.concat (List.map scanned_groups ~fn:(fun (_group, group_entries) -> group_entries))
  in
  let graph = G.make () in
  let registry = Module_registry.create () in
  let (deps_env, root_export_sources) =
    List.fold_left
      scanned_groups
      ~init:(Syn.Deps.Env.empty, HashMap.create ())
      ~fn:(fun acc (group, group_entries) ->
        build_deps_env_for_group config acc group group_entries)
  in
  let analyzed_modules = HashMap.with_capacity ~size:64 in
  let t = {
    config;
    graph;
    registry;
    entries;
    deps_env = Cell.create deps_env;
    root_export_sources;
    analyzed_modules;
  }
  in
  List.for_each
    scanned_groups
    ~fn:(fun (group, group_entries) ->
      scan_sources t group group_entries);
  t

let add_direct_dependency_root = fun t ~package_name ~root_module ->
  let node_value = Module_node.make_package_dependency ~package_name ~root_module in
  let node = G.add_node t.graph node_value in
  Module_registry.register_qualified_name t.registry root_module (G.id node);
  Cell.set
    t.deps_env
    (
      Syn.Deps.Env.add_path (Cell.get t.deps_env) ~path:[ root_module ] ~free_names:[ root_module ]
      |> add_ocaml_stdlib_exports ~root_module
    )

let add_relocation_aliases = fun t ~aliases ->
  let aliases =
    aliases
    |> List.filter
      ~fn:(fun (public_root, compiled_root) -> not (String.equal public_root compiled_root))
    |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
    |> List.unique ~compare:(fun (left, _) (right, _) -> String.compare left right)
  in
  if List.is_empty aliases then
    ()
  else
    let alias_module_name = Package_namespace.compiled_root t.config.package ^ "__Relocation_aliases" in
    let contents =
      "(* Relocation aliases generated by riot *)\n"
      ^ (
        aliases
        |> List.map ~fn:(fun (public_root, compiled_root) ->
          "module " ^ public_root ^ " = " ^ compiled_root)
        |> String.concat "\n"
      )
      ^ "\n"
    in
    let path = Path.v (alias_module_name ^ ".ml-gen") in
    let mod_ = Module.make ~namespace:Namespace.empty ~filename:path in
    let node_value =
      Module_node.make_ml mod_ (Module_node.Generated { path; contents })
    in
    let alias_node = G.add_node t.graph node_value in
    Module_registry.register t.registry mod_ (G.id alias_node);
    let own_public_root = Package_namespace.public_root t.config.package in
    let own_compiled_root = Package_namespace.compiled_root t.config.package in
    let has_own_alias =
      List.any aliases ~fn:(fun (public_root, compiled_root) ->
        String.equal public_root own_public_root && String.equal compiled_root own_compiled_root)
    in
    if has_own_alias then
      (
        try
          Module_registry.get_by_qualified_name t.registry own_compiled_root
          |> List.filter_map
            ~fn:(fun node_id ->
              match G.get_node t.graph node_id with
              | Some node -> (
                  match (G.value node).kind with
                  | Module_node.MLI _ -> Some node
                  | _ -> None
                )
              | None -> None)
          |> List.for_each ~fn:(fun root_node -> G.add_edge alias_node ~depends_on:root_node)
        with
        | Not_found -> ()
      );
    G.iter
      t.graph
      ~fn:(fun node_id node ->
        if not (G.Node_id.eq node_id (G.id alias_node)) then (
          let value = G.value node in
          match value.kind with
          | Module_node.ML mod_
          | Module_node.MLI mod_ ->
              if not (
                match value.file with
                | Module_node.Concrete _ -> true
                | Module_node.Generated _ -> false
              ) then
                ()
              else if has_own_alias && String.equal (Module.namespaced_name mod_) own_compiled_root then
                ()
              else (
                Module_node.set_open_modules value (value.open_modules @ [ alias_node ]);
                G.add_edge node ~depends_on:alias_node
              )
          | _ -> ()
        ))

let add_self_relocation_alias = fun t ->
  let public_root = Package_namespace.public_root t.config.package in
  let compiled_root = Package_namespace.compiled_root t.config.package in
  if String.equal public_root compiled_root then
    ()
  else
    let alias_module_name = compiled_root ^ "__Self_relocation_aliases" in
    let contents =
      "(* Self relocation alias generated by riot *)\n"
      ^ "module "
      ^ public_root
      ^ " = "
      ^ compiled_root
      ^ "\n"
    in
    let path = Path.v (alias_module_name ^ ".ml-gen") in
    let mod_ = Module.make ~namespace:Namespace.empty ~filename:path in
    let node_value =
      Module_node.make_ml mod_ (Module_node.Generated { path; contents })
    in
    let alias_node = G.add_node t.graph node_value in
    Module_registry.register t.registry mod_ (G.id alias_node);
    let root_interface_nodes =
      try
        Module_registry.get_by_qualified_name t.registry compiled_root
        |> List.filter_map
          ~fn:(fun node_id ->
            match G.get_node t.graph node_id with
            | Some node -> (
                match (G.value node).kind with
                | Module_node.MLI _ -> Some node
                | _ -> None
              )
            | None -> None)
      with
      | Not_found -> []
    in
    List.for_each root_interface_nodes ~fn:(fun root_node -> G.add_edge alias_node ~depends_on:root_node);
    G.iter
      t.graph
      ~fn:(fun node_id node ->
        if not (G.Node_id.eq node_id (G.id alias_node)) then (
          let value = G.value node in
          match (value.kind, value.file) with
          | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete path)
              when Option.is_some (binary_for_path t.config path) ->
              Module_node.set_open_modules value (value.open_modules @ [ alias_node ]);
              G.add_edge node ~depends_on:alias_node;
              List.for_each root_interface_nodes ~fn:(fun root_node -> G.add_edge node ~depends_on:root_node)
          | _ -> ()
        ))

let dependency_export_source_path = fun
  (Export_from_ml { source_path; _ } | Export_from_mli { source_path; _ }) -> source_path

let dependency_export_public_root_name = fun
  (Export_from_ml { public_root_name; _ } | Export_from_mli { public_root_name; _ }) ->
  public_root_name

let qualified_name_segments = fun qualified_name ->
  String.split qualified_name ~by:"__"
  |> List.filter ~fn:(fun segment -> not (String.is_empty segment))

let prime_dependency_root_exports = fun dependency_config env root_export_sources ->
  let sorted_sources =
    HashMap.to_list root_export_sources
    |> List.sort
      ~compare:(fun (left_name, _) (right_name, _) ->
        let left_segments = qualified_name_segments left_name in
        let right_segments = qualified_name_segments right_name in
        match Int.compare (List.length left_segments) (List.length right_segments) with
        | Order.EQ -> String.compare left_name right_name
        | order -> order)
  in
  List.fold_left
    sorted_sources
    ~init:env
    ~fn:(fun env (qualified_name, source) ->
      let source_path = dependency_export_source_path source in
      let display_path =
        if Path.is_absolute source_path then
          source_path
        else
          Path.(dependency_config.package.path / source_path)
      in
      match Fs.read display_path with
      | Error _ -> env
      | Ok text ->
          let module_segments = qualified_name_segments qualified_name in
          let alias_segments = module_segments @ [ "Aliases" ] in
          let parse_env = Syn.Deps.Env.open_path env ~path:alias_segments in
          let parse_result = Syn.parse ~filename:display_path (source_slice text) in
          match Syn.Deps.from_parse_result ~env:parse_env parse_result with
          | Error _ -> env
          | Ok deps ->
              let public_root_name = dependency_export_public_root_name source in
              let env =
                Syn.Deps.Env.add_binding
                  env
                  ~path:module_segments
                  ~free_names:[ public_root_name ]
                  ~exports:(Syn.Deps.exports deps)
              in
              add_public_module_binding
                env
                ~module_segments
                ~public_root_name
                ~exports:(Syn.Deps.exports deps))

let add_direct_dependency_package = fun t (package: Package.t) ->
  let root_module = Package_namespace.public_root package in
  add_direct_dependency_root t ~package_name:package.name ~root_module;
  let source_groups = dependency_source_groups package in
  let scanned_groups =
    List.map
      source_groups
      ~fn:(fun group -> (group, dependency_group_entries ~root:package.path group))
  in
  let dependency_config = { t.config with root = package.path; source_groups; package } in
  let (env, root_export_sources) =
    List.fold_left
      scanned_groups
      ~init:(Cell.get t.deps_env, HashMap.create ())
      ~fn:(fun acc (group, group_entries) ->
        build_deps_env_for_group
          dependency_config
          acc
          group
          group_entries)
  in
  let env = prime_dependency_root_exports dependency_config env root_export_sources in
  Cell.set t.deps_env env

(**
   Wire module dependencies using `Syn.Deps`.

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
   - Missing dependencies are recorded for package-edge validation but not wired
   - MLI -> ML dependencies are filtered out to maintain proper compilation
     order
*)
let wire_dependencies = fun ?(on_source_analyzed = fun (_:source_analysis_progress) -> ()) t ->
  let () = HashMap.clear t.analyzed_modules in
  let rec strip_last_namespace = fun __tmp1 ->
    match __tmp1 with
    | [] -> []
    | [ _ ] -> []
    | component :: rest -> component :: strip_last_namespace rest
  in
  let rec qualified_dependency_names simple_name namespace_parts =
    match namespace_parts with
    | [] -> [ simple_name ]
    | _ ->
        let qualified_name =
          Namespace.from_list namespace_parts
          |> fun ns ->
            Namespace.append ns simple_name
            |> Namespace.to_string
        in
        qualified_name
        :: qualified_dependency_names simple_name (strip_last_namespace namespace_parts)
  in
  let implicit_open_modules (open_modules: Module_node.t G.node list) =
    open_modules
    |> List.filter_map
      ~fn:(fun (node: Module_node.t G.node) ->
        match (G.value node).kind with
        | Module_node.ML mod_
        | Module_node.MLI mod_ -> Some (Module.namespaced_name mod_)
        | _ -> None)
  in
  let deps_env_with_implicit_opens base_env open_modules =
    List.fold_left
      open_modules
      ~init:base_env
      ~fn:(fun env (node: Module_node.t G.node) ->
        match (G.value node).kind with
        | Module_node.ML mod_
        | Module_node.MLI mod_ ->
            Syn.Deps.Env.open_path env ~path:(module_name_segments (Module.module_name mod_))
        | _ -> env)
  in
  let module_node_module (node: Module_node.t G.node) =
    match (G.value node).kind with
    | Module_node.ML mod_
    | Module_node.MLI mod_ -> Some mod_
    | _ -> None
  in
  let preferred_dependency_nodes dep_node_ids =
    let rec collect acc has_ml = fun __tmp1 ->
      match __tmp1 with
      | [] -> (List.reverse acc, has_ml)
      | dep_node_id :: rest -> (
          match G.get_node t.graph dep_node_id with
          | Some (dep_node: Module_node.t G.node) -> (
              match (G.value dep_node).kind with
              | Module_node.ML _ -> collect ((dep_node_id, dep_node) :: acc) true rest
              | _ -> collect ((dep_node_id, dep_node) :: acc) has_ml rest
            )
          | None -> collect acc has_ml rest
        )
    in
    let (resolved_nodes, has_ml) = collect [] false dep_node_ids in
    if has_ml then
      List.filter
        resolved_nodes
        ~fn:(fun (_dep_node_id, (dep_node: Module_node.t G.node)) ->
          match (G.value dep_node).kind with
          | Module_node.ML _ -> true
          | _ -> false)
    else
      resolved_nodes
  in
  let resolve_dependency_nodes_for_node (node: Module_node.t G.node) dep_mod_name =
    let simple_name = Module_name.to_string dep_mod_name in
    let namespace_parts =
      Module_name.namespace dep_mod_name
      |> Namespace.to_list
    in
    let candidate_names = qualified_dependency_names simple_name namespace_parts in
    let rec try_candidates = fun __tmp1 ->
      match __tmp1 with
      | [] -> raise Not_found
      | candidate_name :: rest -> (
          try
            let dep_node_ids = Module_registry.get_by_qualified_name t.registry candidate_name in
            let preferred_ids =
              preferred_dependency_nodes dep_node_ids
              |> List.filter_map
                ~fn:(fun (dep_node_id, dep_node) ->
                  if G.Node_id.eq dep_node_id (G.id node) then
                    None
                  else
                    Some (dep_node_id, dep_node))
            in
            if List.is_empty preferred_ids then
              try_candidates rest
            else
              preferred_ids
          with
          | Not_found -> try_candidates rest
        )
    in
    try_candidates candidate_names
  in
  let all_nodes = G.map t.graph ~fn:(fun (node_id, node) -> (node_id, node)) in
  (* Sort nodes by ID to ensure deterministic ordering - G.map uses Hashtbl.to_seq which is non-deterministic *)
  let sorted_nodes =
    List.sort
      all_nodes
      ~compare:(fun (id1, _) (id2, _) -> Int.compare (G.Node_id.to_int id1) (G.Node_id.to_int id2))
  in
  let files_with_nodes =
    List.filter_map
      sorted_nodes
      ~fn:(fun (_node_id, (node: Module_node.t G.node)) ->
        let module_node = G.value node in
        match module_node.kind with
        | Module_node.ML _
        | Module_node.MLI _ -> (
            match module_node.file with
            | Module_node.Concrete path
            | Module_node.Generated { path; _ } -> Some (path, node)
          )
        | _ -> None)
  in
  let source_count = List.length files_with_nodes in
  let reported_sources = HashSet.with_capacity ~size:source_count in
  let report_source_analyzed = fun ~source (node: Module_node.t G.node) ->
    if HashSet.insert reported_sources ~value:(G.id node) then
      on_source_analyzed
        {
          source;
          source_index = HashSet.length reported_sources;
          source_count;
        }
    else
      ()
  in
  let group_for_path path =
    let normalized_path = Path.normalize path in
    List.find
      t.config.source_groups
      ~fn:(fun (group: source_group) ->
        let matches_allowed =
          List.any
            group.allowed_source_files
            ~fn:(fun allowed -> Path.equal (Path.normalize allowed) normalized_path)
        in
        let prefix = Path.to_string group.source_dir in
        let path_str = Path.to_string normalized_path in
        matches_allowed
        || String.equal path_str prefix
        || String.starts_with ~prefix:(prefix ^ "/") path_str)
  in
  let add_local_source_module_paths env =
    List.fold_left
      sorted_nodes
      ~init:env
      ~fn:(fun env (_node_id, (node: Module_node.t G.node)) ->
        match module_node_module node with
        | Some mod_ ->
            let simple_name =
              Module.module_name mod_
              |> Module_name.to_string
            in
            Syn.Deps.Env.add_path env ~path:[ simple_name ] ~free_names:[ simple_name ]
        | None -> env)
  in
  let () = Cell.set t.deps_env (add_local_source_module_paths (Cell.get t.deps_env)) in
  let stringify_dependency_error = fun path ->
    fun (Syn.Deps.Parse_diagnostics diagnostics) ->
      let messages = List.map diagnostics ~fn:Syn.Diagnostic.to_string in
      "failed to parse "
      ^ Path.to_string path
      ^ " for dependency analysis: "
      ^ String.concat "; " messages
  in
  let raw_source_text (node: Module_node.t G.node) =
    let source_result =
      match (G.value node).file with
      | Module_node.Concrete path ->
          let display_path =
            if Path.is_absolute path then
              path
            else
              Path.(t.config.package.path / path)
          in
          Fs.read display_path
          |> Result.map ~fn:(fun text -> (text, display_path))
      | Module_node.Generated { path; contents } ->
          let display_path =
            if Path.is_absolute path then
              path
            else
              Path.(t.config.package.path / path)
          in
          Ok (contents, display_path)
    in
    match source_result with
    | Error err ->
        Error (Planning_error.DependencyAnalysisFailed {
          reason = "failed to read "
          ^ Module_node.file_to_string (G.value node).file
          ^ " for dependency analysis: "
          ^ IO.error_message err;
        })
    | Ok source -> Ok source
  in
  let file_namespace path =
    match group_for_path path with
    | None -> Namespace.empty
    | Some group ->
        let base_namespace =
          match group.root_mode with
          | Library_root { library_name; _ } ->
              Module_name.from_string library_name
              |> Module_name.to_string
              |> fun name -> Namespace.from_list [ name ]
          | Loose_sources -> group.namespace
        in
        let file_str = Path.to_string (Path.normalize path) in
        let source_dir_prefix =
          let prefix = Path.to_string group.source_dir in
          if String.is_empty prefix then
            ""
          else
            prefix ^ "/"
        in
        let rel_path =
          if String.is_empty source_dir_prefix then
            file_str
          else if String.starts_with ~prefix:source_dir_prefix file_str then
            let len = String.length source_dir_prefix in
            String.sub file_str ~offset:len ~len:(String.length file_str - len)
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
            String.split file_dir ~by:"/"
            |> List.map ~fn:String.capitalize_ascii
        in
        List.fold_left subdir_parts ~init:base_namespace ~fn:Namespace.append
  in
  let analyze_node path (node: Module_node.t G.node) =
    match raw_source_text node with
    | Error _ as err -> err
    | Ok (raw_text, display_path) ->
        report_source_analyzed ~source:display_path node;
        let implicit_opens = implicit_open_modules (G.value node).open_modules in
        let parse_result = Syn.parse ~filename:display_path (source_slice raw_text) in
        let source_file = Syn.Ast.SourceFile.make parse_result.tree in
        let executable_main_validation =
          match binary_for_path t.config path with
          | Some binary when Vector.length parse_result.diagnostics = 0 ->
              let source = make_relative ~base:t.config.package.path ~path:binary.path in
              validate_executable_main
                ~package_name:(Package_name.to_string t.config.package.name)
                ~target_name:binary.name
                ~source
                ~file:(package_source_file t.config source)
                source_file
          | _ -> Ok ()
        in
        let deps_env = deps_env_with_implicit_opens (Cell.get t.deps_env) (G.value node).open_modules in
        let deps = Syn.Deps.from_parse_result ~env:deps_env parse_result in
        let source_hash = source_hash ~implicit_opens ~source:raw_text in
        let requested_deps =
          match (deps, (G.value node).file) with
          | (Ok deps, Module_node.Concrete _) -> Syn.Deps.modules deps
          | (Ok _, Module_node.Generated _) -> []
          | (Error _, _) -> []
        in
        let resolved_deps =
          requested_deps
          |> List.map
            ~fn:(fun modname -> Module_name.from_string ~namespace:(file_namespace path) modname)
        in
        let (resolved_dep_ids, unresolved_deps) =
          List.fold_left
            (List.zip requested_deps resolved_deps)
            ~init:([], [])
            ~fn:(fun (resolved_ids, unresolved) (requested_module, dep_mod_name) ->
              try
                let preferred_ids =
                  resolve_dependency_nodes_for_node node dep_mod_name
                  |> List.map ~fn:(fun (dep_node_id, _dep_node) -> dep_node_id)
                in
                (List.reverse preferred_ids @ resolved_ids, unresolved)
              with
              | Not_found -> (resolved_ids, requested_module :: unresolved))
          |> fun (resolved_ids, unresolved) -> (List.reverse resolved_ids, List.reverse unresolved)
        in
        let analyzed = {
          display_path;
          source_hash;
          implicit_opens;
          parse_result;
          deps;
          resolved_deps;
          resolved_dep_ids;
          unresolved_deps;
        }
        in
        let _ = HashMap.insert t.analyzed_modules ~key:(G.id node) ~value:analyzed in
        let add_local_export_bindings env mod_ deps =
          let simple_name =
            Module.module_name mod_
            |> Module_name.to_string
          in
          let module_segments = module_name_segments (Module.module_name mod_) in
          let env =
            match group_for_path path with
            | Some { root_mode = Loose_sources; _ } ->
                Syn.Deps.Env.add_binding
                  env
                  ~path:[ simple_name ]
                  ~free_names:[ simple_name ]
                  ~exports:(Syn.Deps.exports deps)
            | _ -> env
          in
          match List.reverse module_segments with
          | []
          | [ _ ] -> env
          | simple_name :: parent_segments_rev ->
              let parent_segments = List.reverse parent_segments_rev in
              let alias_path = parent_segments @ [ "Aliases" ] in
              let alias_module =
                match parent_segments with
                | [] -> "Aliases"
                | _ -> String.concat "__" parent_segments ^ "__Aliases"
              in
              let alias_exports =
                Syn.Deps.Env.add_binding
                  Syn.Deps.Env.empty
                  ~path:[ simple_name ]
                  ~free_names:[ simple_name ]
                  ~exports:(Syn.Deps.exports deps)
              in
              Syn.Deps.Env.add_scoped_binding
                env
                ~path:alias_path
                ~free_names:[ alias_module ]
                ~exports:alias_exports
        in
        let add_public_root_export_binding env mod_ deps =
          match HashMap.get
            t.root_export_sources
            ~key:(Module_name.qualified_name (Module.module_name mod_)) with
          | Some (Export_from_ml { public_root_name; _ }) when Module.kind mod_ = `implementation ->
              Syn.Deps.Env.add_binding
                env
                ~path:(module_name_segments (Module.module_name mod_))
                ~free_names:[ public_root_name ]
                ~exports:(Syn.Deps.exports deps)
          | Some (Export_from_mli { public_root_name; _ }) when Module.kind mod_ = `interface ->
              Syn.Deps.Env.add_binding
                env
                ~path:(module_name_segments (Module.module_name mod_))
                ~free_names:[ public_root_name ]
                ~exports:(Syn.Deps.exports deps)
          | _ -> env
        in
        let () =
          match (deps, (G.value node).kind, (G.value node).file) with
          | (Ok deps, (Module_node.ML mod_ | Module_node.MLI mod_), Module_node.Concrete _) ->
              Cell.set
                t.deps_env
                (
                  Cell.get t.deps_env
                  |> fun env ->
                    add_local_export_bindings env mod_ deps
                    |> fun env -> add_public_root_export_binding env mod_ deps
                )
          | _ -> ()
        in
        (
          match executable_main_validation with
          | Error _ as error -> error
          | Ok () -> (
              match (deps, (G.value node).file) with
              | (Ok _, Module_node.Concrete _) -> Ok resolved_deps
              | (Error err, Module_node.Concrete _) ->
                  Error (Planning_error.DependencyAnalysisFailed {
                    reason = stringify_dependency_error path err;
                  })
              | (Ok _, Module_node.Generated _) -> Ok []
              | (Error err, Module_node.Generated _) ->
                  Error (Planning_error.DependencyAnalysisFailed {
                    reason = stringify_dependency_error path err;
                  })
            )
        )
  in
  let export_source_node_ids =
    let candidates = HashMap.create () in
    let rank_node (node: Module_node.t G.node) =
      match ((G.value node).kind, (G.value node).file) with
      | (Module_node.MLI mod_, Module_node.Concrete _) ->
          Some (Module_name.qualified_name (Module.module_name mod_), 0)
      | (Module_node.ML mod_, Module_node.Concrete _) ->
          Some (Module_name.qualified_name (Module.module_name mod_), 1)
      | _ -> None
    in
    let () =
      List.for_each
        sorted_nodes
        ~fn:(fun (node_id, node) ->
          match rank_node node with
          | None -> ()
          | Some (qualified_name, rank) -> (
              match HashMap.get candidates ~key:qualified_name with
              | Some (existing_rank, _) when existing_rank <= rank -> ()
              | _ ->
                  let _ = HashMap.insert candidates ~key:qualified_name ~value:(rank, node_id) in
                  ()
            ))
    in
    let selected = HashSet.create () in
    let () =
      HashMap.values candidates
      |> List.for_each
        ~fn:(fun (_rank, node_id) ->
          let _ = HashSet.insert selected ~value:node_id in
          ())
    in
    selected
  in
  let prime_module_exports file_nodes =
    List.fold_left
      file_nodes
      ~init:(Ok ())
      ~fn:(fun acc (path, (node: Module_node.t G.node)) ->
        match acc with
        | Error _ as error -> error
        | Ok () ->
            if HashSet.contains export_source_node_ids ~value:(G.id node) then
              match analyze_node path node with
              | Ok _ -> Ok ()
              | Error _ as error -> error
            else
              Ok ())
  in
  (* Sort files deterministically to ensure consistent hashing *)
  let sorted_file_nodes =
    List.sort
      files_with_nodes
      ~compare:(fun (left_path, _) (right_path, _) ->
        String.compare
          (Path.to_string left_path)
          (Path.to_string right_path))
  in
  let deps =
    let* () = prime_module_exports sorted_file_nodes in
    let* () = prime_module_exports sorted_file_nodes in
    List.fold_left
      sorted_file_nodes
      ~init:(Ok [])
      ~fn:(fun acc (path, node) ->
        match acc with
        | Error _ as error -> error
        | Ok deps -> (
            match analyze_node path node with
            | Error _ as error -> error
            | Ok module_deps -> Ok ((node, module_deps) :: deps)
          ))
  in
  match deps with
  | Error _ as error -> error
  | Ok deps ->
      List.for_each
        deps
        ~fn:(fun ((node: Module_node.t G.node), module_deps) ->
          List.for_each
            module_deps
            ~fn:(fun dep_mod_name ->
              try List.for_each
                (resolve_dependency_nodes_for_node node dep_mod_name)
                ~fn:(fun (dep_node_id, dep_node) -> G.add_edge node ~depends_on:dep_node) with
              | Not_found -> ()));
      Ok ()

let add_library_node = fun t ~name ~includes ->
  let lib_node_value = Module_node.make_library ~name ~includes in
  let lib_node = G.add_node t.graph lib_node_value in
  (* Library archive depends on ALL ML/MLI/C modules.
     Unreachable modules will be filtered later in action_graph.ml based on
     what the library interface actually references.

     IMPORTANT: We iterate over topologically sorted nodes to preserve dependency order.
     This ensures that when we later collect objects from dependencies, they're
     in the correct order for linking.
  *)
  let sorted_nodes =
    match G.topo_sort t.graph with
    | Ok sorted -> sorted
    | Error _cycle_ids ->
        (* Cycle will be caught later in module planning *)
        []
  in
  (* SimpleGraph preserves dependency insertion order, so add edges directly in
     topological order.
  *)
  List.for_each
    sorted_nodes
    ~fn:(fun (node: Module_node.t G.node) ->
      match (G.value node).kind with
      | Module_node.ML _
      | Module_node.MLI _
      | Module_node.C
      | Module_node.Native _ -> G.add_edge lib_node ~depends_on:node
      | _ -> ())

let add_binary_node = fun t ~name ~source ~libraries ~includes ->
  let bin_node_value = Module_node.make_binary ~name ~source ~libraries ~includes in
  let bin_node = G.add_node t.graph bin_node_value in
  G.iter
    t.graph
    ~fn:(fun _node_id node ->
      match (G.value node).kind with
      | Module_node.Library _ -> G.add_edge bin_node ~depends_on:node
      | Module_node.ML _ -> (
          match (G.value node).file with
          | Module_node.Concrete path ->
              let source_rel = make_relative ~base:t.config.package.path ~path:source in
              if Path.equal path source_rel || Path.equal path source then
                G.add_edge bin_node ~depends_on:node
              else
                ()
          | Module_node.Generated _ -> ()
        )
      | _ -> ())

(* Commands are just regular binaries *)

let add_command_node = add_binary_node

let graph = fun t -> t.graph

let analyzed_modules = fun t ->
  HashMap.to_list t.analyzed_modules
  |> List.sort
    ~compare:(fun (left_id, _) (right_id, _) ->
      Int.compare
        (G.Node_id.to_int left_id)
        (G.Node_id.to_int right_id))

let registry = fun t -> t.registry

let entries = fun t -> t.entries
