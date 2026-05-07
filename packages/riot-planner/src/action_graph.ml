open Std
open Std.Sync
open Std.Collections
open Std.Iter
open Riot_model

module G = Std.Graph.SimpleGraph

type t = {
  graph: Action_node.action_spec G.t;
}

let create = fun () -> { graph = G.make () }

let add_node = fun t node_value -> G.add_node t.graph node_value

let add_dependency = fun t node ~depends_on -> G.add_edge node ~depends_on

let topo_sort = fun t ->
  match G.topo_sort t.graph with
  | Ok sorted -> sorted
  | Error _cycle_ids ->
      (* Should never happen - cycles caught in module planning *)
      panic "Unexpected cycle in action graph"

let nodes = fun t -> topo_sort t

let graph = fun t -> t.graph

let clone = fun t ->
  let cloned = create () in
  let node_by_id = HashMap.with_capacity ~size:(List.length (nodes t)) in
  G.iter
    t.graph
    ~fn:(fun _id node ->
      let cloned_node = add_node cloned (G.value node) in
      let _ = HashMap.insert node_by_id ~key:(G.id node) ~value:cloned_node in
      ());
  G.iter
    t.graph
    ~fn:(fun _id node ->
      match HashMap.get node_by_id ~key:(G.id node) with
      | None -> ()
      | Some cloned_node ->
          List.for_each
            (G.deps node)
            ~fn:(fun dep_id ->
              match HashMap.get node_by_id ~key:dep_id with
              | None -> ()
              | Some cloned_dep_node -> add_dependency cloned cloned_node ~depends_on:cloned_dep_node)
    );
  cloned

let to_action_list = fun t ->
  let sorted = topo_sort t in
  sorted
  |> List.map ~fn:(fun (node: Action_node.t) -> (G.value node).actions)
  |> List.concat

let hash_action_node = fun _t (node: Action_node.t) -> (G.value node).hash

let opens = fun mods ->
  List.filter_map
    mods
    ~fn:(fun (node: Module_node.t G.node) ->
      match (G.value node).kind with
      | ML mod_
      | MLI mod_ -> Some (Riot_toolchain.Ocamlc.Open (Module.namespaced_name mod_))
      | _ -> None)

let concrete_module_source = fun ~canonical_path mod_ path ->
  let source_name =
    path
    |> Module_name.from_filename
    |> Module_name.qualified_name
  in
  let module_name =
    mod_
    |> Module.module_name
    |> Module_name.qualified_name
  in
  if String.equal source_name module_name then
    ([], path)
  else
    let source = canonical_path (Module.module_name mod_) in
    ([ Action.CopyFile { source = path; destination = source } ], source)

(** Determine compiler flags for stdlib handling based on package dependencies *)
let stdlib_flags = fun (package: Package.t) ->
  (* Check if this package has stdlib as a dependency *)
  let has_stdlib_dep =
    List.any
      (Package.build_graph_dependencies package)
      ~fn:(fun (dep: Package.dependency) ->
        Package_name.equal
          dep.name
          (
            Package_name.from_string "stdlib"
            |> Result.expect ~msg:"expected valid package name"
          ))
  in
  (* Always add -nopervasives to prevent automatic opening of Stdlib *)
  (* Add -nostdlib only if package doesn't depend on stdlib *)
  if has_stdlib_dep then
    [ Riot_toolchain.Ocamlc.NoPervasives ]
  else
    [ Riot_toolchain.Ocamlc.NoPervasives; Riot_toolchain.Ocamlc.NoStdlib ]

let profile_compile_flags = fun (profile: Profile.t) ->
  let flags = [] in
  let flags =
    if profile.no_alias_deps then
      Riot_toolchain.Ocamlc.NoAliasDeps :: flags
    else
      flags
  in
  let flags =
    match profile.inline with
    | Some threshold -> flags @ [ Riot_toolchain.Ocamlc.Inline threshold ]
    | None -> flags
  in
  let flags =
    if profile.no_assert then
      flags @ [ Riot_toolchain.Ocamlc.NoAssert ]
    else
      flags
  in
  let flags =
    if profile.compact then
      flags @ [ Riot_toolchain.Ocamlc.Compact ]
    else
      flags
  in
  let flags =
    if profile.unsafe then
      flags @ [ Riot_toolchain.Ocamlc.Unsafe ]
    else
      flags
  in
  let flags =
    if List.is_empty profile.warnings then
      flags
    else
      flags @ [ Riot_toolchain.Ocamlc.Warning profile.warnings ]
  in
  let flags =
    if List.is_empty profile.errors then
      flags
    else
      flags @ [ Riot_toolchain.Ocamlc.WarnError profile.errors ]
  in
  let compare_compiler_flag left right =
    let rec compare_flag_parts left right =
      match (left, right) with
      | ([], []) -> Order.EQ
      | ([], _) -> Order.LT
      | (_, []) -> Order.GT
      | (left :: left_rest, right :: right_rest) ->
          let compared = String.compare left right in
          if compared = Order.EQ then
            compare_flag_parts left_rest right_rest
          else
            compared
    in
    compare_flag_parts
      (Riot_toolchain.Ocamlc.flags_to_string [ left ])
      (Riot_toolchain.Ocamlc.flags_to_string [ right ])
  in
  List.unique
    ((flags
    @ List.map profile.open_modules ~fn:(fun mod_name -> Riot_toolchain.Ocamlc.Open mod_name))
    @ List.map profile.ocamlc_flags ~fn:(fun flag -> Riot_toolchain.Ocamlc.Raw flag))
    ~compare:compare_compiler_flag

let module_to_actions
  ~package
  ~profile
  ~ctx
  ~dep_includes
  ~get_dep_outputs
  ~get_dep_kind
  ~depset
  ~needs_unix
  ~needs_dynlink
  (module_node: Module_node.t)
  (deps: G.Node_id.t list) =
  let base_compile_flags = stdlib_flags package @ profile_compile_flags profile in
  let has_interface_dependency = fun mod_ ->
    List.any
      deps
      ~fn:(fun dep_id ->
        match get_dep_kind dep_id with
        | Some (Module_node.MLI interface_mod) -> Module.eq mod_ interface_mod
        | _ -> false)
  in
  match module_node with
  | { kind = MLI mod_; file = Concrete path; open_modules; _ } ->
      let (copy_actions, source) =
        concrete_module_source ~canonical_path:Module_name.canonical_mli mod_ path
      in
      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [ cmti_output; cmi_output ] in
      let sources = [ path ] in
      let compile = Action.CompileInterface {
        source;
        outputs;
        includes = Path.v "." :: dep_includes;
        flags = base_compile_flags @ opens open_modules;
      }
      in
      (copy_actions @ [ compile ], outputs, sources)
  | { kind = ML mod_; file = Concrete path; open_modules; _ } ->
      let (copy_actions, source) =
        concrete_module_source ~canonical_path:Module_name.canonical_ml mod_ path
      in
      let native_object_output = Module.o mod_ in
      let cmx_output = Module.cmx mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs =
        if has_interface_dependency mod_ then
          [ cmt_output; cmx_output; native_object_output; ]
        else
          [ cmt_output; Module.cmi mod_; cmx_output; native_object_output; ]
      in
      let sources = [ path ] in
      let compile = Action.CompileImplementation {
        source;
        outputs;
        includes = Path.v "." :: dep_includes;
        flags = base_compile_flags @ opens open_modules;
      }
      in
      (copy_actions @ [ compile ], outputs, sources)
  | { kind = ML mod_; file = Generated { path; contents }; open_modules; _ } ->
      let write_action = Action.WriteFile { destination = path; content = contents } in
      let native_object_output = Module.o mod_ in
      let cmx_output = Module.cmx mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs =
        if has_interface_dependency mod_ then
          [ cmt_output; cmx_output; native_object_output; ]
        else
          [ cmt_output; Module.cmi mod_; cmx_output; native_object_output; ]
      in
      let sources = [] in
      let requires_impl_flag =
        match Path.extension path with
        | Some ".ml" -> false
        | _ -> true
      in
      let flags =
        if requires_impl_flag then
          base_compile_flags
          @ (Riot_toolchain.Ocamlc.Impl path
          :: Riot_toolchain.Ocamlc.NoAliasDeps
          :: opens open_modules)
        else
          base_compile_flags @ opens open_modules
      in
      let compile_action = Action.CompileImplementation {
        source = path;
        outputs;
        includes = Path.v "." :: dep_includes;
        flags;
      }
      in
      ([ write_action; compile_action ], outputs, sources)
  | { kind = MLI mod_; file = Generated { path; contents }; open_modules; _ } ->
      let write_action = Action.WriteFile { destination = path; content = contents } in
      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [ cmti_output; cmi_output ] in
      let sources = [] in
      let compile_action = Action.CompileInterface {
        source = path;
        outputs;
        includes = Path.v "." :: dep_includes;
        flags = base_compile_flags @ opens open_modules;
      }
      in
      ([ write_action; compile_action ], outputs, sources)
  | { kind = Native { files }; _ } ->
      let c_files =
        List.filter files ~fn:(fun path -> String.ends_with ~suffix:".c" (Path.to_string path))
      in
      (* Use cc_flags from the profile (already has target-specific flags applied) *)
      let base_ccflags = profile.Profile.cc_flags in
      (* Add sysroot if cross-compiling *)
      let ccflags =
        match Build_ctx.sysroot ctx with
        | Some sysroot ->
            let sysroot_flag = "--sysroot=" ^ Path.to_string sysroot in
            sysroot_flag :: base_ccflags
        | None -> base_ccflags
      in
      Log.debug
        ("[ACTION_GRAPH] C compilation cc_flags for "
        ^ Package_name.to_string package.name
        ^ ": "
        ^ String.concat " " ccflags);
      let actions =
        List.map
          c_files
          ~fn:(fun c_file ->
            let obj_file =
              Path.remove_extension c_file
              |> Path.add_extension ~ext:"o"
            in
            let output_name =
              Path.basename obj_file
              |> Path.v
            in
            Action.CompileC { source = c_file; outputs = [ output_name ]; ccflags })
      in
      let outputs =
        List.map
          c_files
          ~fn:(fun c_file ->
            let obj_file =
              Path.remove_extension c_file
              |> Path.add_extension ~ext:"o"
            in
            Path.basename obj_file
            |> Path.v)
      in
      (actions, outputs, files)
  | { kind = C; file = Concrete _; _ }
  | { kind = C; file = Generated _; _ }
  | { kind = H; _ }
  | { kind = Root; _ }
  | { kind = PackageDependency _; _ }
  | { kind = Other _; _ } -> ([], [], [])
  | { kind = Library { name; includes }; _ } ->
      let library_name =
        Module_name.(from_string name
        |> cmxa)
      in
      let archive_name =
        Module_name.(from_string name
        |> a)
      in
      let sources = [] in
      let objects_with_duplicates =
        List.map
          deps
          ~fn:(fun dep_id ->
            let dep_outputs = get_dep_outputs dep_id in
            match get_dep_kind dep_id with
            | Some (Module_node.Native _) ->
                List.filter dep_outputs ~fn:(fun output -> Path.extension output = Some ".o")
            | _ -> List.filter dep_outputs ~fn:(fun output -> Path.extension output = Some ".cmx"))
        |> List.concat
      in
      (* Deduplicate objects without reordering them: OCaml link order must stay
         topological, with dependencies before dependents.
      *)
      let seen_objects = HashSet.create () in
      let objects =
        List.filter_map
          objects_with_duplicates
          ~fn:(fun object_path ->
            if HashSet.insert seen_objects ~value:object_path then
              Some object_path
            else
              None)
      in
      (* Create static library metadata (.cmxa). *)
      let create_lib = Action.CreateLibrary {
        outputs = [ library_name; archive_name ];
        objects;
        includes;
      }
      in
      let all_outputs = [ library_name; archive_name ] in
      ([ create_lib ], all_outputs, sources)
  | { kind = Binary {
               name;
               source;
               libraries;
               includes;
             }; _ } ->
      let sources = [ source ] in
      let objects_with_duplicates =
        List.map
          deps
          ~fn:(fun dep_id ->
            let dep_outputs = get_dep_outputs dep_id in
            match get_dep_kind dep_id with
            | Some (Module_node.ML _) ->
                List.filter dep_outputs ~fn:(fun output -> Path.extension output = Some ".cmx")
            | _ -> [])
        |> List.concat
      in
      let seen_objects = HashSet.create () in
      let objects =
        List.filter_map
          objects_with_duplicates
          ~fn:(fun object_path ->
            if HashSet.insert seen_objects ~value:object_path then
              Some object_path
            else
              None)
      in
      (* Collect foreign library outputs for linking *)
      let cclibs =
        List.map
          package.foreign_dependencies
          ~fn:(fun (fdep: Package.foreign_dependency) ->
            (* Make foreign outputs absolute by joining with foreign dep path and normalizing *)
            List.map
              fdep.outputs
              ~fn:(fun out -> Path.normalize (Path.join fdep.path out)))
        |> List.concat
      in
      let binary_output = Path.v name in
      (* Use cc_flags and ld_flags from the profile (already has target-specific flags applied) *)
      let base_ccflags = profile.Profile.cc_flags in
      let base_ldflags = profile.Profile.ld_flags in
      (* Get target platform for looking up dependency flags *)
      let target_platform = Build_ctx.target_platform_name ctx in
      (* NOTE: Dependency ld_flags must be collected here during linking, not in the profile.
         The profile contains only the current package's target-specific flags (applied in
         package_planner). When linking, we need flags from ALL dependencies transitively,
         which can only be determined at link-time based on the depset.
      *)
      let transitive_deps = Dependency.transitive_closure depset in
      let dep_ldflags =
        List.map
          transitive_deps
          ~fn:(fun (dep: Dependency.t) ->
            match List.find
              dep.package.compiler.target_overrides
              ~fn:(fun (platform, _) -> String.equal platform target_platform) with
            | Some (_, target_override) -> (
                match target_override.profile_override with
                | Some override -> (
                    match override.ld_flags with
                    | Profile.Override flags -> flags
                    | Inherit -> []
                  )
                | None -> []
              )
            | None -> [])
        |> List.concat
      in
      (* Combine: current package + dependencies *)
      let merged_ldflags = base_ldflags @ dep_ldflags in
      Log.debug
        ("[ACTION_GRAPH] Package "
        ^ Package_name.to_string package.name
        ^ ": base ld_flags = ["
        ^ String.concat ", " base_ldflags
        ^ "]");
      Log.debug
        ("[ACTION_GRAPH] Package "
        ^ Package_name.to_string package.name
        ^ ": dependency ld_flags = ["
        ^ String.concat ", " dep_ldflags
        ^ "]");
      Log.debug
        ("[ACTION_GRAPH] Package "
        ^ Package_name.to_string package.name
        ^ ": merged ld_flags = ["
        ^ String.concat ", " merged_ldflags
        ^ "]");
      (* Add sysroot if cross-compiling *)
      let ccflags =
        match Build_ctx.sysroot ctx with
        | Some sysroot ->
            let sysroot_flag = "--sysroot=" ^ Path.to_string sysroot in
            sysroot_flag :: base_ccflags
        | None -> base_ccflags
      in
      let ldflags =
        match Build_ctx.sysroot ctx with
        | Some sysroot ->
            let sysroot_flag = "--sysroot=" ^ Path.to_string sysroot in
            sysroot_flag :: merged_ldflags
        | None -> merged_ldflags
      in
      Log.debug
        ("[ACTION_GRAPH] Final ccopt_flags for linking " ^ name ^ ": " ^ String.concat " " ccflags);
      Log.debug
        ("[ACTION_GRAPH] Final cclib_flags for linking " ^ name ^ ": " ^ String.concat " " ldflags);
      (* Keep ccopt_flags and cclib_flags separate *)
      let ccopt_flags = ccflags in
      let cclib_flags = ldflags in
      let link_action = Action.CreateExecutable {
        outputs = [ binary_output ];
        objects;
        libraries;
        includes;
        cclibs;
        ccopt_flags;
        cclib_flags;
      }
      in
      ([ link_action ], [ binary_output ], sources)

let from_module_graph
  ?analyzed_modules
  ~package
  ~profile
  ~ctx
  ~toolchain
  ~store
  ~depset
  ~needs_unix
  ~needs_dynlink
  (module_graph: Module_node.t G.t) =
  let transitive_deps = Dependency.transitive_closure depset in
  (* Extract dependency cache include paths - no file copying needed! *)
  let dep_cache_includes =
    List.map transitive_deps ~fn:(fun (dep: Dependency.t) -> dep.artifact_dir)
  in
  (* Add stdlib includes if needed *)
  let stdlib_includes =
    (
      if needs_unix then
        [ Path.v "+unix" ]
      else
        []
    ) @ (
      if needs_dynlink then
        [ Path.v "+dynlink" ]
      else
        []
    )
  in
  let dep_includes = stdlib_includes @ dep_cache_includes in
  let action_graph = create () in
  let node_mapping = HashMap.create () in
  let action_spec_hashes = HashMap.create () in
  let node_outputs = HashMap.create () in
  let all_outputs = Cell.create [] in
  let sorted_modules =
    match G.topo_sort module_graph with
    | Ok sorted -> sorted
    | Error _cycle_ids ->
        (* Cycle should have been caught earlier in module planning *)
        panic "Unexpected cycle in action graph - should have been caught in module planning"
  in
  let analyzed_modules_by_id = HashMap.create () in
  let () =
    match analyzed_modules with
    | Some analyzed_modules ->
        List.for_each
          analyzed_modules
          ~fn:(fun (node_id, analyzed_module) ->
            let _ = HashMap.insert analyzed_modules_by_id ~key:node_id ~value:analyzed_module in
            ())
    | None -> ()
  in
  let package_namespace = Package_namespace.compiled_root package in
  let library_root_candidates =
    if Option.is_none package.Package.library then
      []
    else
      List.filter
        sorted_modules
        ~fn:(fun (node: Module_node.t G.node) ->
          match (G.value node).kind with
          | Module_node.ML mod_
          | Module_node.MLI mod_ ->
              String.equal
                (
                  Module.module_name mod_
                  |> Module_name.to_string
                )
                package_namespace
          | _ -> false)
  in
  let concrete_library_root_modules =
    List.filter
      library_root_candidates
      ~fn:(fun (node: Module_node.t G.node) ->
        match (G.value node).file with
        | Module_node.Concrete _ -> true
        | Module_node.Generated _ -> false)
  in
  let library_root_modules =
    if List.is_empty concrete_library_root_modules then
      library_root_candidates
    else
      concrete_library_root_modules
  in
  let same_module_interface_dependency_ids (node: Module_node.t G.node) =
    match (G.value node).kind with
    | Module_node.ML mod_ ->
        let qualified_name = Module.namespaced_name mod_ in
        List.filter
          (G.deps node)
          ~fn:(fun dep_id ->
            match G.get_node module_graph dep_id with
            | Some dep_node -> (
                match (G.value dep_node).kind with
                | Module_node.MLI dep_mod ->
                    String.equal (Module.namespaced_name dep_mod) qualified_name
                | _ -> false
              )
            | None -> false)
    | _ -> []
  in
  let concrete_module_dependency_ids (node: Module_node.t G.node) =
    match HashMap.get analyzed_modules_by_id ~key:(G.id node) with
    | Some analyzed_module ->
        analyzed_module.Module_graph.resolved_dep_ids
        |> List.filter
          ~fn:(fun dep_id ->
            match G.get_node module_graph dep_id with
            | Some dep_node -> (
                match ((G.value dep_node).kind, (G.value dep_node).file) with
                | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _) -> true
                | _ -> false
              )
            | None -> false)
    | None -> []
  in
  let concrete_reachability_dependency_ids (node: Module_node.t G.node) =
    match ((G.value node).kind, (G.value node).file) with
    | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _) ->
        let semantic_dep_ids = HashSet.create () in
        let () =
          List.for_each
            (concrete_module_dependency_ids node)
            ~fn:(fun dep_id ->
              let _ = HashSet.insert semantic_dep_ids ~value:dep_id in
              ())
        in
        let same_interface_dep_ids = HashSet.create () in
        let () =
          List.for_each
            (same_module_interface_dependency_ids node)
            ~fn:(fun dep_id ->
              let _ = HashSet.insert same_interface_dep_ids ~value:dep_id in
              ())
        in
        List.filter
          (G.deps node)
          ~fn:(fun dep_id ->
            match G.get_node module_graph dep_id with
            | Some dep_node -> (
                match ((G.value dep_node).kind, (G.value dep_node).file) with
                | ((Module_node.Root | Module_node.PackageDependency _), _) -> false
                | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _) ->
                    HashSet.contains semantic_dep_ids ~value:dep_id
                    || HashSet.contains same_interface_dep_ids ~value:dep_id
                | ((Module_node.ML _ | Module_node.MLI _), Module_node.Generated _) -> true
                | _ -> true
              )
            | None -> false)
    | ((Module_node.ML _ | Module_node.MLI _), Module_node.Generated _) ->
        List.filter
          (G.deps node)
          ~fn:(fun dep_id ->
            match G.get_node module_graph dep_id with
            | Some dep_node -> (
                match ((G.value dep_node).kind, (G.value dep_node).file) with
                | (
                    (Module_node.ML _ | Module_node.MLI _),
                    (Module_node.Concrete _ | Module_node.Generated _)
                  ) -> true
                | _ -> false
              )
            | None -> false)
    | _ -> []
  in
  let concrete_module_deps_for_scope (node: Module_node.t G.node) =
    let semantic_dep_ids = HashSet.create () in
    let () =
      List.for_each
        (concrete_module_dependency_ids node)
        ~fn:(fun dep_id ->
          let _ = HashSet.insert semantic_dep_ids ~value:dep_id in
          ())
    in
    let same_interface_dep_ids = HashSet.create () in
    let () =
      List.for_each
        (same_module_interface_dependency_ids node)
        ~fn:(fun dep_id ->
          let _ = HashSet.insert same_interface_dep_ids ~value:dep_id in
          ())
    in
    List.filter
      (G.deps node)
      ~fn:(fun dep_id ->
        match G.get_node module_graph dep_id with
        | Some dep_node -> (
            match ((G.value dep_node).kind, (G.value dep_node).file) with
            | ((Module_node.Root | Module_node.PackageDependency _), _) -> false
            | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _) ->
                HashSet.contains semantic_dep_ids ~value:dep_id
                || HashSet.contains same_interface_dep_ids ~value:dep_id
            | ((Module_node.ML _ | Module_node.MLI _), Module_node.Generated _) -> true
            | _ -> true
          )
        | None -> false)
  in
  let concrete_semantic_reachable_set start_nodes =
    let visited = HashSet.create () in
    let rec visit node_id =
      if HashSet.insert visited ~value:node_id then
        match G.get_node module_graph node_id with
        | Some node -> List.for_each (concrete_reachability_dependency_ids node) ~fn:visit
        | None -> ()
    in
    let () = List.for_each start_nodes ~fn:(fun (node: Module_node.t G.node) -> visit (G.id node)) in
    visited
  in
  let concrete_implementation_counterpart_ids (node: Module_node.t G.node) =
    match ((G.value node).kind, (G.value node).file) with
    | (Module_node.MLI mod_, Module_node.Concrete _) ->
        let qualified_name = Module.namespaced_name mod_ in
        List.filter_map
          sorted_modules
          ~fn:(fun (candidate: Module_node.t G.node) ->
            match ((G.value candidate).kind, (G.value candidate).file) with
            | (Module_node.ML candidate_mod, Module_node.Concrete _) when String.equal
              (Module.namespaced_name candidate_mod)
              qualified_name -> Some (G.id candidate)
            | _ -> None)
    | _ -> []
  in
  let generated_library_interface_public_seed_ids =
    List.filter
      library_root_candidates
      ~fn:(fun (node: Module_node.t G.node) ->
        match ((G.value node).kind, (G.value node).file) with
        | (Module_node.MLI _, Module_node.Generated _) -> true
        | _ -> false)
    |> List.map
      ~fn:(fun (node: Module_node.t G.node) ->
        List.map
          (G.deps node)
          ~fn:(fun dep_id ->
            match G.get_node module_graph dep_id with
            | Some dep_node -> (
                match ((G.value dep_node).kind, (G.value dep_node).file) with
                | (Module_node.MLI _, Module_node.Concrete _) ->
                    dep_id :: concrete_implementation_counterpart_ids dep_node
                | _ -> []
              )
            | None -> [])
        |> List.concat)
    |> List.concat
  in
  let concrete_reachable_set_from_ids start_ids =
    List.filter_map start_ids ~fn:(fun node_id -> G.get_node module_graph node_id)
    |> concrete_semantic_reachable_set
  in
  let library_concrete_reachable_set =
    let library_seed_ids =
      List.map concrete_library_root_modules ~fn:(fun (node: Module_node.t G.node) -> G.id node)
      @ generated_library_interface_public_seed_ids
    in
    if List.is_empty library_seed_ids then
      HashSet.create ()
    else
      concrete_reachable_set_from_ids library_seed_ids
  in
  let generated_deps_for_scope concrete_scope_set dep_ids =
    List.filter
      dep_ids
      ~fn:(fun dep_id ->
        match G.get_node module_graph dep_id with
        | Some dep_node -> (
            match ((G.value dep_node).kind, (G.value dep_node).file) with
            | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _) -> (
                match (G.value dep_node).kind with
                | Module_node.MLI _ -> true
                | Module_node.ML _ -> HashSet.contains concrete_scope_set ~value:dep_id
                | _ -> false
              )
            | ((Module_node.Root | Module_node.PackageDependency _), _) -> false
            | _ -> true
          )
        | None -> false)
  in
  let scoped_module_deps concrete_scope_set (node: Module_node.t G.node) =
    match ((G.value node).kind, (G.value node).file) with
    | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _) ->
        concrete_module_deps_for_scope node
    | ((Module_node.ML _ | Module_node.MLI _), Module_node.Generated _) ->
        generated_deps_for_scope concrete_scope_set (G.deps node)
    | _ -> G.deps node
  in
  let scoped_traversal_deps concrete_scope_set (node: Module_node.t G.node) =
    if List.is_empty concrete_library_root_modules then
      G.deps node
    else
      scoped_module_deps concrete_scope_set node
  in
  let traversal_deps (node: Module_node.t G.node) =
    scoped_traversal_deps library_concrete_reachable_set node
  in
  let library_reachable_ids =
    if List.is_empty library_root_modules then
      []
    else
      let visited = HashSet.create () in
      let rec visit node_id =
        if HashSet.insert visited ~value:node_id then
          match G.get_node module_graph node_id with
          | Some node -> List.for_each (traversal_deps node) ~fn:visit
          | None -> ()
      in
      let () = List.for_each library_root_modules ~fn:(fun node -> visit (G.id node)) in
      HashSet.to_list visited
  in
  let library_reachable_set = HashSet.create () in
  let () =
    List.for_each
      library_reachable_ids
      ~fn:(fun node_id ->
        let _ = HashSet.insert library_reachable_set ~value:node_id in
        ())
  in
  let public_root_set = HashSet.create () in
  let () =
    List.for_each
      library_root_candidates
      ~fn:(fun (node: Module_node.t G.node) ->
        let _ = HashSet.insert public_root_set ~value:(G.id node) in
        ())
  in
  let relative_to_package_root path =
    if Path.is_absolute path then
      let package_root = Path.normalize package.path in
      let normalized = Path.normalize path in
      match Path.strip_prefix normalized ~prefix:package_root with
      | Ok rel when not (String.starts_with ~prefix:"../" rel) -> rel
      | Ok _
      | Error _ -> normalized
    else
      path
  in
  let node_matches_source_path source_path (node: Module_node.t G.node) =
    let source_path = relative_to_package_root source_path in
    match ((G.value node).kind, (G.value node).file) with
    | (Module_node.ML _, Module_node.Concrete path)
    | (Module_node.MLI _, Module_node.Concrete path) ->
        Path.equal (relative_to_package_root path) source_path
    | (_, _) -> false
  in
  let binary_source_nodes source_path =
    List.filter sorted_modules ~fn:(node_matches_source_path source_path)
  in
  let binary_concrete_reachable_set source_path =
    concrete_semantic_reachable_set (binary_source_nodes source_path)
  in
  let binary_scope_set source_path =
    let scope_set = HashSet.create () in
    let () =
      List.for_each
        (HashSet.to_list library_concrete_reachable_set)
        ~fn:(fun node_id ->
          let _ = HashSet.insert scope_set ~value:node_id in
          ())
    in
    let () =
      List.for_each
        (HashSet.to_list (binary_concrete_reachable_set source_path))
        ~fn:(fun node_id ->
          let _ = HashSet.insert scope_set ~value:node_id in
          ())
    in
    scope_set
  in
  let binary_private_dependency_ids source_path =
    let scope_set = binary_scope_set source_path in
    let visited = HashSet.create () in
    let rec visit node_id =
      if HashSet.insert visited ~value:node_id then
        match G.get_node module_graph node_id with
        | Some dep_node -> List.for_each (scoped_traversal_deps scope_set dep_node) ~fn:visit
        | None -> ()
    in
    let () =
      List.for_each
        (binary_source_nodes source_path)
        ~fn:(fun (node: Module_node.t G.node) -> visit (G.id node))
    in
    List.filter_map
      sorted_modules
      ~fn:(fun (node: Module_node.t G.node) ->
        if HashSet.contains visited ~value:(G.id node) then
          match (G.value node).kind with
          | Module_node.ML _
          | Module_node.MLI _ ->
              if HashSet.contains library_reachable_set ~value:(G.id node) then
                None
              else
                Some (G.id node)
          | Module_node.Root
          | Module_node.PackageDependency _ -> None
          | _ ->
              if HashSet.contains library_reachable_set ~value:(G.id node) then
                None
              else
                Some (G.id node)
        else
          None)
  in
  let global_concrete_reachable_set =
    let scope_set = HashSet.create () in
    let () =
      if not (List.is_empty concrete_library_root_modules) then
        List.for_each
          (HashSet.to_list library_concrete_reachable_set)
          ~fn:(fun node_id ->
            let _ = HashSet.insert scope_set ~value:node_id in
            ())
    in
    let () =
      List.for_each
        sorted_modules
        ~fn:(fun (node: Module_node.t G.node) ->
          match (G.value node).kind with
          | Module_node.Binary { source; _ } ->
              List.for_each
                (HashSet.to_list (binary_concrete_reachable_set source))
                ~fn:(fun node_id ->
                  let _ = HashSet.insert scope_set ~value:node_id in
                  ())
          | _ -> ())
    in
    scope_set
  in
  let effective_deps (node: Module_node.t G.node) =
    match (G.value node).kind with
    | Module_node.Library _ when List.is_empty library_root_modules -> G.deps node
    | Module_node.Library _ ->
        List.filter
          (G.deps node)
          ~fn:(fun dep_id ->
            match G.get_node module_graph dep_id with
            | Some dep_node -> (
                match (G.value dep_node).kind with
                | Module_node.ML _
                | Module_node.MLI _ -> HashSet.contains library_reachable_set ~value:dep_id
                | Module_node.Root
                | Module_node.PackageDependency _ -> false
                | _ -> true
              )
            | None -> false)
    | Module_node.Binary { source; _ } ->
        binary_private_dependency_ids source @ List.filter
          (G.deps node)
          ~fn:(fun dep_id ->
            match G.get_node module_graph dep_id with
            | Some dep_node -> (
                match (G.value dep_node).kind with
                | Module_node.Library _ -> true
                | _ -> false
              )
            | None -> false)
    | _ when not (List.is_empty concrete_library_root_modules) ->
        scoped_module_deps global_concrete_reachable_set node
    | _ -> G.deps node
  in
  let target_nodes =
    List.filter
      sorted_modules
      ~fn:(fun (node: Module_node.t G.node) ->
        match (G.value node).kind with
        | Module_node.Library _
        | Module_node.Binary _ -> true
        | _ -> false)
  in
  let included_node_ids =
    if List.is_empty target_nodes then
      List.map sorted_modules ~fn:(fun (node: Module_node.t G.node) -> G.id node)
    else
      let visited = HashSet.create () in
      let rec visit node_id =
        if HashSet.insert visited ~value:node_id then
          match G.get_node module_graph node_id with
          | Some node -> List.for_each (effective_deps node) ~fn:visit
          | None -> ()
      in
      let () = List.for_each target_nodes ~fn:(fun node -> visit (G.id node)) in
      HashSet.to_list visited
  in
  let included_node_ids_set = HashSet.create () in
  let () =
    List.for_each
      included_node_ids
      ~fn:(fun node_id ->
        let _ = HashSet.insert included_node_ids_set ~value:node_id in
        ())
  in
  let get_dep_hash dep_id =
    match HashMap.get action_spec_hashes ~key:dep_id with
    | Some h -> h
    | None ->
        panic
          ("Dependency hash not found for node "
          ^ G.Node_id.to_string dep_id
          ^ ". Graph not in topological order!")
  in
  let get_dep_outputs dep_id =
    match HashMap.get node_outputs ~key:dep_id with
    | Some outs -> outs
    | None -> []
  in
  let module_kinds = HashMap.create () in
  List.for_each
    sorted_modules
    ~fn:(fun (module_node: Module_node.t G.node) ->
      let _ = HashMap.insert module_kinds ~key:(G.id module_node) ~value:(G.value module_node).kind in
      ());
  let get_dep_kind dep_id = HashMap.get module_kinds ~key:dep_id in
  List.for_each
    sorted_modules
    ~fn:(fun (module_node: Module_node.t G.node) ->
      if HashSet.contains included_node_ids_set ~value:(G.id module_node) then
        let deps = effective_deps module_node in
        let (actions, outputs, sources) =
          module_to_actions
            ~package
            ~profile
            ~ctx
            ~dep_includes
            ~get_dep_outputs
            ~get_dep_kind
            ~depset
            ~needs_unix
            ~needs_dynlink
            (G.value module_node)
            deps
        in
        let _ = HashMap.insert node_outputs ~key:(G.id module_node) ~value:outputs in
        if actions = [] then
          let placeholder_hash =
            Crypto.hash_string ("no-actions:" ^ G.Node_id.to_string (G.id module_node))
          in
          let _ = HashMap.insert action_spec_hashes ~key:(G.id module_node) ~value:placeholder_hash in
          ()
        else
          let action_spec =
            Action_node.make
              ~actions
              ~outs:outputs
              ~srcs:sources
              ~package
              ~toolchain
              ~dependency_hashes:get_dep_hash
              ~deps
          in
          let action_node = add_node action_graph action_spec in
          List.for_each
            deps
            ~fn:(fun dep_id ->
              match HashMap.get node_mapping ~key:dep_id with
              | Some dep_action_node ->
                  add_dependency action_graph action_node ~depends_on:dep_action_node
              | None -> ());
      let _ = HashMap.insert node_mapping ~key:(G.id module_node) ~value:action_node in
      let _ = HashMap.insert action_spec_hashes ~key:(G.id module_node) ~value:action_spec.hash in
      Cell.set all_outputs (outputs @ Cell.get all_outputs));
  (action_graph, Cell.get all_outputs)

let to_json = fun t ->
  let open Data.Json in
  let all_nodes = nodes t in
  let sorted_nodes =
    List.sort
      all_nodes
      ~compare:(fun (a: Action_node.t) (b: Action_node.t) ->
        Int.compare
          (G.Node_id.to_int (G.id a))
          (G.Node_id.to_int (G.id b)))
  in
  obj [ ("nodes", array (List.map sorted_nodes ~fn:Action_node.to_json)); ]

let from_json = fun json ->
  let open Data.Json in
  let parse_hash hash_json =
    let module Byte_buf = IO.Bytes in
    let hex_value c =
      if c >= '0' && c <= '9' then
        Some (Char.code c - Char.code '0')
      else if c >= 'a' && c <= 'f' then
        Some (10 + Char.code c - Char.code 'a')
      else if c >= 'A' && c <= 'F' then
        Some (10 + Char.code c - Char.code 'A')
      else
        None
    in
    match hash_json with
    | String hex ->
        let len = String.length hex in
        if not (len mod 2 = 0) then
          Error "hash must have even-length hex"
        else
          let out = Byte_buf.create ~size:(len / 2) in
          let rec fill i =
            if i >= len then
              Ok (Crypto.Hash.from_bytes out)
            else
              match (
                hex
                |> String.get_unchecked ~at:i
                |> hex_value,
                hex
                |> String.get_unchecked ~at:(i + 1)
                |> hex_value
              ) with
              | (Some hi, Some lo) ->
                  let byte = Char.from_int_unchecked ((hi lsl 4) lor lo) in
                  let _ = Byte_buf.set out ~at:(i / 2) ~char:byte in
                  fill (i + 2)
              | _ -> Error "hash contains non-hex characters"
          in
          fill 0
    | _ -> Error "hash must be string"
  in
  match get_field "nodes" json with
  | None -> Error "Missing 'nodes' field"
  | Some (Array node_jsons) -> (
      let graph = create () in
      let id_to_node: (int, Action_node.t) HashMap.t = HashMap.create () in
      let dependencies_to_wire: (Action_node.t * int list) vec = vec [] in
      let parse_actions actions_json =
        match actions_json with
        | Array action_jsons ->
            List.fold_left
              action_jsons
              ~init:(Ok [])
              ~fn:(fun acc action_json ->
                match acc with
                | Error _ -> acc
                | Ok actions -> (
                    match Action.from_json action_json with
                    | Ok action -> Ok (action :: actions)
                    | Error err -> Error err
                  ))
            |> Result.map ~fn:List.reverse
        | _ -> Error "actions must be array"
      in
      let parse_paths paths_json =
        match paths_json with
        | Array path_jsons ->
            Ok (
              List.filter_map
                path_jsons
                ~fn:(fun __tmp1 ->
                  match __tmp1 with
                  | String s -> Some (Path.v s)
                  | _ -> None)
            )
        | _ -> Error "paths must be array"
      in
      let parse_dependencies deps_json =
        match deps_json with
        | Array dep_jsons ->
            List.fold_left
              dep_jsons
              ~init:(Ok [])
              ~fn:(fun acc dep_json ->
                match (acc, dep_json) with
                | (Error e, _) -> Error e
                | (Ok deps, Int dep_id) -> Ok (dep_id :: deps)
                | (Ok _, _) -> Error "dependencies must be int array")
            |> Result.map ~fn:List.reverse
        | _ -> Error "dependencies must be array"
      in
      match List.fold_left
        node_jsons
        ~init:(Ok ())
        ~fn:(fun acc node_json ->
          match acc with
          | Error _ -> acc
          | Ok () -> (
              match (
                get_field "package" node_json,
                get_field "package_path" node_json,
                get_field "package_relative_path" node_json,
                get_field "actions" node_json,
                get_field "outputs" node_json,
                get_field "sources" node_json,
                get_field "hash" node_json,
                get_field "id" node_json,
                get_field "dependencies" node_json
              ) with
              | (
                  Some (String pkg_name),
                  pkg_path_json,
                  pkg_rel_path_json,
                  Some actions_json,
                  Some outputs_json,
                  Some sources_json,
                  Some hash_json,
                  Some (Int legacy_id),
                  Some deps_json
                ) -> (
                  match parse_actions actions_json with
                  | Error err -> Error err
                  | Ok actions -> (
                      match (
                        parse_hash hash_json,
                        parse_paths outputs_json,
                        parse_paths sources_json,
                        parse_dependencies deps_json
                      ) with
                      | (Ok hash, Ok outputs, Ok sources, Ok dependency_ids) ->
                          let package_path =
                            match pkg_path_json with
                            | Some (String p) -> Path.v p
                            | _ -> Path.v "."
                          in
                          let package_relative_path =
                            match pkg_rel_path_json with
                            | Some (String p) -> Path.v p
                            | _ -> package_path
                          in
                          (
                            match Package_name.from_string pkg_name with
                            | Error err -> Error (Package_name.error_message err)
                            | Ok package_name ->
                                let package =
                                  Package.synthetic
                                    ~name:package_name
                                    ~path:package_path
                                    ~relative_path:package_relative_path
                                in
                                let toolchain =
                                  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
                                  |> Result.expect ~msg:"Failed to initialize toolchain"
                                in
                                let action_spec: Action_node.action_spec = {
                                  actions;
                                  outs = outputs;
                                  srcs = sources;
                                  package;
                                  toolchain;
                                  hash;
                                }
                                in
                                let node = add_node graph action_spec in
                                let _ = HashMap.insert id_to_node ~key:legacy_id ~value:node in
                                Vector.push dependencies_to_wire ~value:(node, dependency_ids);
                                Ok ()
                          )
                      | (Error err, _, _, _)
                      | (_, Error err, _, _)
                      | (_, _, Error err, _)
                      | (_, _, _, Error err) -> Error err
                    )
                )
              | _ -> Error "Missing required node fields"
            )) with
      | Error err -> Error err
      | Ok () ->
          Vector.iter dependencies_to_wire
          |> Iterator.to_list
          |> List.for_each
            ~fn:(fun (node, dependency_ids) ->
              List.for_each
                dependency_ids
                ~fn:(fun dep_id ->
                  match HashMap.get id_to_node ~key:dep_id with
                  | Some dep_node -> add_dependency graph node ~depends_on:dep_node
                  | None -> ()));
          Ok graph
    )
  | Some _ -> Error "nodes must be array"

let equal = fun g1 g2 ->
  let nodes1 = topo_sort g1 in
  let nodes2 = topo_sort g2 in
  List.compare_lengths ~left:nodes1 ~right:nodes2 = 0
  && List.all (List.zip nodes1 nodes2) ~fn:(fun (left, right) -> Action_node.equal left right)
