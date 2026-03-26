open Std
open Std.Sync
open Std.Collections
open Std.Iter
open Tusk_model
module G = Std.Graph.SimpleGraph

type t = { graph : Action_node.action_spec G.t }

let create () = { graph = G.make () }
let add_node t node_value = G.add_node t.graph node_value
let add_dependency t node ~depends_on = G.add_edge node ~depends_on
let topo_sort t =
  match G.topo_sort t.graph with
  | Ok sorted -> sorted
  | Error _cycle_ids ->
      (* Should never happen - cycles caught in module planning *)
      panic "Unexpected cycle in action graph"

let nodes t = topo_sort t
let graph t = t.graph

let to_action_list t =
  let sorted = topo_sort t in
  List.concat_map (fun (node : Action_node.t) -> node.value.actions) sorted

let hash_action_node _t (node : Action_node.t) = node.value.hash

let opens mods =
  List.filter_map
    (fun (node : Module_node.t G.node) ->
      match node.value.kind with
      | ML mod_ | MLI mod_ ->
          Some (Tusk_toolchain.Ocamlc.Open (Module.namespaced_name mod_))
      | _ -> None)
    mods

(** Determine compiler flags for stdlib handling based on package dependencies *)
let stdlib_flags (package : Package.t) =
  (* Check if this package has stdlib as a dependency *)
  let has_stdlib_dep = 
    List.exists (fun (dep : Package.dependency) -> dep.name = "stdlib") 
      (Package.build_graph_dependencies package)
  in
  (* Always add -nopervasives to prevent automatic opening of Stdlib *)
  (* Add -nostdlib only if package doesn't depend on stdlib *)
  if has_stdlib_dep then
    [ Tusk_toolchain.Ocamlc.NoPervasives ]
  else
    [ Tusk_toolchain.Ocamlc.NoPervasives; Tusk_toolchain.Ocamlc.NoStdlib ]

let module_to_actions ~package ~profile ~ctx ~dep_includes ~get_dep_outputs ~depset ~needs_unix ~needs_dynlink
    (module_node : Module_node.t) (deps : G.Node_id.t list) :
    Action.t list * Path.t list * Path.t list =
  match module_node with
  | { kind = MLI mod_; file = Concrete path; open_modules; _ } ->
      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [ cmti_output; cmi_output ] in
      let sources = [ path ] in
      let compile =
        Action.CompileInterface
          {
            source = path;
            outputs;
            includes = Path.v "." :: dep_includes;
            flags = stdlib_flags package @ opens open_modules;
          }
      in
      ([ compile ], outputs, sources)
  | { kind = ML mod_; file = Concrete path; open_modules; _ } ->
      let cmx_output = Module.cmx mod_ in
      let cmi_output = Module.cmi mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs = [ cmt_output; cmi_output; cmx_output ] in
      let sources = [ path ] in
      let compile =
        Action.CompileImplementation
          {
            source = path;
            outputs;
            includes = Path.v "." :: dep_includes;
            flags = stdlib_flags package @ opens open_modules;
          }
      in
      ([ compile ], outputs, sources)
  | { kind = ML mod_; file = Generated { path; contents }; open_modules; _ } ->
      let write_action =
        Action.WriteFile { destination = path; content = contents }
      in

      let cmx_output = Module.cmx mod_ in
      let cmi_output = Module.cmi mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs = [ cmt_output; cmi_output; cmx_output ] in
      let sources = [] in

      let is_alias_file =
        String.ends_with ~suffix:"Aliases.ml-gen" (Path.to_string path)
      in
      let flags =
        if is_alias_file then
          stdlib_flags package 
          @ (Tusk_toolchain.Ocamlc.Impl path :: Tusk_toolchain.Ocamlc.NoAliasDeps
             :: opens open_modules)
        else 
          stdlib_flags package @ opens open_modules
      in

      let compile_action =
        Action.CompileImplementation
          {
            source = path;
            outputs;
            includes = Path.v "." :: dep_includes;
            flags;
          }
      in
      ([ write_action; compile_action ], outputs, sources)
  | { kind = MLI mod_; file = Generated { path; contents }; open_modules; _ } ->
      let write_action =
        Action.WriteFile { destination = path; content = contents }
      in

      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [ cmti_output; cmi_output ] in
      let sources = [] in
      let compile_action =
        Action.CompileInterface
          {
            source = path;
            outputs;
            includes = Path.v "." :: dep_includes;
            flags = stdlib_flags package @ opens open_modules;
          }
      in
      ([ write_action; compile_action ], outputs, sources)
  | { kind = Native { files }; _ } ->
      let c_files =
        List.filter
          (fun path -> String.ends_with ~suffix:".c" (Path.to_string path))
          files
      in

      (* Use cc_flags from the profile (already has target-specific flags applied) *)
      let base_ccflags = profile.Profile.cc_flags in
      
      (* Add sysroot if cross-compiling *)
      let ccflags = match Build_ctx.sysroot ctx with
        | Some sysroot ->
            let sysroot_flag = "--sysroot=" ^ Path.to_string sysroot in
            sysroot_flag :: base_ccflags
        | None ->
            base_ccflags
      in
      
      Log.debug ("[ACTION_GRAPH] C compilation cc_flags for " ^ package.name ^ ": " ^ String.concat " " ccflags);

      let actions =
        List.map
          (fun c_file ->
            let obj_file =
              Path.remove_extension c_file |> Path.add_extension ~ext:"o"
            in
            let output_name = Path.basename obj_file |> Path.v in
            Action.CompileC { source = c_file; outputs = [ output_name ]; ccflags })
          c_files
      in

      let outputs =
        List.map
          (fun c_file ->
            let obj_file =
              Path.remove_extension c_file |> Path.add_extension ~ext:"o"
            in
            Path.basename obj_file |> Path.v)
          c_files
      in

      (actions, outputs, files)
  | { kind = C; file = Generated _; _ }
  | { kind = H; _ }
  | { kind = Root; _ }
  | { kind = Other _; _ } ->
      ([], [], [])
  | { kind = Library { name; includes }; _ } ->
      let library_name = Module_name.(of_string name |> cmxa) in
      let archive_name = Module_name.(of_string name |> a) in
      let shared_lib_name = Module_name.(of_string name |> cmxs) in
      let sources = [] in

      let objects_with_duplicates =
        List.concat_map
          (fun dep_id ->
            let dep_outputs = get_dep_outputs dep_id in
            List.filter
              (fun output ->
                match Path.extension output with
                | Some ".cmx" | Some ".o" -> true
                | _ -> false)
              dep_outputs)
          deps
      in
      
      (* Deduplicate objects to avoid linking the same file multiple times *)
      (* Use List.unique to preserve topological order required by OCaml linker *)
      let objects = List.unique objects_with_duplicates in

      (* Create static library metadata (.cmxa). *)
      let create_lib = Action.CreateLibrary { 
        outputs = [ library_name; archive_name ];
        objects; 
        includes 
      } in
      
      (* For shared libraries, include external OCaml runtime dependencies *)
      (* These are dependencies that can't be dynamically loaded later (stdlib, unix, dynlink) *)
      let has_stdlib_dep = 
        List.exists (fun (dep : Package.dependency) -> dep.name = "stdlib") 
          (Package.build_graph_dependencies package)
      in
      
      let external_libs = 
        (if has_stdlib_dep then [ Path.v "stdlib.cmxa" ] else [])
        @ (if needs_unix then [ Path.v "unix.cmxa" ] else [])
        @ (if needs_dynlink then [ Path.v "dynlink.cmxa" ] else [])
      in
      
      (* When building .cmxs from .cmxa, the C objects are already embedded in the .cmxa *)
      (* We should NOT pass them again via -cclib as this causes duplicate symbol errors *)
      (* The .cmxa was built with all necessary C objects included (see CreateLibrary above) *)
      
      (* Create shared library (.cmxs) from the .cmxa *)
      let create_shared = Action.CreateSharedLibrary {
        outputs = [ shared_lib_name ];
        objects = [ library_name ];  (* Use the .cmxa as input *)
        libraries = external_libs;   (* Include external OCaml runtime libraries *)
        includes;
        cclibs = [];                 (* Empty - C objects already in .cmxa *)
        ccopt_flags = [];
        cclib_flags = [];
      } in
      
      let all_outputs = [ library_name; archive_name; shared_lib_name ] in
      ([ create_lib; create_shared ], all_outputs, sources)
  | { kind = Binary { name; source; libraries; includes }; _ } ->
      let binary_mod =
        Module.make ~namespace:Namespace.empty ~filename:source
      in
      let binary_cmx = Module.cmx binary_mod in
      let sources = [ source ] in

      let compile_action =
        Action.CompileImplementation
          {
            source;
            outputs = [ binary_cmx ];
            includes = Path.v "." :: dep_includes;
            flags = stdlib_flags package;
          }
      in

      (* Collect foreign library outputs for linking *)
      let cclibs =
        List.concat_map
          (fun (fdep : Package.foreign_dependency) ->
            (* Make foreign outputs absolute by joining with foreign dep path and normalizing *)
            List.map (fun out -> Path.normalize (Path.join fdep.path out)) fdep.outputs)
          package.foreign_dependencies
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
         which can only be determined at link-time based on the depset. *)
      let transitive_deps = Dependency.transitive_closure depset in
      let dep_ldflags = 
        List.concat_map (fun (dep : Dependency.t) ->
          match List.assoc_opt target_platform dep.package.compiler.target_overrides with
          | Some target_override -> (
              match target_override.profile_override with
              | Some override -> (
                  match override.ld_flags with
                  | Profile.Override flags -> flags
                  | Inherit -> [])
              | None -> [])
          | None -> []
        ) transitive_deps
      in
      
      (* Combine: current package + dependencies *)
      let merged_ldflags = base_ldflags @ dep_ldflags in
      
      Log.debug ("[ACTION_GRAPH] Package " ^ package.name ^ ": base ld_flags = [" ^ 
                 String.concat ", " base_ldflags ^ "]");
      Log.debug ("[ACTION_GRAPH] Package " ^ package.name ^ ": dependency ld_flags = [" ^ 
                 String.concat ", " dep_ldflags ^ "]");
      Log.debug ("[ACTION_GRAPH] Package " ^ package.name ^ ": merged ld_flags = [" ^ 
                 String.concat ", " merged_ldflags ^ "]");
      
      (* Add sysroot if cross-compiling *)
      let ccflags = match Build_ctx.sysroot ctx with
        | Some sysroot ->
            let sysroot_flag = "--sysroot=" ^ Path.to_string sysroot in
            sysroot_flag :: base_ccflags
        | None ->
            base_ccflags
      in
      
      let ldflags = match Build_ctx.sysroot ctx with
        | Some sysroot ->
            let sysroot_flag = "--sysroot=" ^ Path.to_string sysroot in
            sysroot_flag :: merged_ldflags
        | None ->
            merged_ldflags
      in
      
      Log.debug ("[ACTION_GRAPH] Final ccopt_flags for linking " ^ name ^ ": " ^ String.concat " " ccflags);
      Log.debug ("[ACTION_GRAPH] Final cclib_flags for linking " ^ name ^ ": " ^ String.concat " " ldflags);
      
      (* Keep ccopt_flags and cclib_flags separate *)
      let ccopt_flags = ccflags in
      let cclib_flags = ldflags in
      
      let link_action =
        Action.CreateExecutable
          {
            outputs = [ binary_output ];
            objects = [ binary_cmx ];
            libraries;
            includes;
            cclibs;
            ccopt_flags;
            cclib_flags;
          }
      in
      ([ compile_action; link_action ], [ binary_output ], sources)

let from_module_graph ~package ~profile ~ctx ~toolchain ~store ~depset ~needs_unix ~needs_dynlink
    (module_graph : Module_node.t G.t) : t * Path.t list =
  let transitive_deps = Dependency.transitive_closure depset in

  (* Extract dependency cache include paths - no file copying needed! *)
  let dep_cache_includes =
    List.map
      (fun (dep : Dependency.t) -> dep.artifact_dir)
      transitive_deps
  in

  (* Add stdlib includes if needed *)
  let stdlib_includes =
    (if needs_unix then [ Path.v "+unix" ] else [])
    @ (if needs_dynlink then [ Path.v "+dynlink" ] else [])
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

  let get_dep_hash dep_id =
    match HashMap.get action_spec_hashes dep_id with
    | Some h -> h
    | None ->
        panic
          "Dependency hash not found for node %s. Graph not in topological \
           order!"
          (G.Node_id.to_string dep_id)
  in

  let get_dep_outputs dep_id =
    match HashMap.get node_outputs dep_id with Some outs -> outs | None -> []
  in

  List.iter
    (fun (module_node : Module_node.t G.node) ->
      let actions, outputs, sources =
        module_to_actions ~package ~profile ~ctx ~dep_includes ~get_dep_outputs ~depset ~needs_unix ~needs_dynlink
          module_node.value module_node.deps
      in

      let _ = HashMap.insert node_outputs module_node.id outputs in
      if actions = [] then
        let placeholder_hash =
          Crypto.hash_string
            ("no-actions:" ^ G.Node_id.to_string module_node.id)
        in
        let _ =
          HashMap.insert action_spec_hashes module_node.id placeholder_hash
        in
        ()
      else
        let action_spec =
          Action_node.make ~actions ~outs:outputs ~srcs:sources ~package
            ~toolchain ~dependency_hashes:get_dep_hash ~deps:module_node.deps
        in

        let action_node = add_node action_graph action_spec in

        List.iter
          (fun dep_id ->
            match HashMap.get node_mapping dep_id with
            | Some dep_action_node ->
                add_dependency action_graph action_node
                  ~depends_on:dep_action_node
            | None -> ())
          module_node.deps;

        let _ = HashMap.insert node_mapping module_node.id action_node in
        let _ =
          HashMap.insert action_spec_hashes module_node.id action_spec.hash
        in
        Cell.set all_outputs (outputs @ Cell.get all_outputs))
    sorted_modules;

  (action_graph, Cell.get all_outputs)

let to_json t =
  let open Data.Json in
  let all_nodes = nodes t in
  let sorted_nodes =
    List.sort
      (fun (a : Action_node.t) (b : Action_node.t) ->
        G.Node_id.to_int a.id - G.Node_id.to_int b.id)
      all_nodes
  in
  obj [ ("nodes", array (List.map Action_node.to_json sorted_nodes)) ]

let from_json json =
  let open Data.Json in
  match get_field "nodes" json with
  | None -> Error "Missing 'nodes' field"
  | Some (Array node_jsons) -> (
      let graph = create () in
      let id_to_node : (int, Action_node.t) HashMap.t = HashMap.create () in
      let dependencies_to_wire : (Action_node.t * int list) vec = vec [] in

      let parse_actions actions_json =
        match actions_json with
        | Array action_jsons ->
            List.fold_left
              (fun acc action_json ->
                match acc with
                | Error _ -> acc
                | Ok actions -> (
                    match Action.from_json action_json with
                    | Ok action -> Ok (action :: actions)
                    | Error err -> Error err))
              (Ok []) action_jsons
            |> Result.map List.rev
        | _ -> Error "actions must be array"
      in

      let parse_paths paths_json =
        match paths_json with
        | Array path_jsons ->
            Ok
              (List.filter_map
                 (function String s -> Some (Path.v s) | _ -> None)
                 path_jsons)
        | _ -> Error "paths must be array"
      in

      let parse_dependencies deps_json =
        match deps_json with
        | Array dep_jsons ->
            List.fold_left
              (fun acc dep_json ->
                match (acc, dep_json) with
                | Error e, _ -> Error e
                | Ok deps, Int dep_id -> Ok (dep_id :: deps)
                | Ok _, _ -> Error "dependencies must be int array")
              (Ok []) dep_jsons
            |> Result.map List.rev
        | _ -> Error "dependencies must be array"
      in

      match
        List.fold_left
          (fun acc node_json ->
            match acc with
            | Error _ -> acc
            | Ok () -> (
                match
                  ( get_field "package" node_json,
                    get_field "actions" node_json,
                    get_field "outputs" node_json,
                    get_field "sources" node_json,
                    get_field "id" node_json,
                    get_field "dependencies" node_json )
                with
                | ( Some (String pkg_name),
                    Some actions_json,
                    Some outputs_json,
                    Some sources_json,
                    Some (Int legacy_id),
                    Some deps_json ) -> (
                    match parse_actions actions_json with
                    | Error err -> Error err
                    | Ok actions -> (
                        match
                          ( parse_paths outputs_json,
                            parse_paths sources_json,
                            parse_dependencies deps_json )
                        with
                        | Ok outputs, Ok sources, Ok dependency_ids ->
                            let package =
                              Package.
                {
                  name = pkg_name;
                  path = Path.v ".";
                  relative_path = Path.v ".";
                  dependencies = [];
                  dev_dependencies = [];
                  build_dependencies = [];
                  foreign_dependencies = [];
                  binaries = [];
                  library = None;
                  sources =
                    { src = []; native = []; tests = []; examples = []; bench = [] };
                  compiler = { profile_overrides = []; target_overrides = [] };
                  commands = [];
                  fix_providers = [];
                }
                            in
                            let toolchain =
                              Tusk_toolchain.init
                                ~config:Tusk_model.Toolchain_config.default
                              |> Result.expect
                                   ~msg:"Failed to initialize toolchain"
                            in
                            let action_spec =
                              Action_node.make ~actions ~outs:outputs
                                ~srcs:sources ~package ~toolchain
                                ~dependency_hashes:(fun _ ->
                                  Crypto.hash_string "")
                                ~deps:[]
                            in
                            let node = add_node graph action_spec in
                            let _ = HashMap.insert id_to_node legacy_id node in
                            Vector.push dependencies_to_wire
                              (node, dependency_ids);
                            Ok ()
                        | Error err, _, _
                        | _, Error err, _
                        | _, _, Error err ->
                            Error err))
                | _ -> Error "Missing required node fields"))
          (Ok ()) node_jsons
      with
      | Error err -> Error err
      | Ok () ->
          Vector.into_iter dependencies_to_wire
          |> Iterator.to_list
          |> List.iter (fun (node, dependency_ids) ->
                 List.iter
                   (fun dep_id ->
                     match HashMap.get id_to_node dep_id with
                     | Some dep_node ->
                         add_dependency graph node ~depends_on:dep_node
                     | None -> ())
                   dependency_ids);
          Ok graph)
  | Some _ -> Error "nodes must be array"

let equal g1 g2 =
  let nodes1 = topo_sort g1 in
  let nodes2 = topo_sort g2 in
  try List.for_all2 Action_node.equal nodes1 nodes2 with _ -> false
