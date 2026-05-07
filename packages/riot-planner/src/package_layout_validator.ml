open Std
open Std.Collections
open Riot_model

module G = Std.Graph.SimpleGraph

let sorted_nodes = fun module_graph ->
  match G.topo_sort module_graph with
  | Ok nodes -> nodes
  | Error _cycle_ids -> G.map module_graph ~fn:(fun (_id, node) -> node)

let analyzed_modules_by_id = fun analyzed_modules ->
  let by_id = HashMap.create () in
  let () =
    List.for_each
      analyzed_modules
      ~fn:(fun (node_id, analyzed_module) ->
        let _ = HashMap.insert by_id ~key:node_id ~value:analyzed_module in
        ())
  in
  by_id

let library_root_candidates = fun ~package nodes ->
  let package_namespace = Package_namespace.compiled_root package in
  if Option.is_none package.Package.library then
    []
  else
    List.filter
      nodes
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

let concrete_library_reachable_set = fun library_roots module_graph ->
  let reachable = HashSet.create () in
  let rec visit node_id =
    if HashSet.insert reachable ~value:node_id then
      match G.get_node module_graph node_id with
      | Some (node: Module_node.t G.node) ->
          List.for_each
            (G.deps node)
            ~fn:(fun dep_id ->
              match G.get_node module_graph dep_id with
              | Some (dep_node: Module_node.t G.node) -> (
                  match (G.value dep_node).kind with
                  | Module_node.Root
                  | Module_node.PackageDependency _
                  | Module_node.Library _
                  | Module_node.Binary _ -> ()
                  | _ -> visit dep_id
                )
              | None -> ())
      | None -> ()
  in
  let () = List.for_each library_roots ~fn:(fun (node: Module_node.t G.node) -> visit (G.id node)) in
  reachable

let binary_source_nodes = fun ~source_path nodes ->
  List.filter
    nodes
    ~fn:(fun (node: Module_node.t G.node) ->
      match ((G.value node).kind, (G.value node).file) with
      | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete path) ->
          Path.equal path source_path
      | _ -> false)

let target_root_nodes = fun package nodes ->
  List.filter_map
    package.Package.binaries
    ~fn:(fun (binary: Package.binary) ->
      binary_source_nodes ~source_path:binary.path nodes
      |> List.map ~fn:(fun node -> (binary.name, node))
      |> List.head)

let target_reachable_set = fun start_nodes module_graph ->
  let reachable = HashSet.create () in
  let rec visit node_id =
    if HashSet.insert reachable ~value:node_id then
      match G.get_node module_graph node_id with
      | Some (node: Module_node.t G.node) ->
          List.for_each
            (G.deps node)
            ~fn:(fun dep_id ->
              match G.get_node module_graph dep_id with
              | Some (dep_node: Module_node.t G.node) -> (
                  match (G.value dep_node).kind with
                  | Module_node.Root
                  | Module_node.PackageDependency _
                  | Module_node.Library _
                  | Module_node.Binary _ -> ()
                  | _ -> visit dep_id
                )
              | None -> ())
      | None -> ()
  in
  let () = List.for_each start_nodes ~fn:(fun (node: Module_node.t G.node) -> visit (G.id node)) in
  reachable

let requested_modules = fun analyzed_module ->
  match analyzed_module.Module_graph.deps with
  | Ok deps -> Syn.Deps.modules deps
  | Error _ -> []

let classify_other_target_root_error = fun
  ~target_name
  ~source
  ~requested_modules
  ~other_target_name
  ~public_module
  (dep_node: Module_node.t G.node) ->
  match (G.value dep_node).kind with
  | Module_node.ML mod_
  | Module_node.MLI mod_ ->
      let simple_name =
        Module.module_name mod_
        |> Module_name.to_string
      in
      let other_target_module = Module.namespaced_name mod_ in
      let requested_module =
        if List.any requested_modules ~fn:(String.equal other_target_module) then
          other_target_module
        else if List.any requested_modules ~fn:(String.equal simple_name) then
          simple_name
        else
          other_target_module
      in
      Planning_error.TargetDependsOnOtherTargetRoot {
        target_name;
        source;
        requested_module;
        other_target_name;
        other_target_module;
        public_module;
      }
  | _ -> panic "expected concrete ML/MLI dependency when classifying other target root error"

let classify_internal_module_error = fun
  ~target_name ~source ~requested_modules (dep_node: Module_node.t G.node) ~public_module ->
  match (G.value dep_node).kind with
  | Module_node.ML mod_
  | Module_node.MLI mod_ ->
      let simple_name =
        Module.module_name mod_
        |> Module_name.to_string
      in
      let internal_name = Module.namespaced_name mod_ in
      if List.any requested_modules ~fn:(String.equal internal_name) then
        Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
          target_name;
          source;
          requested_module = internal_name;
          internal_module = internal_name;
          public_module;
        }
      else
        Planning_error.TargetDependsOnInternalLibraryModule {
          target_name;
          source;
          requested_module =
            if List.any requested_modules ~fn:(String.equal simple_name) then
              simple_name
            else
              internal_name;
          internal_module = internal_name;
          public_module;
        }
  | _ -> panic "expected concrete ML/MLI dependency when classifying internal module error"

let validate_target_source = fun
  ~target_name
  ~(source_node:Module_node.t G.node)
  ~library_reachable_set
  ~public_root_ids
  ~public_module
  ~module_graph
  ~analyzed_modules_by_id
  ~other_target_root_ids ->
  let nodes_to_check =
    let reachable = target_reachable_set [ source_node ] module_graph in
    HashSet.to_list reachable
    |> List.filter_map
      ~fn:(fun node_id ->
        match G.get_node module_graph node_id with
        | Some (node: Module_node.t G.node) -> (
            match ((G.value node).kind, (G.value node).file) with
            | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _) ->
                if
                  G.Node_id.eq (G.id node) (G.id source_node)
                  || not (HashSet.contains library_reachable_set ~value:(G.id node))
                then
                  Some node
                else
                  None
            | _ -> None
          )
        | None -> None)
  in
  List.fold_left
    nodes_to_check
    ~init:(Ok ())
    ~fn:(fun acc (node: Module_node.t G.node) ->
      match acc with
      | Error _ as err -> err
      | Ok () -> (
          match HashMap.get analyzed_modules_by_id ~key:(G.id node) with
          | None -> Ok ()
          | Some analyzed_module ->
              let requested = requested_modules analyzed_module in
              match List.find
                (G.deps node)
                ~fn:(fun dep_id -> Option.is_some (HashMap.get other_target_root_ids ~key:dep_id)) with
              | Some dep_id -> (
                  match G.get_node module_graph dep_id with
                  | Some (dep_node: Module_node.t G.node) ->
                      let other_target_name =
                        HashMap.get other_target_root_ids ~key:dep_id
                        |> Option.unwrap_or ~default:"unknown"
                      in
                      Error (
                        classify_other_target_root_error
                          ~target_name
                          ~source:(
                            (G.value node).file
                            |> fun __tmp1 ->
                              match __tmp1 with
                              | Module_node.Concrete path -> path
                              | Module_node.Generated { path; _ } -> path
                          )
                          ~requested_modules:requested
                          ~other_target_name
                          ~public_module
                          dep_node
                      )
                  | None -> Ok ()
                )
              | None -> (
                  match List.find
                    (G.deps node)
                    ~fn:(fun dep_id ->
                      if HashSet.contains public_root_ids ~value:dep_id then
                        false
                      else if not (HashSet.contains library_reachable_set ~value:dep_id) then
                        false
                      else
                        match G.get_node module_graph dep_id with
                        | Some (dep_node: Module_node.t G.node) -> (
                            match ((G.value dep_node).kind, (G.value dep_node).file) with
                            | ((Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _) -> true
                            | _ -> false
                          )
                        | None -> false) with
                  | Some dep_id -> (
                      match G.get_node module_graph dep_id with
                      | Some (dep_node: Module_node.t G.node) ->
                          Error (
                            classify_internal_module_error
                              ~target_name
                              ~source:(
                                (G.value node).file
                                |> fun __tmp1 ->
                                  match __tmp1 with
                                  | Module_node.Concrete path -> path
                                  | Module_node.Generated { path; _ } -> path
                              )
                              ~requested_modules:requested
                              dep_node
                              ~public_module
                          )
                      | None -> Ok ()
                    )
                  | None -> Ok ()
                )
        ))

let unique_sorted_strings = fun values ->
  values
  |> List.sort ~compare:String.compare
  |> List.unique ~compare:String.compare

let levenshtein_distance = fun left right ->
  let left = String.lowercase_ascii left in
  let right = String.lowercase_ascii right in
  let left_len = String.length left in
  let right_len = String.length right in
  let previous = Array.init ~count:(right_len + 1) ~fn:(fun index -> index) in
  let current = Array.make ~count:(right_len + 1) ~value:0 in
  for left_index = 1 to left_len do
    Array.set_unchecked current ~at:0 ~value:left_index;
    for right_index = 1 to right_len do
      let cost =
        if
          String.get_unchecked left ~at:(left_index - 1)
          = String.get_unchecked right ~at:(right_index - 1)
        then
          0
        else
          1
      in
      let deletion = Array.get_unchecked previous ~at:right_index + 1 in
      let insertion = Array.get_unchecked current ~at:(right_index - 1) + 1 in
      let substitution = Array.get_unchecked previous ~at:(right_index - 1) + cost in
      Array.set_unchecked
        current
        ~at:right_index
        ~value:(Int.min deletion (Int.min insertion substitution))
    done;
    for right_index = 0 to right_len do
      Array.set_unchecked
        previous
        ~at:right_index
        ~value:(Array.get_unchecked current ~at:right_index)
    done
  done;
  Array.get_unchecked previous ~at:right_len

let suggestion_threshold = fun requested_module ->
  let len = String.length requested_module in
  if Int.(len <= 3) then
    1
  else
    Int.max 2 (Int.div len 3)

let module_node_suggestion_names = fun (node: Module_node.t G.node) ->
  match (G.value node).kind with
  | Module_node.ML mod_
  | Module_node.MLI mod_ ->
      let simple_name =
        Module.module_name mod_
        |> Module_name.to_string
      in
      let qualified_name = Module.namespaced_name mod_ in
      if String.equal simple_name qualified_name then
        [ simple_name ]
      else
        [ simple_name; qualified_name ]
  | Module_node.PackageDependency { root_module; _ } -> [ root_module ]
  | _ -> []

let available_module_names = fun ~package ~direct_dependency_modules module_graph ->
  let node_names =
    sorted_nodes module_graph
    |> List.flat_map ~fn:module_node_suggestion_names
  in
  unique_sorted_strings ((Package_namespace.public_root package :: direct_dependency_modules) @ node_names)

let suggested_modules = fun ~requested_module ~available_modules ->
  let threshold = suggestion_threshold requested_module in
  available_modules
  |> List.filter_map
    ~fn:(fun candidate ->
      if String.equal candidate requested_module then
        None
      else
        let distance = levenshtein_distance requested_module candidate in
        if Int.(distance <= threshold) then
          Some (candidate, distance)
        else
          None)
  |> List.sort
    ~compare:(fun (left_name, left_distance) (right_name, right_distance) ->
      match Int.compare left_distance right_distance with
      | Order.EQ -> String.compare left_name right_name
      | ordering -> ordering)
  |> List.map ~fn:(fun (name, _distance) -> name)
  |> List.take ~len:3

let validate_dependency_edges = fun
  ~package ~direct_dependency_modules ~module_graph ~analyzed_modules ->
  let allowed_modules =
    unique_sorted_strings (Package_namespace.public_root package :: direct_dependency_modules)
  in
  let available_modules = available_module_names ~package ~direct_dependency_modules module_graph in
  List.fold_left
    analyzed_modules
    ~init:(Ok ())
    ~fn:(fun acc (_node_id, analyzed_module) ->
      match acc with
      | Error _ as err -> err
      | Ok () -> (
          match unique_sorted_strings analyzed_module.Module_graph.unresolved_deps with
          | [] -> Ok ()
          | requested_module :: _ ->
              Error (
                Planning_error.SourceDependsOnUndeclaredPackageModule {
                  package_name = Package_name.to_string package.name;
                  source = analyzed_module.display_path;
                  requested_module;
                  allowed_modules;
                  suggested_modules = suggested_modules ~requested_module ~available_modules;
                }
              )
        ))

let validate = fun ~direct_dependency_modules ~package ~module_graph ~analyzed_modules ->
  match validate_dependency_edges
    ~package
    ~direct_dependency_modules
    ~module_graph
    ~analyzed_modules with
  | Error _ as err -> err
  | Ok () ->
      let nodes = sorted_nodes module_graph in
      let analyzed_modules_by_id = analyzed_modules_by_id analyzed_modules in
      let public_roots = library_root_candidates ~package nodes in
      let public_root_ids = HashSet.create () in
      let () =
        List.for_each
          public_roots
          ~fn:(fun (node: Module_node.t G.node) ->
            let _ = HashSet.insert public_root_ids ~value:(G.id node) in
            ())
      in
      let library_reachable_set = concrete_library_reachable_set public_roots module_graph in
      let public_module = Package_namespace.public_root package in
      let target_root_nodes = target_root_nodes package nodes in
      List.fold_left
        nodes
        ~init:(Ok ())
        ~fn:(fun acc (node: Module_node.t G.node) ->
          match acc with
          | Error _ as err -> err
          | Ok () -> (
              match (G.value node).kind with
              | Module_node.Binary { name; source; _ } ->
                  let source_nodes = binary_source_nodes ~source_path:source nodes in
                  let other_target_root_ids = HashMap.create () in
                  let () =
                    List.for_each
                      target_root_nodes
                      ~fn:(fun (other_target_name, other_target_node) ->
                        if not (String.equal other_target_name name) then
                          let _ =
                            HashMap.insert
                              other_target_root_ids
                              ~key:(G.id other_target_node)
                              ~value:other_target_name
                          in
                          ())
                  in
                  List.fold_left
                    source_nodes
                    ~init:(Ok ())
                    ~fn:(fun source_acc (source_node: Module_node.t G.node) ->
                      match source_acc with
                      | Error _ as err -> err
                      | Ok () ->
                          validate_target_source
                            ~target_name:name
                            ~source_node
                            ~library_reachable_set
                            ~public_root_ids
                            ~public_module
                            ~module_graph
                            ~analyzed_modules_by_id
                            ~other_target_root_ids)
              | _ -> Ok ()
            ))
