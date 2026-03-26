open Std
open Std.Collections
open Std.Iter
open Tusk_model
open Tusk_store
module G = Graph.SimpleGraph

exception Cycle_detected of string list

type missing_dependency = {
  package : string;
  dependency : string;
}

type create_error = 
  | MissingPackages of { missing : missing_dependency list }

type build_status = Cached | Fresh
type build_scope = Build | Runtime | Dev
type package_scope = build_scope

type package_node =
  | Unplanned of { package : Package.t; scope : package_scope }
  | Planned of {
      package : Package.t;
      scope : package_scope;
      module_graph : Module_node.t G.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
    }
  | Built of {
      package : Package.t;
      scope : package_scope;
      module_graph : Module_node.t G.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
      artifact : Artifact.t;
      status : build_status;
      depset : Dependency.t list;
    }
  | Failed of {
      package : Package.t;
      scope : package_scope;
      hash : Std.Crypto.hash;
      error : string;
    }
  | Skipped of { package : Package.t; scope : package_scope; reason : string }

type t = {
  graph : package_node G.t;
  name_to_node : (Package.key, package_node G.node) HashMap.t;
}

let get_package = function
  | Unplanned { package; _ } -> package
  | Planned { package; _ } -> package
  | Built { package; _ } -> package
  | Failed { package; _ } -> package
  | Skipped { package; _ } -> package

let get_scope = function
  | Unplanned { scope; _ } -> scope
  | Planned { scope; _ } -> scope
  | Built { scope; _ } -> scope
  | Failed { scope; _ } -> scope
  | Skipped { scope; _ } -> scope

let package_key ~package_name scope =
  Package.key_of_string
    (package_name ^ ":"
   ^
   match scope with
   | Build -> "build"
   | Runtime -> "runtime"
   | Dev -> "dev")

let get_key node =
  let package = get_package node in
  package_key ~package_name:package.name (get_scope node)

let is_planned = function
  | Unplanned _ -> false
  | Planned _ -> true
  | Built _ -> true
  | Failed _ -> true
  | Skipped _ -> true

let get_hash = function
  | Unplanned _ -> None
  | Planned { hash; _ } -> Some hash
  | Built { hash; _ } -> Some hash
  | Failed { hash; _ } -> Some hash
  | Skipped _ -> None

let get_planned_data = function
  | Unplanned _ -> None
  | Planned { package; module_graph; action_graph; hash; _ } ->
      Some (package, module_graph, action_graph, hash)
  | Built { package; module_graph; action_graph; hash; _ } ->
      Some (package, module_graph, action_graph, hash)
  | Failed _ -> None
  | Skipped _ -> None

let is_well_known_package name =
  (* OCaml standard library packages that are distributed with OCaml *)
  match name with
  | "unix" | "stdlib" | "threads" | "str" | "bigarray" | "dynlink" 
  | "compiler-libs" | "graphics" -> true
  | _ -> false

let dependencies_for_scope scope (pkg : Package.t) =
  match scope with
  | Build -> pkg.build_dependencies
  | Runtime -> pkg.dependencies
  | Dev -> pkg.dev_dependencies

let projected_package scope pkg =
  match scope with
  | Build -> Package.for_scope Package.Build pkg
  | Runtime -> Package.for_scope Package.Normal pkg
  | Dev -> Package.for_scope Package.Dev pkg

let needs_build_scope_node (pkg : Package.t) =
  List.length pkg.build_dependencies > 0

let create ~scope (workspace : Workspace.t) : (t, create_error) result =
  let graph = G.make () in
  let name_to_node = HashMap.create () in
  let missing = vec[] in

  let insert_node package scope =
    let node = G.add_node graph (Unplanned { package; scope }) in
    let _ =
      HashMap.insert name_to_node
        (package_key ~package_name:package.name scope)
        node
    in
    ()
  in

  (match scope with
  | Build ->
      List.iter
        (fun (pkg : Package.t) ->
          insert_node (projected_package Build pkg) Build)
        workspace.packages
  | Runtime | Dev ->
      List.iter
        (fun (pkg : Package.t) ->
          if needs_build_scope_node pkg then
            insert_node (projected_package Build pkg) Build)
        workspace.packages);

  (match scope with
  | Build -> ()
  | Runtime | Dev ->
      List.iter
        (fun (pkg : Package.t) -> insert_node (projected_package Runtime pkg) Runtime)
        workspace.packages);

  (match scope with
  | Dev ->
      List.iter
        (fun (pkg : Package.t) -> insert_node (projected_package Dev pkg) Dev)
        workspace.packages
  | Build | Runtime -> ());

  List.iter
    (fun (pkg : Package.t) ->
      let add_dep_edge ~from_scope dep_name =
        match
          HashMap.get name_to_node
            (package_key ~package_name:pkg.name from_scope)
        with
        | None -> ()
        | Some from_node -> (
            let dep_scope =
              match from_scope with
              | Build -> Runtime
              | Runtime -> Runtime
              | Dev -> Runtime
            in
            match
              HashMap.get name_to_node
                (package_key ~package_name:dep_name dep_scope)
            with
            | Some dep_node -> G.add_edge from_node ~depends_on:dep_node
            | None ->
                if not (is_well_known_package dep_name) then
                  Vector.push missing { package = pkg.name; dependency = dep_name })
      in
      (match
         ( HashMap.get name_to_node (package_key ~package_name:pkg.name Runtime),
           HashMap.get name_to_node (package_key ~package_name:pkg.name Build) )
       with
      | Some runtime_node, Some build_node ->
          G.add_edge runtime_node ~depends_on:build_node
      | _ -> ());
      (match
         ( HashMap.get name_to_node (package_key ~package_name:pkg.name Dev),
           HashMap.get name_to_node (package_key ~package_name:pkg.name Runtime) )
       with
      | Some dev_node, Some runtime_node ->
          G.add_edge dev_node ~depends_on:runtime_node
      | _ -> ());
      (* Build-phase dependencies are for build-tooling surfaces such as fused
         tusk-fix providers and future build scripts. Runtime/dev scopes only
         materialize pkg.build nodes for packages that declare
         build_dependencies, avoiding needless duplicate package planning while
         still preserving build-dependency ordering.

         They should not pull build-time libraries into the normal runtime graph
         or we recreate cycles like:

           std.runtime -> std.build -> tusk-fix-api.runtime -> syn.runtime -> std.runtime

         A dedicated Build graph can still wire these edges when build-only
         tooling is the target. *)
      (match scope with
      | Build ->
          List.iter
            (fun (dep : Package.dependency) -> add_dep_edge ~from_scope:Build dep.name)
            pkg.build_dependencies
      | Runtime | Dev -> ());
      List.iter
        (fun (dep : Package.dependency) -> add_dep_edge ~from_scope:Runtime dep.name)
        pkg.dependencies;
      match scope with
      | Build | Runtime -> ()
      | Dev ->
          List.iter
            (fun (dep : Package.dependency) -> add_dep_edge ~from_scope:Dev dep.name)
            pkg.dev_dependencies)
    workspace.packages;

  if Vector.len missing > 0 then
    Error (MissingPackages { missing = Vector.into_iter missing |> Iterator.to_list })
  else
    Ok { graph; name_to_node }

let get_node pg package =
  HashMap.get pg.name_to_node
    (package_key ~package_name:package.Package.name Runtime)

let get_node_by_key pg key = HashMap.get pg.name_to_node key

let mark_planned pg package_key ~module_graph ~action_graph ~hash =
  match HashMap.get pg.name_to_node package_key with
  | None -> ()
  | Some node ->
      let package = get_package node.value in
      let scope = get_scope node.value in
      node.value <- Planned { package; scope; module_graph; action_graph; hash }

let size pg = HashMap.len pg.name_to_node
let packages pg = G.map pg.graph ~fn:(fun (_id, node) -> get_package node.value)

let find_package pg name =
  match HashMap.get pg.name_to_node (package_key ~package_name:name Runtime) with
  | Some node -> Some (get_package node.value)
  | None -> None

let get_package_node pg package =
  match get_node pg package with
  | Some node -> Some node.value
  | None -> None

let filter_for_package pg pkg_name =
  let target_key =
    if
      Option.is_some
        (HashMap.get pg.name_to_node
           (package_key ~package_name:pkg_name Dev))
    then
      package_key ~package_name:pkg_name Dev
    else if
      Option.is_some
        (HashMap.get pg.name_to_node
           (package_key ~package_name:pkg_name Runtime))
    then package_key ~package_name:pkg_name Runtime
    else package_key ~package_name:pkg_name Runtime
  in
  match HashMap.get pg.name_to_node target_key with
  | None -> { graph = G.make (); name_to_node = HashMap.create () }
  | Some target_node ->
      let reachable_ids = G.reachable_from pg.graph [ target_node ] in
      let reachable_set = HashSet.create () in
      List.iter
        (fun id ->
          let _ = HashSet.insert reachable_set id in
          ())
        (target_node.id :: reachable_ids);

      let filtered_graph = G.make () in
      let filtered_name_to_node = HashMap.create () in

      G.iter pg.graph ~fn:(fun id node ->
          if HashSet.contains reachable_set id then
            let new_node = G.add_node filtered_graph node.value in
            let _ = HashMap.insert filtered_name_to_node (get_key node.value) new_node in
            ());

      G.iter pg.graph ~fn:(fun id node ->
          if HashSet.contains reachable_set id then
            match HashMap.get filtered_name_to_node (get_key node.value) with
            | None -> ()
            | Some new_node ->
                List.iter
                  (fun dep_id ->
                    if HashSet.contains reachable_set dep_id then
                      match G.get_node pg.graph dep_id with
                      | Some dep_node ->
                          (match
                             HashMap.get filtered_name_to_node
                               (get_key dep_node.value)
                           with
                          | Some new_dep_node ->
                              G.add_edge new_node ~depends_on:new_dep_node
                          | None -> ())
                      | None -> ())
                  node.deps);

      { graph = filtered_graph; name_to_node = filtered_name_to_node }

let get_graph_node pg node_id = G.get_node pg.graph node_id

let get_dependencies_for_node pg (node : package_node G.node) =
  List.filter_map
    (fun dep_id ->
      match G.get_node pg.graph dep_id with
      | Some dep_node -> Some dep_node.value
      | None -> None)
    node.deps

let get_dependencies graph (package : Package.t) =
  let filtered_graph = filter_for_package graph package.name in
  match
    HashMap.get filtered_graph.name_to_node
      (package_key ~package_name:package.name Runtime)
  with
  | None -> []
  | Some runtime_node ->
      get_dependencies_for_node filtered_graph runtime_node

let get_unplanned_dependencies pg (pkg : Package.t) =
  let deps = get_dependencies pg pkg in
  List.filter_map
    (fun dep -> if not (is_planned dep) then Some (get_package dep) else None)
    deps

let iter_nodes pg ~fn = G.iter pg.graph ~fn:(fun _id node -> fn node)
let topological_sort pg =
  match G.topo_sort pg.graph with
  | Ok sorted_nodes ->
      List.map (fun (node : package_node G.node) -> node.value) sorted_nodes
  | Error node_ids ->
      let names =
        List.filter_map
          (fun id ->
            match G.get_node pg.graph id with
            | Some node -> Some (get_package node.value).name
            | None -> None)
          node_ids
      in
      raise (Cycle_detected names)
