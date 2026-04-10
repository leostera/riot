open Std
open Riot_model
module G = Std.Graph.SimpleGraph

let ( let* ) = Result.and_then

type interface_source = {
  source_path: Path.t;
  relative_path: Path.t;
  module_name: string;
  module_path: string list;
  qualified_name: string;
  content: string;
}

type lookup = {
  sources: interface_source list;
  by_module_path: (string * interface_source) list;
  by_qualified_name: (string * interface_source) list;
}

let module_path_key = fun module_path ->
  String.concat "." module_path

let package_module_name = fun package_name ->
  package_name |> String.map
    (fun ch ->
      match ch with
      | '-' -> '_'
      | _ -> ch) |> String.capitalize_ascii

let profile_for = fun release ->
  if release then
    Profile.release
  else
    Profile.debug

let relative_path_for = fun ~package_path source_path ->
  if Path.is_absolute source_path then
    match Path.strip_prefix source_path ~prefix:package_path with
    | Ok rel -> rel
    | Error _ -> source_path
  else
    source_path

let interface_source_of_node = fun ~(package:Riot_model.Package.t) (
  node: Riot_planner.Module_node.t G.node
) ->
  match node.value.kind, node.value.file with
  | Riot_planner.Module_node.MLI mod_, Riot_planner.Module_node.Concrete path when Path.to_string path
  != "" ->
      let source_path =
        if Path.is_absolute path then
          path
        else
          Path.(package.path / path)
      in
      let relative_path = relative_path_for ~package_path:package.path path in
      let module_name = Module.module_name mod_ |> Module_name.to_string in
      let module_path = Module.module_name mod_
      |> Module_name.namespace
      |> Namespace.to_list
      |> fun prefix -> prefix @ [ module_name ] in
      let qualified_name = Module.qualified_name mod_ in
      let content = Fs.read source_path |> Result.unwrap_or ~default:"" in
      Some {
        source_path = relative_path;
        relative_path;
        module_name;
        module_path;
        qualified_name;
        content;
      }
  | Riot_planner.Module_node.MLI mod_, Riot_planner.Module_node.Generated { path; contents } ->
      let module_name = Module.module_name mod_ |> Module_name.to_string in
      let module_path = Module.module_name mod_
      |> Module_name.namespace
      |> Namespace.to_list
      |> fun prefix -> prefix @ [ module_name ] in
      let qualified_name = Module.qualified_name mod_ in
      Some {
        source_path = path;
        relative_path = path;
        module_name;
        module_path;
        qualified_name;
        content = contents;
      }
  | _ ->
      None

let collect_interfaces = fun ~workspace ~store ~release (package: Riot_model.Package.t) ->
  let profile = profile_for release in
  let ctx = Build_ctx.make ~session_id:(Session_id.make ()) ~profile () in
  let toolchain_config = Toolchain_config.from_workspace workspace in
  let* toolchain = Riot_toolchain.init ~config:toolchain_config
  |> Result.map_error
    (fun err -> "failed to initialize toolchain for documentation planning: " ^ err) in
  let plan_input: Riot_planner.Module_planner.plan_input = {
    package;
    profile;
    ctx;
    toolchain;
    workspace;
    planning_root = Path.v "src";
    allowed_source_files = package.sources.src;
    root_mode = (
      match package.library with
      | Some _ -> Riot_planner.Module_graph.Library_root { library_name = package.name }
      | None -> Riot_planner.Module_graph.Loose_sources
    );
    depset = [];
    store;
  }
  in
  let* plan = Riot_planner.Module_planner.plan_node plan_input
  |> Result.map_error Riot_planner.Planning_error.to_string in
  Ok (
    G.map plan.module_graph ~fn:(fun (_id, node) -> interface_source_of_node ~package node)
    |> List.filter_map
      (
        function
        | Some source -> Some source
        | None -> None
      )
    |> List.sort_uniq
      (fun left right ->
        String.compare (module_path_key left.module_path) (module_path_key right.module_path))
  )

let build_lookup = fun sources ->
  {
    sources;
    by_module_path = List.map (fun source -> (module_path_key source.module_path, source)) sources;
    by_qualified_name = List.map (fun source -> (source.qualified_name, source)) sources
  }

let find_root_interface = fun ~package_name sources ->
  let expected_path = [ package_module_name package_name ] in
  List.find_opt (fun source -> source.module_path = expected_path) sources

let find_by_module_path = fun lookup module_path ->
  List.assoc_opt (module_path_key module_path) lookup.by_module_path

let source_prefix = fun source ->
  match List.rev source.module_path with
  | _name :: rest -> List.rev rest
  | [] -> []

let candidate_module_paths = fun ~current_path target_path ->
  match target_path with
  | [] ->
      []
  | [ target_name ] ->
      let root_path =
        match current_path with
        | root :: _ -> [ root; target_name ]
        | [] -> [ target_name ]
      in
      [ current_path @ [ target_name ]; root_path; [ target_name ]; ]
  | segments ->
      let rooted_segments =
        match current_path, segments with
        | root :: _, first :: _ when first != root -> root :: segments
        | _ -> segments
      in
      [ rooted_segments; segments; ]

let resolve_module_path = fun lookup ~current_path ~target_path ->
  let rec first_match = function
    | [] -> None
    | module_path :: rest -> (
        match find_by_module_path lookup module_path with
        | Some source -> Some source
        | None -> first_match rest
      )
  in
  let qualified_match =
    match target_path with
    | [ qualified_name ] when String.contains qualified_name "_" -> List.assoc_opt
      qualified_name
      lookup.by_qualified_name
    | _ -> None
  in
  match qualified_match with
  | Some source -> Some source
  | None -> (
      match first_match (candidate_module_paths ~current_path target_path) with
      | Some source -> Some source
      | None -> (
          match List.rev target_path with
          | [] -> None
          | target_name :: _ ->
              match lookup.sources
              |> List.find_opt
                (fun source -> source.module_name = target_name && source_prefix source = current_path) with
              | Some source -> Some source
              | None -> List.find_opt (fun source -> source.module_name = target_name) lookup.sources
        )
    )

let source_signature = fun sources ->
  let state = Crypto.Sha256.create () in
  List.iter
    (fun source ->
      Crypto.Sha256.write state (Path.to_string source.source_path);
      Crypto.Sha256.write state (module_path_key source.module_path);
      Crypto.Sha256.write state source.qualified_name;
      Crypto.Sha256.write state source.content)
    sources;
  Crypto.Digest.hex (Crypto.Sha256.finish state)
