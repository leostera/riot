open Std

type analysis_slot = {
  source_id: SourceId.t;
  source: Source.t;
  config: TypConfig.t;
  mutable base_analysis: SourceAnalysis.t option;
  mutable analysis: SourceAnalysis.t option;
}

type t = {
  revision: int;
  roots: SourceId.t list;
  analyses: analysis_slot list;
  qualified_typings_cache: (string, ModuleTypings.t list) Collections.HashMap.t;
  module_results_cache: (string, (string * ModulePairing.t) list) Collections.HashMap.t;
}

let make = fun ~revision ~roots ~config ~sources ->
  let analyses =
    sources
    |> List.map
      (fun (source: Source.t) ->
        {
          source_id = source.source_id;
          source;
          config;
          base_analysis = None;
          analysis = None;
        })
  in
  {
    revision;
    roots;
    analyses;
    qualified_typings_cache = Collections.HashMap.with_capacity 8;
    module_results_cache = Collections.HashMap.with_capacity 8;
  }

let qualify_exports = fun module_name exports ->
  List.map (fun (name, scheme) -> (module_name ^ "." ^ name, scheme)) exports

let qualify_type_decls = fun module_name type_decls ->
  List.map
    (fun (type_decl: FileSummary.type_decl) ->
      {
        FileSummary.scope_path = module_name :: type_decl.scope_path;
        declaration = type_decl.declaration;
      })
    type_decls

let module_names_of_slots = fun slots ->
  let rec loop order seen = function
    | [] -> List.rev order
    | (slot: analysis_slot) :: tail ->
        let module_name = Source.module_name slot.source in
        if List.mem module_name seen then
          loop order seen tail
        else
          loop (module_name :: order) (module_name :: seen) tail
  in
  loop [] [] slots

let slot_of_source_id = fun snapshot source_id ->
  snapshot.analyses
  |> List.find_opt (fun (slot: analysis_slot) -> SourceId.equal slot.source_id source_id)

let module_slots = fun snapshot module_name ->
  snapshot.analyses
  |> List.filter
    (fun (slot: analysis_slot) ->
      String.equal module_name (Source.module_name slot.source))

let rooted_slots = fun snapshot ->
  snapshot.analyses
  |> List.filter
    (fun (slot: analysis_slot) ->
      snapshot.roots |> List.exists (SourceId.equal slot.source_id))

let rooted_module_names = fun snapshot ->
  rooted_slots snapshot |> module_names_of_slots

let loaded_ambient_env_for = fun (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  slot.config.loaded_modules
  |> List.filter
    (fun typings -> not (String.equal (ModuleTypings.module_name typings) current_module_name))
  |> List.map
    (fun typings ->
      ModuleTypings.exports typings |> qualify_exports (ModuleTypings.module_name typings))
  |> List.flatten

let loaded_ambient_type_decls_for = fun (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  slot.config.loaded_modules
  |> List.filter
    (fun typings -> not (String.equal (ModuleTypings.module_name typings) current_module_name))
  |> List.map
    (fun typings -> ModuleTypings.type_decls typings |> qualify_type_decls (ModuleTypings.module_name typings))
  |> List.flatten

let visiting_key = fun visiting ->
  visiting
  |> List.map SourceId.to_int
  |> List.sort Int.compare
  |> List.map Int.to_string
  |> String.concat ","

let force_base_analysis = fun (slot: analysis_slot) ->
  match slot.base_analysis with
  | Some analysis -> analysis
  | None ->
      let config =
        slot.config
        |> TypConfig.with_ambient ~ambient:(loaded_ambient_env_for slot)
        |> TypConfig.with_ambient_type_decls ~ambient_type_decls:(loaded_ambient_type_decls_for slot)
      in
      let analysis = SourceAnalysis.analyze ~config slot.source in
      let () = slot.base_analysis <- Some analysis in
      analysis

let rec module_results_for = fun (snapshot: t) visiting ->
  let key = visiting_key visiting in
  match Collections.HashMap.get snapshot.module_results_cache key with
  | Some results -> results
  | None ->
      let results =
        snapshot.analyses
        |> module_names_of_slots
        |> List.map
          (fun module_name ->
            (
              module_name,
              module_result_for snapshot visiting module_name
            ))
      in
      let _ = Collections.HashMap.insert snapshot.module_results_cache key results in
      results

and qualified_typings = fun (snapshot: t) ?(visiting = []) () ->
  let key = visiting_key visiting in
  match Collections.HashMap.get snapshot.qualified_typings_cache key with
  | Some typings -> typings
  | None ->
      let typings =
        module_results_for snapshot visiting
        |> List.map (fun (_module_name, result) -> result.ModulePairing.module_typings)
      in
      let _ = Collections.HashMap.insert snapshot.qualified_typings_cache key typings in
      typings

and ambient_env_for = fun (snapshot: t) visiting (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  let local_modules =
    module_results_for snapshot visiting
    |> List.filter
      (fun (module_name, _result) -> not (String.equal current_module_name module_name))
    |> List.map
      (fun (module_name, result) ->
        ModuleTypings.exports result.ModulePairing.module_typings |> qualify_exports module_name)
    |> List.flatten
  in
  local_modules @ loaded_ambient_env_for slot

and ambient_type_decls_for = fun (snapshot: t) visiting (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  let local_type_decls =
    module_results_for snapshot visiting
    |> List.filter
      (fun (module_name, _result) -> not (String.equal current_module_name module_name))
    |> List.map
      (fun (module_name, result) ->
        ModuleTypings.type_decls result.ModulePairing.module_typings |> qualify_type_decls module_name)
    |> List.flatten
  in
  local_type_decls @ loaded_ambient_type_decls_for slot

and force_analysis = fun (snapshot: t) ?(visiting = []) (slot: analysis_slot) ->
  match slot.analysis with
  | Some analysis -> analysis
  | None ->
      let visiting = slot.source_id :: visiting in
      let config =
        slot.config
        |> TypConfig.with_ambient ~ambient:(ambient_env_for snapshot visiting slot)
        |> TypConfig.with_ambient_type_decls ~ambient_type_decls:(ambient_type_decls_for snapshot visiting slot)
      in
      let analysis = SourceAnalysis.analyze ~config slot.source in
      let () =
        match visiting with
        | [ source_id ] when SourceId.equal source_id slot.source_id ->
            slot.analysis <- Some analysis
        | _ -> ()
      in
      analysis

and module_result_for = fun (snapshot: t) visiting module_name ->
  let sources =
    module_slots snapshot module_name
    |> List.map
      (fun (slot: analysis_slot) ->
        let analysis =
          if List.exists (SourceId.equal slot.source_id) visiting then
            force_base_analysis slot
          else
            force_analysis snapshot ~visiting:(slot.source_id :: visiting) slot
        in
        (slot.source, analysis))
  in
  ModulePairing.of_sources ~module_name sources

let revision = fun snapshot -> snapshot.revision

let roots = fun snapshot -> snapshot.roots

let module_result_of_source = fun snapshot source_id ->
  slot_of_source_id snapshot source_id
  |> Option.map
    (fun (slot: analysis_slot) ->
      let module_name = Source.module_name slot.source in
      module_name,
      (module_results_for snapshot []
       |> List.assoc_opt module_name))

let is_root = fun snapshot source_id ->
  snapshot.roots |> List.exists (SourceId.equal source_id)

let analyses = fun snapshot ->
  rooted_slots snapshot
  |> List.filter_map
    (fun (slot: analysis_slot) ->
      match module_results_for snapshot []
        |> List.assoc_opt (Source.module_name slot.source)
      with
      | Some result -> List.assoc_opt slot.source_id result.ModulePairing.analyses_by_source
      | None -> None)

let file_summaries = fun snapshot ->
  analyses snapshot |> List.map (fun (analysis: SourceAnalysis.t) -> analysis.file_summary)

let module_typings = fun snapshot ->
  let rooted_module_names = rooted_module_names snapshot in
  rooted_module_names
  |> List.filter_map
    (fun module_name ->
      module_results_for snapshot []
      |> List.assoc_opt module_name
      |> Option.map (fun result -> result.ModulePairing.module_typings))

let find_module_typings = fun snapshot source_id ->
  if not (is_root snapshot source_id) then
    None
  else
    match module_result_of_source snapshot source_id with
    | Some (_module_name, Some result) -> Some result.ModulePairing.module_typings
    | Some (_, None)
    | None -> None

let find_analysis = fun snapshot source_id ->
  if not (is_root snapshot source_id) then
    None
  else
    match module_result_of_source snapshot source_id with
    | Some (_module_name, Some result) -> List.assoc_opt source_id result.ModulePairing.analyses_by_source
    | Some (_, None)
    | None -> None
