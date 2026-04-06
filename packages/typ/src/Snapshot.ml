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
  qualified_typings_cache: (string, (SourceId.t * string * ModuleTypings.t) list) Collections.HashMap.t;
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

let source_extension = fun (source: Source.t) ->
  match source.origin with
  | Source.Path path -> Path.extension path
  | Source.Label label -> Path.v label |> Path.extension

let summary_slot_rank = fun (slot: analysis_slot) ->
  match source_extension slot.source with
  | Some ".ml" -> 0
  | Some ".mli" -> 1
  | _ -> 2

let prefer_summary_slot = fun existing candidate ->
  summary_slot_rank candidate < summary_slot_rank existing

let canonical_summary_slots = fun slots ->
  let rec loop order selected = function
    | [] ->
        order |> List.rev |> List.filter_map (fun module_name -> List.assoc_opt module_name selected)
    | (slot: analysis_slot) :: tail ->
        let module_name = Source.module_name slot.source in
        let (order, selected) =
          match List.assoc_opt module_name selected with
          | None -> (module_name :: order, (module_name, slot) :: selected)
          | Some existing when prefer_summary_slot existing slot ->
              (order, (module_name, slot) :: List.remove_assoc module_name selected)
          | Some _ -> (order, selected)
        in
        loop order selected tail
  in
  loop [] [] slots

let force_base_analysis = fun (slot: analysis_slot) ->
  match slot.base_analysis with
  | Some analysis -> analysis
  | None ->
      let config = slot.config
      |> TypConfig.with_ambient ~ambient:(loaded_ambient_env_for slot)
      |> TypConfig.with_ambient_type_decls ~ambient_type_decls:(loaded_ambient_type_decls_for slot) in
      let analysis = SourceAnalysis.analyze ~config slot.source in
      let () =
        slot.base_analysis <- Some analysis
      in
      analysis

let module_typings_of_analysis = fun (slot: analysis_slot) (analysis: SourceAnalysis.t) ->
  let module_name = Source.module_name slot.source in
  let typings =
    ModuleTypings.of_file_summary
      ~module_name
      ~source_hash:(Source.input_hash slot.source)
      analysis.file_summary
  in
  (slot.source_id, module_name, typings)

let visiting_key = fun visiting ->
  visiting
  |> List.map SourceId.to_int
  |> List.sort Int.compare
  |> List.map Int.to_string
  |> String.concat ","

let rec qualified_typings_of = fun (snapshot: t) visiting (slot: analysis_slot) ->
  if List.exists (SourceId.equal slot.source_id) visiting then
    module_typings_of_analysis slot (force_base_analysis slot)
  else
    module_typings_of_analysis slot (force_analysis snapshot ~visiting:(slot.source_id :: visiting) slot)

and qualified_typings = fun (snapshot: t) ?(visiting = []) () ->
  let key = visiting_key visiting in
  match Collections.HashMap.get snapshot.qualified_typings_cache key with
  | Some typings -> typings
  | None ->
      let typings =
        snapshot.analyses
        |> canonical_summary_slots
        |> List.map (qualified_typings_of snapshot visiting)
      in
      let _ = Collections.HashMap.insert snapshot.qualified_typings_cache key typings in
      typings

and ambient_env_for = fun (snapshot: t) visiting (slot: analysis_slot) ->
  let local_modules = qualified_typings snapshot ~visiting ()
  |> List.filter
    (fun (candidate_source_id, _, _) -> not (SourceId.equal candidate_source_id slot.source_id))
  |> List.map
    (fun (_, module_name, typings) -> ModuleTypings.exports typings |> qualify_exports module_name) in
  List.flatten local_modules @ loaded_ambient_env_for slot

and ambient_type_decls_for = fun (snapshot: t) visiting (slot: analysis_slot) ->
  let local_modules = qualified_typings snapshot ~visiting ()
  |> List.filter
    (fun (candidate_source_id, _, _) -> not (SourceId.equal candidate_source_id slot.source_id))
  |> List.map
    (fun (_, module_name, typings) -> ModuleTypings.type_decls typings |> qualify_type_decls module_name) in
  List.flatten local_modules @ loaded_ambient_type_decls_for slot

and force_analysis = fun (snapshot: t) ?(visiting = []) (slot: analysis_slot) ->
  match slot.analysis with
  | Some analysis -> analysis
  | None ->
      let visiting = slot.source_id :: visiting in
      let config = slot.config
      |> TypConfig.with_ambient ~ambient:(ambient_env_for snapshot visiting slot)
      |> TypConfig.with_ambient_type_decls ~ambient_type_decls:(ambient_type_decls_for snapshot visiting slot) in
      let analysis = SourceAnalysis.analyze ~config slot.source in
      let () =
        slot.analysis <- Some analysis
      in
      analysis

let revision = fun snapshot -> snapshot.revision

let roots = fun snapshot -> snapshot.roots

let is_root = fun snapshot source_id -> snapshot.roots |> List.exists (SourceId.equal source_id)

let rooted_slots = fun snapshot ->
  snapshot.analyses
  |> List.filter (fun (slot: analysis_slot) -> is_root snapshot slot.source_id)

let rooted_canonical_summary_slots = fun snapshot ->
  rooted_slots snapshot |> canonical_summary_slots

let module_typings_of_slot = fun snapshot (slot: analysis_slot) ->
  let analysis = force_analysis snapshot slot in
  ModuleTypings.of_file_summary
    ~module_name:(Source.module_name slot.source)
    ~source_hash:(Source.input_hash slot.source)
    analysis.file_summary

let analyses = fun snapshot ->
  rooted_slots snapshot |> List.map (force_analysis snapshot)

let file_summaries = fun snapshot ->
  analyses snapshot |> List.map (fun (analysis: SourceAnalysis.t) -> analysis.file_summary)

let module_typings = fun snapshot ->
  rooted_canonical_summary_slots snapshot |> List.map (module_typings_of_slot snapshot)

let find_module_typings = fun snapshot source_id ->
  if not (is_root snapshot source_id) then
    None
  else
    match List.find_opt
      (fun (slot: analysis_slot) ->
        SourceId.equal slot.source_id source_id)
      snapshot.analyses
    with
    | None -> None
    | Some slot ->
        let module_name = Source.module_name slot.source in
        rooted_canonical_summary_slots snapshot
        |> List.find_opt (fun (candidate: analysis_slot) ->
          String.equal module_name (Source.module_name candidate.source))
        |> Option.map (module_typings_of_slot snapshot)

let find_analysis = fun snapshot source_id ->
  if not (is_root snapshot source_id) then
    None
  else
    List.find_opt
      (fun (slot: analysis_slot) ->
        SourceId.equal slot.source_id source_id)
      snapshot.analyses |> function
    | Some slot -> Some (force_analysis snapshot slot)
    | None -> None
