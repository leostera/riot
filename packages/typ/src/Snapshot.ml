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
  mutable qualified_summaries: (SourceId.t * string * PersistedSummary.t) list option;
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
  { revision; roots; analyses; qualified_summaries = None }

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
    (fun summary -> not (String.equal (ModuleSummary.module_name summary) current_module_name))
  |> List.map
    (fun summary ->
      ModuleSummary.exports summary |> qualify_exports (ModuleSummary.module_name summary))
  |> List.flatten

let loaded_ambient_type_decls_for = fun (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  slot.config.loaded_modules
  |> List.filter
    (fun summary -> not (String.equal (ModuleSummary.module_name summary) current_module_name))
  |> List.map
    (fun summary -> ModuleSummary.type_decls summary |> qualify_type_decls (ModuleSummary.module_name summary))
  |> List.flatten

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

let summary_of_analysis = fun (slot: analysis_slot) (analysis: SourceAnalysis.t) ->
  let module_name = Source.module_name slot.source in
  let persisted_summary = analysis.file_summary |> PersistedSummary.of_file_summary in
  (slot.source_id, module_name, persisted_summary)

let rec qualified_summary_of = fun (snapshot: t) visiting (slot: analysis_slot) ->
  if List.exists (SourceId.equal slot.source_id) visiting then
    summary_of_analysis slot (force_base_analysis slot)
  else
    summary_of_analysis slot (force_analysis snapshot ~visiting:(slot.source_id :: visiting) slot)

and qualified_summaries = fun (snapshot: t) ?(visiting = []) () ->
  match (visiting, snapshot.qualified_summaries) with
  | [], Some summaries -> summaries
  | _ ->
      let summaries =
        snapshot.analyses
        |> List.map (qualified_summary_of snapshot visiting)
      in
      let () =
        if visiting = [] then
          snapshot.qualified_summaries <- Some summaries
      in
      summaries

and ambient_env_for = fun (snapshot: t) visiting (slot: analysis_slot) ->
  let local_modules = qualified_summaries snapshot ~visiting ()
  |> List.filter
    (fun (candidate_source_id, _, _) -> not (SourceId.equal candidate_source_id slot.source_id))
  |> List.map
    (fun (_, module_name, summary) -> PersistedSummary.exports summary |> qualify_exports module_name) in
  List.flatten local_modules @ loaded_ambient_env_for slot

and ambient_type_decls_for = fun (snapshot: t) visiting (slot: analysis_slot) ->
  let local_modules = qualified_summaries snapshot ~visiting ()
  |> List.filter
    (fun (candidate_source_id, _, _) -> not (SourceId.equal candidate_source_id slot.source_id))
  |> List.map
    (fun (_, module_name, summary) -> PersistedSummary.type_decls summary |> qualify_type_decls module_name) in
  List.flatten local_modules @ loaded_ambient_type_decls_for slot

and force_analysis = fun (snapshot: t) ?(visiting = []) (slot: analysis_slot) ->
  match slot.analysis with
  | Some analysis -> analysis
  | None ->
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

let analyses = fun snapshot ->
  snapshot.analyses
  |> List.filter (fun (slot: analysis_slot) -> is_root snapshot slot.source_id)
  |> List.map (force_analysis snapshot)

let file_summaries = fun snapshot ->
  analyses snapshot |> List.map (fun (analysis: SourceAnalysis.t) -> analysis.file_summary)

let persisted_summaries = fun snapshot -> file_summaries snapshot |> List.map PersistedSummary.of_file_summary

let module_summaries = fun snapshot ->
  analyses snapshot
  |> List.map
    (fun (analysis: SourceAnalysis.t) ->
      ModuleSummary.make
        ~module_name:(Source.module_name analysis.source)
        ~source_hash:(Source.input_hash analysis.source)
        ~summary:(PersistedSummary.of_file_summary analysis.file_summary))

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
