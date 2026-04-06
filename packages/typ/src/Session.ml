open Std

type t = {
  config: TypConfig.t;
  next_source_id: int;
  next_revision: int;
  sources: Source.t list;
}

let empty = fun ~config -> { config; next_source_id = 0; next_revision = 0; sources = [] }

let config = fun session -> session.config

let create_source = fun session ~kind ~origin ~text ->
  let source_id = SourceId.of_int session.next_source_id in
  let source = Source.make ~source_id ~kind ~origin ~revision:session.next_revision ~text in
  (
    {
      session
      with next_source_id = session.next_source_id + 1;
      next_revision = session.next_revision + 1;
      sources = session.sources @ [ source ]
    },
    source_id
  )

let update_source_text = fun session source_id ~text ->
  let revision = session.next_revision in
  let sources =
    session.sources
    |> List.map
      (fun (source: Source.t) ->
        if SourceId.equal source.source_id source_id then
          Source.update_text source ~revision ~text
        else
          source)
  in
  { session with next_revision = revision + 1; sources }

let remove_source = fun session source_id ->
  {
    session
    with next_revision = session.next_revision + 1;
    sources = List.filter
      (fun (source: Source.t) -> not (SourceId.equal source.source_id source_id))
      session.sources
  }

let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

let module_segments_of_export_name = fun export_name ->
  let rec loop acc = function
    | segment :: rest when String.length segment > 0 && is_uppercase_ascii segment.[0] ->
        loop (segment :: acc) rest
    | _ -> List.rev acc
  in
  String.split_on_char '.' export_name |> loop []

let deps_env_for_loaded_modules = fun loaded_modules ->
  let add_summary_paths = fun env summary ->
    let module_name = ModuleSummary.module_name summary in
    let env = Syn.Deps.Env.add_path env ~path:[ module_name ] ~free_names:[ module_name ] in
    let env =
      ModuleSummary.exports summary
      |> List.fold_left
        (fun env (export_name, _) ->
          match module_segments_of_export_name export_name with
          | [] -> env
          | segments ->
              Syn.Deps.Env.add_path env ~path:(module_name :: segments) ~free_names:[ module_name ])
        env
    in
    ModuleSummary.type_decls summary
    |> List.fold_left
      (fun env (type_decl: FileSummary.type_decl) ->
        match type_decl.scope_path with
        | [] -> env
        | scope_path ->
            Syn.Deps.Env.add_path env ~path:(module_name :: scope_path) ~free_names:[ module_name ])
      env
  in
  List.fold_left add_summary_paths Syn.Deps.Env.empty loaded_modules

let collect_missing_module_summaries = fun session roots ->
  let deps_env = deps_env_for_loaded_modules session.config.loaded_modules in
  let declared_modules = fun (source: Source.t) ->
    let filename =
      match source.origin with
      | Source.Path path -> path
      | Source.Label label ->
          Path.of_string label
          |> Result.unwrap_or ~default:(Path.v "<fragment>")
    in
    let parse_result = Syn.parse ~filename source.text in
    match Syn.build_cst parse_result with
    | Ok (Syn.Cst.Implementation implementation) ->
        implementation.Syn.Cst.items
        |> List.filter_map
          (
            fun (item: Syn.Cst.StructureItem.t) ->
              match item with
              | Syn.Cst.StructureItem.ModuleDeclaration declaration ->
                  Some (Syn.Cst.ModuleStructure.name declaration)
              | _ -> None
          )
    | Ok (Syn.Cst.Interface interface) ->
        interface.Syn.Cst.items
        |> List.filter_map
          (
            fun (item: Syn.Cst.SignatureItem.t) ->
              match item with
              | Syn.Cst.SignatureItem.ModuleDeclaration declaration ->
                  Some (Syn.Cst.ModuleSignature.name declaration)
              | _ -> None
          )
    | Error _ -> []
  in
  let parse_module_dependencies = fun (source: Source.t) ->
    let filename =
      match source.origin with
      | Source.Path path -> path
      | Source.Label label ->
          Path.of_string label
          |> Result.unwrap_or ~default:(Path.v "<fragment>")
    in
    let parse_result = Syn.parse ~filename source.text in
    match Syn.Deps.of_parse_result ~env:deps_env parse_result with
    | Ok deps -> Syn.Deps.modules deps
    | Error _ -> []
  in
  let loaded_module_names =
    session.config.loaded_modules
    |> List.map ModuleSummary.module_name
    |> List.sort_uniq String.compare
  in
  let local_source_of_module = fun module_name ->
    session.sources
    |> List.find_opt
      (
        fun (source: Source.t) ->
          String.equal module_name (Source.module_name source)
      )
    |> Option.map (fun (source: Source.t) -> source.source_id)
  in
  let local_nested_module_prefixes = fun source_id ->
    match List.find_opt
      (fun (source: Source.t) -> SourceId.equal source.source_id source_id)
      session.sources
    with
    | None -> []
    | Some source ->
        declared_modules source
  in
  let loaded_nested_module_prefixes = fun module_name ->
    match session.config.loaded_modules
      |> List.find_opt
        (fun summary ->
          String.equal module_name (ModuleSummary.module_name summary))
    with
    | None -> []
    | Some summary ->
        ModuleSummary.exports summary
        |> List.map fst
        |> List.filter_map
          (
            fun export_name ->
              let segments = String.split_on_char '.' export_name in
              match segments with
              | head :: _ :: _ when String.length head > 0 && is_uppercase_ascii head.[0] ->
                  Some head
              | _ -> None
          )
        |> List.sort_uniq String.compare
  in
  let rec add_missing = fun missing module_name requested_by ->
    let rec loop = function
      | [] -> [ (module_name, [ requested_by ]) ]
      | (name, requesters) :: tail when String.equal name module_name ->
          let requesters' =
            if List.exists (SourceId.equal requested_by) requesters then
              requesters
            else
              requesters @ [ requested_by ]
          in
          (name, requesters') :: tail
      | head :: tail -> head :: loop tail
    in
    loop missing
  in
  let rec discover = fun to_visit seen missing ->
    match to_visit with
    | [] -> missing
    | source_id :: rest ->
        if List.exists (SourceId.equal source_id) seen then
          discover rest seen missing
        else (
          let seen = source_id :: seen in
          match List.find_opt
            (fun (source: Source.t) -> SourceId.equal source.source_id source_id)
            session.sources
          with
          | None -> discover rest seen missing
          | Some source ->
              let source_modules = parse_module_dependencies source in
              let dependency_nested_modules =
                source_modules
                |> List.concat_map
                  (fun module_name ->
                    let local_nested_modules =
                      match local_source_of_module module_name with
                      | Some dependency_source_id ->
                          local_nested_module_prefixes dependency_source_id
                      | None -> []
                    in
                    let loaded_nested_modules =
                      if List.mem module_name loaded_module_names then
                        loaded_nested_module_prefixes module_name
                      else
                        []
                    in
                    local_nested_modules @ loaded_nested_modules)
                |> List.sort_uniq String.compare
              in
              let (missing', additional) =
                source_modules
                |> List.fold_left
                  (
                    fun (missing, additional) module_name ->
                      if List.mem module_name loaded_module_names then
                        (missing, additional)
                      else
                        match local_source_of_module module_name with
                        | Some dependency_source_id ->
                            (missing, dependency_source_id :: additional)
                        | None ->
                            if List.mem module_name dependency_nested_modules then
                              (missing, additional)
                            else
                              (add_missing missing module_name source_id, additional)
                  ) 
                  (missing, [])
              in
              discover (additional @ rest) seen missing'
        )
  in
  let source_ids =
    roots |> List.filter_map
      (
        fun source_id ->
          if
            List.exists
              (fun (source: Source.t) -> SourceId.equal source.source_id source_id)
              session.sources
          then
            Some source_id
          else
            None
      )
  in
  let missing = discover source_ids [] [] in
  MissingRequirements.of_list
    (missing
    |> List.map
      (
        fun (module_name, requested_by) ->
          MissingRequirements.MissingModuleSummary { module_name; requested_by }
      ))


let prepare_snapshot = fun session ~roots ->
  let missing_roots =
    roots
    |> List.filter
      (fun root_id ->
        session.sources |> List.exists
          (fun (source: Source.t) ->
            SourceId.equal source.source_id root_id) |> not)
    |> List.map (fun source_id -> MissingRequirements.MissingRootSource { source_id })
  in
  let missing_modules = collect_missing_module_summaries session roots in
  let missing_requirements =
    MissingRequirements.of_list
      (missing_roots @ MissingRequirements.requirements missing_modules)
  in
  if MissingRequirements.(missing_requirements |> is_empty) then
    Ok (Snapshot.make
      ~revision:session.next_revision
      ~roots
      ~config:session.config
      ~sources:session.sources)
  else
    Error missing_requirements

let snapshot = fun session ->
  let roots = session.sources |> List.map (fun (source: Source.t) -> source.source_id) in
  match prepare_snapshot session ~roots with
  | Ok snapshot -> snapshot
  | Error _ -> panic "Session.snapshot: current session sources should always prepare successfully"
