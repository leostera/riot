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

let collect_missing_module_summaries = fun session roots ->
  let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z' in
  let exported_module_prefixes = session.config.loaded_modules
    |> List.map ModuleSummary.exports
    |> List.concat
    |> List.map fst
    |> List.filter_map
      (
        fun export_name ->
          let segments = String.split_on_char '.' export_name in
        match segments with
        | head :: [] -> None
        | head :: _ when String.length head > 0 && is_uppercase_ascii head.[0] -> Some head
        | _ -> None
      )
    |> List.sort_uniq String.compare
  in
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
    match Syn.Deps.of_parse_result parse_result with
    | Ok deps -> Syn.Deps.modules deps
    | Error _ -> []
  in
  let loaded_module_names = fun () ->
    session.config.loaded_modules
    |> List.map ModuleSummary.module_name
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
  let declared_modules_by_source =
    session.sources
    |> List.map (fun (source: Source.t) -> (source.source_id, declared_modules source))
  in
  let declared_modules_all =
    declared_modules_by_source
    |> List.map (fun (_, names) -> names)
    |> List.concat
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
              let loaded_names = loaded_module_names () in
              let source_modules = parse_module_dependencies source in
              let (missing', additional) =
                source_modules
                |> List.fold_left
                  (
                    fun (missing, additional) module_name ->
                      if List.mem module_name loaded_names
                        || List.mem module_name declared_modules_all
                        || List.exists
                          (fun (name, _) -> String.equal name module_name)
                          (session.sources
                           |> List.map (fun (source: Source.t) -> (Source.module_name source, source.source_id)))
                      then (
                        let additional' =
                          match local_source_of_module module_name with
                          | None -> additional
                          | Some dependency_source_id -> dependency_source_id :: additional
                        in
                        (missing, additional')
                      )
                      else if List.mem module_name exported_module_prefixes then
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
