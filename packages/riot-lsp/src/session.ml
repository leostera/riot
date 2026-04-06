open Std
open Std.Data

let ( let* ) = Result.and_then

let lint_rules = Riot_fix.Pipeline.default_rules ()

type document = {
  uri: Lsp.Uri.t;
  version: int;
  text: string;
  path: Path.t option;
}

type fixable_lint_diagnostic = {
  diagnostic: Riot_fix.Diagnostic.t;
  lsp_diagnostic: Lsp.Diagnostic.t;
  fix: Riot_fix.Fix.fix;
}

type t = {
  initialized: bool;
  shutdown_requested: bool;
  documents: document list;
  workspace_manager: Riot_model.Workspace_manager.t;
}

type outcome = {
  state: t;
  outbound: Json.t list;
  exit_code: int option;
}

let empty = {
  initialized = false;
  shutdown_requested = false;
  documents = [];
  workspace_manager = Riot_model.Workspace_manager.create ()
}

let uri_equal = fun left ->
  fun right ->
    String.equal (Lsp.Uri.to_string left) (Lsp.Uri.to_string right)

let upsert_document = fun state document ->
  {
    state
    with documents = document
    :: List.filter (fun existing -> not (uri_equal existing.uri document.uri)) state.documents
  }

let remove_document = fun state uri ->
  {
    state
    with documents = List.filter (fun document -> not (uri_equal document.uri uri)) state.documents
  }

let find_document = fun state uri ->
  List.find_opt (fun document -> uri_equal document.uri uri) state.documents

let response_error = fun ~id ~code ~message ?data () ->
  Lsp.error_response_to_json ~id Lsp.{ code; message; data }

let ok = fun state ?exit_code outbound -> { state; outbound; exit_code }

let filename_of_uri = fun uri ->
  match Lsp.Uri.to_path uri with
  | Ok path -> path
  | Error _ -> Path.v "buffer.ml"

let compare_paths = fun left right ->
  String.compare (Path.to_string left) (Path.to_string right)

let dedupe_paths = fun paths -> paths |> List.sort_uniq compare_paths

let document_path_key = fun (document: document) ->
  match document.path with
  | None -> None
  | Some path -> Some (Path.normalize path |> Path.to_string)

let document_in_root = fun root ->
  fun (document: document) ->
    match document.path with
    | None -> false
    | Some path ->
        let root = Path.normalize root in
        let path = Path.normalize path in
        Path.equal path root || match Path.strip_prefix path ~prefix:root with
        | Ok _ -> true
        | Error _ -> false

let filename_of_document = fun (document: document) ->
  match document.path with
  | Some path -> path
  | None -> filename_of_uri document.uri

let typ_source_origin_of_document = fun (document: document) ->
  match document.path with
  | Some path -> Typ.Source.Path path
  | None -> Typ.Source.Label (Lsp.Uri.to_string document.uri)

let package_scope_for_file = fun state path ->
  let start_dir = Path.dirname path in
  match Riot_model.Workspace_manager.scan state.workspace_manager start_dir with
  | Error _ -> None
  | Ok (workspace, _errors) -> (
      match Riot_model.Workspace.find_package_for_path workspace ~path with
      | None -> None
      | Some pkg -> Some pkg
    )

let package_source_files = fun (pkg: Riot_model.Package.t) ->
  pkg.sources.src @ pkg.sources.tests @ pkg.sources.examples @ pkg.sources.bench
  |> List.map (fun relative -> Path.(pkg.path / relative))
  |> dedupe_paths

let package_typ_summary_source_files = fun (pkg: Riot_model.Package.t) ->
  pkg.sources.src
  |> List.map (fun relative -> Path.(pkg.path / relative))
  |> dedupe_paths

let workspace_typ_store_root = fun (workspace: Riot_model.Workspace.t) ->
  Path.(workspace.target_dir_root / Path.v "typ-cache")

let workspace_typ_store = fun (workspace: Riot_model.Workspace.t) ->
  let contentstore =
    Contentstore.create
      ~root:(workspace_typ_store_root workspace)
      ~policy:Contentstore.Policy.default
      ()
  in
  Typ.Store.create contentstore ()

let sanitize_module_name = fun name ->
  String.map
    (fun ch ->
      if ch = '-' then
        '_'
      else
        ch)
    name

let module_name_for_path = fun path ->
  path
  |> Path.remove_extension
  |> Path.basename
  |> sanitize_module_name
  |> String.capitalize_ascii

let load_package_module_typings_from_store = fun store pkg ->
  let module_names =
    package_typ_summary_source_files pkg
    |> List.map module_name_for_path
  in
  let loaded =
    module_names
    |> List.filter_map (fun module_name -> Typ.Store.load_module_typings store ~module_name)
  in
  if List.length loaded = List.length module_names then
    Some loaded
  else
    None

let persist_module_typings = fun store typings ->
  typings
  |> List.iter
    (fun typings ->
      ignore (Typ.Store.save_module_typings store typings))

let workspace_package_by_name = fun (workspace: Riot_model.Workspace.t) package_name ->
  workspace.packages
  |> List.find_opt
    (fun (pkg: Riot_model.Package.t) ->
      Riot_model.Package.is_workspace_member pkg && String.equal pkg.name package_name)

let merge_module_exports = fun preferred fallback ->
  let rec loop seen acc remaining =
    match remaining with
    | [] -> List.rev acc
    | ((name, _) as export) :: tail ->
        if List.mem name seen then
          loop seen acc tail
        else
          loop (name :: seen) (export :: acc) tail
  in
  loop [] [] (preferred @ fallback)

let type_decl_key = fun (type_decl: Typ.FileSummary.type_decl) ->
  String.concat "." (type_decl.scope_path @ [ type_decl.declaration.type_name ])

let merge_module_type_decls = fun preferred fallback ->
  let rec loop seen acc remaining =
    match remaining with
    | [] -> List.rev acc
    | ((type_decl: Typ.FileSummary.type_decl) as candidate) :: tail ->
        let key = type_decl_key candidate in
        if List.mem key seen then
          loop seen acc tail
        else
          loop (key :: seen) (candidate :: acc) tail
  in
  loop [] [] (preferred @ fallback)

let typings_with_payload = fun template ~source_hash ~exports ~type_decls ->
  let module_name = Typ.ModuleTypings.module_name template in
  match Typ.ModuleTypings.export_result template, exports with
  | Typ.FileSummary.TrustedExport _, _ ->
      Typ.ModuleTypings.trusted ~module_name ~source_hash ~type_decls exports
  | Typ.FileSummary.ErroredExport _, _ ->
      Typ.ModuleTypings.errored ~module_name ~source_hash ~type_decls exports
  | Typ.FileSummary.NoExport, [] ->
      Typ.ModuleTypings.missing ~module_name ~source_hash ~type_decls ()
  | Typ.FileSummary.NoExport, _ ->
      Typ.ModuleTypings.errored ~module_name ~source_hash ~type_decls exports

let merge_module_typings = fun preferred fallback ->
  let module_name = Typ.ModuleTypings.module_name preferred in
  let exports =
    merge_module_exports
      (Typ.ModuleTypings.exports preferred)
      (Typ.ModuleTypings.exports fallback)
  in
  let type_decls =
    merge_module_type_decls
      (Typ.ModuleTypings.type_decls preferred)
      (Typ.ModuleTypings.type_decls fallback)
  in
  let template =
    match Typ.ModuleTypings.export_result preferred, exports with
    | Typ.FileSummary.NoExport, _ :: _ -> fallback
    | _ -> preferred
  in
  let source_hash =
    Typ.ModuleTypings.synthetic_source_hash
      ~module_name
      ~export_result:(Typ.ModuleTypings.export_result template)
      ~type_decls
  in
  typings_with_payload template ~source_hash ~exports ~type_decls

let merge_loaded_module_typings = fun preferred fallback ->
  let rec loop order merged remaining =
    match remaining with
    | [] ->
        order
        |> List.rev
        |> List.filter_map (fun module_name -> List.assoc_opt module_name merged)
    | summary :: tail ->
        let module_name = Typ.ModuleTypings.module_name summary in
        let (order, merged) =
          match List.assoc_opt module_name merged with
          | None ->
              (module_name :: order, (module_name, summary) :: merged)
          | Some existing ->
              (order, (module_name, merge_module_typings existing summary) :: List.remove_assoc module_name merged)
        in
        loop order merged tail
  in
  loop [] [] (preferred @ fallback)

let typ_session_with_paths = fun ~config paths ->
  paths
  |> List.fold_left
    (fun (session, source_ids, sources) path ->
      match Fs.read path with
      | Error _ -> (session, source_ids, sources)
      | Ok text ->
          let (session, source_id) = Typ.Session.create_source
            session
            ~kind:Typ.Source.File
            ~origin:(Typ.Source.Path path)
            ~text in
          let source =
            Typ.Source.make
              ~source_id
              ~kind:Typ.Source.File
              ~origin:(Typ.Source.Path path)
              ~revision:0
              ~text
          in
          (session, source_ids @ [ source_id ], sources @ [ source ]))
    (Typ.Session.empty ~config, [], [])

let module_typings_for_roots = fun session roots ->
  let summaries_for_snapshot snapshot roots =
    roots
    |> List.filter_map (Typ.Query.module_typings_of snapshot)
  in
  match Typ.Session.prepare_snapshot session ~roots with
  | Ok snapshot ->
      summaries_for_snapshot snapshot roots
  | Error _ ->
      roots
      |> List.filter_map
        (fun source_id ->
          match Typ.Session.prepare_snapshot session ~roots:[ source_id ] with
          | Ok snapshot -> Typ.Query.module_typings_of snapshot source_id
          | Error _ -> None)
      |> merge_loaded_module_typings []

let workspace_dependency_packages = fun ~include_dev (workspace: Riot_model.Workspace.t) (pkg: Riot_model.Package.t) ->
  let dependencies =
    if include_dev then
      Riot_model.Package.build_graph_dependencies pkg
    else
      pkg.dependencies
  in
  dependencies
  |> List.filter_map
    (fun (dependency: Riot_model.Package.dependency) -> workspace_package_by_name workspace dependency.name)
  |> List.sort_uniq
    (fun (left: Riot_model.Package.t) (right: Riot_model.Package.t) -> String.compare left.name right.name)

let workspace_module_typings_for_package =
  let rec load cache typ_store (workspace: Riot_model.Workspace.t) ?(visiting = []) (pkg: Riot_model.Package.t) =
    match List.assoc_opt pkg.name !cache with
    | Some typings -> typings
    | None when List.mem pkg.name visiting -> []
    | None ->
        let dependency_typings =
          workspace_dependency_packages ~include_dev:false workspace pkg
          |> List.concat_map
            (fun dependency_pkg ->
              load cache typ_store workspace ~visiting:(pkg.name :: visiting) dependency_pkg)
        in
        let package_typings =
          match load_package_module_typings_from_store typ_store pkg with
          | Some typings -> typings
          | None ->
              let loaded_modules =
                merge_loaded_module_typings dependency_typings Typ.Config.default.loaded_modules
              in
              let config = Typ.Config.default
              |> Typ.Config.with_loaded_modules ~loaded_modules
              |> Typ.Config.with_store ~store:(Some typ_store)
              in
              let (session, roots, _sources) =
                typ_session_with_paths ~config (package_typ_summary_source_files pkg)
              in
              let typings =
                match roots with
                | [] -> []
                | _ -> module_typings_for_roots session roots
              in
              let () = persist_module_typings typ_store typings in
              typings
        in
        let typings =
          merge_loaded_module_typings package_typings dependency_typings
        in
        let () =
          cache := (pkg.name, typings) :: !cache
        in
        typings
  in
  load

let typ_target_files = fun state ->
  fun (document: document) ->
    match document.path with
    | None -> []
    | Some path -> (
        match package_scope_for_file state path with
        | Some pkg -> (
            let package_root = pkg.path in
            let package_files = package_source_files pkg in
            let open_documents = state.documents
            |> List.filter (document_in_root package_root)
            |> List.filter_map (fun document -> document.path) in
            dedupe_paths (package_files @ open_documents @ [ path ])
          )
        | None -> [ path ]
      )

let text_for_path = fun state path ->
  let key = Path.normalize path |> Path.to_string in
  state.documents |> List.find_opt
    (fun document ->
      match document_path_key document with
      | Some candidate -> String.equal candidate key
      | None -> false) |> function
  | Some document -> Some document.text
  | None -> (
      match Fs.read path with
      | Ok text -> Some text
      | Error _ -> None
    )

let typ_config_for_document = fun state ->
  fun (document: document) ->
    match document.path with
    | None -> Typ.Config.default
    | Some path -> (
        match package_scope_for_file state path with
        | None -> Typ.Config.default
        | Some pkg -> (
            match Riot_model.Workspace_manager.scan state.workspace_manager pkg.path with
            | Error _ -> Typ.Config.default
            | Ok (workspace, _errors) ->
                let typ_store = workspace_typ_store workspace in
                let summary_cache = ref [] in
                let dependency_summaries =
                  workspace_dependency_packages ~include_dev:true workspace pkg
                  |> List.concat_map
                  (fun dependency_pkg ->
                    workspace_module_typings_for_package summary_cache typ_store workspace dependency_pkg)
              in
              let loaded_modules =
                merge_loaded_module_typings dependency_summaries Typ.Config.default.loaded_modules
                in
                Typ.Config.default
                |> Typ.Config.with_loaded_modules ~loaded_modules
                |> Typ.Config.with_store ~store:(Some typ_store)
          )
      )

let typ_analysis_for_document = fun state ->
  fun (document: document) ->
    let config = typ_config_for_document state document in
    let current_key =
      match document.path with
      | Some path -> Some (Path.normalize path |> Path.to_string)
      | None -> None
    in
    let paths = typ_target_files state document in
    let initial = (Typ.Session.empty ~config, None, []) in
    let from_paths =
      List.fold_left
        (fun (session, current_source_id, sources) path ->
          match text_for_path state path with
          | None -> (session, current_source_id, sources)
          | Some text ->
              let (session, source_id) = Typ.Session.create_source
                session
                ~kind:Typ.Source.File
                ~origin:(Typ.Source.Path path)
                ~text in
              let revision =
                match current_key with
                | Some key when String.equal key
                  (Path.normalize path |> Path.to_string) -> document.version
                | _ -> 0
              in
              let source =
                Typ.Source.make
                  ~source_id
                  ~kind:Typ.Source.File
                  ~origin:(Typ.Source.Path path)
                  ~revision
                  ~text
              in
              let current_source_id =
                match current_key with
                | Some key when String.equal key
                  (Path.normalize path |> Path.to_string) -> Some source_id
                | _ -> current_source_id
              in
              (session, current_source_id, sources @ [ source ]))
        initial
        paths
    in
    match from_paths with
    | (session, Some source_id, sources) -> (
        match Typ.Session.prepare_snapshot session ~roots:[ source_id ] with
        | Ok snapshot ->
            let () =
              match config.Typ.Config.store with
              | None -> ()
              | Some store -> persist_module_typings store (Typ.Snapshot.module_typings snapshot)
            in
            Typ.Query.analysis_of_source snapshot source_id
        | Error _ ->
            Typ.Snapshot.make ~revision:document.version ~roots:[ source_id ] ~config ~sources
            |> fun snapshot -> Typ.Query.analysis_of_source snapshot source_id
      )
    | (session, None, sources) ->
        let (session, source_id) = Typ.Session.create_source
          session
          ~kind:Typ.Source.File
          ~origin:(typ_source_origin_of_document document)
          ~text:document.text in
        let source =
          Typ.Source.make
            ~source_id
            ~kind:Typ.Source.File
            ~origin:(typ_source_origin_of_document document)
            ~revision:document.version
            ~text:document.text
        in
        (
          match Typ.Session.prepare_snapshot session ~roots:[ source_id ] with
          | Ok snapshot ->
              let () =
                match config.Typ.Config.store with
                | None -> ()
                | Some store -> persist_module_typings store (Typ.Snapshot.module_typings snapshot)
              in
              Typ.Query.analysis_of_source snapshot source_id
          | Error _ ->
              Typ.Snapshot.make
                ~revision:document.version
                ~roots:[ source_id ]
                ~config
                ~sources:(sources @ [ source ])
              |> fun snapshot -> Typ.Query.analysis_of_source snapshot source_id
        )

let diagnostic_to_lsp = fun text ->
  fun (diagnostic: Syn.Diagnostic.t) ->
    {
      Lsp.Diagnostic.range = Lsp.Utf16.range_of_offsets
        text
        ~start_offset:diagnostic.span.start
        ~end_offset:diagnostic.span.end_;
      severity = Some Lsp.Diagnostic.Error;
      code = Some (Syn.Diagnostic.id diagnostic);
      source = Some "syn";
      message = Syn.Diagnostic.main_message diagnostic;
      tags = None;
      data = Some (Syn.Diagnostic.to_json diagnostic);
    }

let lint_diagnostic_severity = fun severity ->
  match severity with
  | Riot_fix.Diagnostic.Error -> Lsp.Diagnostic.Error
  | Riot_fix.Diagnostic.Warning -> Lsp.Diagnostic.Warning
  | Riot_fix.Diagnostic.Info -> Lsp.Diagnostic.Information
  | Riot_fix.Diagnostic.Hint -> Lsp.Diagnostic.Hint

let lint_diagnostic_to_lsp = fun text ->
  fun (diagnostic: Riot_fix.Diagnostic.t) ->
    let span = Riot_fix.Diagnostic.span diagnostic in
    {
      Lsp.Diagnostic.range = Lsp.Utf16.range_of_offsets
        text
        ~start_offset:span.start
        ~end_offset:span.end_;
      severity = Some (lint_diagnostic_severity (Riot_fix.Diagnostic.severity diagnostic));
      code = Some (Riot_fix.Diagnostic.rule_id diagnostic);
      source = Some "riot-fix";
      message = Riot_fix.Diagnostic.message diagnostic;
      tags = None;
      data = Some (Riot_fix.Diagnostic.to_json diagnostic);
    }

let typ_diagnostic_severity = fun severity ->
  match severity with
  | Typ.Diagnostic.Error -> Lsp.Diagnostic.Error
  | Typ.Diagnostic.Warning -> Lsp.Diagnostic.Warning

let typ_diagnostic_to_lsp = fun text ->
  fun (diagnostic: Typ.Diagnostic.t) ->
    let span = Typ.Diagnostic.primary_span diagnostic in
    {
      Lsp.Diagnostic.range = Lsp.Utf16.range_of_offsets
        text
        ~start_offset:span.start
        ~end_offset:span.end_;
      severity = Some (typ_diagnostic_severity (Typ.Diagnostic.severity diagnostic));
      code = Some (Typ.Diagnostic.code diagnostic);
      source = Some "typ";
      message = Typ.Diagnostic.message diagnostic;
      tags = None;
      data = Some (Typ.Diagnostic.to_json diagnostic);
    }

let analyze_document = fun document ->
  Riot_fix.Source_runner.run ~rules:lint_rules ~filename:(filename_of_document document) document.text

let compare_position = fun (left: Lsp.Position.t) ->
  fun (right: Lsp.Position.t) ->
    match Int.compare left.line right.line with
    | 0 -> Int.compare left.character right.character
    | n -> n

let ranges_overlap = fun (left: Lsp.Range.t) ->
  fun (right: Lsp.Range.t) ->
    compare_position left.end_ right.start_ >= 0 && compare_position right.end_ left.start_ >= 0

let same_range = fun (left: Lsp.Range.t) ->
  fun (right: Lsp.Range.t) ->
    compare_position left.start_ right.start_ = 0 && compare_position left.end_ right.end_ = 0

let same_lsp_diagnostic = fun (left: Lsp.Diagnostic.t) ->
  fun (right: Lsp.Diagnostic.t) ->
    same_range left.range right.range
    && Option.equal String.equal left.code right.code
    && Option.equal String.equal left.source right.source
    && String.equal left.message right.message

let action_kind_allowed = fun only actual ->
  let actual_name = Lsp.Action_kind.to_string actual in
  match only with
  | None -> true
  | Some requested ->
      List.exists
        (fun requested_kind ->
          let requested_name = Lsp.Action_kind.to_string requested_kind in
          String.equal requested_name actual_name
          || String.starts_with ~prefix:((requested_name ^ ".")) actual_name)
        requested

let lint_diagnostic_requested = fun context range diagnostic ->
  if List.is_empty context.Lsp.Text_document_methods.Code_action.diagnostics then
    ranges_overlap diagnostic.Lsp.Diagnostic.range range
  else
    List.exists (same_lsp_diagnostic diagnostic) context.diagnostics

let document_range = fun text ->
  Lsp.Utf16.range_of_offsets text ~start_offset:0 ~end_offset:(String.length text)

let workspace_edit_of_text = fun document text ->
  {
    Lsp.Workspace_edit.changes = [
      (document.uri, [ { Lsp.Text_edit.range = document_range document.text; new_text = text } ])
    ]
  }

let maybe_format_text = fun document text ->
  let parse_result = Syn.parse ~filename:(filename_of_uri document.uri) text in
  if not (List.is_empty parse_result.diagnostics) then
    text
  else
    match Krasny.format parse_result with
    | Ok formatted -> formatted
    | Error _ -> text

let finalized_workspace_edit_of_text = fun document text ->
  let text = maybe_format_text document text in
  workspace_edit_of_text document text

let fixable_lint_diagnostics = fun document result ->
  result.Riot_fix.Source_runner.diagnostics |> List.filter_map
    (fun diagnostic ->
      match Riot_fix.Diagnostic.fix diagnostic with
      | None -> None
      | Some fix -> Some {
        diagnostic;
        lsp_diagnostic = lint_diagnostic_to_lsp document.text diagnostic;
        fix
      })

let quickfix_action_of_entry = fun document entry ->
  match Riot_fix.Fix.apply_fix ~source:document.text entry.fix with
  | Error _ -> None
  | Ok text ->
      Some (
        Lsp.Code_action_or_command.Action {
          Lsp.Code_action.title = Riot_fix.Fix.title entry.fix;
          kind = Some Lsp.Action_kind.Quick_fix;
          diagnostics = Some [ entry.lsp_diagnostic ];
          is_preferred = Some true;
          edit = Some (finalized_workspace_edit_of_text document text);
          command = None;
          data = None;
        }
      )

let fix_all_action = fun document entries ->
  match Riot_fix.Fix.apply_fixes ~source:document.text (List.map (fun entry -> entry.fix) entries) with
  | Error _ -> None
  | Ok text ->
      Some (
        Lsp.Code_action_or_command.Action {
          Lsp.Code_action.title = "Fix all auto-fixable Riot diagnostics";
          kind = Some Lsp.Action_kind.Source_fix_all;
          diagnostics = Some (List.map (fun entry -> entry.lsp_diagnostic) entries);
          is_preferred = None;
          edit = Some (finalized_workspace_edit_of_text document text);
          command = None;
          data = None;
        }
      )

let typ_diagnostics = fun state ->
  fun document ->
    match typ_analysis_for_document state document with
    | None -> []
    | Some analysis ->
        (analysis.Typ.SourceAnalysis.lowering_diagnostics
        @ analysis.Typ.SourceAnalysis.typing_diagnostics)
        |> List.map (typ_diagnostic_to_lsp document.text)

let publish_diagnostics = fun state ->
  fun document ->
    let result = analyze_document document in
    let diagnostics = List.map (diagnostic_to_lsp document.text) result.parse_diagnostics
    @ List.map (lint_diagnostic_to_lsp document.text) result.diagnostics
    @ typ_diagnostics state document in
    let params: Lsp.Text_document_methods.Publish_diagnostics.params = {
      uri = document.uri;
      version = Some document.version;
      diagnostics
    } in
    Lsp.notification_to_json Lsp.Text_document_methods.Publish_diagnostics.notification params

let clear_diagnostics = fun uri ->
  let params: Lsp.Text_document_methods.Publish_diagnostics.params = {
    uri;
    version = None;
    diagnostics = []
  } in
  Lsp.notification_to_json Lsp.Text_document_methods.Publish_diagnostics.notification params

let splice_text = fun text ->
  fun range ->
    fun replacement ->
      let* start_offset = Lsp.Utf16.offset_of_position text range.Lsp.Range.start_ in
      let* end_offset = Lsp.Utf16.offset_of_position text range.end_ in
      if start_offset > end_offset then
        Error "invalid text edit range"
      else
        let prefix = String.sub text 0 start_offset in
        let suffix = String.sub text end_offset (String.length text - end_offset) in
        Ok (prefix ^ replacement ^ suffix)

let apply_change = fun text ->
  fun (change: Lsp.Text_document.content_change_event) ->
    match change.range with
    | None -> Ok change.text
    | Some range -> splice_text text range change.text

let apply_changes = fun text ->
  fun changes ->
    List.fold_left
      (fun acc change ->
        let* current = acc in
        apply_change current change)
      (Ok text)
      changes

let capabilities = {
  Lsp.Initialize.Server_capabilities.position_encoding = Some "utf-16";
  text_document_sync = Some (Lsp.Initialize.Server_capabilities.Sync_options {
    open_close = Some true;
    change = Some Lsp.Text_document.Sync_kind.Full;
    save = None
  });
  document_formatting_provider = Some true;
  code_action_provider = Some (Lsp.Initialize.Server_capabilities.Provider_options {
    code_action_kinds = Some [ Lsp.Action_kind.Quick_fix; Source_fix_all ];
    resolve_provider = Some false
  });
  experimental = None;
}

let initialize_result: Lsp.Initialize.result = {
  capabilities;
  server_info = Some { Lsp.Server_info.name = "riot-lsp"; version = None }
}

let debug_json = fun state ->
  let documents =
    state.documents
    |> List.sort
      (fun left right ->
        String.compare (Lsp.Uri.to_string left.uri) (Lsp.Uri.to_string right.uri))
    |> List.map
      (fun document ->
        Json.obj [ ("uri", Lsp.Uri.to_json document.uri); ("version", Json.int document.version); ])
  in
  Json.obj
    [
      ("initialized", Json.bool state.initialized);
      ("shutdownRequested", Json.bool state.shutdown_requested);
      ("documents", Json.array documents);
    ]

let outcome_to_json = fun outcome ->
  Json.obj
    [ ("outbound", Json.array outcome.outbound); (
        "exitCode",
        match outcome.exit_code with
        | None -> Json.Null
        | Some code -> Json.int code
      ); ("state", debug_json outcome.state); ]

let handle_initialize = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Initialize.request payload with
    | Error reason -> ok
      state
      [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, _params) ->
        if state.initialized then
          ok
            state
            [
              response_error
                ~id
                ~code:Lsp.Error_code.invalid_request
                ~message:"initialize was already called"
                ()
            ]
        else
          let state = { state with initialized = true; shutdown_requested = false } in
          ok state [ Lsp.response_to_json ~id Lsp.Initialize.request initialize_result ]

let handle_shutdown = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Shutdown.request payload with
    | Error reason -> ok
      state
      [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, ()) ->
        let state = { state with shutdown_requested = true } in
        ok state [ Lsp.response_to_json ~id Lsp.Shutdown.request () ]

let handle_formatting = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Formatting.request payload with
    | Error reason -> ok
      state
      [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None -> ok
          state
          [
            response_error
              ~id
              ~code:Lsp.Error_code.invalid_params
              ~message:"formatting requested for a document that is not open"
              ();
          ]
        | Some document ->
            let parse_result = Syn.parse ~filename:(filename_of_uri document.uri) document.text in
            if not (List.is_empty parse_result.diagnostics) then
              ok state [ Lsp.response_to_json ~id Lsp.Text_document_methods.Formatting.request None ]
            else
              match Krasny.format parse_result with
              | Ok formatted ->
                  let result =
                    if String.equal formatted document.text then
                      Some []
                    else
                      Some [
                        { Lsp.Text_edit.range = document_range document.text; new_text = formatted }
                      ]
                  in
                  ok
                    state
                    [ Lsp.response_to_json ~id Lsp.Text_document_methods.Formatting.request result ]
              | Error error -> ok
                state
                [
                  response_error
                    ~id
                    ~code:Lsp.Error_code.internal_error
                    ~message:(Krasny.format_error_to_string error)
                    ();
                ]
      )

let handle_code_action = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Code_action.request payload with
    | Error reason -> ok
      state
      [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None -> ok
          state
          [
            response_error
              ~id
              ~code:Lsp.Error_code.invalid_params
              ~message:"code actions requested for a document that is not open"
              ();
          ]
        | Some document ->
            let analysis = analyze_document document in
            let fixable = fixable_lint_diagnostics document analysis in
            let actions = [] in
            let actions =
              if action_kind_allowed params.context.only Lsp.Action_kind.Quick_fix then
                actions
                @ (fixable
                |> List.filter
                  (fun entry -> lint_diagnostic_requested params.context params.range entry.lsp_diagnostic)
                |> List.filter_map (quickfix_action_of_entry document))
              else
                actions
            in
            let actions =
              if action_kind_allowed params.context.only Lsp.Action_kind.Source_fix_all then
                match fixable with
                | [] -> actions
                | _ -> (
                    match fix_all_action document fixable with
                    | Some action -> actions @ [ action ]
                    | None -> actions
                  )
              else
                actions
            in
            let result =
              match actions with
              | [] -> None
              | _ -> Some actions
            in
            ok
              state
              [ Lsp.response_to_json ~id Lsp.Text_document_methods.Code_action.request result ]
      )

let handle_request = fun state ->
  fun request ->
    fun payload ->
      if (not state.initialized) && not (String.equal request.Jsonrpc.method_ "initialize") then
        let id = Option.unwrap_or request.Jsonrpc.id ~default:Jsonrpc.Null in
        ok
          state
          [
            response_error
              ~id
              ~code:Lsp.Error_code.server_not_initialized
              ~message:"server not initialized"
              ()
          ]
      else
        match request.Jsonrpc.method_ with
        | "initialize" ->
            handle_initialize state payload
        | "shutdown" ->
            handle_shutdown state payload
        | "textDocument/formatting" ->
            handle_formatting state payload
        | "textDocument/codeAction" ->
            handle_code_action state payload
        | method_ ->
            let id = Option.unwrap_or request.Jsonrpc.id ~default:Jsonrpc.Null in
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.method_not_found
                  ~message:(("unknown method `" ^ method_ ^ "`"))
                  ()
              ]

let handle_did_open = fun state ->
  fun payload ->
    match Lsp.notification_of_json Lsp.Text_document_methods.Did_open.notification payload with
    | Error _reason -> ok state []
    | Ok params -> (
        match find_document state params.text_document.uri with
        | Some existing ->
            let document = {
              uri = params.text_document.uri;
              version = params.text_document.version;
              text = params.text_document.text;
              path = existing.path
            } in
            let state = upsert_document state document in
            ok state [ publish_diagnostics state document ]
        | None ->
            let document = {
              uri = params.text_document.uri;
              version = params.text_document.version;
              text = params.text_document.text;
              path =
                match Lsp.Uri.to_path params.text_document.uri with
                | Ok path -> Some path
                | Error _ -> None;
            }
            in
            let state = upsert_document state document in
            ok state [ publish_diagnostics state document ]
      )

let handle_did_change = fun state ->
  fun payload ->
    match Lsp.notification_of_json Lsp.Text_document_methods.Did_change.notification payload with
    | Error _reason -> ok state []
    | Ok params -> (
        match find_document state params.text_document.uri with
        | None -> ok state []
        | Some document -> (
            match apply_changes document.text params.content_changes with
            | Error _ -> ok state []
            | Ok text ->
                let document = {
                  uri = document.uri;
                  version = params.text_document.version;
                  text;
                  path = document.path
                } in
                let state = upsert_document state document in
                ok state [ publish_diagnostics state document ]
          )
      )

let handle_did_close = fun state ->
  fun payload ->
    match Lsp.notification_of_json Lsp.Text_document_methods.Did_close.notification payload with
    | Error _reason -> ok state []
    | Ok params -> (
        match find_document state params.text_document.uri with
        | None ->
            let state = remove_document state params.text_document.uri in
            ok state [ clear_diagnostics params.text_document.uri ]
        | Some _document ->
            let state = remove_document state params.text_document.uri in
            ok state [ clear_diagnostics params.text_document.uri ]
      )

let handle_notification = fun state ->
  fun request ->
    fun payload ->
      if not state.initialized then
        match request.Jsonrpc.method_ with
        | "exit" -> ok state ~exit_code:1 []
        | _ -> ok state []
      else
        match request.Jsonrpc.method_ with
        | "initialized" ->
            ok state []
        | "textDocument/didOpen" ->
            handle_did_open state payload
        | "textDocument/didChange" ->
            handle_did_change state payload
        | "textDocument/didClose" ->
            handle_did_close state payload
        | "exit" ->
            let exit_code =
              if state.shutdown_requested then
                0
              else
                1
            in
            ok state ~exit_code []
        | _ ->
            ok state []

let handle_payload = fun state ->
  fun payload ->
    match Json.of_string payload with
    | Error error -> ok
      state
      [
        response_error
          ~id:Jsonrpc.Null
          ~code:Lsp.Error_code.parse_error
          ~message:(Json.error_to_string error)
          ();
      ]
    | Ok json -> (
        match Jsonrpc.request_of_json json with
        | Error reason -> ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_request ~message:reason () ]
        | Ok request -> (
            match request.Jsonrpc.id with
            | Some _ -> handle_request state request json
            | None -> handle_notification state request json
          )
      )
