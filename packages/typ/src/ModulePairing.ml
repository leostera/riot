open Std

type t = {
  module_typings: ModuleTypings.t;
  analyses_by_source: (SourceId.t * SourceAnalysis.t) list;
}

type source_kind =
  | Interface
  | Implementation
  | Other

let source_kind = fun (source: Source.t) ->
  match source.origin with
  | Source.Path path -> (
      match Path.extension path with
      | Some ".mli" -> Interface
      | Some ".ml" -> Implementation
      | _ -> Other
    )
  | Source.Label label -> (
      match Path.(Path.v label |> extension) with
      | Some ".mli" -> Interface
      | Some ".ml" -> Implementation
      | _ -> Other
    )

let prefer_source = fun current candidate ->
  match (source_kind current, source_kind candidate) with
  | Interface, Interface
  | Implementation, Implementation
  | Other, Other -> false
  | _, Interface -> true
  | Interface, _ -> false
  | Other, Implementation -> true
  | Implementation, Other -> false

let select_source = fun sources desired_kind ->
  let rec loop selected = function
    | [] -> selected
    | ((source, analysis) as candidate) :: tail ->
        if not (source_kind source = desired_kind) then
          loop selected tail
        else
          (
            match selected with
            | None -> loop (Some candidate) tail
            | Some (existing, _) when prefer_source existing source ->
                loop (Some candidate) tail
            | Some _ -> loop selected tail
          )
  in
  loop None sources

let source_span = fun (source: Source.t) ->
  Syn.Ceibo.Span.make ~start:0 ~end_:(String.length source.text)

let qualified_name = fun scope_path name ->
  match scope_path with
  | [] -> name
  | _ -> String.concat "." (scope_path @ [ name ])

let type_decl_key = fun (type_decl: FileSummary.type_decl) ->
  qualified_name type_decl.scope_path type_decl.declaration.type_name

let is_abstract_type_decl = fun (decl: TypeDecl.t) ->
  List.is_empty decl.constructors
  && List.is_empty decl.labels
  && Option.is_none decl.manifest

let mark_analysis_errored = fun (analysis: SourceAnalysis.t) diagnostics ->
  if List.is_empty diagnostics then
    analysis
  else
    let file_summary =
      match analysis.file_summary.FileSummary.export_result with
      | FileSummary.TrustedExport { exports }
      | FileSummary.ErroredExport { exports } ->
          FileSummary.errored
            ~source_id:analysis.source.source_id
            ~type_decls:(FileSummary.type_decls analysis.file_summary)
            exports
      | FileSummary.NoExport ->
          FileSummary.missing
            ~source_id:analysis.source.source_id
            ~type_decls:(FileSummary.type_decls analysis.file_summary)
            ()
    in
    {
      analysis with
      typing_diagnostics = analysis.typing_diagnostics @ diagnostics;
      file_summary;
    }

let module_typings_of_summary = fun ~module_name ~source_hash summary ->
  match summary.FileSummary.export_result with
  | FileSummary.TrustedExport { exports } ->
      ModuleTypings.trusted ~module_name ~source_hash ~type_decls:summary.type_decls exports
  | FileSummary.ErroredExport { exports } ->
      ModuleTypings.errored ~module_name ~source_hash ~type_decls:summary.type_decls exports
  | FileSummary.NoExport ->
      ModuleTypings.missing ~module_name ~source_hash ~type_decls:summary.type_decls ()

let find_declared_value_span = fun (analysis: SourceAnalysis.t) export_name ->
  match analysis.semantic_tree with
  | None -> None
  | Some semantic_tree ->
      semantic_tree.item_tree
      |> ItemTree.items
      |> List.find_map
        (function
          | ItemTree.DeclaredValue item
            when String.equal export_name (qualified_name item.scope_path item.value_name) ->
                OriginMap.find semantic_tree.origin_map item.origin_id
                |> Option.map (fun (origin: OriginMap.origin) -> origin.span)
          | _ -> None)

let find_type_decl_span = fun (analysis: SourceAnalysis.t) type_name ->
  match analysis.semantic_tree with
  | None -> None
  | Some semantic_tree ->
      semantic_tree.item_tree
      |> ItemTree.items
      |> List.find_map
        (function
          | ItemTree.Type item
            when String.equal type_name (qualified_name item.scope_path item.declaration.type_name) ->
                OriginMap.find semantic_tree.origin_map item.origin_id
                |> Option.map (fun (origin: OriginMap.origin) -> origin.span)
          | _ -> None)

let interface_span_for_mismatch = fun (source, analysis) mismatch ->
  match mismatch with
  | Diagnostic.MissingValue { name }
  | Diagnostic.ValueTypeMismatch { name; _ } ->
      (
        match find_declared_value_span analysis name with
        | Some span -> span
        | None -> source_span source
      )
  | Diagnostic.MissingTypeDeclaration { name }
  | Diagnostic.TypeDeclarationMismatch { name; _ } ->
      (
        match find_type_decl_span analysis name with
        | Some span -> span
        | None -> source_span source
      )

let implementation_span_for_mismatch = fun (source, _analysis) _mismatch ->
  source_span source

let interface_diagnostic = fun interface_pair implementation_pair mismatch ->
  Diagnostic.SignatureInclusionError {
    mismatch_span = interface_span_for_mismatch interface_pair mismatch;
    counterpart_span = Some (implementation_span_for_mismatch implementation_pair mismatch);
    mismatch;
  }

let implementation_diagnostic = fun interface_pair implementation_pair mismatch ->
  Diagnostic.SignatureInclusionError {
    mismatch_span = implementation_span_for_mismatch implementation_pair mismatch;
    counterpart_span = Some (interface_span_for_mismatch interface_pair mismatch);
    mismatch;
  }

let value_mismatches = fun interface_exports implementation_exports ->
  interface_exports
  |> List.filter_map
    (fun (name, expected_scheme) ->
      match List.assoc_opt name implementation_exports with
      | None -> Some (Diagnostic.MissingValue { name })
      | Some actual_scheme ->
          let expected = TypePrinter.scheme_to_string expected_scheme in
          let actual = TypePrinter.scheme_to_string actual_scheme in
          if String.equal expected actual then
            None
          else
            Some (Diagnostic.ValueTypeMismatch { name; expected; actual }))

let type_decl_matches = fun interface_decl implementation_decl ->
  let interface_arity = List.length interface_decl.TypeDecl.param_ids in
  let implementation_arity = List.length implementation_decl.TypeDecl.param_ids in
  if not (interface_arity = implementation_arity) then
    false
  else if is_abstract_type_decl interface_decl then
    true
  else
    String.equal
      (TypeDecl.to_string interface_decl)
      (TypeDecl.to_string implementation_decl)

let type_decl_mismatches = fun interface_decls implementation_decls ->
  interface_decls
  |> List.filter_map
    (fun (interface_decl: FileSummary.type_decl) ->
      let name = type_decl_key interface_decl in
      match implementation_decls
        |> List.find_opt
          (fun (implementation_decl: FileSummary.type_decl) ->
            String.equal name (type_decl_key implementation_decl))
      with
      | None -> Some (Diagnostic.MissingTypeDeclaration { name })
      | Some implementation_decl ->
          if type_decl_matches interface_decl.declaration implementation_decl.declaration then
            None
          else
            Some
              (Diagnostic.TypeDeclarationMismatch {
                name;
                expected = TypeDecl.to_string interface_decl.declaration;
                actual = TypeDecl.to_string implementation_decl.declaration;
              }))

let interface_shaped_export_result = fun interface_summary implementation_summary ->
  let exports = FileSummary.exports interface_summary in
  match (interface_summary.FileSummary.export_result, implementation_summary.FileSummary.export_result) with
  | FileSummary.NoExport, _
  | _, FileSummary.NoExport -> FileSummary.NoExport
  | FileSummary.TrustedExport _, FileSummary.TrustedExport _ -> FileSummary.TrustedExport { exports }
  | _ -> FileSummary.ErroredExport { exports }

let analyses_by_source = fun sources ->
  sources
  |> List.map
    (fun (_source, (analysis: SourceAnalysis.t)) -> (analysis.source.source_id, analysis))

let extra_analyses = fun excluded_ids sources ->
  sources
  |> List.filter_map
    (fun (_source, (analysis: SourceAnalysis.t)) ->
      if List.exists (SourceId.equal analysis.source.source_id) excluded_ids then
        None
      else
        Some (analysis.source.source_id, analysis))

let pair_interface_and_implementation = fun ~module_name interface_pair implementation_pair ->
  let (_interface_source, (interface_analysis: SourceAnalysis.t)) = interface_pair in
  let (_implementation_source, (implementation_analysis: SourceAnalysis.t)) = implementation_pair in
  let mismatches =
    value_mismatches
      (FileSummary.exports interface_analysis.file_summary)
      (FileSummary.exports implementation_analysis.file_summary)
    @ type_decl_mismatches
      (FileSummary.type_decls interface_analysis.file_summary)
      (FileSummary.type_decls implementation_analysis.file_summary)
  in
  if List.is_empty mismatches then
    let export_result =
      interface_shaped_export_result
        interface_analysis.file_summary
        implementation_analysis.file_summary
    in
    let type_decls =
      match export_result with
      | FileSummary.NoExport -> []
      | _ -> FileSummary.type_decls interface_analysis.file_summary
    in
    let source_hash =
      ModuleTypings.synthetic_source_hash ~module_name ~export_result ~type_decls
    in
    let module_typings =
      module_typings_of_summary
        ~module_name
        ~source_hash
        {
          FileSummary.source_id = interface_analysis.source.source_id;
          export_result;
          type_decls;
        }
    in
    {
      module_typings;
      analyses_by_source = [
        (interface_analysis.source.source_id, interface_analysis);
        (implementation_analysis.source.source_id, implementation_analysis);
      ]
      @ extra_analyses
        [ interface_analysis.source.source_id; implementation_analysis.source.source_id ]
        [ interface_pair; implementation_pair ];
    }
  else
    let interface_diagnostics =
      mismatches |> List.map (interface_diagnostic interface_pair implementation_pair)
    in
    let implementation_diagnostics =
      mismatches |> List.map (implementation_diagnostic interface_pair implementation_pair)
    in
    let interface_analysis = mark_analysis_errored interface_analysis interface_diagnostics in
    let implementation_analysis = mark_analysis_errored implementation_analysis implementation_diagnostics in
    let module_typings =
      ModuleTypings.missing
        ~module_name
        ~source_hash:
          (ModuleTypings.synthetic_source_hash
            ~module_name
            ~export_result:FileSummary.NoExport
            ~type_decls:[])
        ()
    in
    {
      module_typings;
      analyses_by_source = [
        (interface_analysis.source.source_id, interface_analysis);
        (implementation_analysis.source.source_id, implementation_analysis);
      ]
      @ extra_analyses
        [ interface_analysis.source.source_id; implementation_analysis.source.source_id ]
        [ interface_pair; implementation_pair ];
    }

let singleton_module_typings = fun ~module_name (source, (analysis: SourceAnalysis.t)) ->
  module_typings_of_summary
    ~module_name
    ~source_hash:(Source.input_hash source)
    analysis.file_summary

let of_sources = fun ~module_name sources ->
  match (select_source sources Interface, select_source sources Implementation) with
  | Some interface_pair, Some implementation_pair ->
      pair_interface_and_implementation ~module_name interface_pair implementation_pair
  | Some interface_pair, None ->
      let (source, analysis) = interface_pair in
      {
        module_typings = singleton_module_typings ~module_name (source, analysis);
        analyses_by_source = analyses_by_source sources;
      }
  | None, Some implementation_pair ->
      let (source, analysis) = implementation_pair in
      {
        module_typings = singleton_module_typings ~module_name (source, analysis);
        analyses_by_source = analyses_by_source sources;
      }
  | None, None ->
      let source_hash =
        ModuleTypings.synthetic_source_hash
          ~module_name
          ~export_result:FileSummary.NoExport
          ~type_decls:[]
      in
      {
        module_typings = ModuleTypings.missing ~module_name ~source_hash ();
        analyses_by_source = analyses_by_source sources;
      }
