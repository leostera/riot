open Std

let check_source = fun ~filename source ->
  let session = Session.empty ~config:TypConfig.default in
  let (session, source_id) =
    Session.create_source
      session
      ~kind:Source.File
      ~origin:(Source.Path filename)
      ~text:source
  in
  let snapshot = Session.snapshot session in
  match Query.analysis_of_source snapshot source_id with
  | None ->
      {
        Check_result.source_id;
        filename;
        source;
        parse_diagnostics = [];
        item_tree = None;
        body_arena = None;
        origin_map = None;
        semantic_tree = None;
        lowering_diagnostics = [];
        typing_diagnostics = [];
        file_summary = FileSummary.missing ~source_id;
        type_index = TypeIndex.empty;
        exports = [];
        item_traces = [];
        expr_traces = [];
      }
  | Some analysis ->
      let (item_tree, body_arena, origin_map) =
        match analysis.semantic_tree with
        | Some semantic_tree ->
            (Some semantic_tree.item_tree, Some semantic_tree.body_arena, Some semantic_tree.origin_map)
        | None ->
            (None, None, None)
      in
      {
        Check_result.source_id;
        filename;
        source;
        parse_diagnostics = analysis.parse_diagnostics;
        item_tree;
        body_arena;
        origin_map;
        semantic_tree = analysis.semantic_tree;
        lowering_diagnostics = analysis.lowering_diagnostics;
        typing_diagnostics = analysis.typing_diagnostics;
        file_summary = analysis.file_summary;
        type_index = analysis.type_index;
        exports = SourceAnalysis.exports analysis;
        item_traces = analysis.item_traces;
        expr_traces = analysis.expr_traces;
      }
