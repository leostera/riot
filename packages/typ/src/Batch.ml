open Std

let check_source = fun ~filename source ->
  let config = TypConfig.default in
  let session = Session.empty ~config in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Path filename)
    ~text:source in
  let source = Source.make
    ~source_id
    ~kind:Source.File
    ~origin:(Source.Path filename)
    ~revision:0
    ~text:source in
  let fallback_analysis = SourceAnalysis.analyze ~config source in
  let analysis =
    match Session.prepare_snapshot session ~roots:[ source_id ] with
    | Ok snapshot -> (
        match Query.analysis_of_source snapshot source_id with
        | Some analysis -> analysis
        | None -> fallback_analysis
      )
    | Error _ -> fallback_analysis
  in
  let (item_tree, body_arena, origin_map) =
    match analysis.semantic_tree with
    | Some semantic_tree -> (
      Some semantic_tree.item_tree,
      Some semantic_tree.body_arena,
      Some semantic_tree.origin_map
    )
    | None -> (None, None, None)
  in
  {
    Check_result.source_id;
    filename;
    source = analysis.source.text;
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
