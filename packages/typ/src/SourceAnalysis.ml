open Std
module Typ_diagnostic = Diagnostic
open Syn

type t = {
  source: Source.t;
  parse_diagnostics: Syn.Diagnostic.t list;
  cst: Syn.Cst.source_file option;
  semantic_tree: SemanticTree.file option;
  lowering_diagnostics: Typ_diagnostic.t list;
  typing_diagnostics: Typ_diagnostic.t list;
  file_summary: FileSummary.t;
  type_index: TypeIndex.t;
  item_traces: Check_result.item_trace list;
  expr_traces: Check_result.expr_trace list;
}

let exports = fun analysis -> FileSummary.exports analysis.file_summary

let analyze = fun ~config (source: Source.t) ->
  let filename =
    match source.origin with
    | Source.Path path -> path
    | Source.Label label -> Path.of_string label |> Result.unwrap_or ~default:(Path.v "<fragment>")
  in
  let parsed = Syn.parse ~filename source.text in
  match Syn.build_cst parsed with
  | Ok cst ->
      let semantic_tree = Lower.lower_source_file ~source cst in
      let inferred = Infer.infer_file ~config semantic_tree in
      let traced_exprs = inferred.expr_traces
      |> List.map
        (fun (trace: Check_result.expr_trace) ->
          {
            TypeIndex.expr_id = trace.expr_id;
            origin_id = trace.origin_id;
            inferred_type = trace.inferred_type
          }) in
      let type_index = TypeIndex.of_traced_exprs ~origin_map:semantic_tree.origin_map traced_exprs in
      let diagnostics = semantic_tree.diagnostics @ inferred.diagnostics in
      let file_summary =
        if diagnostics = [] then
          FileSummary.trusted ~source_id:source.source_id ~type_decls:inferred.type_decls inferred.exports
        else
          FileSummary.errored ~source_id:source.source_id ~type_decls:inferred.type_decls inferred.exports
      in
      {
        source;
        parse_diagnostics = parsed.Parser.diagnostics;
        cst = Some cst;
        semantic_tree = Some semantic_tree;
        lowering_diagnostics = semantic_tree.diagnostics;
        typing_diagnostics = inferred.diagnostics;
        file_summary;
        type_index;
        item_traces = inferred.item_traces;
        expr_traces = inferred.expr_traces;
      }
  | Error (Syn.Parse_diagnostics diagnostics) ->
      {
        source;
        parse_diagnostics = diagnostics;
        cst = None;
        semantic_tree = None;
        lowering_diagnostics = [];
        typing_diagnostics = [];
        file_summary = FileSummary.missing ~source_id:source.source_id ();
        type_index = TypeIndex.empty;
        item_traces = [];
        expr_traces = [];
      }
  | Error (Syn.Cst_builder_error error) ->
      {
        source;
        parse_diagnostics = parsed.Parser.diagnostics;
        cst = None;
        semantic_tree = None;
        lowering_diagnostics = [ Typ_diagnostic.CstBuilderError { builder_error = error }; ];
        typing_diagnostics = [];
        file_summary = FileSummary.missing ~source_id:source.source_id ();
        type_index = TypeIndex.empty;
        item_traces = [];
        expr_traces = [];
      }
