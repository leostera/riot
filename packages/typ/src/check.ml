open Std
module Typ_diagnostic = Diagnostic
open Syn

let check_source = fun ~filename source ->
  let parsed = Syn.parse ~filename source in
  match Syn.build_cst parsed with
  | Ok cst ->
      let semantic_tree = Lower.lower_source_file cst in
      let inferred = Infer.infer_file semantic_tree in
      {
        Check_result.filename;
        source;
        parse_diagnostics = parsed.Parser.diagnostics;
        semantic_tree = Some semantic_tree;
        lowering_diagnostics = semantic_tree.diagnostics;
        typing_diagnostics = inferred.diagnostics;
        exports = inferred.exports;
        item_traces = inferred.item_traces;
        expr_traces = inferred.expr_traces;
      }
  | Error (Syn.Parse_diagnostics diagnostics) ->
      {
        Check_result.filename;
        source;
        parse_diagnostics = diagnostics;
        semantic_tree = None;
        lowering_diagnostics = [];
        typing_diagnostics = [];
        exports = [];
        item_traces = [];
        expr_traces = [];
      }
  | Error (Syn.Cst_builder_error error) ->
      {
        Check_result.filename;
        source;
        parse_diagnostics = parsed.Parser.diagnostics;
        semantic_tree = None;
        lowering_diagnostics = [
          Typ_diagnostic.CstBuilderError { builder_error = error };
        ];
        typing_diagnostics = [];
        exports = [];
        item_traces = [];
        expr_traces = [];
      }
