open Std

let render_syn_diagnostic = fun diagnostic ->
  Syn.Diagnostic.to_string diagnostic

let render_diagnostics = fun render diagnostics ->
  match diagnostics with
  | [] -> "  none\n"
  | _ ->
      diagnostics
      |> List.map render
      |> List.map (fun line -> "  " ^ String.concat "\n  " (String.split_on_char '\n' line))
      |> String.concat "\n"
      |> fun text -> text ^ "\n"

let render_exports = fun exports ->
  match exports with
  | [] -> "  none\n"
  | _ ->
      exports
      |> List.map (fun (name, scheme) -> "  " ^ name ^ " : " ^ TypePrinter.scheme_to_string scheme)
      |> String.concat "\n"
      |> fun text -> text ^ "\n"

let render_item_trace = fun trace ->
  let names =
    match trace.Check_result.binding_names with
    | [] -> ""
    | names -> String.concat ", " names
  in
  let exports =
    trace.exports_after
    |> List.map (fun (name, scheme) -> "    " ^ name ^ " : " ^ TypePrinter.scheme_to_string scheme)
    |> String.concat "\n"
  in
  "  item#" ^ Int.to_string trace.item_id ^ " names=[" ^ names ^ "]\n" ^ exports

let render_expr_trace = fun semantic_tree trace ->
  let origin_text =
    match SemanticTree.find_origin semantic_tree trace.Check_result.origin_id with
    | Some origin ->
        origin.label ^ " @ " ^ Syn.Ceibo.Span.to_string origin.span
    | None -> "unknown"
  in
  let env_lines =
    match trace.env_before with
    | [] -> "    <empty>"
    | env ->
        env
        |> List.map (fun (name, scheme) -> "    " ^ name ^ " : " ^ TypePrinter.scheme_to_string scheme)
        |> String.concat "\n"
  in
  "  expr#"
  ^ Int.to_string trace.expr_id
  ^ " "
  ^ origin_text
  ^ "\n"
  ^ env_lines
  ^ "\n    => "
  ^ TypePrinter.type_to_string trace.inferred_type

let render_report = fun report ->
  let semantic_tree_section =
    match report.Check_result.semantic_tree with
    | Some semantic_tree ->
        let item_traces =
          match report.item_traces with
          | [] -> "  none\n"
          | traces ->
              traces
              |> List.map render_item_trace
              |> String.concat "\n"
              |> fun text -> text ^ "\n"
        in
        let expr_traces =
          match report.expr_traces with
          | [] -> "  none\n"
          | traces ->
              traces
              |> List.map (render_expr_trace semantic_tree)
              |> String.concat "\n"
              |> fun text -> text ^ "\n"
        in
        String.concat
          ""
          [
            "semantic tree:\n";
            SemanticTree.to_string semantic_tree;
            "typing diagnostics:\n";
            render_diagnostics Diagnostic.to_string report.typing_diagnostics;
            "exports:\n";
            render_exports report.exports;
            "item traces:\n";
            item_traces;
            "expr traces:\n";
            expr_traces;
          ]
    | None ->
        "semantic tree:\n  unavailable\n"
  in
  String.concat
    ""
    [
      "file: ";
      Path.to_string report.filename;
      "\n\nparse diagnostics:\n";
      render_diagnostics render_syn_diagnostic report.parse_diagnostics;
      "lowering diagnostics:\n";
      render_diagnostics Diagnostic.to_string report.lowering_diagnostics;
      semantic_tree_section;
    ]
