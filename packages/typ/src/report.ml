open Std
open Std.Data

let env_to_json = fun env ->
  Json.Array (
    env
    |> List.map (fun (name, scheme) ->
      Json.Object [
        ("name", Json.String name);
        ("scheme", Json.String (TypePrinter.scheme_to_string scheme));
      ])
  )

let item_trace_to_json = fun (trace: Check_result.item_trace) ->
  Json.Object [
    ("item_id", Json.Int (ItemId.to_int trace.item_id));
    ("binding_names", Json.Array (List.map (fun name -> Json.String name) trace.binding_names));
    ("exports_after", env_to_json trace.exports_after);
  ]

let expr_trace_to_json = fun origin_map (trace: Check_result.expr_trace) ->
  let origin_json =
    match OriginMap.find origin_map trace.origin_id with
    | Some origin ->
        Json.Object [
          ("label", Json.String origin.label);
          ("syntax_kind", Json.String (Syn.SyntaxKind.to_string origin.syntax_kind));
          ("span", Json.Object [
            ("start", Json.Int origin.span.start);
            ("end", Json.Int origin.span.end_);
          ]);
        ]
    | None ->
        Json.Null
  in
  Json.Object [
    ("expr_id", Json.Int (ExprId.to_int trace.expr_id));
    ("origin_id", Json.Int (OriginId.to_int trace.origin_id));
    ("origin", origin_json);
    ("env_before", env_to_json trace.env_before);
    ("inferred_type", Json.String (TypePrinter.type_to_string trace.inferred_type));
  ]

let option_json = fun render value ->
  match value with
  | Some value -> render value
  | None -> Json.Null

let to_json = fun (report: Check_result.t) ->
  let expr_traces_json =
    match report.origin_map with
    | None -> Json.Null
    | Some origin_map ->
        Json.Array (List.map (expr_trace_to_json origin_map) report.expr_traces)
  in
  Json.Object [
    ("source_id", Json.Int (SourceId.to_int report.source_id));
    ("filename", Json.String (Path.to_string report.filename));
    ("parse_diagnostics", Json.Array (List.map Syn.Diagnostic.to_json report.parse_diagnostics));
    ("lowering_diagnostics", Json.Array (List.map Diagnostic.to_json report.lowering_diagnostics));
    ("typing_diagnostics", Json.Array (List.map Diagnostic.to_json report.typing_diagnostics));
    ("origin_map", option_json OriginMap.to_json report.origin_map);
    ("item_tree", option_json ItemTree.to_json report.item_tree);
    ("body_arena", option_json BodyArena.to_json report.body_arena);
    ("file_summary", FileSummary.to_json report.file_summary);
    ("type_index", TypeIndex.to_json report.type_index);
    ("exports", env_to_json report.exports);
    ("item_traces", Json.Array (List.map item_trace_to_json report.item_traces));
    ("expr_traces", expr_traces_json);
  ]

let render_report = fun (report: Check_result.t) ->
  Json.to_string_pretty (to_json report)
