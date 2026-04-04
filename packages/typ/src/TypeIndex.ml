open Std

type entry = {
  expr_id: ExprId.t;
  origin_id: OriginId.t;
  span: Syn.Ceibo.Span.t;
  inferred_type: TypeRepr.t;
}

type t = entry list

type traced_expr = {
  expr_id: ExprId.t;
  origin_id: OriginId.t;
  inferred_type: TypeRepr.t;
}

let empty = []

let span_size = fun (span: Syn.Ceibo.Span.t) ->
  span.end_ - span.start

let sort_entries = fun entries ->
  entries
  |> List.sort (fun (left: entry) (right: entry) ->
    Int.compare (span_size left.span) (span_size right.span))

let of_traced_exprs = fun ~origin_map traced_exprs ->
  traced_exprs
  |> List.filter_map (fun (trace: traced_expr) ->
    match OriginMap.find origin_map trace.origin_id with
    | Some origin ->
        Some {
          expr_id = trace.expr_id;
          origin_id = trace.origin_id;
          span = origin.span;
          inferred_type = trace.inferred_type;
        }
    | None ->
        None)
  |> sort_entries

let entries = fun index ->
  index

let find_at = fun index position ->
  List.find_opt
    (fun (entry: entry) -> Position.is_within_span position entry.span)
    index

let span_to_json = fun (span: Syn.Ceibo.Span.t) ->
  Data.Json.Object [
    ("start", Data.Json.Int span.start);
    ("end", Data.Json.Int span.end_);
  ]

let entry_to_json = fun (entry: entry) ->
  Data.Json.Object [
    ("expr_id", Data.Json.Int (ExprId.to_int entry.expr_id));
    ("origin_id", Data.Json.Int (OriginId.to_int entry.origin_id));
    ("span", span_to_json entry.span);
    ("inferred_type", Data.Json.String (TypePrinter.type_to_string entry.inferred_type));
  ]

let to_json = fun index ->
  Data.Json.Array (List.map entry_to_json index)
