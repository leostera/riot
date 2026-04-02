open Std

type node = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

type t = {
  source: string;
  backing_source: string;
  span: Syn.Ceibo.Span.t option;
  parsed_expr: node option;
}

let make = fun ?span source -> {
  source;
  backing_source = source;
  span;
  parsed_expr = None;
}

let of_expr_node = fun ~env node ->
  {
    source = Macro_environment.source_of_node env node;
    backing_source = Macro_environment.source env;
    span = Macro_environment.span_of_node node;
    parsed_expr = Some node;
  }

let source = fun stream -> stream.source

let span = fun stream -> stream.span

let parsed_expr = fun stream -> stream.parsed_expr

let source_of_span = fun stream (span: Syn.Ceibo.Span.t) ->
  let width = span.end_ - span.start in
  if width <= 0 then
    ""
  else
    String.sub stream.backing_source span.start width

let span_of_node = fun (node: node) ->
  match Syn.Ceibo.Red.SyntaxNode.first_token node, Syn.Ceibo.Red.SyntaxNode.last_token node with
  | Some first_token, Some last_token ->
      let first_span = Syn.Ceibo.Red.SyntaxToken.span first_token in
      let last_span = Syn.Ceibo.Red.SyntaxToken.span last_token in
      Some (Syn.Ceibo.Span.make ~start:first_span.start ~end_:last_span.end_)
  | _ -> None

let source_of_node = fun stream node ->
  match span_of_node node with
  | Some span -> source_of_span stream span
  | None -> ""
