open Std

type t = {
  filename: Path.t;
  source: string;
  parsed: Syn.Parser.parse_result;
}

let create = fun ~filename source -> {
  filename;
  source;
  parsed = Syn.parse ~filename source;
}

let filename = fun env -> env.filename

let source = fun env -> env.source

let parsed = fun env -> env.parsed

let source_of_span = fun env (span: Syn.Ceibo.Span.t) ->
  let width = span.end_ - span.start in
  if width <= 0 then
    ""
  else
    String.sub env.source span.start width

let token_body_span = fun first_token last_token ->
  let first_span = Syn.Ceibo.Red.SyntaxToken.span first_token in
  let last_span = Syn.Ceibo.Red.SyntaxToken.span last_token in
  Syn.Ceibo.Span.make ~start:first_span.start ~end_:last_span.end_

let span_of_node = fun (node: (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node) ->
  match Syn.Ceibo.Red.SyntaxNode.first_token node, Syn.Ceibo.Red.SyntaxNode.last_token node with
  | Some first_token, Some last_token -> Some (token_body_span first_token last_token)
  | _ -> None

let span_of_node_with_leading_trivia = fun (node: (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node) ->
  match Syn.Ceibo.Red.SyntaxNode.first_token node, Syn.Ceibo.Red.SyntaxNode.last_token node with
  | Some first_token, Some last_token ->
      let start =
        match Syn.Ceibo.Red.SyntaxToken.leading_trivia first_token with
        | first_trivia :: _ -> (Syn.Ceibo.Red.SyntaxTrivia.span first_trivia).start
        | [] -> (Syn.Ceibo.Red.SyntaxToken.span first_token).start
      in
      let end_ = (Syn.Ceibo.Red.SyntaxToken.span last_token).end_ in
      Some (Syn.Ceibo.Span.make ~start ~end_)
  | _ -> None

let source_of_node = fun env node ->
  match span_of_node node with
  | Some span -> source_of_span env span
  | None -> ""

let source_of_node_with_leading_trivia = fun env node ->
  match span_of_node_with_leading_trivia node with
  | Some span -> source_of_span env span
  | None -> ""
