open Std

module Error: sig
  type t = {
    message: string;
    span: Syn.Ceibo.Span.t option;
  }

  val make: ?span:Syn.Ceibo.Span.t -> string -> t

  val message: t -> string
end

module Environment: sig
  type t

  val create: filename:Path.t -> string -> t

  val filename: t -> Path.t

  val source: t -> string

  val parsed: t -> Syn.Parser.parse_result

  val source_of_span: t -> Syn.Ceibo.Span.t -> string

  val span_of_node: (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> Syn.Ceibo.Span.t option

  val span_of_node_with_leading_trivia:
    (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> Syn.Ceibo.Span.t option

  val source_of_node: t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> string

  val source_of_node_with_leading_trivia:
    t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node -> string
end

module Parser: sig
  type invocation = {
    name: string;
    span: Syn.Ceibo.Span.t;
    body: (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node;
  }

  val collect_invocations: Environment.t -> (invocation list, Error.t) result

  val unwrap_grouping:
    (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node ->
    (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

  val body_arguments: invocation -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node list
end

module Format: sig
  val expand: env:Environment.t -> Parser.invocation -> (string, Error.t) result
end

module Expander: sig
  type expansion = {
    source: string;
    changed: bool;
  }

  val expand_source: filename:Path.t -> string -> (expansion, Error.t) result
end

type error = Error.t = {
  message: string;
  span: Syn.Ceibo.Span.t option;
}

type expansion = Expander.expansion = {
  source: string;
  changed: bool;
}

val error_message: error -> string

val expand_source: filename: Path.t -> string -> (expansion, error) result
