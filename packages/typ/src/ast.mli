open Std

type origin = {
  span: Syn.Ceibo.Span.t;
  kind: Syn.SyntaxKind.t;
}
type file_kind =
[
  `Implementation
  | `Interface
]
type path = Model.Surface_path.t
type literal =
  | Int
  | Float
  | Char
  | String
  | Bool
  | Unit
  | Unknown
type core_type = {
  origin: origin;
  kind: core_type_kind;
}

and core_type_kind =
  | Wildcard
  | Var of string option
  | Path of path
  | Apply of { argument: core_type; constructor: core_type }
  | Arrow of { left: core_type; right: core_type }
  | Tuple of core_type list
  | Labeled of core_type
  | Poly of { parameters: string list; body: core_type }
  | Parenthesized of core_type
type type_parameter = string option
type type_constructor = {
  origin: origin;
  name: string;
  payload: core_type option;
}
type record_field_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
}
type type_definition = {
  origin: origin;
  kind: type_definition_kind;
}

and type_definition_kind =
  | Abstract
  | Alias of core_type
  | Variant of type_constructor list
  | Record of record_field_declaration list
type type_declaration = {
  origin: origin;
  name: string;
  parameters: type_parameter list;
  definition: type_definition;
}
type parameter = {
  origin: origin;
  kind: parameter_kind;
}

and parameter_kind =
  | Labeled of { label: string; pattern: pattern option }
  | Optional of { label: string; pattern: pattern option }
  | OptionalDefault of { label: string; pattern: pattern option; default: expression }

and pattern = {
  origin: origin;
  kind: pattern_kind;
}

and pattern_kind =
  | Wildcard
  | Path of path
  | Apply of { callee: pattern; argument: pattern }
  | Literal of literal
  | Tuple of pattern list
  | List of pattern list
  | Cons of { head: pattern; tail: pattern }
  | Constraint of { pattern: pattern; annotation: core_type }
  | Alias of { pattern: pattern; alias: pattern }
  | Attribute of pattern
  | Parenthesized of pattern
  | LabeledParameter of parameter
  | OptionalParameter of parameter
  | OptionalParameterDefault of parameter

and let_binding = {
  origin: origin;
  pattern: pattern;
  parameters: pattern list;
  body: expression;
  type_annotation: core_type option;
}

and expression = {
  origin: origin;
  type_hint: core_type option;
  kind: expression_kind;
}

and expression_kind =
  | Literal of literal
  | Path of path
  | Tuple of expression list
  | List of expression list
  | Record of record_expression_field list
  | FieldAccess of { receiver: expression; field: path }
  | Sequence of { left: expression; right: expression }
  | If of { condition: expression; then_branch: expression; else_branch: expression option }
  | Match of { scrutinee: expression; cases: match_case list }
  | Function of { parameters: pattern list; body: function_body }
  | Apply of { callee: expression; arguments: argument list }
  | Infix of { left: expression; operator: path; right: expression }
  | Let of { first_binding: let_binding; body: expression }
  | Assert of expression

and function_body =
  | Body of expression
  | Cases of match_case list

and match_case = {
  origin: origin;
  pattern: pattern;
  guard: expression option;
  body: expression;
}

and record_expression_field = {
  origin: origin;
  name: path;
  value: expression;
}

and argument = {
  origin: origin;
  kind: argument_kind;
}

and argument_kind =
  | Positional of expression
  | Labeled of { label: string; value: expression option }
  | Optional of { label: string; value: expression option }
type let_declaration = {
  origin: origin;
  recursive: bool;
  bindings: let_binding list;
}
type value_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
}
type external_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
}
type structure_item = {
  origin: origin;
  kind: structure_item_kind;
}

and structure_item_kind =
  | Let of let_declaration
  | Type of type_declaration list
  | Expression of expression
  | External of external_declaration
type signature_item = {
  origin: origin;
  kind: signature_item_kind;
}

and signature_item_kind =
  | Value of value_declaration
  | Type of type_declaration list
  | External of external_declaration
type t = {
  origin: origin;
  kind: source_file_kind;
}

and source_file_kind =
  | Implementation of structure_item list
  | Interface of signature_item list
  | Empty of file_kind
val core_type_origin: core_type -> origin

val parameter_origin: parameter -> origin

val pattern_origin: pattern -> origin

val expression_origin: expression -> origin

val structure_item_origin: structure_item -> origin

val signature_item_origin: signature_item -> origin

val from_parse_result:
  source:Model.Source.t -> Syn.Parser.parse_result -> (t, Diagnostics.Diagnostic.t list) Result.t

val serializer: t Serde.Ser.t
