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
type type_tuple_separator =
[
  `Star
  | `Comma
  | `Unknown
]
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
  | Tuple of { separator: type_tuple_separator; elements: core_type list }
  | Labeled of core_type
  | Poly of { parameters: string list; body: core_type }
  | PolyVariant of poly_variant_type_field list
  | Package of package_type
  | Parenthesized of core_type

and poly_variant_type_field = {
  origin: origin;
  tag: string;
  payload: core_type option;
}

and package_type = {
  origin: origin;
  binder: string option;
  module_type: path;
  constraints: package_type_constraint list;
}

and package_type_constraint = {
  origin: origin;
  type_name: path;
  manifest: core_type;
}
type type_parameter = string option
type type_constructor = {
  origin: origin;
  name: string;
  payload: core_type option;
  result: core_type option;
  inline_record: record_field_declaration list option;
}

and record_field_declaration = {
  origin: origin;
  name: string;
  mutable_: bool;
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

and record_pattern_field = {
  origin: origin;
  name: path;
  pattern: pattern option;
}

and pattern_kind =
  | Wildcard
  | Path of path
  | Apply of { callee: pattern; argument: pattern }
  | Literal of literal
  | PolyVariant of poly_variant_pattern
  | Tuple of pattern list
  | List of pattern list
  | Record of record_pattern_field list
  | Or of { left: pattern; right: pattern }
  | Cons of { head: pattern; tail: pattern }
  | Constraint of { pattern: pattern; annotation: core_type }
  | Alias of { pattern: pattern; alias: pattern }
  | Attribute of pattern
  | Parenthesized of pattern
  | LabeledParameter of parameter
  | OptionalParameter of parameter
  | OptionalParameterDefault of parameter
  | LocallyAbstractType of string list
  | FirstClassModule of { binder: string option; package_type: package_type option }

and poly_variant_pattern = {
  tag: string;
  payload: pattern option;
}

and let_binding = {
  origin: origin;
  pattern: pattern;
  parameters: pattern list;
  body: expression;
  type_annotation: core_type option;
}

and expression_type_hint_kind =
  | Annotation
  | Coercion

and expression_type_hint = {
  kind: expression_type_hint_kind;
  type_: core_type;
}

and expression = {
  origin: origin;
  type_hint: expression_type_hint option;
  kind: expression_kind;
}

and module_unpack = {
  origin: origin;
  expression: expression;
  package_type: package_type option;
}

and expression_kind =
  | Literal of literal
  | Path of path
  | Tuple of expression list
  | List of expression list
  | Array of expression list
  | PolyVariant of poly_variant_expression
  | Record of record_expression_field list
  | RecordUpdate of { base: expression; fields: record_expression_field list }
  | FieldAccess of { receiver: expression; field: path }
  | ArrayIndex of { receiver: expression; index: expression }
  | Assign of { target: expression; value: expression }
  | Sequence of { left: expression; right: expression }
  | If of { condition: expression; then_branch: expression; else_branch: expression option }
  | Match of { scrutinee: expression; cases: match_case list }
  | While of { condition: expression; body: expression }
  | For of { pattern: pattern; start_: expression; stop: expression; body: expression }
  | Function of { parameters: pattern list; body: function_body }
  | Apply of { callee: expression; arguments: argument list }
  | Infix of { left: expression; operator: path; right: expression }
  | Let of { first_binding: let_binding; body: expression }
  | LetModule of {
      name: string;
      items: structure_item list;
      alias: path option;
      unpack: module_unpack option;
      body: expression
    }
  | LocalOpen of { module_path: path; body: expression }
  | FirstClassModule of { module_path: path; package_type: package_type option }
  | Assert of expression

and poly_variant_expression = {
  tag: string;
  payload: expression option;
}

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

and let_declaration = {
  origin: origin;
  recursive: bool;
  bindings: let_binding list;
}

and value_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
}

and external_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
  primitives: string list;
}

and module_declaration = {
  origin: origin;
  name: string;
  parameters: functor_parameter list;
  items: structure_item list;
  alias: path option;
  module_type: path option;
  application: module_application option;
}

and functor_parameter = {
  origin: origin;
  name: string;
  module_type: path option;
}

and module_application = {
  callee: path;
  argument: path;
}

and module_type_declaration = {
  origin: origin;
  name: string;
  items: signature_item list;
}

and structure_item = {
  origin: origin;
  kind: structure_item_kind;
}

and structure_item_kind =
  | Let of let_declaration
  | Type of type_declaration list
  | Expression of expression
  | External of external_declaration
  | Module of module_declaration list
  | ModuleType of module_type_declaration
  | Include of path

and signature_item = {
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
