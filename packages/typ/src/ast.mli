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
  view: core_type_view;
}

and core_type_view =
  | TypeWildcard
  | TypeVar of string option
  | TypePath of path
  | TypeApply of { argument: core_type option; constructor: core_type option }
  | TypeArrow of { left: core_type option; right: core_type option }
  | TypeTuple of core_type list
  | TypeLabeled of { annotation: core_type option }
  | TypePoly of { body: core_type option }
  | TypeUnsupported of string
  | TypeError of string
type parameter = {
  origin: origin;
  view: parameter_view;
}

and parameter_view =
  | Labeled of { label: string option; pattern: pattern option }
  | Optional of { label: string option; pattern: pattern option }
  | OptionalDefault of { label: string option; pattern: pattern option; default: expr option }
  | UnknownParameter of string

and pattern = {
  origin: origin;
  view: pattern_view;
}

and pattern_view =
  | PatternWildcard
  | PatternPath of path
  | PatternApply of { callee: pattern option; argument: pattern option }
  | PatternLiteral of literal
  | PatternTuple of pattern list
  | PatternList of pattern list
  | PatternCons of { head: pattern option; tail: pattern option }
  | PatternConstraint of { pattern: pattern option; annotation: core_type option }
  | PatternAlias of { pattern: pattern option; alias: pattern option }
  | PatternAttribute of { inner: pattern option }
  | PatternLabeledParam of parameter
  | PatternOptionalParam of parameter
  | PatternOptionalParamDefault of parameter
  | PatternUnsupported of string
  | PatternError of string

and let_binding = {
  origin: origin;
  pattern: pattern option;
  parameters: pattern list;
  body: expr option;
  type_annotation: core_type option;
}

and expr = {
  origin: origin;
  view: expr_view;
}

and expr_view =
  | ExprLiteral of literal
  | ExprPath of path
  | ExprParenthesized of { inner: expr option }
  | ExprAttribute of { inner: expr option }
  | ExprTyped of { expr: expr option; annotation: core_type option }
  | ExprTuple of expr list
  | ExprList of expr list
  | ExprSequence of { left: expr option; right: expr option }
  | ExprIf of { condition: expr option; then_branch: expr option; else_branch: expr option }
  | ExprApply of { callee: expr option; argument: expr option }
  | ExprInfix of { left: expr option; operator: path option; right: expr option }
  | ExprPrefix of { operator: path option; operand: expr option }
  | ExprLet of { first_binding: let_binding option; body: expr option }
  | ExprAssert of { argument: expr option }
  | ExprLabeledArg of { label: string option; value: expr option }
  | ExprOptionalArg of { label: string option; value: expr option }
  | ExprUnsupported of string
  | ExprError of string
type let_declaration = {
  origin: origin;
  recursive: bool;
  bindings: let_binding list;
}
type value_declaration = {
  origin: origin;
  name: string option;
  type_annotation: core_type option;
}
type external_declaration = {
  origin: origin;
  name: string option;
  type_annotation: core_type option;
}
type structure_item = {
  origin: origin;
  view: structure_item_view;
}

and structure_item_view =
  | StructureLet of let_declaration
  | StructureExpr of expr option
  | StructureExternal of external_declaration
  | StructureUnsupported of string
  | StructureError of string
type signature_item = {
  origin: origin;
  view: signature_item_view;
}

and signature_item_view =
  | SignatureValue of value_declaration
  | SignatureExternal of external_declaration
  | SignatureUnsupported of string
  | SignatureError of string
type view =
  | Implementation of structure_item list
  | Interface of signature_item list
  | Empty
type t = {
  kind: file_kind;
  origin: origin;
  view: view;
}
val from_parse_result: source:Model.Source.t -> Syn.Parser.parse_result -> t

val serializer: t Serde.Ser.t
