(** Riot's typed syntax tree.

    `Typ.Ast` is the semantic tree owned by the type checker. It is built from
    `Syn.Ast`, keeps source origins for diagnostics and editor features, and is
    then annotated in-place as inference/checking discovers types.

    This tree is intentionally not a second concrete syntax tree:

    - It does not preserve comments or formatting trivia.
    - It keeps only syntax that matters to typing and later compiler stages.
    - Nodes either have enough structure to be checked, or AST construction
      fails with diagnostics. *)
open Std

(** Source origin attached to every typed-tree node.

    `span` identifies the source range. `kind` records the syntax kind that
    produced the typed node, which keeps diagnostics useful even after lowering
    from `Syn.Ast`. *)
type origin = {
  (** Source byte span for the node. *)
  span: Syn.Ceibo.Span.t;
  (** Original syntax kind that produced this semantic node. *)
  kind: Syn.SyntaxKind.t;
}
(** Source-level identifier as written by the program.

    This includes both single names, such as `value`, and qualified names, such
    as `Module.value`. It is the key used for surface lookup before a name has
    been resolved to a binding/entity id. *)
type ident = Model.Surface_path.t

(** Opaque identity for solver and generalized type variables.

    Values are allocated monotonically by the inference state, but callers should
    treat them as tokens rather than depending on their representation. *)
module TypeVar: sig
  (** Type-variable identity. *)
  type t

  (** First variable id in a fresh sequence. *)
  val first: t

  (** Return the next variable id in allocation order. *)
  val next: t -> t

  (** Structural equality over variable ids. *)
  val equal: t -> t -> bool

  (** Deterministic ordering for maps, sets, and snapshots. *)
  val compare: t -> t -> Std.Order.t

  (** Debug rendering, currently similar to `'<n>`. *)
  val to_string: t -> string
end

(** Inference type algebra.

    This is the type representation manipulated by the unifier. Source type
    annotations still live as `core_type`; `Type.t` is the solved/inferred
    semantic type attached to expressions, patterns, and core-type nodes. *)
module Type: sig
  (** Function argument labels inside inferred arrow types. *)
  module Label: sig
    (** Arrow argument label. *)
    type t =
      (** Positional argument. *)
      | NoLabel
      (** Required labeled argument, for example `~x:int -> ...`. *)
      | Labelled of string
      (** Optional labeled argument, for example `?x:int -> ...`. *)
      | Optional of string

    (** Structural equality over labels. *)
    val equal: t -> t -> bool
  end

  (** Unification variable.

      `link` is `None` while the variable is unsolved. Once solved, it points to
      the representative type. The unifier may rewrite links during pruning to
      compress chains. *)
  type variable = {
    (** Stable id for this solver variable. *)
    id: TypeVar.t;
    (** Optional solution for this variable. *)
    mutable link: t option;
  }

  (** Function type payload. *)
  and arrow = {
    (** Argument label accepted by this arrow. *)
    label: Label.t;
    (** Parameter type. *)
    parameter: t;
    (** Result type. *)
    result: t;
  }

  (** Nominal type application.

      `ident` names the type constructor, such as `int`, `list`, or `Result.t`.
      `arguments` are the applied type arguments. *)
  and constructor = {
    (** Nominal type identifier. *)
    ident: ident;
    (** Applied type arguments. *)
    arguments: t list;
  }

  (** Semantic type manipulated by inference. *)
  and t =
    (** Mutable solver variable. *)
    | Var of variable
    (** Generalized type variable in a type scheme. *)
    | Generic of TypeVar.t
    (** Tuple type. *)
    | Tuple of t list
    (** Function arrow type. *)
    | Arrow of arrow
    (** Nominal type constructor application. *)
    | Constructor of constructor

  (** True when both values are the same solver variable id. *)
  val same_var: variable -> variable -> bool

  (** Structural equality after following solved variable links. *)
  val equal: t -> t -> bool

  (** Debug/user-facing rendering for inferred types. *)
  val to_string: t -> string
end

(** Literal category.

    The AST stores only the category needed by the current checker slices, not
    the exact literal text. Use the `origin` span when exact source text is
    needed. *)
type literal =
  | Int
  | Float
  | Char
  | String
  | Bool
(** Source type expression plus optional solved semantic type. *)
type core_type = {
  (** Source origin of the type expression. *)
  origin: origin;
  (** Inferred/checked semantic type for this source type, when available. *)
  mutable type_: Type.t option;
  (** Source-level shape of the type expression. *)
  kind: core_type_kind;
}

(** Argument label in source-level arrow type syntax. *)
and arrow_label =
  (** Positional arrow. *)
  | NoLabel
  (** Required labeled arrow. *)
  | Labelled of string
  (** Optional labeled arrow. *)
  | Optional of string

(** Source-level type-expression shape. *)
and core_type_kind =
  (** Wildcard type, written `_`. *)
  | Wildcard
  (** Type variable, written `'a`; `None` means the source omitted the name. *)
  | Var of string option
  (** Named type identifier, such as `int` or `A.t`. *)
  | TypeIdent of ident
  (** Type application, such as `int list` or `(int, string) result`. *)
  | Apply of type_application
  (** Function arrow type. *)
  | Arrow of arrow_type
  (** Tuple type. *)
  | Tuple of core_type list
  (** Explicit universal quantification, such as `'a. 'a -> 'a`. *)
  | ForAll of forall_type
  (** Polymorphic variant row. *)
  | PolyVariant of poly_variant_type_field list
  (** First-class module package type. *)
  | Package of package_type
  (** Parenthesized type expression kept while the AST is still source-shaped. *)
  | Parenthesized of core_type

(** Type application payload. *)
and type_application = {
  (** Applied type constructor. *)
  constructor: core_type;
  (** Type arguments applied to the constructor. *)
  arguments: core_type list;
}

(** Function arrow type payload. *)
and arrow_type = {
  (** Arrow label. *)
  label: arrow_label;
  (** Parameter type. *)
  parameter: core_type;
  (** Result type. *)
  result: core_type;
}

(** Explicit universal quantifier payload. *)
and forall_type = {
  (** Bound type parameter names. *)
  parameters: string list;
  (** Quantified body type. *)
  body: core_type;
}

(** One polymorphic-variant row field in a source type. *)
and poly_variant_type_field = {
  (** Source origin for the row field. *)
  origin: origin;
  (** Tag name without the leading backtick. *)
  tag: string;
  (** Optional payload type carried by the tag. *)
  payload: core_type option;
}

(** First-class module package type. *)
and package_type = {
  (** Source origin for the package type. *)
  origin: origin;
  (** Optional local module binder. *)
  binder: string option;
  (** Module type identifier. *)
  module_type: ident;
  (** `with type` constraints attached to the package. *)
  constraints: package_type_constraint list;
}

(** Type equality constraint inside a first-class module package. *)
and package_type_constraint = {
  (** Source origin for the constraint. *)
  origin: origin;
  (** Type member being constrained. *)
  type_name: ident;
  (** Replacement/manifest type. *)
  manifest: core_type;
}
(** Type declaration parameter.

    `Some name` represents a named parameter such as `'a`; `None` represents a
    parameter whose name could not be recovered. *)
type type_parameter = string option
(** Variant constructor declaration. *)
type type_constructor = {
  (** Source origin for the constructor declaration. *)
  origin: origin;
  (** Constructor name. *)
  name: string;
  (** Optional tuple-style payload. *)
  payload: core_type option;
  (** Optional explicit result type, used by GADT-style constructors. *)
  result: core_type option;
  (** Optional inline record payload. *)
  inline_record: record_field_declaration list option;
}

(** Record field declaration inside a record type or inline record constructor. *)
and record_field_declaration = {
  (** Source origin for the field declaration. *)
  origin: origin;
  (** Field name. *)
  name: string;
  (** Whether the field was declared `mutable`. *)
  mutable_: bool;
  (** Field type annotation. *)
  type_annotation: core_type;
}
(** Type declaration definition. *)
type type_definition = {
  (** Source origin for the definition. *)
  origin: origin;
  (** Definition shape. *)
  kind: type_definition_kind;
}

(** Type declaration body. *)
and type_definition_kind =
  (** Abstract type with no manifest body. *)
  | Abstract
  (** Open/extensible variant type. *)
  | Extensible
  (** Type alias. *)
  | Alias of core_type
  (** Ordinary variant type. *)
  | Variant of type_constructor list
  (** Record type. *)
  | Record of record_field_declaration list
(** Top-level or nested type declaration. *)
type type_declaration = {
  (** Source origin for the declaration. *)
  origin: origin;
  (** Declared type name. *)
  name: string;
  (** Declared type parameters. *)
  parameters: type_parameter list;
  (** Type body. *)
  definition: type_definition;
}
(** Runtime function parameter.

    A parameter always binds or destructures a runtime value. Non-runtime
    binders such as `(type a)` are stored on the enclosing function-shaped node
    as `type_binders`. *)
type parameter = {
  (** Source origin for the parameter. *)
  origin: origin;
  (** Argument label accepted by this parameter. *)
  label: parameter_label;
  (** Value pattern bound by the parameter. *)
  pattern: pattern;
  (** Optional parameter-local type annotation. *)
  annotation: core_type option;
  (** Optional default expression for optional labeled parameters. *)
  default: expression option;
}

(** Runtime function parameter label. *)
and parameter_label =
  (** Positional argument. *)
  | Unlabeled
  (** Required labeled argument. *)
  | Labeled of ident
  (** Optional labeled argument. *)
  | Optional of ident

(** Pattern node plus optional inferred type. *)
and pattern = {
  (** Source origin for the pattern. *)
  origin: origin;
  (** Inferred type of the pattern, once checked. *)
  mutable type_: Type.t option;
  (** Pattern shape. *)
  kind: pattern_kind;
}

(** Record field inside a pattern. *)
and record_pattern_field = {
  (** Source origin for the record pattern field. *)
  origin: origin;
  (** Field identifier. *)
  name: ident;
  (** Optional explicit field pattern; `None` represents punning. *)
  pattern: pattern option;
}

(** Pattern shape. *)
and pattern_kind =
  (** Wildcard pattern, written `_`. *)
  | Wildcard
  (** Binding or constructor surface identifier. *)
  | Bind of ident
  (** Constructor/application pattern. *)
  | Apply of pattern_application
  (** Literal pattern. *)
  | Literal of literal
  (** Polymorphic variant pattern. *)
  | PolyVariant of poly_variant_pattern
  (** Tuple pattern. *)
  | Tuple of pattern list
  (** List pattern. *)
  | List of pattern list
  (** Record pattern. *)
  | Record of record_pattern_field list
  (** Or-pattern. *)
  | Or of or_pattern
  (** Cons pattern. *)
  | Cons of cons_pattern
  (** Type-constrained pattern. *)
  | Constraint of constrained_pattern
  (** Alias pattern, such as `p as name`. *)
  | Alias of alias_pattern
  (** Pattern with attributes preserved as a source-shaped wrapper. *)
  | Attribute of pattern
  (** First-class module pattern. *)
  | FirstClassModule of first_class_module_pattern

(** Pattern application payload. *)
and pattern_application = {
  (** Applied pattern, usually a constructor. *)
  callee: pattern;
  (** Runtime argument pattern. *)
  argument: pattern;
}

(** Or-pattern payload. *)
and or_pattern = {
  (** Left alternative. *)
  left: pattern;
  (** Right alternative. *)
  right: pattern;
}

(** List cons pattern payload. *)
and cons_pattern = {
  (** Head element pattern. *)
  head: pattern;
  (** Tail list pattern. *)
  tail: pattern;
}

(** Type-constrained pattern payload. *)
and constrained_pattern = {
  (** Constrained pattern. *)
  pattern: pattern;
  (** Source type annotation. *)
  annotation: core_type;
}

(** Alias pattern payload. *)
and alias_pattern = {
  (** Pattern being aliased. *)
  pattern: pattern;
  (** Alias binding pattern. *)
  alias: pattern;
}

(** First-class module pattern payload. *)
and first_class_module_pattern = {
  (** Optional bound module name. *)
  binder: string option;
  (** Optional package type annotation. *)
  package_type: package_type option;
}

(** Polymorphic variant pattern payload. *)
and poly_variant_pattern = {
  (** Tag name without the leading backtick. *)
  tag: string;
  (** Optional payload pattern. *)
  payload: pattern option;
}

(** Single `let` binding. *)
and let_binding = {
  (** Source origin for the binding. *)
  origin: origin;
  (** Bound pattern. *)
  pattern: pattern;
  (** Function-style parameters attached to the binding. *)
  type_binders: string list;
  (** Runtime function parameters attached to the binding. *)
  parameters: parameter list;
  (** Binding body. *)
  body: expression;
  (** Optional result/type annotation. *)
  type_annotation: core_type option;
}

(** Kind of type hint attached to an expression. *)
and expression_type_hint_kind =
  (** Ordinary type annotation, written `: type`. *)
  | Annotation
  (** Coercion, written `:> type` or `: type :> type`. *)
  | Coercion

(** Source type hint attached to an expression node. *)
and expression_type_hint = {
  (** Hint kind. *)
  kind: expression_type_hint_kind;
  (** Type supplied by the hint. *)
  type_: core_type;
}

(** Expression node plus optional inferred type. *)
and expression = {
  (** Source origin for the expression. *)
  origin: origin;
  (** Inferred type of the expression, once checked. *)
  mutable type_: Type.t option;
  (** Optional source type hint. *)
  type_hint: expression_type_hint option;
  (** Expression shape. *)
  kind: expression_kind;
}

(** Module unpack expression payload. *)
and module_unpack = {
  (** Source origin for the unpack. *)
  origin: origin;
  (** Expression producing the packed module. *)
  expression: expression;
  (** Optional package type annotation. *)
  package_type: package_type option;
}

(** Record expression payload. *)
and record_expression = {
  (** Optional base record for update syntax, such as `{ base with x = y }`. *)
  update: expression option;
  (** Record fields supplied by the expression. *)
  fields: record_expression_field list;
}

(** Function expression/declaration payload. *)
and fun_decl = {
  (** Type binders introduced by locally abstract type syntax. *)
  type_binders: string list;
  (** Runtime value parameters. *)
  parameters: parameter list;
  (** Function body. *)
  body: function_body;
}

(** Expression shape. *)
and expression_kind =
  (** Literal expression. *)
  | Literal of literal
  (** Identifier, constructor, or qualified value identifier. *)
  | Ident of ident
  (** Tuple expression. *)
  | Tuple of expression list
  (** List expression. *)
  | List of expression list
  (** Array expression. *)
  | Array of expression list
  (** Polymorphic variant expression. *)
  | PolyVariant of poly_variant_expression
  (** Record literal. *)
  | Record of record_expression
  (** Record or object field access. *)
  | FieldAccess of field_access
  (** Assignment expression. *)
  | Assign of assignment
  (** Sequencing expression. *)
  | Sequence of sequence
  (** Conditional expression. *)
  | If of conditional
  (** Pattern match expression. *)
  | Match of match_expression
  (** Exception handler expression. *)
  | Try of try_expression
  (** While loop. *)
  | While of while_loop
  (** For loop. *)
  | For of for_loop
  (** Function expression. *)
  | Function of fun_decl
  (** Function application. *)
  | Apply of application
  (** Infix operator application. *)
  | Infix of infix_operation
  (** Local `let` expression. *)
  | Let of let_expression
  (** Local module binding expression. *)
  | LetModule of let_module
  (** Local open expression. *)
  | LocalOpen of local_open
  (** First-class module expression. *)
  | FirstClassModule of first_class_module
  (** Assertion expression. *)
  | Assert of expression

(** Polymorphic variant expression payload. *)
and poly_variant_expression = {
  (** Tag name without the leading backtick. *)
  tag: string;
  (** Optional payload expression. *)
  payload: expression option;
}

(** Function body after parameters have been collected. *)
and function_body =
  (** Direct expression body. *)
  | Body of expression
  (** `function`-style case body. *)
  | Cases of match_case list

(** Pattern match case. *)
and match_case = {
  (** Source origin for the case. *)
  origin: origin;
  (** Left-hand pattern. *)
  pattern: pattern;
  (** Optional guard expression. *)
  guard: expression option;
  (** Right-hand body expression. *)
  body: expression;
}

(** Record field inside an expression. *)
and record_expression_field = {
  (** Source origin for the field. *)
  origin: origin;
  (** Field identifier. *)
  name: ident;
  (** Field value. *)
  value: expression;
}

(** Record or object field-access payload. *)
and field_access = {
  (** Expression producing the record/object. *)
  receiver: expression;
  (** Field being accessed. *)
  field: ident;
}

(** Assignment payload. *)
and assignment = {
  (** Assignable target expression. *)
  target: expression;
  (** New value expression. *)
  value: expression;
}

(** Sequence expression payload. *)
and sequence = {
  (** Expression evaluated first. *)
  left: expression;
  (** Expression evaluated second. *)
  right: expression;
}

(** Conditional expression payload. *)
and conditional = {
  (** Boolean condition. *)
  condition: expression;
  (** Branch evaluated when the condition is true. *)
  then_branch: expression;
  (** Optional branch evaluated when the condition is false. *)
  else_branch: expression option;
}

(** Match expression payload. *)
and match_expression = {
  (** Scrutinee expression. *)
  scrutinee: expression;
  (** Match cases. *)
  cases: match_case list;
}

(** Try expression payload. *)
and try_expression = {
  (** Protected body expression. *)
  body: expression;
  (** Exception handler cases. *)
  cases: match_case list;
}

(** While loop payload. *)
and while_loop = {
  (** Boolean loop condition. *)
  condition: expression;
  (** Loop body. *)
  body: expression;
}

(** For loop payload. *)
and for_loop = {
  (** Iterator binding pattern. *)
  pattern: pattern;
  (** Start expression. *)
  start_: expression;
  (** Stop expression. *)
  stop: expression;
  (** Loop body. *)
  body: expression;
}

(** Function application payload. *)
and application = {
  (** Callee expression. *)
  callee: expression;
  (** Supplied arguments. *)
  arguments: argument list;
}

(** Infix operator application payload. *)
and infix_operation = {
  (** Left operand. *)
  left: expression;
  (** Surface operator identifier. *)
  operator: ident;
  (** Right operand. *)
  right: expression;
}

(** Local let-expression payload. *)
and let_expression = {
  (** First binding in the local declaration. *)
  first_binding: let_binding;
  (** Expression body scoped by the binding. *)
  body: expression;
}

(** Local module binding payload. *)
and let_module = {
  (** Local module name. *)
  name: string;
  (** Inline module structure, when present. *)
  items: structure_item list;
  (** Module alias target, when present. *)
  alias: ident option;
  (** Module unpack source, when present. *)
  unpack: module_unpack option;
  (** Expression body scoped by the module. *)
  body: expression;
}

(** Local open expression payload. *)
and local_open = {
  (** Module opened for the body. *)
  module_: ident;
  (** Expression body evaluated under the open. *)
  body: expression;
}

(** First-class module expression payload. *)
and first_class_module = {
  (** Module being packed. *)
  module_: ident;
  (** Optional package type annotation. *)
  package_type: package_type option;
}

(** Function argument. *)
and argument = {
  (** Source origin for the argument. *)
  origin: origin;
  (** Argument shape. *)
  kind: argument_kind;
}

(** Function argument shape. *)
and argument_kind =
  (** Positional argument. *)
  | Positional of expression
  (** Required labeled argument. `None` represents an omitted value. *)
  | Labeled of labeled_argument
  (** Optional labeled argument. `None` represents an omitted value. *)
  | Optional of labeled_argument

(** Labeled or optional argument payload. *)
and labeled_argument = {
  (** Argument label without the leading `~` or `?`. *)
  label: string;
  (** Argument expression; `None` is an omitted labeled argument. *)
  value: expression option;
}

(** Top-level or local `let` group. *)
and let_declaration = {
  (** Source origin for the declaration. *)
  origin: origin;
  (** Whether the group was declared `rec`. *)
  recursive: bool;
  (** Bindings in source order. *)
  bindings: let_binding list;
}

(** Value declaration from an interface. *)
and value_declaration = {
  (** Source origin for the declaration. *)
  origin: origin;
  (** Declared value name. *)
  name: string;
  (** Declared value type. *)
  type_annotation: core_type;
}

(** External declaration. *)
and external_declaration = {
  (** Source origin for the declaration. *)
  origin: origin;
  (** Declared external name. *)
  name: string;
  (** Declared external type. *)
  type_annotation: core_type;
  (** Primitive names listed in the declaration. *)
  primitives: string list;
}

(** Type extension declaration. *)
and type_extension_declaration = {
  (** Source origin for the declaration. *)
  origin: origin;
  (** Extended type name. *)
  name: ident;
  (** Extension constructors. *)
  constructors: type_constructor list;
}

(** Exception declaration. *)
and exception_declaration = {
  (** Source origin for the declaration. *)
  origin: origin;
  (** Exception constructor name. *)
  name: string;
  (** Optional exception payload type. *)
  payload: core_type option;
}

(** Module declaration. *)
and module_declaration = {
  (** Source origin for the declaration. *)
  origin: origin;
  (** Module name. *)
  name: string;
  (** Whether this is a recursive module declaration. *)
  recursive: bool;
  (** Functor parameters declared on the module. *)
  parameters: functor_parameter list;
  (** Inline structure items when the module body is a structure. *)
  items: structure_item list;
  (** Alias target when the module body is an identifier alias. *)
  alias: ident option;
  (** Optional module type identifier annotation. *)
  module_type: ident option;
  (** Module application payload when the body is a module application. *)
  application: module_application option;
}

(** Functor parameter. *)
and functor_parameter = {
  (** Source origin for the parameter. *)
  origin: origin;
  (** Parameter module name. *)
  name: string;
  (** Optional module type identifier. *)
  module_type: ident option;
}

(** Module application payload. *)
and module_application = {
  (** Applied functor identifier. *)
  callee: ident;
  (** Argument module identifier. *)
  argument: ident;
}

(** Module type declaration. *)
and module_type_declaration = {
  (** Source origin for the declaration. *)
  origin: origin;
  (** Module type name. *)
  name: string;
  (** Signature items in the module type. *)
  items: signature_item list;
}

(** Top-level or structure-level item. *)
and structure_item = {
  (** Source origin for the item. *)
  origin: origin;
  (** Item shape. *)
  kind: structure_item_kind;
}

(** Structure item shape. *)
and structure_item_kind =
  (** Value binding group. *)
  | Let of let_declaration
  (** Type declaration group. *)
  | Type of type_declaration list
  (** Type extension declaration. *)
  | TypeExtension of type_extension_declaration
  (** Bare expression item. *)
  | Expression of expression
  (** External value declaration. *)
  | External of external_declaration
  (** Exception declaration. *)
  | Exception of exception_declaration
  (** Module declaration group. *)
  | Module of module_declaration list
  (** Module type declaration. *)
  | ModuleType of module_type_declaration
  (** Include by module identifier. *)
  | Include of ident

(** Signature item. *)
and signature_item = {
  (** Source origin for the item. *)
  origin: origin;
  (** Item shape. *)
  kind: signature_item_kind;
}

(** Signature item shape. *)
and signature_item_kind =
  (** Value declaration. *)
  | Value of value_declaration
  (** Type declaration group. *)
  | Type of type_declaration list
  (** Type extension declaration. *)
  | TypeExtension of type_extension_declaration
  (** External value declaration. *)
  | External of external_declaration
  (** Exception declaration. *)
  | Exception of exception_declaration
(** Complete typed source file. *)
type t = {
  (** Source origin for the file root. *)
  origin: origin;
  (** File body. *)
  kind: source_file_kind;
}

(** Source file body. *)
and source_file_kind =
  (** Implementation with structure items. *)
  | Implementation of structure_item list
  (** Interface with signature items. *)
  | Interface of signature_item list

(** Return the source origin of a type expression. *)
val core_type_origin: core_type -> origin

(** Return the inferred semantic type attached to a type expression, if any. *)
val core_type_type: core_type -> Type.t option

(** Return the source origin of a parameter. *)
val parameter_origin: parameter -> origin

(** Return the source origin of a pattern. *)
val pattern_origin: pattern -> origin

(** Return the inferred semantic type attached to a pattern, if any. *)
val pattern_type: pattern -> Type.t option

(** Return the source origin of an expression. *)
val expression_origin: expression -> origin

(** Return the inferred semantic type attached to an expression, if any. *)
val expression_type: expression -> Type.t option

(** Return the source origin of a structure item. *)
val structure_item_origin: structure_item -> origin

(** Return the source origin of a signature item. *)
val signature_item_origin: signature_item -> origin

(** Build `Typ.Ast` from a Syn parser result.

    The build is all-or-nothing: unsupported source syntax returns structured
    diagnostics instead of placing error nodes in `Typ.Ast`. *)
val from_parse_result:
  source:Model.Source.t -> Syn.Parser.parse_result -> (t, Diagnostics.Diagnostic.t list) Result.t

(** Serializer used by snapshot tests and future cache payloads. *)
val serializer: t Serde.Ser.t
