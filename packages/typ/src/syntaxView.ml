open Std

(** Typed syntax-tree view consumed by typ lowering.

    This is the local copy of the future Syn AST contract. It stays at the
    typ boundary: inference should consume typ-owned surface/HIR structures,
    not this view directly. *)
module type S = sig
  type span = Syn.Ceibo.Span.t
  type 'a many = 'a Collections.Vector.t
  type syntax_kind
  type node
  type token
  type source_file
  type structure
  type signature_
  type structure_item
  type signature_item
  type expr
  type pattern
  type core_type
  type module_type
  type module_expr
  type path
  type literal
  type attribute
  type extension
  type payload
  type parameter
  type argument
  type match_case
  type let_binding
  type let_group
  type type_decl
  type type_constructor
  type record_field_decl
  type type_parameter
  type type_constraint
  type module_decl
  type module_type_decl
  type module_type_constraint
  type functor_parameter
  type open_decl
  type include_decl
  type val_decl
  type external_decl
  type exception_decl
  type file_kind =
    | Implementation
    | Interface
  module Node: sig
    type t = node
    val kind: t -> syntax_kind

    val span: t -> span

    val text: t -> string
  end

  module Token: sig
    type t = token
    val kind: t -> syntax_kind

    val span: t -> span

    val text: t -> string
  end

  module Path: sig
    type t = path
    val node: t -> node

    val segments: t -> token many

    val last: t -> token option

    val text: t -> string
  end

  module Payload: sig
    type t = payload
    type view =
      | Opaque of token many
    val view: t -> view
  end

  module Attribute: sig
    type t = attribute
    val node: t -> node

    val name: t -> path

    val payload: t -> payload option
  end

  module Extension: sig
    type t = extension
    val node: t -> node

    val name: t -> path

    val payload: t -> payload option

    val attributes: t -> attribute many
  end

  module Literal: sig
    type t = literal
    type integer_base =
      | Decimal
      | Hexadecimal
      | Octal
      | Binary
    type exponent_sign =
      | Positive
      | Negative
    type view =
      | String of { token: token; text: string; terminated: bool }
      | Int of { token: token; base: integer_base; digits: string; suffix: string option }
      | Float of {
          token: token;
          integral_digits: string;
          fractional_digits: string;
          exponent: (exponent_sign option * string) option;
          suffix: string option
        }
      | Char of { token: token; contents: string }
      | Bool of { token: token; value: bool }
      | Unit
    val node: t -> node

    val view: t -> view
  end

  module TypeBinder: sig
    type t =
      | Quoted of { node: node; name: token }
      | Bare of { name: token }
  end

  module Label: sig
    type t =
      | Unlabeled
      | Labeled of token
      | Optional of token
  end

  module CoreType: sig
    type t = core_type
    type arrow_label =
      | Unlabeled
      | Labeled of { label: token }
      | Optional of { label: token }
    type package = {
      path: path;
      constraints: module_type_constraint many;
    }
    type poly_variant_bound =
      | Exact
      | UpperBound of token
      | LowerBound of token
    type row_field =
      | Tag of { name: token; payload: t option }
      | Inherit of t
    type view =
      | Wildcard
      | Var of { name: token }
      | Constr of { path: path; args: t many }
      | Alias of { type_: t; name: token }
      | Poly of { binders: TypeBinder.t many; body: t }
      | Arrow of { label: arrow_label; param: t; result: t }
      | Tuple of t many
      | Record of record_field_decl many
      | PolyVariant of { bound: poly_variant_bound; fields: row_field many }
      | FirstClassModule of package
      | Parenthesized of t
      | Attribute of { type_: t; attribute: attribute }
      | Extension of extension
      | Error of node
    val node: t -> node

    val view: t -> view
  end

  module ModuleTypeConstraint: sig
    type t = module_type_constraint
    type view = {
      constrained_type: core_type;
      replacement_type: core_type;
      separator: token;
    }
    val node: t -> node

    val view: t -> view
  end

  module ModuleType: sig
    type t = module_type
    type view =
      | Path of path
      | TypeOf of path
      | Signature of signature_
      | Functor of { params: functor_parameter many; result: t }
      | With of { base: t; constraints: module_type_constraint many }
      | Parenthesized of t
      | Attribute of { module_type: t; attribute: attribute }
      | Extension of extension
      | Error of node
    val node: t -> node

    val view: t -> view
  end

  module FunctorParameter: sig
    type t = functor_parameter
    type view = {
      name: token;
      module_type: module_type;
    }
    val node: t -> node

    val view: t -> view
  end

  module TypeParameter: sig
    type variance =
      | Covariant of token
      | Contravariant of token
    type t = type_parameter
    type view = {
      variance: variance option;
      injective: token option;
      name: token option;
    }
    val node: t -> node

    val view: t -> view
  end

  module RecordFieldDecl: sig
    type t = record_field_decl
    type view = {
      name: token;
      mutable_: token option;
      type_: core_type;
      attributes: attribute many;
    }
    val node: t -> node

    val view: t -> view
  end

  module TypeConstructor: sig
    type args =
      | Tuple of core_type many
      | Record of record_field_decl many
    type t = type_constructor
    type view = {
      name: token;
      args: args option;
      result: core_type option;
      attributes: attribute many;
    }
    val node: t -> node

    val view: t -> view
  end

  module TypeDecl: sig
    type t = type_decl
    type private_flag =
      | Public
      | Private of token
    type definition =
      | Abstract
      | Alias of core_type
      | Extensible
      | Record of record_field_decl many
      | Variant of type_constructor many
      | PolyVariant of CoreType.row_field many
      | FirstClassModule of CoreType.package
      | Error of node
    type view = {
      name: path;
      params: type_parameter many;
      private_: private_flag;
      definition: definition;
      constraints: type_constraint many;
      attributes: attribute many;
    }
    val node: t -> node

    val view: t -> view
  end

  module Pattern: sig
    type t = pattern
    type record_closedness =
      | Closed
      | Open
    type record_field = {
      path: path;
      value: t option;
    }
    type tuple_element = {
      label: token option;
      pattern: t;
    }
    type first_class_module_binding =
      | Named of token
      | Anonymous of token
    type view =
      | Identifier of token
      | Wildcard
      | Literal of literal
      | Extension of extension
      | Lazy of t
      | Exception of t
      | Range of { lower: literal; upper: literal }
      | Operator of token many
      | FirstClassModule of {
          binding: first_class_module_binding;
          package_type: CoreType.package option
        }
      | PolyVariant of { tag: token; payload: t option }
      | PolyVariantInherit of path
      | Constructor of { path: path; args: t many }
      | Tuple of { elements: tuple_element many; open_tail: bool }
      | List of t many
      | Array of t many
      | Record of { fields: record_field many; closedness: record_closedness }
      | Cons of { head: t; tail: t }
      | Or of t many
      | Alias of { pattern: t; name: token }
      | Typed of { pattern: t; type_: core_type }
      | Effect of { effect_: t; continuation: t }
      | LocalOpen of { module_path: path; pattern: t }
      | Parenthesized of t
      | Error of node
    val node: t -> node

    val view: t -> view

    val attributes: t -> attribute many
  end

  module Parameter: sig
    type t = parameter
    type view =
      | Positional of pattern
      | Labeled of { label: token; pattern: pattern option }
      | Optional of { label: token; pattern: pattern option; default: expr option }
      | LocallyAbstract of TypeBinder.t many
    val node: t -> node

    val view: t -> view
  end

  module Argument: sig
    type t = argument
    type view =
      | Positional of expr
      | Labeled of { label: token; value: expr option }
      | Optional of { label: token; value: expr option }
    val node: t -> node

    val view: t -> view
  end

  module MatchCase: sig
    type t = match_case
    type view = {
      pattern: pattern;
      guard: expr option;
      body: expr;
    }
    val node: t -> node

    val view: t -> view
  end

  module Expr: sig
    type t = expr
    type for_direction =
      | To of token
      | Downto of token
    type record_field = {
      path: path;
      value: t;
      punned: bool;
    }
    type record_view =
      | Literal of record_field many
      | Update of { base: t; fields: record_field many }
    type local_open =
      | LetOpen of { module_path: path; body: t }
      | Delimited of { module_path: path; body: t }
    type type_ascription =
      | Type of core_type
      | Coerce of core_type
      | ConstraintCoerce of { from_type: core_type; to_type: core_type }
    type fun_body =
      | Body of t
      | Cases of match_case many
    type view =
      | Path of path
      | Constructor of { path: path; payload: t option }
      | Operator of token many
      | Literal of literal
      | Unreachable
      | Extension of extension
      | PolyVariant of { tag: token; payload: t option }
      | ModulePack of { module_expr: module_expr; package_type: CoreType.package option }
      | LetModule of { name: token; module_expr: module_expr; body: t }
      | LetException of { decl: exception_decl; body: t }
      | Assert of t
      | Lazy of t
      | While of { condition: t; body: t }
      | For of { iterator: token; start: t; direction: for_direction; stop: t; body: t }
      | Apply of { callee: t; args: argument many }
      | Prefix of { op: token; operand: t }
      | FieldAccess of { receiver: t; field: path }
      | Index of { collection: t; index: t }
      | FieldAssign of { target: t; field: path; value: t }
      | Assign of { target: t; value: t }
      | Infix of { left: t; op: token many; right: t }
      | TypeAscription of { expr: t; kind: type_ascription }
      | Polymorphic of { expr: t; type_: core_type }
      | Sequence of t many
      | Tuple of t many
      | List of t many
      | Array of t many
      | Record of record_view
      | LocalOpen of local_open
      | Fun of { params: parameter many; return_type: core_type option; body: fun_body }
      | Function of match_case many
      | LetOperator of { bindings: let_binding many; body: t }
      | Let of { group: let_group; body: t option }
      | Match of { scrutinee: t; cases: match_case many }
      | Try of { body: t; cases: match_case many }
      | If of { condition: t; then_: t; else_: t option }
      | Parenthesized of t
      | Error of node
    val node: t -> node

    val view: t -> view

    val attributes: t -> attribute many
  end

  module LetBinding: sig
    type t = let_binding
    type view = {
      pattern: pattern;
      params: parameter many;
      return_type: core_type option;
      value: expr;
      attributes: attribute many;
    }
    val node: t -> node

    val view: t -> view
  end

  module LetGroup: sig
    type t = let_group
    type view = {
      recursive: bool;
      bindings: let_binding many;
    }
    val node: t -> node

    val view: t -> view
  end

  module ModuleExpr: sig
    type t = module_expr
    type view =
      | Path of path
      | Structure of structure
      | Functor of { params: functor_parameter many; body: t }
      | Apply of { callee: t; argument: t }
      | ApplyUnit of { callee: t }
      | Constraint of { module_expr: t; module_type: module_type }
      | ModuleUnpack of { expr: expr; package_type: CoreType.package option }
      | Parenthesized of t
      | Attribute of { module_expr: t; attribute: attribute }
      | Extension of extension
      | Error of node
    val node: t -> node

    val view: t -> view
  end

  module ModuleDecl: sig
    type t = module_decl
    type view = {
      recursive: bool;
      name: token;
      params: functor_parameter many;
      module_type: module_type option;
      module_expr: module_expr option;
    }
    val node: t -> node

    val view: t -> view
  end

  module ModuleTypeDecl: sig
    type t = module_type_decl
    type view = {
      name: token;
      module_type: module_type option;
    }
    val node: t -> node

    val view: t -> view
  end

  module OpenDecl: sig
    type t = open_decl
    type target =
      | Path of path
      | ModuleExpr of module_expr
    type view = {
      target: target;
      override: bool;
    }
    val node: t -> node

    val view: t -> view
  end

  module IncludeDecl: sig
    type t = include_decl
    type target =
      | ModuleExpr of module_expr
      | ModuleType of module_type
    type view = {
      target: target;
    }
    val node: t -> node

    val view: t -> view
  end

  module ValDecl: sig
    type t = val_decl
    type view = {
      name: token many;
      type_: core_type;
      attributes: attribute many;
    }
    val node: t -> node

    val view: t -> view
  end

  module ExternalDecl: sig
    type t = external_decl
    type view = {
      name: token many;
      type_: core_type;
      primitives: token many;
      attributes: attribute many;
    }
    val node: t -> node

    val view: t -> view
  end

  module ExceptionDecl: sig
    type t = exception_decl
    type rhs =
      | Alias of path
      | Payload of core_type
    type view = {
      name: token;
      rhs: rhs option;
    }
    val node: t -> node

    val view: t -> view
  end

  module StructureItem: sig
    type t = structure_item
    type view =
      | Let of let_group
      | Type of type_decl many
      | TypeExtension of { type_name: path; constructors: type_constructor many }
      | Module of module_decl many
      | ModuleType of module_type_decl
      | Open of open_decl
      | Include of include_decl
      | External of external_decl
      | Exception of exception_decl
      | Expr of expr
      | Attribute of attribute
      | Extension of extension
      | Error of node
    val node: t -> node

    val view: t -> view
  end

  module SignatureItem: sig
    type t = signature_item
    type view =
      | Val of val_decl
      | Type of type_decl many
      | TypeExtension of { type_name: path; constructors: type_constructor many }
      | Module of module_decl many
      | ModuleType of module_type_decl
      | Open of open_decl
      | Include of include_decl
      | External of external_decl
      | Exception of exception_decl
      | Attribute of attribute
      | Extension of extension
      | Error of node
    val node: t -> node

    val view: t -> view
  end

  module Structure: sig
    type t = structure
    val node: t -> node

    val items: t -> structure_item many
  end

  module Signature: sig
    type t = signature_
    val node: t -> node

    val items: t -> signature_item many
  end

  module SourceFile: sig
    type t = source_file
    type view =
      | Implementation of structure
      | Interface of signature_
    val node: t -> node

    val kind: t -> file_kind

    val view: t -> view
  end

  module Layout: sig
    type trivia =
      | Whitespace of token
      | Comment of token
      | Docstring of token
    val leading_trivia: node -> trivia many

    val trailing_trivia: node -> trivia many

    val trivia_between: node -> node -> trivia many
  end
end
