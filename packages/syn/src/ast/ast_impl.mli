open Std
open Std.Collections

(**
   Typed views over `Syntax_tree`.

   Ast values are small handles: a tree pointer plus a node or token id. They
   do not own syntax, allocate a second CST, or guarantee that a node is
   complete. Accessors return `option` where recovery or malformed input can
   leave a child missing.
*)
type node = {
  tree: Syntax_tree.t;
  id: int;
}
type token = {
  tree: Syntax_tree.t;
  id: int;
}
type source_file
type implementation
type interface
type structure_item
type signature_item
type let_declaration
type let_binding
type type_declaration
type type_extension_declaration
type module_declaration
type module_expr
type module_type_expr
type module_type_declaration
type module_type_constraint
type open_declaration
type include_declaration
type value_declaration
type external_declaration
type exception_declaration
type extension_item
type attribute_item
type expr_item
type expr
type pattern
type parameter
type match_case
type type_expr
type record_type
type record_field
type record_expr_field
type variant_type
type variant_constructor
type cast_error = {
  expected: Syntax_kind.t list;
  actual: Syntax_kind.t;
  node: node;
}
type 'value cast_result =
  | Node of 'value
  | Unknown of node
  | Error of cast_error
type 'value control =
  | Continue of 'value
  | Return of 'value

val cast_result_to_option: 'value cast_result -> 'value option

module Ident: sig
  type t =
    | Bare of token
    | Qualified of token * t
  type view = t

  val cast: node -> t cast_result

  val from_node: node -> t

  val from_node_option: node -> t option

  val from_child_range: node -> start_index:int -> stop_index:int -> t

  val from_child_range_option: node -> start_index:int -> stop_index:int -> t option

  val kind: t -> Syntax_kind.t

  val width: t -> int

  val span: t -> Span.t

  val text: t -> string

  val node_is_single_text: node -> string -> bool

  val view: t -> view option

  val first_segment: t -> token option

  val last_segment: t -> token option

  val fold_token: t -> init:'acc -> fn:(token -> 'acc -> 'acc control) -> 'acc

  val fold_segment: t -> init:'acc -> fn:(token -> 'acc -> 'acc control) -> 'acc

  val segment_count: t -> int
end

type record_expr_field_view =
  | RecordExprField of {
      ident: Ident.t;
      value: expr option;
      node: record_expr_field;
    }
  | UnknownRecordExprField of {
      node: record_expr_field;
    }
type record_pattern_field_view =
  | RecordPatternField of {
      ident: Ident.t;
      pattern: pattern option;
      node: pattern;
    }
  | UnknownRecordPatternField of {
      node: pattern;
    }
type first_class_module_pattern_ascription =
  | NoAscription
  | IdentAscription
  | UnsupportedAscription
type type_item =
  | TypeDeclarationItem of type_declaration
  | TypeExtensionItem of type_extension_declaration

(** Root view for a parsed syntax tree. *)
val root: Syntax_tree.t -> node

module Token: sig
  type t = token
  (**
     Delimited comment/docstring trivia split once by the lexer/Ast layer.

     `content` excludes the opening and closing delimiters, so formatter and
     documentation tools do not need to rescan raw comment text.
  *)
  type delimited_trivia = {
    text: string;
    opening: string;
    content: string;
    closing: string option;
  }
  type leading_trivia =
    | Whitespace
    | Comment of delimited_trivia
    | Docstring of delimited_trivia

  val kind: t -> Syntax_kind.t

  val width: t -> int

  val contains_char: t -> char -> bool

  val text_is: t -> string -> bool

  val text_equal: t -> t -> bool

  val has_newline: t -> bool

  val slice: t -> IO.IoVec.IoSlice.t

  val text: t -> string

  (** Source span of the token body, excluding leading trivia. *)
  val span: t -> Span.t

  val span_start: t -> int

  val span_end: t -> int

  (** Materialize all leading trivia attached to this token. *)
  val leading_text: t -> string

  (** Fold raw leading trivia as syntax kind/text pairs. *)
  val fold_leading_trivia:
    t ->
    init:'acc ->
    fn:(kind:Syntax_kind.t -> text:string -> 'acc -> 'acc control) ->
    'acc

  (**
     Fold normalized leading trivia items. Whitespace is structural and
     comment/docstring delimiters are split from their content.
  *)
  val fold_leading_trivia_item:
    t ->
    init:'acc ->
    fn:(leading_trivia -> 'acc -> 'acc control) ->
    'acc

  val has_leading_whitespace: t -> bool

  val has_leading_comment: t -> bool

  val has_leading_docstring: t -> bool

  val full_text: t -> string

  (** Raw-token range owned by this token, including leading trivia. *)
  val raw_range: t -> int * int
end

module Node: sig
  type t = node

  val kind: t -> Syntax_kind.t

  val text: t -> string

  (** Source span covered by this node's syntactic tokens, excluding leading trivia. *)
  val span: t -> Span.t

  val span_start: t -> int

  val span_end: t -> int

  val raw_range: t -> int * int

  val full_width: t -> int

  val token_width: t -> int

  val width: t -> int

  val child_count: t -> int

  (**
     Access a raw child edge by index. Most callers should prefer typed view
     accessors on the domain-specific modules below.
  *)
  val child_at: t -> int -> Syntax_tree.child option

  val fold_child: t -> init:'acc -> fn:(Syntax_tree.child -> 'acc -> 'acc control) -> 'acc

  val fold_child_node: t -> init:'acc -> fn:(t -> 'acc -> 'acc control) -> 'acc

  val fold_child_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val fold_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val first_child_node: t -> kind:Syntax_kind.t -> t option

  val first_child_token: t -> kind:Syntax_kind.t -> Token.t option

  val first_token: t -> Token.t option

  val first_descendant_token: t -> Token.t option
end

module TypeExpr: sig
  type t = type_expr
  type arrow_label = {
    name: Token.t option;
    optional_: bool;
  }
  type view =
    | Ident of {
        ident: Ident.t;
      }
    | Var of {
        name: Token.t;
      }
    | Wildcard
    | Arrow of {
        label: arrow_label option;
        arg: t;
        ret: t;
      }
    | Forall of {
        names: Token.t Vector.t;
        body: t;
      }
    | Alias of {
        typ: t;
        name: Token.t;
      }
    | Tuple of {
        parts: t Vector.t;
      }
    | Apply of {
        ident: Ident.t;
        args: t Vector.t;
      }
    | Error of Node.t
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val text: t -> string

  val view: t -> view

  val poly_type_keyword_token: t -> Token.t option

  val fold_poly_type_name: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val poly_type_name_count: t -> int

  val fold_child_type: t -> init:'acc -> fn:(t -> 'acc -> 'acc control) -> 'acc

  val child_type_count: t -> int

  val inner_without_attribute_suffix: t -> t option

  val fold_attribute_suffix_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val attribute_suffix_token_count: t -> int
end

module RecordField: sig
  type t = record_field
  type view =
    | Field of {
        mutable_token: Token.t option;
        name: Ident.t;
        colon_token: Token.t;
        annotation: type_expr;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val mutable_token: t -> Token.t option

  val name: t -> Ident.t option

  val colon_token: t -> Token.t option

  val type_annotation: t -> type_expr option
end

module RecordType: sig
  type t = record_type

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val private_token: t -> Token.t option

  val opening_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val fold_field: t -> init:'acc -> fn:(record_field -> 'acc -> 'acc control) -> 'acc

  val field_count: t -> int
end

module RecordExprField: sig
  type t = record_expr_field

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int
end

module VariantConstructor: sig
  type t = variant_constructor
  type payload =
    | TypeExpr of type_expr
    | Record of record_type
  type rhs =
    | Plain
    | Payload of {
        of_token: Token.t;
        payload: payload;
      }
    | Gadt of {
        colon_token: Token.t;
        record_payload: record_type option;
        arrow_token: Token.t option;
        result: type_expr;
      }
  type view =
    | Constructor of {
        pipe_token: Token.t option;
        name: Ident.t;
        rhs: rhs;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val pipe_token: t -> Token.t option

  val name: t -> Ident.t option

  val of_token: t -> Token.t option

  val colon_token: t -> Token.t option

  val payload_type: t -> type_expr option

  val result_type: t -> type_expr option

  val record_payload: t -> record_type option
end

module VariantType: sig
  type t = variant_type

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val private_token: t -> Token.t option

  val fold_constructor: t -> init:'acc -> fn:(variant_constructor -> 'acc -> 'acc control) -> 'acc

  val constructor_count: t -> int

  val fold_inherited_type: t -> init:'acc -> fn:(type_expr -> 'acc -> 'acc control) -> 'acc

  val inherited_type_count: t -> int
end

module Pattern: sig
  type t = pattern
  type view =
    | Unit
    | Wildcard
    | Ident of {
        ident: Ident.t;
      }
    | Constructor of {
        constructor: Ident.t;
        payload: t option;
      }
    | Literal of {
        token: Token.t;
      }
    | Tuple of {
        parts: t Vector.t;
      }
    | List of {
        items: t Vector.t;
      }
    | Array of {
        items: t Vector.t;
      }
    | Record of {
        fields: record_pattern_field_view Vector.t;
        open_wildcard: Token.t option;
      }
    | PolyVariant of {
        tag: Token.t;
        payload: t option;
      }
    | FirstClassModule of {
        binder: Ident.t;
        ascription: first_class_module_pattern_ascription;
        ascription_ident: Ident.t option;
      }
    | Interval of { left: t; right: t }
    | Constraint of {
        pattern: t;
        annotation: type_expr;
      }
    | Alias of { pattern: t; alias: t }
    | Or of { left: t; right: t }
    | Cons of { head: t; tail: t }
    | Lazy of { pattern: t }
    | Exception of { pattern: t; payload: t option }
    | Error of Node.t
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val text: t -> string

  val view: t -> view

  val literal_token: t -> Token.t option

  val literal_sign_token: t -> Token.t option

  val fold_child_pattern: t -> init:'acc -> fn:(t -> 'acc -> 'acc control) -> 'acc

  val child_pattern_count: t -> int
end

module AttributePattern: sig
  type t = pattern

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val inner: t -> pattern option

  val fold_shell_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int
end

module ExtensionPattern: sig
  type t = pattern

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val fold_shell_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int
end

module LocallyAbstractTypePattern: sig
  type t = pattern

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val opening_token: t -> Token.t option

  val type_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val type_ident: t -> Ident.t option

  val fold_type_ident: t -> init:'acc -> fn:(Ident.t -> 'acc -> 'acc control) -> 'acc

  val type_ident_count: t -> int
end

module FirstClassModulePattern: sig
  type t = pattern
  type ascription = first_class_module_pattern_ascription =
    | NoAscription
    | IdentAscription
    | UnsupportedAscription

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val opening_token: t -> Token.t option

  val module_token: t -> Token.t option

  val binder: t -> Ident.t option

  val colon_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val ascription: t -> ascription

  val ascription_ident: t -> Ident.t option
end

module RecordPattern: sig
  type t = pattern
  type field = record_pattern_field_view

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val open_wildcard: t -> Token.t option

  val fold_field: t -> init:'acc -> fn:(field -> 'acc -> 'acc control) -> 'acc

  val field_count: t -> int
end

module LocalOpenPattern: sig
  type t = pattern
  type view =
    | Delimited of {
        module_ident: Ident.t;
        dot_token: Token.t;
        opening_token: Token.t;
        pattern: pattern;
        closing_token: Token.t;
      }
    | Unknown of Node.t

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val dot_token: t -> Token.t option

  val opening_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val pattern: t -> pattern option

  val module_ident: t -> Ident.t option
end

module Parameter: sig
  type t = parameter
  type label =
    | NoLabel
    | Labeled of {
        name: Token.t option;
      }
    | Optional of {
        name: Token.t option;
        default: expr option;
      }
  type view =
    | Param of {
        label: label;
        pattern: pattern option;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val label: t -> label

  val label_token: t -> Token.t option

  val pattern: t -> pattern option

  val default: t -> expr option

  val has_explicit_pattern_parens: t -> bool
end

module MatchCase: sig
  type t = match_case
  type view =
    | Case of {
        pattern: pattern;
        guard: expr option;
        body: expr;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val pattern: t -> pattern option

  val guard: t -> expr option

  val body: t -> expr option
end

module LetBinding: sig
  type t = let_binding
  type view =
    | Binding of {
        pattern: pattern;
        body: expr;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val pattern: t -> pattern option

  val body: t -> expr option

  val fold_parameter: t -> init:'acc -> fn:(parameter -> 'acc -> 'acc control) -> 'acc

  val parameter_count: t -> int

  val return_type_annotation: t -> type_expr option

  val type_annotation: t -> type_expr option
end

module Expr: sig
  type t = expr
  type fun_body =
    | Body_expr of t
    | Body_cases of {
        first_case: match_case;
      }
  type view =
    | Unit
    | Let of {
        first_binding: let_binding;
        body: t;
      }
    | LocalOpen of { body: t }
    | LetModule of { body: t }
    | LetException of { body: t }
    | If of {
        condition: t;
        then_branch: t;
        else_branch: t option;
      }
    | Match of {
        scrutinee: t;
        first_case: match_case;
      }
    | Fun of {
        parameters: parameter Vector.t;
        return_annotation: type_expr option;
        body: fun_body;
      }
    | Try of {
        body: t;
        first_case: match_case;
      }
    | While of { condition: t; body: t }
    | For of {
        pattern: pattern;
        start_: t;
        stop: t;
        body: t;
      }
    | Sequence of {
        left: t;
        right: t option;
      }
    | Apply of { callee: t; argument: t }
    | Infix of {
        left: t;
        operator: Token.t;
        right: t;
      }
    | Prefix of {
        operator: Token.t;
        operand: t;
      }
    | Assign of {
        target: t;
        operator: Token.t;
        value: t;
      }
    | FieldAccess of {
        target: t;
        field: Ident.t;
      }
    | PolyVariant of {
        tag: Token.t;
        payload: t option;
      }
    | Constructor of {
        constructor: Ident.t;
        payload: t option;
      }
    | Ident of {
        ident: Ident.t;
      }
    | Literal of {
        token: Token.t;
      }
    | Tuple of {
        items: t Vector.t;
      }
    | List of {
        items: t Vector.t;
      }
    | Array of {
        items: t Vector.t;
      }
    | Record of {
        base: t option;
        fields: record_expr_field_view Vector.t;
      }
    | Annotated of {
        expr: t;
        annotation: type_expr;
      }
    | Error of Node.t
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val literal_token: t -> Token.t option

  val list_has_trailing_separator: t -> bool

  val fold_child_expr: t -> init:'acc -> fn:(t -> 'acc -> 'acc control) -> 'acc

  val child_expr_count: t -> int

  val fold_match_case: t -> init:'acc -> fn:(match_case -> 'acc -> 'acc control) -> 'acc

  val match_case_count: t -> int

  val fold_parameter: t -> init:'acc -> fn:(parameter -> 'acc -> 'acc control) -> 'acc

  val parameter_count: t -> int
end

module AttributeExpr: sig
  type t = expr

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val inner: t -> expr option

  val fold_shell_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int
end

module ExtensionExpr: sig
  type t = expr

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val fold_shell_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int
end

module RecordExpr: sig
  type t = expr
  type field = record_expr_field_view

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val base: t -> expr option

  val fold_field: t -> init:'acc -> fn:(field -> 'acc -> 'acc control) -> 'acc

  val field_count: t -> int
end

module LocalOpenExpr: sig
  type t = expr
  type view =
    | LetOpen of {
        let_token: Token.t;
        open_token: Token.t;
        bang_token: Token.t option;
        module_ident: Ident.t;
        in_token: Token.t;
        body: expr;
      }
    | Delimited of {
        module_ident: Ident.t;
        dot_token: Token.t;
        opening_token: Token.t;
        body: expr;
        closing_token: Token.t;
      }
    | Unknown of Node.t

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view
end

module LetModuleExpr: sig
  type t = expr
  type module_body =
    | Ident
    | Struct
    | Unsupported

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val let_token: t -> Token.t option

  val module_token: t -> Token.t option

  val name: t -> Ident.t option

  val equals_token: t -> Token.t option

  val in_token: t -> Token.t option

  val module_body: t -> module_body

  val module_body_node: t -> Node.t option

  val body: t -> expr option

  val module_body_ident: t -> Ident.t option
end

module LetExceptionExpr: sig
  type t = expr

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val let_token: t -> Token.t option

  val exception_token: t -> Token.t option

  val name: t -> Ident.t option

  val of_token: t -> Token.t option

  val in_token: t -> Token.t option

  val body: t -> expr option

  val fold_payload_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val payload_token_count: t -> int
end

module UnreachableExpr: sig
  type t = expr

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val dot_token: t -> Token.t option
end

module FirstClassModuleExpr: sig
  type t = expr
  type ascription =
    | NoAscription
    | IdentAscription
    | UnsupportedAscription

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val opening_token: t -> Token.t option

  val module_token: t -> Token.t option

  val colon_token: t -> Token.t option

  val closing_token: t -> Token.t option

  val module_ident: t -> Ident.t option

  val ascription: t -> ascription

  val ascription_ident: t -> Ident.t option
end

module BindingOperatorExpr: sig
  type t = expr
  type clause = {
    keyword: Token.t option;
    operator: Token.t option;
    binding: let_binding;
  }

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val in_token: t -> Token.t option

  val body: t -> expr option

  val fold_clause: t -> init:'acc -> fn:(clause -> 'acc -> 'acc control) -> 'acc

  val clause_count: t -> int
end

module ModuleTypeExpr: sig
  type t = module_type_expr
  type view =
    | Ident of {
        ident: Ident.t;
      }
    | Signature of {
        body: Node.t;
      }
    | With of {
        body: Node.t;
        base: t option;
        constraints: module_type_constraint Vector.t;
      }
    | Typeof of {
        body: module_expr option;
      }
    | Functor of {
        body: Node.t;
      }
    | Error of Node.t
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val sig_token: t -> Token.t option

  val end_token: t -> Token.t option

  val ident: t -> Ident.t option

  val fold_signature_item: t -> init:'acc -> fn:(signature_item -> 'acc -> 'acc control) -> 'acc

  val signature_item_count: t -> int

  val fold_sig_body_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val sig_body_token_count: t -> int
end

module ModuleExpr: sig
  type t = module_expr
  type view =
    | Ident of {
        ident: Ident.t;
      }
    | Structure of {
        body: Node.t;
      }
    | Functor of {
        body: Node.t;
      }
    | Apply of {
        body: Node.t;
        callee: t option;
        argument: t option;
      }
    | Constraint of {
        body: Node.t;
        expr: t option;
        ascription: module_type_expr option;
      }
    | Opaque of Node.t
    | Error of Node.t
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val struct_token: t -> Token.t option

  val end_token: t -> Token.t option

  val ident: t -> Ident.t option

  val fold_structure_item: t -> init:'acc -> fn:(structure_item -> 'acc -> 'acc control) -> 'acc

  val structure_item_count: t -> int
end

module StructureItem: sig
  type t = structure_item
  type view =
    | Let of let_declaration
    | Type of type_item
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Extension of extension_item
    | Attribute of attribute_item
    | Expr of expr_item
    | Error of Node.t
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val declaration: t -> Node.t option

  val fold_attribute_suffix: t -> init:'acc -> fn:(attribute_item -> 'acc -> 'acc control) -> 'acc

  val attribute_suffix_count: t -> int
end

module SignatureItem: sig
  type t = signature_item
  type view =
    | Value of value_declaration
    | Type of type_item
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Extension of extension_item
    | Attribute of attribute_item
    | Error of Node.t
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val declaration: t -> Node.t option

  val fold_attribute_suffix: t -> init:'acc -> fn:(attribute_item -> 'acc -> 'acc control) -> 'acc

  val attribute_suffix_count: t -> int
end

module LetDeclaration: sig
  type t = let_declaration

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val rec_token: t -> Token.t option

  val first_binding: t -> let_binding option

  val fold_binding: t -> init:'acc -> fn:(let_binding -> 'acc -> 'acc control) -> 'acc

  val binding_count: t -> int
end

module TypeDeclaration: sig
  type t = type_declaration
  type member
  type parameter =
    | Named of {
        name: Token.t;
        quote: Token.t option;
        variance: Token.t option;
        injective: Token.t option;
      }
    | Wildcard of {
        wildcard: Token.t;
        variance: Token.t option;
        injective: Token.t option;
      }

  module Member: sig
    type t = member

    val declaration: t -> type_declaration

    val start_index: t -> int

    val stop_index: t -> int

    val covers_declaration: t -> bool

    val child_count: t -> int

    val child_at: t -> int -> Syntax_tree.child option

    val child_token_at: t -> int -> Token.t option

    val child_node_at: t -> int -> Node.t option

    val child_token_kind_is: t -> int -> Syntax_kind.t -> bool

    val fold_child: t -> init:'acc -> fn:(Syntax_tree.child -> 'acc -> 'acc control) -> 'acc

    val fold_child_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

    val fold_child_node: t -> init:'acc -> fn:(Node.t -> 'acc -> 'acc control) -> 'acc

    val record_type: t -> record_type option

    val variant_type: t -> variant_type option

    val shell_token: t -> Token.t option

    val nonrec_token: t -> Token.t option

    val name: t -> Ident.t option

    val fold_parameter: t -> init:'acc -> fn:(parameter -> 'acc -> 'acc control) -> 'acc

    val parameter_count: t -> int

    val manifest: t -> type_expr option
  end

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val fold_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val token_count: t -> int

  val keyword_token: t -> Token.t option

  val nonrec_token: t -> Token.t option

  val name: t -> Ident.t option

  val fold_parameter: t -> init:'acc -> fn:(parameter -> 'acc -> 'acc control) -> 'acc

  val parameter_count: t -> int

  val manifest: t -> type_expr option

  val fold_member: t -> init:'acc -> fn:(member -> 'acc -> 'acc control) -> 'acc

  val member_count: t -> int

  val fold_members: t -> 'acc -> ('acc -> member -> 'acc) -> 'acc
end

module TypeExtensionDeclaration: sig
  type t = type_extension_declaration
  type parameter = TypeDeclaration.parameter

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val keyword_token: t -> Token.t option

  val plus_token: t -> Token.t option

  val equals_token: t -> Token.t option

  val name: t -> Ident.t option

  val fold_parameter: t -> init:'acc -> fn:(parameter -> 'acc -> 'acc control) -> 'acc

  val parameter_count: t -> int

  val variant_type: t -> variant_type option
end

module ModuleDeclaration: sig
  type t = module_declaration
  type member

  module Member: sig
    type t = member
    type functor_parameter = {
      name: Ident.t option;
      annotation: Ident.t option;
    }

    val declaration: t -> module_declaration

    val start_index: t -> int

    val stop_index: t -> int

    val child_count: t -> int

    val child_at: t -> int -> Syntax_tree.child option

    val child_token_at: t -> int -> Token.t option

    val child_node_at: t -> int -> Node.t option

    val child_token_kind_is: t -> int -> Syntax_kind.t -> bool

    val fold_child: t -> init:'acc -> fn:(Syntax_tree.child -> 'acc -> 'acc control) -> 'acc

    val fold_child_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

    val fold_child_node: t -> init:'acc -> fn:(Node.t -> 'acc -> 'acc control) -> 'acc

    val name: t -> Ident.t option

    val find_token: t -> Syntax_kind.t -> int option

    val find_node: t -> matches:(Syntax_kind.t -> bool) -> Node.t option

    val module_expr: t -> Node.t option

    val module_type: t -> Node.t option

    val fold_functor_parameter:
      t ->
      init:'acc ->
      fn:(functor_parameter -> 'acc -> 'acc control) ->
      'acc

    val functor_parameter_count: t -> int
  end

  type body =
    | Expr of {
        body: module_expr;
      }
    | Type of {
        body: module_type_expr;
      }
    | Unsupported of {
        body: Node.t option;
      }

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val name: t -> Ident.t option

  val rec_token: t -> Token.t option

  val is_recursive: t -> bool

  val separator_token: t -> Token.t option

  val fold_member: t -> init:'acc -> fn:(member -> 'acc -> 'acc control) -> 'acc

  val member_count: t -> int

  val fold_members: t -> 'acc -> ('acc -> member -> 'acc) -> 'acc

  val body: t -> body

  val struct_token: t -> Token.t option

  val sig_token: t -> Token.t option

  val end_token: t -> Token.t option

  val body_ident: t -> Ident.t option

  val has_typeof_body: t -> bool

  val typeof_body_ident: t -> Ident.t option

  val fold_structure_item: t -> init:'acc -> fn:(structure_item -> 'acc -> 'acc control) -> 'acc

  val structure_item_count: t -> int

  val fold_signature_item: t -> init:'acc -> fn:(signature_item -> 'acc -> 'acc control) -> 'acc

  val signature_item_count: t -> int

  val fold_sig_body_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val sig_body_token_count: t -> int
end

module ModuleTypeDeclaration: sig
  type t = module_type_declaration
  type body =
    | Abstract
    | Manifest of {
        body: module_type_expr;
      }
    | Unsupported of {
        body: Node.t option;
      }

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val name: t -> Ident.t option

  val equals_token: t -> Token.t option

  val fold_head_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val head_token_count: t -> int

  val body: t -> body

  val sig_token: t -> Token.t option

  val end_token: t -> Token.t option

  val body_ident: t -> Ident.t option

  val fold_signature_item: t -> init:'acc -> fn:(signature_item -> 'acc -> 'acc control) -> 'acc

  val signature_item_count: t -> int

  val fold_sig_body_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val sig_body_token_count: t -> int

  val base_module_type: t -> Node.t option

  val fold_constraint: t -> init:'acc -> fn:(module_type_constraint -> 'acc -> 'acc control) -> 'acc

  val constraint_count: t -> int
end

module ModuleTypeConstraint: sig
  type t = module_type_constraint
  type view =
    | Type of {
        ident: Ident.t;
        operator: Token.t;
        body: type_expr;
      }
    | Module of {
        ident: Ident.t;
        operator: Token.t;
        body: Node.t;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view
end

module OpenDeclaration: sig
  type t = open_declaration

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val ident: t -> Ident.t option
end

module IncludeDeclaration: sig
  type t = include_declaration

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val body_node: t -> Node.t option

  val body_ident: t -> Ident.t option
end

module ValueDeclaration: sig
  type t = value_declaration
  type view =
    | Value of {
        name: Ident.t;
        colon_token: Token.t;
        annotation: type_expr;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val name: t -> Ident.t option

  val colon_token: t -> Token.t option

  val type_annotation: t -> type_expr option

  val fold_annotation_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val annotation_token_count: t -> int
end

module ExternalDeclaration: sig
  type t = external_declaration
  type view =
    | External of {
        name: Ident.t;
        colon_token: Token.t;
        annotation: type_expr;
        equals_token: Token.t;
        primitives: Token.t Vector.t;
        attributes: Token.t Vector.t;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val name: t -> Ident.t option

  val colon_token: t -> Token.t option

  val type_annotation: t -> type_expr option

  val fold_primitive_string: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val primitive_string_count: t -> int

  val fold_attribute_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val attribute_token_count: t -> int
end

module ExceptionDeclaration: sig
  type t = exception_declaration
  type payload =
    | TypeExpr of type_expr
    | Record of record_type
  type view =
    | Bare
    | Alias of {
        equals_token: Token.t;
        ident: Ident.t;
      }
    | Payload of {
        of_token: Token.t;
        payload: payload;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val keyword_token: t -> Token.t option

  val name: t -> Ident.t option

  val view: t -> view
end

module ExtensionItem: sig
  type t = extension_item

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val fold_shell_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int
end

module AttributeItem: sig
  type t = attribute_item

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val fold_shell_token: t -> init:'acc -> fn:(Token.t -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int
end

module ExprItem: sig
  type t = expr_item

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val expr: t -> expr option
end

module Implementation: sig
  type t = implementation

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val fold_item: t -> init:'acc -> fn:(structure_item -> 'acc -> 'acc control) -> 'acc

  val item_count: t -> int
end

module Interface: sig
  type t = interface

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val fold_item: t -> init:'acc -> fn:(signature_item -> 'acc -> 'acc control) -> 'acc

  val item_count: t -> int
end

module SourceFile: sig
  type t = source_file
  type view =
    | Implementation of implementation
    | Interface of interface

  val make: Syntax_tree.t -> t

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val full_width: t -> int

  val view: t -> view

  val implementation: t -> implementation option

  val interface: t -> interface option

  val fold_item: t -> init:'acc -> fn:(Node.t -> 'acc -> 'acc control) -> 'acc

  val item_count: t -> int

  val fold_structure_item: t -> init:'acc -> fn:(structure_item -> 'acc -> 'acc control) -> 'acc

  val structure_item_count: t -> int

  val fold_signature_item: t -> init:'acc -> fn:(signature_item -> 'acc -> 'acc control) -> 'acc

  val signature_item_count: t -> int
end
