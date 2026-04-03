open Std
open Std.Collections

type syntax_node = (Syntax_kind.t, string) Ceibo.Red.syntax_node

type syntax_token = (Syntax_kind.t, string) Ceibo.Red.syntax_token

type syntax_trivia = (Syntax_kind.t, string) Ceibo.Red.syntax_trivia

type green_node = (Syntax_kind.t, string) Ceibo.Green.node

let is_trivia = fun kind -> let open Syntax_kind in kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

module Token = struct
  type fixed_operator =
    | BooleanAnd
    | BooleanOr
    | PipeForward
    | PrefixMinus
    | PrefixNegate

  type t = {
    syntax_token: syntax_token;
  }

  let syntax_token = fun token -> token.syntax_token

  let text = fun token -> Ceibo.Red.SyntaxToken.text token.syntax_token

  let full_text = fun token ->
    let leading = Ceibo.Red.SyntaxToken.leading_trivia token.syntax_token
    |> List.map Ceibo.Red.SyntaxTrivia.text
    |> String.concat "" in
    leading ^ text token

  let leading_trivia = fun token -> Ceibo.Red.SyntaxToken.leading_trivia token.syntax_token

  let span = fun token -> Ceibo.Red.SyntaxToken.span token.syntax_token

  let same_text = fun left right ->
    String.equal (text left) (text right)

  let fixed_operator = fun token ->
    match text token with
    | "&&" -> Some BooleanAnd
    | "||" -> Some BooleanOr
    | "|>" -> Some PipeForward
    | "-" -> Some PrefixMinus
    | "~-" -> Some PrefixNegate
    | _ -> None

  let is_keyword_operator_name = function
    | "mod"
    | "land"
    | "lor"
    | "lxor"
    | "lsl"
    | "lsr"
    | "asr"
    | "or" -> true
    | _ -> false

  let is_identifier_char = function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_'
    | '\'' -> true
    | _ -> false

  let is_operator_like_name = fun token ->
    let token_text = text token in
    let rec contains_non_identifier_char index =
      if index >= String.length token_text then
        false
      else if is_identifier_char token_text.[index] then
        contains_non_identifier_char (index + 1)
      else
        true
    in
    String.length token_text > 0
    && (is_keyword_operator_name token_text || contains_non_identifier_char 0)

  let is_identifier_like_name = fun token -> not (is_operator_like_name token)
end

type docstring_kind =
  | Ordinary
  | Section

type docstring = {
  syntax_node: syntax_node;
  docstring_token: Token.t;
  kind: docstring_kind;
}

type comment = {
  syntax_node: syntax_node;
  comment_token: Token.t;
}

type trivia =
  | Docstring of docstring
  | Comment of comment

module Ident = struct
  type t =
    | Ident of { syntax_node: syntax_node; name_token: Token.t }
    | Qualified of { syntax_node: syntax_node; prefix: t; dot_token: Token.t; name_token: Token.t }

  let syntax_node = function
    | Ident { syntax_node; _ } -> syntax_node
    | Qualified { syntax_node; _ } -> syntax_node

  let rec segments = function
    | Ident { name_token; _ } -> [ name_token ]
    | Qualified { prefix; name_token; _ } -> segments prefix @ [ name_token ]

  let last_segment = function
    | Ident { name_token; _ } -> Some name_token
    | Qualified { name_token; _ } -> Some name_token

  let name = fun path ->
    match last_segment path with
    | Some segment -> Some (Token.text segment)
    | None -> None

  let from_string = fun text ->
    let open Ceibo in
      let make_ident_segment segment =
        let green_token = Green.make_token
          ~leading_trivia:[]
          ~kind:Syntax_kind.IDENT_EXPR
          ~text:segment
          ~width:(String.length segment) in
        let syntax_token = Red.new_token
          green_token
          (Span.make ~start:0 ~end_:(String.length segment)) in
        { Token.syntax_token = syntax_token }
      in
      let make_node () = Green.make_node_list ~kind:Syntax_kind.PATH_EXPR [] |> Red.new_root in
      let segments = String.split_on_char '.' text in
      match segments with
      | []
      | [ "" ] -> raise (Failure "Syn.Cst.Ident.from_string requires a non-empty path")
      | first :: rest ->
          let first_token = make_ident_segment first in
          let first_width = String.length first in
          let first_node = make_node () in
          let initial = Ident { syntax_node = first_node; name_token = first_token } in
          List.fold_left
            (fun ((prefix, width)) segment ->
              if String.length segment = 0 then
                raise (Failure "Syn.Cst.Ident.from_string does not allow empty path segments");
              let name_token = make_ident_segment segment in
              let dot_token = make_ident_segment "." in
              let width = width + 1 + String.length segment in
              (Qualified { syntax_node = make_node (); prefix; dot_token; name_token }, width))
            (initial, first_width)
            rest |> fst

  let equal = fun left right ->
    let left_segments = segments left |> List.map Token.text in
    let right_segments = segments right |> List.map Token.text in
    List.equal String.equal left_segments right_segments
end

type attribute = {
  syntax_node: syntax_node;
  sigil_tokens: Token.t list;
  name: Ident.t;
  payload: payload option;
}

and extension = {
  syntax_node: syntax_node;
  sigil_tokens: Token.t list;
  name: Ident.t;
  payload: payload option;
  attributes: attribute list;
}

and payload =
  | Opaque of { tokens: Token.t list }

and object_type_field = {
  syntax_node: syntax_node;
  field_name: Token.t;
  colon_token: Token.t;
  field_type: core_type;
  semicolon_token: Token.t option;
}

and type_binder =
  | Quoted of { syntax_node: syntax_node; name_token: Token.t }
  | Bare of { name_token: Token.t }

and record_type_field = {
  syntax_node: syntax_node;
  field_name: Token.t;
  mutable_token: Token.t option;
  colon_token: Token.t;
  field_type: core_type;
  semicolon_token: Token.t option;
  attributes: attribute list;
}

and poly_variant_tag = {
  syntax_node: syntax_node;
  attributes: attribute list;
  bar_token: Token.t option;
  tag_name: Token.t;
  separator_token: Token.t option;
  payload_type: core_type option;
}

and poly_variant_bound =
  | Exact
  | UpperBound of { marker_token: Token.t }
  | LowerBound of { marker_token: Token.t }

and row_field =
  | Tag of poly_variant_tag
  | Inherit of { bar_token: Token.t option; syntax_node: syntax_node; type_: core_type }

and poly_variant = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  kind: poly_variant_bound;
  fields: row_field list;
  closing_token: Token.t;
}

and type_constraint = {
  syntax_node: syntax_node;
  left: core_type;
  equals_token: Token.t;
  right: core_type;
}

and private_flag =
  | Public
  | Private of { private_token: Token.t }

and module_type_constraint = {
  syntax_node: syntax_node;
  constrained_type: core_type;
  replacement_type: core_type;
  separator_token: Token.t;
}

and package_type = {
  syntax_node: syntax_node;
  module_type_path: Ident.t;
  constraints: module_type_constraint list;
  attribute: attribute option;
}

and functor_parameter = {
  syntax_node: syntax_node;
  name_token: Token.t;
  colon_token: Token.t;
  module_type: module_type;
}

and arrow_label =
  | Named of { sigil_token: Token.t option; label_token: Token.t; colon_token: Token.t }
  | OptionalNamed of { sigil_token: Token.t; label_token: Token.t; colon_token: Token.t }

and core_type =
  | Wildcard of { syntax_node: syntax_node; wildcard_token: Token.t }
  | Var of { syntax_node: syntax_node; sigil_token: Token.t option; name_token: Token.t }
  | Constr of { syntax_node: syntax_node; constructor_path: Ident.t; arguments: core_type list }
  | Class of {
      syntax_node: syntax_node;
      hash_token: Token.t;
      class_path: Ident.t;
      arguments: core_type list
    }
  | Alias of {
      syntax_node: syntax_node;
      type_: core_type;
      sigil_token: Token.t option;
      name_token: Token.t
    }
  | Attribute of { syntax_node: syntax_node; type_: core_type; attribute: attribute }
  | Extension of extension
  | Poly of {
      syntax_node: syntax_node;
      type_keyword_token: Token.t option;
      dot_token: Token.t;
      binders: type_binder list;
      body: core_type
    }
  | Arrow of {
      syntax_node: syntax_node;
      label: arrow_label option;
      parameter_type: core_type;
      result_type: core_type
    }
  | Tuple of { syntax_node: syntax_node; elements: core_type list }
  | Parenthesized of { syntax_node: syntax_node; inner: core_type }
  | PolyVariant of poly_variant
  | Record of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      fields: record_type_field list;
      separator_tokens: Token.t list;
      closing_token: Token.t
    }
  | FirstClassModule of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      package_type: package_type;
      closing_token: Token.t
    }
  | Object of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      fields: object_type_field list;
      closing_token: Token.t
    }

and module_type =
  | Path of Ident.t
  | TypeOf of { syntax_node: syntax_node; of_token: Token.t; module_path: Ident.t }
  | Signature of { syntax_node: syntax_node; signature_syntax_node: syntax_node }
  | Functor of { syntax_node: syntax_node; parameters: functor_parameter list; result: module_type }
  | With of { syntax_node: syntax_node; base: module_type; constraints: module_type_constraint list }
  | Parenthesized of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      inner: module_type;
      closing_token: Token.t
    }
  | Attribute of { syntax_node: syntax_node; module_type: module_type; attribute: attribute }
  | Extension of extension

and class_type =
  | Path of Ident.t
  | Signature of { syntax_node: syntax_node; fields: class_type_field list }
  | Arrow of {
      syntax_node: syntax_node;
      label: arrow_label option;
      parameter_type: core_type;
      result_type: class_type
    }
  | Parenthesized of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      inner: class_type;
      closing_token: Token.t
    }
  | Attribute of { syntax_node: syntax_node; class_type: class_type; attribute: attribute }
  | Extension of extension

and class_type_field =
  | Inherit of { syntax_node: syntax_node; class_type: class_type }
  | Value of {
      syntax_node: syntax_node;
      name_token: Token.t;
      colon_token: Token.t;
      type_: core_type;
      modifier_tokens: Token.t list
    }
  | Method of {
      syntax_node: syntax_node;
      name_token: Token.t;
      colon_token: Token.t;
      type_: core_type;
      modifier_tokens: Token.t list
    }
  | Constraint of {
      syntax_node: syntax_node;
      left: core_type;
      equals_token: Token.t;
      right: core_type
    }
  | Attribute of { syntax_node: syntax_node; field: class_type_field; attribute: attribute }
  | Extension of extension

module CoreType = struct
  type t = core_type =
    | Wildcard of { syntax_node: syntax_node; wildcard_token: Token.t }
    | Var of { syntax_node: syntax_node; sigil_token: Token.t option; name_token: Token.t }
    | Constr of { syntax_node: syntax_node; constructor_path: Ident.t; arguments: core_type list }
    | Class of {
        syntax_node: syntax_node;
        hash_token: Token.t;
        class_path: Ident.t;
        arguments: core_type list
      }
    | Alias of {
        syntax_node: syntax_node;
        type_: core_type;
        sigil_token: Token.t option;
        name_token: Token.t
      }
    | Attribute of { syntax_node: syntax_node; type_: core_type; attribute: attribute }
    | Extension of extension
    | Poly of {
        syntax_node: syntax_node;
        type_keyword_token: Token.t option;
        dot_token: Token.t;
        binders: type_binder list;
        body: core_type
      }
    | Arrow of {
        syntax_node: syntax_node;
        label: arrow_label option;
        parameter_type: core_type;
        result_type: core_type
      }
    | Tuple of { syntax_node: syntax_node; elements: core_type list }
    | Parenthesized of { syntax_node: syntax_node; inner: core_type }
    | PolyVariant of poly_variant
    | Record of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        fields: record_type_field list;
        separator_tokens: Token.t list;
        closing_token: Token.t
      }
    | FirstClassModule of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        package_type: package_type;
        closing_token: Token.t
      }
    | Object of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        fields: object_type_field list;
        closing_token: Token.t
      }

  let syntax_node = function
    | Wildcard { syntax_node; _ }
    | Var { syntax_node; _ }
    | Constr { syntax_node; _ }
    | Class { syntax_node; _ }
    | Alias { syntax_node; _ }
    | Attribute { syntax_node; _ }
    | Extension { syntax_node; _ }
    | Poly { syntax_node; _ }
    | Arrow { syntax_node; _ }
    | Tuple { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | Record { syntax_node; _ }
    | FirstClassModule { syntax_node; _ }
    | Object { syntax_node; _ } -> syntax_node
    | PolyVariant poly_variant -> poly_variant.syntax_node
end

module ModuleTypeConstraint = struct
  type t = module_type_constraint = {
    syntax_node: syntax_node;
    constrained_type: core_type;
    replacement_type: core_type;
    separator_token: Token.t;
  }
end

module TypeConstraint = struct
  type t = type_constraint = {
    syntax_node: syntax_node;
    left: core_type;
    equals_token: Token.t;
    right: core_type;
  }
end

module ArrowLabel = struct
  type t = arrow_label =
    | Named of { sigil_token: Token.t option; label_token: Token.t; colon_token: Token.t }
    | OptionalNamed of { sigil_token: Token.t; label_token: Token.t; colon_token: Token.t }

  let sigil_token = function
    | Named { sigil_token; _ } -> sigil_token
    | OptionalNamed { sigil_token; _ } -> Some sigil_token

  let label_token = function
    | Named { label_token; _ }
    | OptionalNamed { label_token; _ } -> label_token

  let colon_token = function
    | Named { colon_token; _ }
    | OptionalNamed { colon_token; _ } -> colon_token

  let name = fun label -> Token.text (label_token label)

  let is_optional = function
    | Named _ -> false
    | OptionalNamed _ -> true
end

module FunctorParameter = struct
  type t = functor_parameter = {
    syntax_node: syntax_node;
    name_token: Token.t;
    colon_token: Token.t;
    module_type: module_type;
  }
end

module ModuleType = struct
  type t = module_type =
    | Path of Ident.t
    | TypeOf of { syntax_node: syntax_node; of_token: Token.t; module_path: Ident.t }
    | Signature of { syntax_node: syntax_node; signature_syntax_node: syntax_node }
    | Functor of {
        syntax_node: syntax_node;
        parameters: functor_parameter list;
        result: module_type
      }
    | With of {
        syntax_node: syntax_node;
        base: module_type;
        constraints: module_type_constraint list
      }
    | Parenthesized of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        inner: module_type;
        closing_token: Token.t
      }
    | Attribute of { syntax_node: syntax_node; module_type: module_type; attribute: attribute }
    | Extension of extension

  let syntax_node = function
    | Path path -> Ident.syntax_node path
    | TypeOf { syntax_node; _ }
    | Signature { syntax_node; _ }
    | Functor { syntax_node; _ }
    | With { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | Attribute { syntax_node; _ } -> syntax_node
    | Extension extension -> extension.syntax_node
end

module ClassType = struct
  type t = class_type =
    | Path of Ident.t
    | Signature of { syntax_node: syntax_node; fields: class_type_field list }
    | Arrow of {
        syntax_node: syntax_node;
        label: arrow_label option;
        parameter_type: core_type;
        result_type: class_type
      }
    | Parenthesized of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        inner: class_type;
        closing_token: Token.t
      }
    | Attribute of { syntax_node: syntax_node; class_type: class_type; attribute: attribute }
    | Extension of extension

  let syntax_node = function
    | Path path -> Ident.syntax_node path
    | Signature { syntax_node; _ }
    | Arrow { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | Attribute { syntax_node; _ } -> syntax_node
    | Extension extension -> extension.syntax_node
end

module ClassTypeField = struct
  type t = class_type_field =
    | Inherit of { syntax_node: syntax_node; class_type: class_type }
    | Value of {
        syntax_node: syntax_node;
        name_token: Token.t;
        colon_token: Token.t;
        type_: core_type;
        modifier_tokens: Token.t list
      }
    | Method of {
        syntax_node: syntax_node;
        name_token: Token.t;
        colon_token: Token.t;
        type_: core_type;
        modifier_tokens: Token.t list
      }
    | Constraint of {
        syntax_node: syntax_node;
        left: core_type;
        equals_token: Token.t;
        right: core_type
      }
    | Attribute of { syntax_node: syntax_node; field: class_type_field; attribute: attribute }
    | Extension of extension

  let syntax_node = function
    | Inherit { syntax_node; _ }
    | Value { syntax_node; _ }
    | Method { syntax_node; _ }
    | Constraint { syntax_node; _ }
    | Attribute { syntax_node; _ } -> syntax_node
    | Extension extension -> extension.syntax_node
end

type string_delimiter =
  | DoubleQuote
  | Quoted of { marker: string }

type integer_base =
  | Decimal
  | Hexadecimal
  | Octal
  | Binary

type exponent_sign =
  | Positive
  | Negative

type string_constant = {
  syntax_node: syntax_node;
  literal_token: Token.t;
  delimiter: string_delimiter;
  contents: string;
  terminated: bool;
  attributes: attribute list;
}

type integer_constant = {
  syntax_node: syntax_node;
  sign_token: Token.t option;
  literal_token: Token.t;
  base: integer_base;
  prefix: string option;
  digits: string;
  suffix: string option;
  attributes: attribute list;
}

type float_exponent = {
  marker: string;
  sign: exponent_sign option;
  digits: string;
}

type float_constant = {
  syntax_node: syntax_node;
  sign_token: Token.t option;
  literal_token: Token.t;
  integral_digits: string;
  fractional_digits: string;
  exponent: float_exponent option;
  suffix: string option;
  attributes: attribute list;
}

type char_constant = {
  syntax_node: syntax_node;
  literal_token: Token.t;
  contents: string;
  attributes: attribute list;
}

type bool_constant = {
  syntax_node: syntax_node;
  literal_token: Token.t;
  value: bool;
  attributes: attribute list;
}

type constant =
  | String of string_constant
  | Int of integer_constant
  | Float of float_constant
  | Char of char_constant
  | Bool of bool_constant
  | Unit of { syntax_node: syntax_node; attributes: attribute list }

module Constant = struct
  type t = constant =
    | String of string_constant
    | Int of integer_constant
    | Float of float_constant
    | Char of char_constant
    | Bool of bool_constant
    | Unit of { syntax_node: syntax_node; attributes: attribute list }

  let syntax_node = function
    | String { syntax_node; _ }
    | Int { syntax_node; _ }
    | Float { syntax_node; _ }
    | Char { syntax_node; _ }
    | Bool { syntax_node; _ }
    | Unit { syntax_node; _ } -> syntax_node

  let attributes = function
    | String { attributes; _ }
    | Int { attributes; _ }
    | Float { attributes; _ }
    | Char { attributes; _ }
    | Bool { attributes; _ }
    | Unit { attributes; _ } -> attributes
end

module PatternLiteral = Constant

module TypeBinder = struct
  type t = type_binder =
    | Quoted of { syntax_node: syntax_node; name_token: Token.t }
    | Bare of { name_token: Token.t }

  let name_token = function
    | Quoted { name_token; _ }
    | Bare { name_token } -> name_token

  let name = fun binder -> Token.text (name_token binder)

  let text = function
    | Quoted { name_token; _ } -> "'" ^ Token.text name_token
    | Bare { name_token } -> Token.text name_token

  let is_quoted = function
    | Quoted _ -> true
    | Bare _ -> false
end

type pattern_literal = PatternLiteral.t

type pattern =
  | Identifier of identifier_pattern
  | Wildcard of wildcard_pattern
  | Extension of extension_pattern
  | Literal of literal_pattern
  | Lazy of lazy_pattern
  | Exception of exception_pattern
  | Range of range_pattern
  | Operator of operator_pattern
  | FirstClassModule of first_class_module_pattern
  | PolyVariant of poly_variant_pattern
  | PolyVariantInherit of poly_variant_inherit_pattern
  | Constructor of constructor_pattern
  | Tuple of tuple_pattern
  | List of list_pattern
  | Array of array_pattern
  | Record of record_pattern
  | Cons of cons_pattern
  | Or of or_pattern
  | Alias of alias_pattern
  | Typed of typed_pattern
  | Effect of effect_pattern
  | LocalOpen of local_open_pattern
  | Parenthesized of parenthesized_pattern

and identifier_pattern = {
  syntax_node: syntax_node;
  name_token: Token.t;
  attributes: attribute list;
}

and wildcard_pattern = {
  syntax_node: syntax_node;
  attributes: attribute list;
}

and literal_pattern = {
  syntax_node: syntax_node;
  literal: pattern_literal;
  attributes: attribute list;
}

and extension_pattern = {
  syntax_node: syntax_node;
  extension: extension;
  attributes: attribute list;
}

and lazy_pattern = {
  syntax_node: syntax_node;
  pattern: pattern;
  attributes: attribute list;
}

and exception_pattern = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  pattern: pattern;
  attributes: attribute list;
}

and range_pattern = {
  syntax_node: syntax_node;
  lower: pattern_literal;
  upper: pattern_literal;
  attributes: attribute list;
}

and operator_pattern = {
  syntax_node: syntax_node;
  operator_tokens: Token.t list;
  attributes: attribute list;
}

and first_class_module_pattern_binding =
  | Named of { name_token: Token.t }
  | Anonymous of { wildcard_token: Token.t }

and first_class_module_pattern = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  binding: first_class_module_pattern_binding;
  colon_token: Token.t option;
  package_type: package_type option;
  closing_token: Token.t;
  attributes: attribute list;
}

and poly_variant_pattern = {
  syntax_node: syntax_node;
  tag_token: Token.t;
  payload: pattern option;
  attributes: attribute list;
}

and poly_variant_inherit_pattern = {
  syntax_node: syntax_node;
  type_path: Ident.t;
  attributes: attribute list;
}

and constructor_pattern_existentials = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  type_keyword_token: Token.t;
  binders: type_binder list;
  closing_token: Token.t;
}

and constructor_pattern = {
  syntax_node: syntax_node;
  constructor_path: Ident.t;
  existentials: constructor_pattern_existentials option;
  arguments: pattern list;
  attributes: attribute list;
}

and tuple_pattern = {
  syntax_node: syntax_node;
  elements: tuple_pattern_element list;
  open_tail: tuple_pattern_open_tail option;
  attributes: attribute list;
}

and tuple_pattern_element = {
  label_token: Token.t option;
  pattern: pattern;
}

and tuple_pattern_open_tail = {
  dotdot_token: Token.t;
}

and list_pattern = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  elements: pattern list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

and array_pattern = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  elements: pattern list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

and record_pattern = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  fields: record_pattern_field list;
  separator_tokens: Token.t list;
  closedness: record_pattern_closedness;
  closing_token: Token.t;
  attributes: attribute list;
}

and record_pattern_closedness =
  | Closed
  | Open of { wildcard_token: Token.t }

and record_pattern_field = {
  syntax_node: syntax_node;
  field_path: Ident.t;
  equals_token: Token.t option;
  pattern: pattern option;
}

and cons_pattern = {
  syntax_node: syntax_node;
  head: pattern;
  tail: pattern;
  attributes: attribute list;
}

and or_pattern = {
  syntax_node: syntax_node;
  alternatives: pattern list;
  separator_tokens: Token.t list;
  attributes: attribute list;
}

and alias_pattern = {
  syntax_node: syntax_node;
  pattern: pattern;
  name_token: Token.t;
  attributes: attribute list;
}

and typed_pattern = {
  syntax_node: syntax_node;
  pattern: pattern;
  colon_token: Token.t;
  type_: core_type;
  attributes: attribute list;
}

and effect_pattern = {
  syntax_node: syntax_node;
  effect_pattern: pattern;
  continuation: pattern;
  attributes: attribute list;
}

and local_open_pattern = {
  syntax_node: syntax_node;
  module_path: Ident.t;
  dot_token: Token.t;
  opening_token: Token.t option;
  pattern: pattern;
  closing_token: Token.t option;
  attributes: attribute list;
}

and parenthesized_pattern = {
  syntax_node: syntax_node;
  inner: pattern;
  attributes: attribute list;
}

module Literal = Constant

type literal = Literal.t

and positional_parameter = {
  syntax_node: syntax_node;
  pattern: pattern;
  name_token: Token.t option;
}

and labeled_parameter = {
  syntax_node: syntax_node;
  sigil_token: Token.t;
  label_token: Token.t;
  colon_token: Token.t option;
  binding_name_token: Token.t option;
  binding_name_matches_label: bool;
  binding_pattern: pattern option;
}

and optional_parameter = {
  syntax_node: syntax_node;
  sigil_token: Token.t;
  label_token: Token.t;
  colon_token: Token.t option;
  equals_token: Token.t option;
  binding_name_token: Token.t option;
  binding_name_matches_label: bool;
  default_value: expression option;
  binding_pattern: pattern option;
}

and locally_abstract_type_parameter = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  type_keyword_token: Token.t;
  binders: type_binder list;
  closing_token: Token.t;
}

and parameter =
  | Positional of positional_parameter
  | Labeled of labeled_parameter
  | Optional of optional_parameter
  | LocallyAbstract of locally_abstract_type_parameter

and exception_rhs =
  | Alias of { equals_token: Token.t; alias: Ident.t }
  | Payload of { of_token: Token.t; payload_type: core_type }

and exception_declaration = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  name_token: Token.t;
  rhs: exception_rhs option;
}

and expression =
  | Path of path_expression
  | Constructor of constructor_expression
  | Operator of operator_expression
  | Literal of literal
  | Unreachable of unreachable_expression
  | Extension of extension
  | Object of object_expression
  | PolyVariant of poly_variant_expression
  | ModulePack of module_pack_expression
  | LetModule of let_module_expression
  | LetException of let_exception_expression
  | Assert of assert_expression
  | Lazy of lazy_expression
  | While of while_expression
  | For of for_expression
  | Apply of apply_expression
  | MethodCall of method_call_expression
  | New of new_expression
  | Prefix of prefix_expression
  | FieldAccess of field_access_expression
  | Index of index_expression
  | ObjectOverride of object_override_expression
  | InstanceVariableAssign of instance_variable_assign_expression
  | FieldAssign of field_assign_expression
  | Assign of assign_expression
  | Infix of infix_expression
  | TypeAscription of type_ascription_expression
  | Polymorphic of polymorphic_expression
  | Sequence of sequence_expression
  | Tuple of tuple_expression
  | List of list_expression
  | Array of array_expression
  | Record of record_expression
  | LocalOpen of local_open_expression
  | Fun of fun_expression
  | Function of function_expression
  | LetOperator of let_operator_expression
  | Let of let_expression
  | Match of match_expression
  | Try of try_expression
  | If of if_expression
  | Parenthesized of parenthesized_expression

and path_expression = {
  syntax_node: syntax_node;
  path: Ident.t;
  attributes: attribute list;
}

and constructor_expression = {
  syntax_node: syntax_node;
  constructor_path: Ident.t;
  payload: expression option;
  attributes: attribute list;
}

and operator_expression = {
  syntax_node: syntax_node;
  operator_tokens: Token.t list;
  attributes: attribute list;
}

and unreachable_expression = {
  syntax_node: syntax_node;
  dot_token: Token.t;
  attributes: attribute list;
}

and object_expression = {
  syntax_node: syntax_node;
  self_pattern: pattern option;
  members: object_member list;
  attributes: attribute list;
}

and object_member =
  | Method of object_method
  | Value of object_value
  | Inherit of object_inherit
  | Extension of extension
  | Initializer of object_initializer

and method_definition =
  | ConcreteMethod of { body: expression; type_: (Token.t * core_type) option }
  | VirtualMethod of { virtual_token: Token.t; type_: core_type }

and value_definition =
  | ConcreteValue of { value: expression; type_: (Token.t * core_type) option }
  | VirtualValue of { virtual_token: Token.t; type_: core_type }

and object_method = {
  syntax_node: syntax_node;
  attributes: attribute list;
  name_token: Token.t;
  body: expression;
  equals_token: Token.t;
  type_: core_type option;
  colon_token: Token.t option;
  modifier_tokens: Token.t list;
}

and object_value = {
  syntax_node: syntax_node;
  attributes: attribute list;
  name_token: Token.t;
  value: expression;
  equals_token: Token.t;
  type_: core_type option;
  colon_token: Token.t option;
  modifier_tokens: Token.t list;
}

and object_inherit = {
  syntax_node: syntax_node;
  attributes: attribute list;
  expression: expression;
}

and object_initializer = {
  syntax_node: syntax_node;
  body: expression;
}

and poly_variant_expression = {
  syntax_node: syntax_node;
  tag_token: Token.t;
  payload: expression option;
  attributes: attribute list;
}

and module_pack_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  module_expression: module_expression;
  colon_token: Token.t option;
  package_type: package_type option;
  closing_token: Token.t;
  attributes: attribute list;
}

and let_module_expression = {
  syntax_node: syntax_node;
  module_name_token: Token.t;
  equals_token: Token.t;
  module_expression: module_expression;
  body: expression;
  attributes: attribute list;
}

and let_exception_expression = {
  syntax_node: syntax_node;
  exception_declaration: exception_declaration;
  body: expression;
  attributes: attribute list;
}

and assert_expression = {
  syntax_node: syntax_node;
  asserted: expression;
  attributes: attribute list;
}

and lazy_expression = {
  syntax_node: syntax_node;
  body: expression;
  attributes: attribute list;
}

and while_expression = {
  syntax_node: syntax_node;
  condition: expression;
  body: expression;
  attributes: attribute list;
}

and for_direction =
  | To of { direction_token: Token.t }
  | Downto of { direction_token: Token.t }

and for_expression = {
  syntax_node: syntax_node;
  iterator_token: Token.t;
  equals_token: Token.t;
  start_expr: expression;
  direction: for_direction;
  end_expr: expression;
  body: expression;
  attributes: attribute list;
}

and apply_argument =
  | Positional of expression
  | Labeled of labeled_apply_argument
  | Optional of optional_apply_argument

and labeled_apply_argument = {
  syntax_node: syntax_node;
  sigil_token: Token.t;
  label_token: Token.t;
  value: expression option;
}

and optional_apply_argument = {
  syntax_node: syntax_node;
  sigil_token: Token.t;
  label_token: Token.t;
  value: expression option;
}

and apply_expression = {
  syntax_node: syntax_node;
  callee: expression;
  argument: apply_argument;
  attributes: attribute list;
}

and method_call_expression = {
  syntax_node: syntax_node;
  receiver: expression;
  hash_token: Token.t;
  method_name: Token.t;
  attributes: attribute list;
}

and new_expression = {
  syntax_node: syntax_node;
  class_path: Ident.t;
  attributes: attribute list;
}

and prefix_expression = {
  syntax_node: syntax_node;
  operator_token: Token.t;
  operand: expression;
  attributes: attribute list;
}

and field_access_expression = {
  syntax_node: syntax_node;
  receiver: expression;
  field_name: Token.t;
  attributes: attribute list;
}

and index_expression = {
  syntax_node: syntax_node;
  collection: expression;
  opening_tokens: Token.t list;
  index: expression;
  closing_token: Token.t;
  attributes: attribute list;
}

and object_override_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  fields: object_override_field list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

and instance_variable_assign_expression = {
  syntax_node: syntax_node;
  name_token: Token.t;
  operator_token: Token.t;
  value: expression;
  attributes: attribute list;
}

and field_assign_expression = {
  syntax_node: syntax_node;
  target: field_access_expression;
  operator_token: Token.t;
  value: expression;
  attributes: attribute list;
}

and assign_expression = {
  syntax_node: syntax_node;
  target: expression;
  operator_token: Token.t;
  value: expression;
  attributes: attribute list;
}

and infix_expression = {
  syntax_node: syntax_node;
  left: expression;
  operator_tokens: Token.t list;
  right: expression;
  attributes: attribute list;
}

and type_ascription_expression = {
  syntax_node: syntax_node;
  expression: expression;
  kind: type_ascription_kind;
  attributes: attribute list;
}

and type_ascription_kind =
  | Type of { colon_token: Token.t; type_: core_type }
  | Coerce of { coercion_tokens: Token.t list; type_: core_type }
  | ConstraintCoerce of {
      colon_token: Token.t;
      from_type: core_type;
      coercion_tokens: Token.t list;
      to_type: core_type
    }

and polymorphic_expression = {
  syntax_node: syntax_node;
  expression: expression;
  colon_token: Token.t;
  type_: core_type;
  attributes: attribute list;
}

and sequence_expression = {
  syntax_node: syntax_node;
  separator_token: Token.t;
  separator_tokens: Token.t list;
  expressions: expression list;
  attributes: attribute list;
}

and tuple_expression = {
  syntax_node: syntax_node;
  elements: expression list;
  attributes: attribute list;
}

and list_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  elements: expression list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

and array_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  elements: expression list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

and record_expression =
  | Literal of record_literal_expression
  | Update of record_update_expression

and record_literal_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  fields: record_expression_field list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

and record_update_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  base: expression;
  with_token: Token.t;
  fields: record_expression_field list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

and record_expression_field_source =
  | Explicit
  | Punned

and record_expression_field = {
  syntax_node: syntax_node;
  field_path: Ident.t;
  field_name: Token.t;
  equals_token: Token.t option;
  value: expression;
  source: record_expression_field_source;
}

and object_override_field = {
  syntax_node: syntax_node;
  field_name: Token.t;
  equals_token: Token.t option;
  value: expression option;
}

and local_open_expression =
  | LetOpen of {
      syntax_node: syntax_node;
      let_token: Token.t;
      open_token: Token.t;
      module_path: Ident.t;
      in_token: Token.t;
      body: expression;
      attributes: attribute list
    }
  | Delimited of {
      syntax_node: syntax_node;
      module_path: Ident.t;
      dot_token: Token.t;
      opening_token: Token.t option;
      body: expression;
      closing_token: Token.t option;
      attributes: attribute list
    }

and function_case_body = {
  syntax_node: syntax_node;
  cases: match_case list;
}

and fun_body =
  | Expression of expression
  | Cases of function_case_body

and fun_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  colon_token: Token.t option;
  arrow_token: Token.t;
  parameters: parameter list;
  return_type: core_type option;
  body: fun_body;
  attributes: attribute list;
}

and function_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  cases: match_case list;
  attributes: attribute list;
}

and let_binding = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  rec_token: Token.t option;
  equals_token: Token.t;
  attributes: attribute list;
  binding_pattern: pattern;
  parameters: parameter list;
  value: expression;
  and_binding: let_binding option;
}

and binding_operator_binding = {
  keyword_token: Token.t;
  operator_token: Token.t;
  equals_token: Token.t;
  binding_pattern: pattern;
  bound_value: expression;
  and_binding: binding_operator_binding option;
}

and let_operator_expression = {
  syntax_node: syntax_node;
  binding: binding_operator_binding;
  in_token: Token.t;
  body: expression;
  attributes: attribute list;
}

and let_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  rec_token: Token.t option;
  equals_token: Token.t;
  in_token: Token.t;
  binding_pattern: pattern;
  parameters: parameter list;
  bound_value: expression;
  and_binding: let_binding option;
  body: expression;
  attributes: attribute list;
}

and match_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  with_token: Token.t;
  scrutinee: expression;
  cases: match_case list;
  attributes: attribute list;
}

and try_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  with_token: Token.t;
  body: expression;
  cases: match_case list;
  attributes: attribute list;
}

and match_case = {
  syntax_node: syntax_node;
  bar_token: Token.t option;
  when_token: Token.t option;
  arrow_token: Token.t;
  pattern: pattern;
  guard: expression option;
  body: expression;
}

and if_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  then_token: Token.t;
  else_token: Token.t option;
  condition: expression;
  then_branch: expression;
  else_branch: expression option;
  attributes: attribute list;
}

and expression_grouping =
  | Parens
  | BeginEnd

and parenthesized_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  closing_token: Token.t;
  grouping: expression_grouping;
  inner: expression;
  attributes: attribute list;
}

and class_expression =
  | Path of Ident.t
  | Structure of class_structure
  | Fun of class_fun_expression
  | Apply of class_apply_expression
  | Let of class_let_expression
  | Constraint of class_constraint_expression
  | LocalOpen of local_open_class_expression
  | Parenthesized of parenthesized_class_expression
  | Attribute of {
      syntax_node: syntax_node;
      class_expression: class_expression;
      attribute: attribute
    }
  | Extension of extension

and class_structure = {
  syntax_node: syntax_node;
  self_pattern: pattern option;
  fields: class_field list;
}

and class_field =
  | Method of class_method
  | Value of class_value
  | Inherit of class_inherit
  | Constraint of class_constraint
  | Initializer of class_initializer
  | Attribute of { syntax_node: syntax_node; field: class_field; attribute: attribute }
  | Extension of extension

and class_method = {
  syntax_node: syntax_node;
  name_token: Token.t;
  concrete_equals_token: Token.t option;
  virtual_colon_token: Token.t option;
  definition: method_definition;
  modifier_tokens: Token.t list;
}

and class_value = {
  syntax_node: syntax_node;
  name_token: Token.t;
  concrete_equals_token: Token.t option;
  virtual_colon_token: Token.t option;
  definition: value_definition;
  modifier_tokens: Token.t list;
}

and class_inherit = {
  syntax_node: syntax_node;
  class_expression: class_expression;
}

and class_constraint = {
  syntax_node: syntax_node;
  left: core_type;
  equals_token: Token.t;
  right: core_type;
}

and class_initializer = {
  syntax_node: syntax_node;
  body: expression;
}

and class_apply_expression = {
  syntax_node: syntax_node;
  callee: class_expression;
  argument: apply_argument;
}

and class_fun_expression = {
  syntax_node: syntax_node;
  parameters: parameter list;
  body: class_expression;
}

and class_let_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  rec_token: Token.t option;
  equals_token: Token.t;
  in_token: Token.t;
  binding_pattern: pattern;
  parameters: parameter list;
  bound_value: expression;
  and_binding: let_binding option;
  body: class_expression;
}

and class_constraint_expression = {
  syntax_node: syntax_node;
  class_expression: class_expression;
  colon_token: Token.t;
  class_type: class_type;
}

and local_open_class_expression =
  | LetOpen of {
      syntax_node: syntax_node;
      let_token: Token.t;
      open_token: Token.t;
      module_path: Ident.t;
      in_token: Token.t;
      body: class_expression
    }
  | Delimited of {
      syntax_node: syntax_node;
      module_path: Ident.t;
      dot_token: Token.t;
      opening_token: Token.t option;
      body: class_expression;
      closing_token: Token.t option
    }

and parenthesized_class_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  inner: class_expression;
  closing_token: Token.t;
}

and module_expression =
  | Path of Ident.t
  | Structure of { syntax_node: syntax_node; item_syntax_nodes: syntax_node list }
  | Functor of {
      syntax_node: syntax_node;
      parameters: functor_parameter list;
      body: module_expression
    }
  | Apply of { syntax_node: syntax_node; callee: module_expression; argument: module_expression }
  | ApplyUnit of { syntax_node: syntax_node; callee: module_expression }
  | Constraint of {
      syntax_node: syntax_node;
      module_expression: module_expression;
      colon_token: Token.t;
      module_type: module_type
    }
  | ModuleUnpack of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      expression: expression;
      colon_token: Token.t option;
      package_type: package_type option;
      closing_token: Token.t
    }
  | Parenthesized of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      inner: module_expression;
      closing_token: Token.t
    }
  | Attribute of {
      syntax_node: syntax_node;
      module_expression: module_expression;
      attribute: attribute
    }
  | Extension of extension

module Expression = struct
  type t = expression =
    | Path of path_expression
    | Constructor of constructor_expression
    | Operator of operator_expression
    | Literal of literal
    | Unreachable of unreachable_expression
    | Extension of extension
    | Object of object_expression
    | PolyVariant of poly_variant_expression
    | ModulePack of module_pack_expression
    | LetModule of let_module_expression
    | LetException of let_exception_expression
    | Assert of assert_expression
    | Lazy of lazy_expression
    | While of while_expression
    | For of for_expression
    | Apply of apply_expression
    | MethodCall of method_call_expression
    | New of new_expression
    | Prefix of prefix_expression
    | FieldAccess of field_access_expression
    | Index of index_expression
    | ObjectOverride of object_override_expression
    | InstanceVariableAssign of instance_variable_assign_expression
    | FieldAssign of field_assign_expression
    | Assign of assign_expression
    | Infix of infix_expression
    | TypeAscription of type_ascription_expression
    | Polymorphic of polymorphic_expression
    | Sequence of sequence_expression
    | Tuple of tuple_expression
    | List of list_expression
    | Array of array_expression
    | Record of record_expression
    | LocalOpen of local_open_expression
    | Fun of fun_expression
    | Function of function_expression
    | LetOperator of let_operator_expression
    | Let of let_expression
    | Match of match_expression
    | Try of try_expression
    | If of if_expression
    | Parenthesized of parenthesized_expression

  let syntax_node = function
    | Path expr ->
        expr.syntax_node
    | Constructor expr ->
        expr.syntax_node
    | Operator expr ->
        expr.syntax_node
    | Literal literal ->
        Constant.syntax_node literal
    | Unreachable expr ->
        expr.syntax_node
    | Extension ext ->
        ext.syntax_node
    | Object expr ->
        expr.syntax_node
    | PolyVariant expr ->
        expr.syntax_node
    | ModulePack expr ->
        expr.syntax_node
    | LetModule expr ->
        expr.syntax_node
    | LetException expr ->
        expr.syntax_node
    | Assert expr ->
        expr.syntax_node
    | Lazy expr ->
        expr.syntax_node
    | While expr ->
        expr.syntax_node
    | For expr ->
        expr.syntax_node
    | Apply expr ->
        expr.syntax_node
    | MethodCall expr ->
        expr.syntax_node
    | New expr ->
        expr.syntax_node
    | Prefix expr ->
        expr.syntax_node
    | FieldAccess expr ->
        expr.syntax_node
    | Index expr ->
        expr.syntax_node
    | ObjectOverride expr ->
        expr.syntax_node
    | InstanceVariableAssign expr ->
        expr.syntax_node
    | FieldAssign expr ->
        expr.syntax_node
    | Assign expr ->
        expr.syntax_node
    | Infix expr ->
        expr.syntax_node
    | TypeAscription expr ->
        expr.syntax_node
    | Polymorphic expr ->
        expr.syntax_node
    | Sequence expr ->
        expr.syntax_node
    | Tuple expr ->
        expr.syntax_node
    | List expr ->
        expr.syntax_node
    | Array expr ->
        expr.syntax_node
    | Record expr -> (
        match expr with
        | Literal record -> record.syntax_node
        | Update record -> record.syntax_node
      )
    | LocalOpen expr -> (
        match expr with
        | LetOpen { syntax_node; _ }
        | Delimited { syntax_node; _ } -> syntax_node
      )
    | Fun expr ->
        expr.syntax_node
    | Function expr ->
        expr.syntax_node
    | LetOperator expr ->
        expr.syntax_node
    | Let expr ->
        expr.syntax_node
    | Match expr ->
        expr.syntax_node
    | Try expr ->
        expr.syntax_node
    | If expr ->
        expr.syntax_node
    | Parenthesized expr ->
        expr.syntax_node

  let attributes = function
    | Path expr ->
        expr.attributes
    | Constructor expr ->
        expr.attributes
    | Operator expr ->
        expr.attributes
    | Literal literal ->
        Constant.attributes literal
    | Unreachable expr ->
        expr.attributes
    | Extension ext ->
        ext.attributes
    | Object expr ->
        expr.attributes
    | PolyVariant expr ->
        expr.attributes
    | ModulePack expr ->
        expr.attributes
    | LetModule expr ->
        expr.attributes
    | LetException expr ->
        expr.attributes
    | Assert expr ->
        expr.attributes
    | Lazy expr ->
        expr.attributes
    | While expr ->
        expr.attributes
    | For expr ->
        expr.attributes
    | Apply expr ->
        expr.attributes
    | MethodCall expr ->
        expr.attributes
    | New expr ->
        expr.attributes
    | Prefix expr ->
        expr.attributes
    | FieldAccess expr ->
        expr.attributes
    | Index expr ->
        expr.attributes
    | ObjectOverride expr ->
        expr.attributes
    | InstanceVariableAssign expr ->
        expr.attributes
    | FieldAssign expr ->
        expr.attributes
    | Assign expr ->
        expr.attributes
    | Infix expr ->
        expr.attributes
    | TypeAscription expr ->
        expr.attributes
    | Polymorphic expr ->
        expr.attributes
    | Sequence expr ->
        expr.attributes
    | Tuple expr ->
        expr.attributes
    | List expr ->
        expr.attributes
    | Array expr ->
        expr.attributes
    | Record expr -> (
        match expr with
        | Literal record -> record.attributes
        | Update record -> record.attributes
      )
    | LocalOpen expr -> (
        match expr with
        | LetOpen { attributes; _ }
        | Delimited { attributes; _ } -> attributes
      )
    | Fun expr ->
        expr.attributes
    | Function expr ->
        expr.attributes
    | LetOperator expr ->
        expr.attributes
    | Let expr ->
        expr.attributes
    | Match expr ->
        expr.attributes
    | Try expr ->
        expr.attributes
    | If expr ->
        expr.attributes
    | Parenthesized expr ->
        expr.attributes
end

module Parameter = struct
  type t = parameter =
    | Positional of positional_parameter
    | Labeled of labeled_parameter
    | Optional of optional_parameter
    | LocallyAbstract of locally_abstract_type_parameter

  let syntax_node = function
    | Positional param -> param.syntax_node
    | Labeled param -> param.syntax_node
    | Optional param -> param.syntax_node
    | LocallyAbstract param -> param.syntax_node

  let sigil_token = function
    | Positional _
    | LocallyAbstract _ -> None
    | Labeled param -> Some param.sigil_token
    | Optional param -> Some param.sigil_token

  let name_token = function
    | Positional param -> param.name_token
    | Labeled param -> Some param.label_token
    | Optional param -> Some param.label_token
    | LocallyAbstract _ -> None

  let name = fun param ->
    match name_token param with
    | Some token -> Some (Token.text token)
    | None -> None

  let is_named = function
    | Labeled _
    | Optional _ -> true
    | Positional _
    | LocallyAbstract _ -> false

  let binding_name_matches_label = function
    | Labeled param -> param.binding_name_matches_label
    | Optional param -> param.binding_name_matches_label
    | Positional _
    | LocallyAbstract _ -> false

  let default_value = function
    | Optional param -> param.default_value
    | Positional _
    | Labeled _
    | LocallyAbstract _ -> None

  let binding_pattern = function
    | Positional param -> Some param.pattern
    | Labeled param -> param.binding_pattern
    | Optional param -> param.binding_pattern
    | LocallyAbstract _ -> None
end

module ObjectMember = struct
  type t = object_member =
    | Method of object_method
    | Value of object_value
    | Inherit of object_inherit
    | Extension of extension
    | Initializer of object_initializer

  let syntax_node = function
    | Method member -> member.syntax_node
    | Value member -> member.syntax_node
    | Inherit member -> member.syntax_node
    | Extension extension -> extension.syntax_node
    | Initializer member -> member.syntax_node
end

module ClassExpression = struct
  type t = class_expression =
    | Path of Ident.t
    | Structure of class_structure
    | Fun of class_fun_expression
    | Apply of class_apply_expression
    | Let of class_let_expression
    | Constraint of class_constraint_expression
    | LocalOpen of local_open_class_expression
    | Parenthesized of parenthesized_class_expression
    | Attribute of {
        syntax_node: syntax_node;
        class_expression: class_expression;
        attribute: attribute
      }
    | Extension of extension

  let syntax_node = function
    | Path path ->
        Ident.syntax_node path
    | Structure structure ->
        structure.syntax_node
    | Fun expr ->
        expr.syntax_node
    | Apply expr ->
        expr.syntax_node
    | Let expr ->
        expr.syntax_node
    | Constraint expr ->
        expr.syntax_node
    | LocalOpen expr -> (
        match expr with
        | LetOpen { syntax_node; _ }
        | Delimited { syntax_node; _ } -> syntax_node
      )
    | Parenthesized expr ->
        expr.syntax_node
    | Attribute { syntax_node; _ } ->
        syntax_node
    | Extension extension ->
        extension.syntax_node
end

module ClassField = struct
  type t = class_field =
    | Method of class_method
    | Value of class_value
    | Inherit of class_inherit
    | Constraint of class_constraint
    | Initializer of class_initializer
    | Attribute of { syntax_node: syntax_node; field: class_field; attribute: attribute }
    | Extension of extension

  let syntax_node = function
    | Method field -> field.syntax_node
    | Value field -> field.syntax_node
    | Inherit field -> field.syntax_node
    | Constraint field -> field.syntax_node
    | Initializer field -> field.syntax_node
    | Attribute { syntax_node; _ } -> syntax_node
    | Extension extension -> extension.syntax_node
end

module ModuleExpression = struct
  type t = module_expression =
    | Path of Ident.t
    | Structure of { syntax_node: syntax_node; item_syntax_nodes: syntax_node list }
    | Functor of {
        syntax_node: syntax_node;
        parameters: functor_parameter list;
        body: module_expression
      }
    | Apply of { syntax_node: syntax_node; callee: module_expression; argument: module_expression }
    | ApplyUnit of { syntax_node: syntax_node; callee: module_expression }
    | Constraint of {
        syntax_node: syntax_node;
        module_expression: module_expression;
        colon_token: Token.t;
        module_type: module_type
      }
    | ModuleUnpack of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        expression: expression;
        colon_token: Token.t option;
        package_type: package_type option;
        closing_token: Token.t
      }
    | Parenthesized of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        inner: module_expression;
        closing_token: Token.t
      }
    | Attribute of {
        syntax_node: syntax_node;
        module_expression: module_expression;
        attribute: attribute
      }
    | Extension of extension

  let syntax_node = function
    | Path path -> Ident.syntax_node path
    | Structure { syntax_node; _ }
    | Functor { syntax_node; _ }
    | Apply { syntax_node; _ }
    | ApplyUnit { syntax_node; _ }
    | Constraint { syntax_node; _ }
    | ModuleUnpack { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | Attribute { syntax_node; _ } -> syntax_node
    | Extension extension -> extension.syntax_node
end

module Pattern = struct
  type t = pattern =
    | Identifier of identifier_pattern
    | Wildcard of wildcard_pattern
    | Extension of extension_pattern
    | Literal of literal_pattern
    | Lazy of lazy_pattern
    | Exception of exception_pattern
    | Range of range_pattern
    | Operator of operator_pattern
    | FirstClassModule of first_class_module_pattern
    | PolyVariant of poly_variant_pattern
    | PolyVariantInherit of poly_variant_inherit_pattern
    | Constructor of constructor_pattern
    | Tuple of tuple_pattern
    | List of list_pattern
    | Array of array_pattern
    | Record of record_pattern
    | Cons of cons_pattern
    | Or of or_pattern
    | Alias of alias_pattern
    | Typed of typed_pattern
    | Effect of effect_pattern
    | LocalOpen of local_open_pattern
    | Parenthesized of parenthesized_pattern

  let syntax_node = function
    | Identifier pattern -> pattern.syntax_node
    | Wildcard pattern -> pattern.syntax_node
    | Extension pattern -> pattern.syntax_node
    | Literal pattern -> pattern.syntax_node
    | Lazy pattern -> pattern.syntax_node
    | Exception pattern -> pattern.syntax_node
    | Range pattern -> pattern.syntax_node
    | Operator pattern -> pattern.syntax_node
    | FirstClassModule pattern -> pattern.syntax_node
    | PolyVariant pattern -> pattern.syntax_node
    | PolyVariantInherit pattern -> pattern.syntax_node
    | Constructor pattern -> pattern.syntax_node
    | Tuple pattern -> pattern.syntax_node
    | List pattern -> pattern.syntax_node
    | Array pattern -> pattern.syntax_node
    | Record pattern -> pattern.syntax_node
    | Cons pattern -> pattern.syntax_node
    | Or pattern -> pattern.syntax_node
    | Alias pattern -> pattern.syntax_node
    | Typed pattern -> pattern.syntax_node
    | Effect pattern -> pattern.syntax_node
    | LocalOpen pattern -> pattern.syntax_node
    | Parenthesized pattern -> pattern.syntax_node

  let attributes = function
    | Identifier pattern -> pattern.attributes
    | Wildcard pattern -> pattern.attributes
    | Extension pattern -> pattern.attributes
    | Literal pattern -> pattern.attributes
    | Lazy pattern -> pattern.attributes
    | Exception pattern -> pattern.attributes
    | Range pattern -> pattern.attributes
    | Operator pattern -> pattern.attributes
    | FirstClassModule pattern -> pattern.attributes
    | PolyVariant pattern -> pattern.attributes
    | PolyVariantInherit pattern -> pattern.attributes
    | Constructor pattern -> pattern.attributes
    | Tuple pattern -> pattern.attributes
    | List pattern -> pattern.attributes
    | Array pattern -> pattern.attributes
    | Record pattern -> pattern.attributes
    | Cons pattern -> pattern.attributes
    | Or pattern -> pattern.attributes
    | Alias pattern -> pattern.attributes
    | Typed pattern -> pattern.attributes
    | Effect pattern -> pattern.attributes
    | LocalOpen pattern -> pattern.attributes
    | Parenthesized pattern -> pattern.attributes
end

module InfixExpression = struct
  type t = infix_expression = {
    syntax_node: syntax_node;
    left: expression;
    operator_tokens: Token.t list;
    right: expression;
    attributes: attribute list;
  }

  let syntax_node = fun expr -> expr.syntax_node

  let left = fun expr -> expr.left

  let operator_tokens = fun expr -> expr.operator_tokens

  let operator = fun expr -> expr.operator_tokens |> List.map Token.text |> String.concat ""

  let right = fun expr -> expr.right

  let attributes = fun expr -> expr.attributes
end

module RecordExpression = struct
  type t = record_expression =
    | Literal of record_literal_expression
    | Update of record_update_expression

  let syntax_node = function
    | Literal expr -> expr.syntax_node
    | Update expr -> expr.syntax_node
end

module Payload = struct
  type t = payload =
    | Opaque of { tokens: Token.t list }

  let tokens = function
    | Opaque { tokens } -> tokens
end

module TypeVariable = struct
  type t = {
    syntax_node: syntax_node;
    name_token: Token.t;
  }

  let syntax_node = fun type_variable -> type_variable.syntax_node

  let name_token = fun type_variable -> type_variable.name_token

  let text = fun type_variable ->
    Ceibo.Red.SyntaxNode.children type_variable.syntax_node |> List.filter_map
      (
        function
        | Ceibo.Red.Token tok when not (is_trivia (Ceibo.Red.SyntaxToken.kind tok)) -> Some (Ceibo.Red.SyntaxToken.text
          tok)
        | _ -> None
      ) |> String.concat ""

  let name = fun type_variable -> Token.text type_variable.name_token
end

module TypeParameterVariance = struct
  type t =
    | Covariant of { marker_token: Token.t }
    | Contravariant of { marker_token: Token.t }

  let marker_token = function
    | Covariant { marker_token }
    | Contravariant { marker_token } -> marker_token
end

module TypeParameter = struct
  type t = {
    syntax_node: syntax_node;
    variance: TypeParameterVariance.t option;
    injectivity_token: Token.t option;
    type_variable: TypeVariable.t option;
  }

  let syntax_node = fun type_param -> type_param.syntax_node

  let variance = fun type_param -> type_param.variance

  let injectivity_token = fun type_param -> type_param.injectivity_token

  let type_variable = fun type_param -> type_param.type_variable
end

module PrivateFlag = struct
  type t = private_flag =
    | Public
    | Private of { private_token: Token.t }

  let private_token = function
    | Public -> None
    | Private { private_token } -> Some private_token

  let is_private = function
    | Public -> false
    | Private _ -> true
end

module RecordField = struct
  type t = {
    syntax_node: syntax_node;
    field_name: Token.t;
    mutable_token: Token.t option;
    colon_token: Token.t;
    field_type: core_type;
    semicolon_token: Token.t option;
    attributes: attribute list;
  }

  let syntax_node = fun field -> field.syntax_node

  let field_name_token = fun field -> field.field_name

  let mutable_token = fun field -> field.mutable_token

  let colon_token = fun field -> field.colon_token

  let field_type = fun field -> field.field_type

  let semicolon_token = fun field -> field.semicolon_token

  let name = fun field -> Token.text field.field_name

  let attributes = fun field -> field.attributes
end

module ConstructorArguments = struct
  type t =
    | Tuple of core_type list
    | Record of { opening_token: Token.t; fields: RecordField.t list; closing_token: Token.t }
end

module VariantConstructor = struct
  type t = {
    syntax_node: syntax_node;
    attributes: attribute list;
    bar_token: Token.t option;
    constructor_name: Token.t;
    separator_token: Token.t option;
    arguments: ConstructorArguments.t option;
    payload_type: core_type option;
    arrow_token: Token.t option;
    result_type: core_type option;
  }

  let syntax_node = fun constr -> constr.syntax_node

  let attributes = fun constr -> constr.attributes

  let bar_token = fun constr -> constr.bar_token

  let constructor_name_token = fun constr -> constr.constructor_name

  let separator_token = fun constr -> constr.separator_token

  let arguments = fun constr -> constr.arguments

  let payload_type = fun constr -> constr.payload_type

  let arrow_token = fun constr -> constr.arrow_token

  let result_type = fun constr -> constr.result_type

  let name = fun constr -> Token.text constr.constructor_name
end

module PolyVariantTag = struct
  type t = poly_variant_tag = {
    syntax_node: syntax_node;
    attributes: attribute list;
    bar_token: Token.t option;
    tag_name: Token.t;
    separator_token: Token.t option;
    payload_type: core_type option;
  }

  let syntax_node = fun tag -> tag.syntax_node

  let attributes = fun tag -> tag.attributes

  let bar_token = fun tag -> tag.bar_token

  let tag_name_token = fun tag -> tag.tag_name

  let separator_token = fun tag -> tag.separator_token

  let payload_type = fun tag -> tag.payload_type

  let name = fun tag -> Token.text tag.tag_name
end

module PolyVariantBound = struct
  type t = poly_variant_bound =
    | Exact
    | UpperBound of { marker_token: Token.t }
    | LowerBound of { marker_token: Token.t }

  let marker_token = function
    | Exact -> None
    | UpperBound { marker_token }
    | LowerBound { marker_token } -> Some marker_token
end

module RowField = struct
  type t = row_field =
    | Tag of poly_variant_tag
    | Inherit of { bar_token: Token.t option; syntax_node: syntax_node; type_: core_type }

  let syntax_node = function
    | Tag tag -> tag.syntax_node
    | Inherit { syntax_node; _ } -> syntax_node

  let tag = function
    | Tag tag -> Some tag
    | Inherit _ -> None

  let bar_token = function
    | Tag tag -> tag.bar_token
    | Inherit { bar_token; _ } -> bar_token

  let inherited_type = function
    | Tag _ -> None
    | Inherit { type_; _ } -> Some type_
end

module PolyVariant = struct
  type t = poly_variant = {
    syntax_node: syntax_node;
    opening_token: Token.t;
    kind: poly_variant_bound;
    fields: row_field list;
    closing_token: Token.t;
  }

  let syntax_node = fun poly_variant -> poly_variant.syntax_node

  let kind = fun poly_variant -> poly_variant.kind

  let fields = fun poly_variant -> poly_variant.fields

  let tags = fun poly_variant ->
    poly_variant.fields |> List.filter_map
      (
        function
        | RowField.Tag tag -> Some tag
        | RowField.Inherit _ -> None
      )
end

module TypeDefinition = struct
  type t =
    | Abstract
    | Alias of { syntax_node: syntax_node; manifest: core_type }
    | Extensible of { syntax_node: syntax_node }
    | FirstClassModule of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        package_type: package_type;
        closing_token: Token.t
      }
    | Object of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        fields: object_type_field list;
        closing_token: Token.t
      }
    | Record of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        fields: RecordField.t list;
        closing_token: Token.t
      }
    | Variant of { syntax_node: syntax_node; constructors: VariantConstructor.t list }
    | PolyVariant of PolyVariant.t
end

module TypeDeclaration = struct
  type t = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    nonrec_token: Token.t option;
    type_name: Ident.t;
    type_params: TypeParameter.t list;
    type_definition: TypeDefinition.t;
    manifest_equals_token: Token.t option;
    manifest_alias: core_type option;
    definition_equals_token: Token.t option;
    destructive_substitution_token: Token.t option;
    private_flag: private_flag;
    constraints: type_constraint list;
    attributes: attribute list;
    next_and_declaration: t option;
  }

  let syntax_node = fun decl -> decl.syntax_node

  let keyword_token = fun decl -> decl.keyword_token

  let nonrec_token = fun decl -> decl.nonrec_token

  let type_name = fun decl -> decl.type_name

  let type_params = fun decl -> decl.type_params

  let type_definition = fun decl -> decl.type_definition

  let manifest_equals_token = fun decl -> decl.manifest_equals_token

  let manifest_alias = fun decl -> decl.manifest_alias

  let definition_equals_token = fun decl -> decl.definition_equals_token

  let destructive_substitution_token = fun decl -> decl.destructive_substitution_token

  let private_flag = fun decl -> decl.private_flag

  let constraints = fun decl -> decl.constraints

  let attributes = fun decl -> decl.attributes

  let next_and_declaration = fun decl -> decl.next_and_declaration

  let is_private = fun decl -> PrivateFlag.is_private decl.private_flag

  let name_token = fun decl ->
    match Ident.last_segment decl.type_name with
    | Some token -> token
    | None -> panic "TypeDeclaration.name_token: missing type name token"
end

module TypeExtension = struct
  type t = {
    syntax_node: syntax_node;
    type_name: Ident.t;
    type_params: TypeParameter.t list;
    extension_operator_tokens: Token.t list;
    constructors: VariantConstructor.t list;
  }

  let syntax_node = fun decl -> decl.syntax_node

  let type_name = fun decl -> decl.type_name

  let type_params = fun decl -> decl.type_params

  let extension_operator_tokens = fun decl -> decl.extension_operator_tokens

  let constructors = fun decl -> decl.constructors

  let name_token = fun decl ->
    match Ident.last_segment decl.type_name with
    | Some token -> token
    | None -> panic "TypeExtension.name_token: missing type name token"
end

module LetBinding = struct
  type t = let_binding = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    rec_token: Token.t option;
    equals_token: Token.t;
    attributes: attribute list;
    binding_pattern: pattern;
    parameters: Parameter.t list;
    value: expression;
    and_binding: let_binding option;
  }

  let syntax_node = fun binding -> binding.syntax_node

  let keyword_token = fun binding -> binding.keyword_token

  let rec_token = fun binding -> binding.rec_token

  let equals_token = fun binding -> binding.equals_token

  let attributes = fun binding -> binding.attributes

  let binding_pattern = fun binding -> binding.binding_pattern

  let rec binding_name_token_from_pattern = function
    | Pattern.Identifier { name_token; _ } -> Some name_token
    | Pattern.Typed { pattern; _ }
    | Pattern.Lazy { pattern; _ }
    | Pattern.LocalOpen { pattern; _ } -> binding_name_token_from_pattern pattern
    | Pattern.Parenthesized { inner; _ } -> binding_name_token_from_pattern inner
    | Pattern.Alias { name_token; _ } -> Some name_token
    | _ -> None

  let binding_name_token = fun binding -> binding_name_token_from_pattern binding.binding_pattern

  let name = fun binding ->
    match binding_name_token binding with
    | Some token -> Token.text token
    | None -> panic "LetBinding.name: missing binding name token"

  let parameters = fun binding -> binding.parameters

  let value = fun binding -> binding.value

  let rec and_bindings = fun binding ->
    match binding.and_binding with
    | None -> []
    | Some next -> next :: and_bindings next

  let and_binding = fun binding -> binding.and_binding

  let value_syntax_node = fun binding -> Expression.syntax_node binding.value

  let has_direct_token_text = fun node expected ->
    Ceibo.Red.SyntaxNode.direct_tokens node |> List.exists
      (fun token ->
        String.equal (Ceibo.Red.SyntaxToken.text token) expected)

  let is_recursive = fun binding ->
    Option.is_some binding.rec_token || match Ceibo.Red.SyntaxNode.parent binding.syntax_node with
    | Some parent -> has_direct_token_text parent "rec"
    | None -> false

  let is_function = fun binding ->
    List.length binding.parameters > 0
    || match Ceibo.Red.SyntaxNode.kind (value_syntax_node binding) with
    | Syntax_kind.FUN_EXPR
    | Syntax_kind.FUNCTION_EXPR -> true
    | _ -> false
end

module ModuleSignature = struct
  type definition =
    | Signature of module_type
    | Alias of module_expression

  type t = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    rec_token: Token.t option;
    module_name: Token.t;
    functor_parameters: functor_parameter list;
    colon_token: Token.t option;
    equals_token: Token.t option;
    definition: definition;
    next_and_declaration: t option;
  }

  let syntax_node = fun decl -> decl.syntax_node

  let keyword_token = fun decl -> decl.keyword_token

  let rec_token = fun decl -> decl.rec_token

  let module_name_token = fun decl -> decl.module_name

  let functor_parameters = fun decl -> decl.functor_parameters

  let colon_token = fun decl -> decl.colon_token

  let equals_token = fun decl -> decl.equals_token

  let definition = fun decl -> decl.definition

  let module_type = function
    | { definition=Signature module_type; _ } -> Some module_type
    | { definition=Alias _; _ } -> None

  let module_expression = function
    | { definition=Signature _; _ } -> None
    | { definition=Alias module_expression; _ } -> Some module_expression

  let next_and_declaration = fun decl -> decl.next_and_declaration

  let has_direct_token_text = fun node expected ->
    Ceibo.Red.SyntaxNode.direct_tokens node |> List.exists
      (fun token ->
        String.equal (Ceibo.Red.SyntaxToken.text token) expected)

  let is_recursive = fun decl ->
    Option.is_some decl.rec_token || match Ceibo.Red.SyntaxNode.parent decl.syntax_node with
    | Some parent -> has_direct_token_text parent "rec"
    | None -> false

  let name = fun decl -> Token.text decl.module_name
end

module ModuleStructure = struct
  type t = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    rec_token: Token.t option;
    module_name: Token.t;
    functor_parameters: functor_parameter list;
    colon_token: Token.t option;
    equals_token: Token.t;
    module_type: module_type option;
    module_expression: module_expression;
    next_and_declaration: t option;
  }

  let syntax_node = fun decl -> decl.syntax_node

  let keyword_token = fun decl -> decl.keyword_token

  let rec_token = fun decl -> decl.rec_token

  let module_name_token = fun decl -> decl.module_name

  let functor_parameters = fun decl -> decl.functor_parameters

  let colon_token = fun decl -> decl.colon_token

  let equals_token = fun decl -> decl.equals_token

  let module_type = fun decl -> decl.module_type

  let module_expression = fun decl -> decl.module_expression

  let next_and_declaration = fun decl -> decl.next_and_declaration

  let has_direct_token_text = fun node expected ->
    Ceibo.Red.SyntaxNode.direct_tokens node |> List.exists
      (fun token ->
        String.equal (Ceibo.Red.SyntaxToken.text token) expected)

  let is_recursive = fun decl ->
    Option.is_some decl.rec_token || match Ceibo.Red.SyntaxNode.parent decl.syntax_node with
    | Some parent -> has_direct_token_text parent "rec"
    | None -> false

  let name = fun decl -> Token.text decl.module_name
end

module ModuleTypeDeclaration = struct
  type t = {
    syntax_node: syntax_node;
    module_keyword_token: Token.t;
    type_keyword_token: Token.t;
    module_type_name: Token.t;
    equals_token: Token.t option;
    module_type: module_type option;
  }

  let syntax_node = fun decl -> decl.syntax_node

  let module_keyword_token = fun decl -> decl.module_keyword_token

  let type_keyword_token = fun decl -> decl.type_keyword_token

  let module_type_name_token = fun decl -> decl.module_type_name

  let equals_token = fun decl -> decl.equals_token

  let module_type = fun decl -> decl.module_type

  let name = fun decl -> Token.text decl.module_type_name
end

module OpenStatement = struct
  type target =
    | Path of Ident.t
    | ModuleExpression of module_expression

  type t = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    target: target;
    bang_token: Token.t option;
  }

  let syntax_node = fun stmt -> stmt.syntax_node

  let keyword_token = fun stmt -> stmt.keyword_token

  let target = fun stmt -> stmt.target

  let module_expression = fun stmt ->
    match stmt.target with
    | ModuleExpression expr -> Some expr
    | Path _ -> None

  let module_path = fun stmt ->
    match stmt.target with
    | Path path
    | ModuleExpression (ModuleExpression.Path path) -> Some path
    | ModuleExpression _ -> None

  let bang_token = fun stmt -> stmt.bang_token

  let has_bang = fun stmt -> Option.is_some stmt.bang_token
end

module Docstring = struct
  type kind = docstring_kind =
    | Ordinary
    | Section

  type t = docstring = {
    syntax_node: syntax_node;
    docstring_token: Token.t;
    kind: kind;
  }

  let syntax_node = fun doc -> doc.syntax_node

  let token = fun doc -> doc.docstring_token

  let kind = fun doc -> doc.kind

  let is_section = function
    | { kind=Section; _ } -> true
    | { kind=Ordinary; _ } -> false

  let text = fun doc -> Token.text doc.docstring_token
end

module Comment = struct
  type t = comment = {
    syntax_node: syntax_node;
    comment_token: Token.t;
  }

  let syntax_node = fun comment -> comment.syntax_node

  let token = fun comment -> comment.comment_token

  let text = fun comment -> Token.text comment.comment_token
end

module Trivia = struct
  type t = trivia =
    | Docstring of docstring
    | Comment of comment

  let syntax_node = function
    | Docstring docstring -> docstring.syntax_node
    | Comment comment -> comment.syntax_node

  let token = function
    | Docstring docstring -> docstring.docstring_token
    | Comment comment -> comment.comment_token

  let text = fun trivia -> Token.text (token trivia)

  let is_docstring = function
    | Docstring _ -> true
    | Comment _ -> false

  let is_comment = function
    | Docstring _ -> false
    | Comment _ -> true
end

let synthetic_syntax_node_wrapping_token = fun syntax_token ->
  let green_token = Ceibo.Red.SyntaxToken.green syntax_token in
  let wrapped_node = Ceibo.Green.make_node_list
    ~kind:(Ceibo.Red.SyntaxToken.kind syntax_token)
    [ Ceibo.Green.Token green_token ] in
  let root = Ceibo.Green.make_node_list
    ~kind:Syntax_kind.SOURCE_FILE [ Ceibo.Green.Node wrapped_node ] in
  match Ceibo.Red.SyntaxNode.child (Ceibo.Red.new_root root) 0 with
  | Some (Ceibo.Red.Node node) -> node
  | _ -> panic "synthetic_syntax_node_wrapping_token: missing wrapped child node"

let docstring_kind_from_text = fun comment_text ->
  let len = String.length comment_text in
  if len < 5 then
    Ordinary
  else
    let body = String.sub comment_text 3 (len - 5) |> String.trim in
    if String.length body > 0 && (Char.equal body.[0] '{' || Char.equal body.[0] '#') then
      Section
    else
      Ordinary

let syntax_token_from_trivia = fun trivia ->
  let span = Ceibo.Red.SyntaxTrivia.span trivia in
  let green_token = Ceibo.Green.make_token
    ~leading_trivia:[]
    ~kind:(Ceibo.Red.SyntaxTrivia.kind trivia)
    ~text:(Ceibo.Red.SyntaxTrivia.text trivia)
    ~width:((span.end_ - span.start)) in
  Ceibo.Red.new_token green_token span

let trivia_of_syntax_trivia = fun trivia ->
  let syntax_token = syntax_token_from_trivia trivia in
  match Ceibo.Red.SyntaxTrivia.kind trivia with
  | Syntax_kind.COMMENT -> Some (Comment {
    syntax_node = synthetic_syntax_node_wrapping_token syntax_token;
    comment_token = { Token.syntax_token = syntax_token }
  })
  | Syntax_kind.DOCSTRING -> Some (Docstring {
    syntax_node = synthetic_syntax_node_wrapping_token syntax_token;
    docstring_token = { Token.syntax_token = syntax_token };
    kind = docstring_kind_from_text (Ceibo.Red.SyntaxToken.text syntax_token)
  })
  | _ -> None

let leading_trivia_after = fun ~after token ->
  Ceibo.Red.SyntaxToken.leading_trivia token.Token.syntax_token |> List.filter_map
    (fun trivia ->
      let span = Ceibo.Red.SyntaxTrivia.span trivia in
      if span.start >= after then
        trivia_of_syntax_trivia trivia
      else
        None)

let leading_trivia_before_node = fun ~after syntax_node ->
  match Ceibo.Red.SyntaxNode.first_token syntax_node with
  | None -> []
  | Some first_token -> leading_trivia_after ~after { Token.syntax_token = first_token }

let leading_trivia_after_token_before_node = fun ~after token syntax_node ->
  let pending = leading_trivia_after ~after token in
  match Ceibo.Red.SyntaxNode.first_token syntax_node with
  | None -> pending
  | Some first_token -> pending
  @ leading_trivia_after
    ~after:(Ceibo.Red.SyntaxToken.span token.Token.syntax_token).end_
    { Token.syntax_token = first_token }

let token_body_span = fun syntax_node ->
  let full_span = Ceibo.Red.SyntaxNode.span syntax_node in
  match Ceibo.Red.SyntaxNode.tokens syntax_node with
  | [] -> full_span
  | first :: rest ->
      let last =
        List.fold_left (fun _ token -> token) first rest
      in
      {
        Ceibo.Span.start = (Ceibo.Red.SyntaxToken.span first).start;
        end_ = (Ceibo.Red.SyntaxToken.span last).end_
      }

let syntax_kind = fun syntax_node -> Ceibo.Red.SyntaxNode.kind syntax_node

type value_declaration = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  name_tokens: Token.t list;
  colon_token: Token.t;
  type_: core_type;
  trailing_comment: comment option;
}

module ValueDeclaration = struct
  type t = value_declaration = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    name_tokens: Token.t list;
    colon_token: Token.t;
    type_: core_type;
    trailing_comment: comment option;
  }

  let syntax_node = fun decl -> decl.syntax_node

  let keyword_token = fun decl -> decl.keyword_token

  let name_tokens = fun decl -> decl.name_tokens

  let colon_token = fun decl -> decl.colon_token

  let type_ = fun decl -> decl.type_

  let trailing_comment = fun decl -> decl.trailing_comment
end

type external_declaration = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  name_tokens: Token.t list;
  colon_token: Token.t;
  type_: core_type;
  equals_token: Token.t;
  primitive_name_tokens: Token.t list;
  attributes: attribute list;
}

module ClassDeclaration = struct
  type t = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    type_params: TypeParameter.t list;
    declaration_extension: extension option;
    declaration_attributes: attribute list;
    class_name: Token.t;
    colon_token: Token.t;
    class_type: class_type;
  }

  let syntax_node = fun declaration -> declaration.syntax_node

  let keyword_token = fun declaration -> declaration.keyword_token

  let type_params = fun declaration -> declaration.type_params

  let declaration_extension = fun declaration -> declaration.declaration_extension

  let declaration_attributes = fun declaration -> declaration.declaration_attributes

  let class_name_token = fun declaration -> declaration.class_name

  let colon_token = fun declaration -> declaration.colon_token

  let class_type = fun declaration -> declaration.class_type

  let name = fun declaration -> Token.text declaration.class_name
end

module ClassDefinition = struct
  type t = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    type_params: TypeParameter.t list;
    declaration_extension: extension option;
    declaration_attributes: attribute list;
    class_name: Token.t;
    colon_token: Token.t option;
    class_type: class_type option;
    equals_token: Token.t;
    class_body: class_expression;
  }

  let syntax_node = fun definition -> definition.syntax_node

  let keyword_token = fun definition -> definition.keyword_token

  let type_params = fun definition -> definition.type_params

  let declaration_extension = fun definition -> definition.declaration_extension

  let declaration_attributes = fun definition -> definition.declaration_attributes

  let class_name_token = fun definition -> definition.class_name

  let colon_token = fun definition -> definition.colon_token

  let class_type = fun definition -> definition.class_type

  let equals_token = fun definition -> definition.equals_token

  let class_body = fun definition -> definition.class_body

  let name = fun definition -> Token.text definition.class_name
end

type class_type_declaration = {
  syntax_node: syntax_node;
  class_keyword_token: Token.t;
  type_keyword_token: Token.t;
  type_params: TypeParameter.t list;
  declaration_extension: extension option;
  declaration_attributes: attribute list;
  class_type_name: Token.t;
  equals_token: Token.t;
  class_type_body: class_type;
}

type include_target =
  | ModuleExpression of module_expression
  | ModuleType of module_type

type include_statement = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  target: include_target;
}

module StructureItem = struct
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | TypeExtension of TypeExtension.t
    | LetBinding of LetBinding.t
    | Expression of Expression.t
    | Attribute of attribute
    | Extension of extension
    | ClassDeclaration of ClassDefinition.t
    | ClassTypeDeclaration of class_type_declaration
    | ModuleDeclaration of ModuleStructure.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | OpenStatement of OpenStatement.t
    | Docstring of Docstring.t
    | Comment of Comment.t
    | ExternalDeclaration of external_declaration
    | IncludeStatement of include_statement
    | ExceptionDeclaration of exception_declaration

  let syntax_node = function
    | TypeDeclaration decl -> TypeDeclaration.syntax_node decl
    | TypeExtension decl -> TypeExtension.syntax_node decl
    | LetBinding binding -> LetBinding.syntax_node binding
    | Expression expr -> Expression.syntax_node expr
    | Attribute attribute -> attribute.syntax_node
    | Extension extension -> extension.syntax_node
    | ClassDeclaration decl -> ClassDefinition.syntax_node decl
    | ClassTypeDeclaration decl -> decl.syntax_node
    | ModuleDeclaration decl -> ModuleStructure.syntax_node decl
    | ModuleTypeDeclaration decl -> ModuleTypeDeclaration.syntax_node decl
    | OpenStatement stmt -> OpenStatement.syntax_node stmt
    | Docstring doc -> Docstring.syntax_node doc
    | Comment comment -> Comment.syntax_node comment
    | ExternalDeclaration decl -> decl.syntax_node
    | IncludeStatement stmt -> stmt.syntax_node
    | ExceptionDeclaration decl -> decl.syntax_node
end

module SignatureItem = struct
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | TypeExtension of TypeExtension.t
    | Attribute of attribute
    | Extension of extension
    | ClassDeclaration of ClassDeclaration.t
    | ClassTypeDeclaration of class_type_declaration
    | ModuleDeclaration of ModuleSignature.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | OpenStatement of OpenStatement.t
    | Docstring of Docstring.t
    | Comment of Comment.t
    | ValueDeclaration of value_declaration
    | ExternalDeclaration of external_declaration
    | IncludeStatement of include_statement
    | ExceptionDeclaration of exception_declaration

  let syntax_node = function
    | TypeDeclaration decl -> TypeDeclaration.syntax_node decl
    | TypeExtension decl -> TypeExtension.syntax_node decl
    | Attribute attribute -> attribute.syntax_node
    | Extension extension -> extension.syntax_node
    | ClassDeclaration decl -> ClassDeclaration.syntax_node decl
    | ClassTypeDeclaration decl -> decl.syntax_node
    | ModuleDeclaration decl -> ModuleSignature.syntax_node decl
    | ModuleTypeDeclaration decl -> ModuleTypeDeclaration.syntax_node decl
    | OpenStatement stmt -> OpenStatement.syntax_node stmt
    | Docstring doc -> Docstring.syntax_node doc
    | Comment comment -> Comment.syntax_node comment
    | ValueDeclaration decl -> decl.syntax_node
    | ExternalDeclaration decl -> decl.syntax_node
    | IncludeStatement stmt -> stmt.syntax_node
    | ExceptionDeclaration decl -> decl.syntax_node
end

type implementation = {
  syntax_node: syntax_node;
  items: StructureItem.t list;
  phrase_separator_tokens: Token.t list;
  trailing_phrase_separator_tokens: Token.t list list;
}

type interface = {
  syntax_node: syntax_node;
  items: SignatureItem.t list;
  phrase_separator_tokens: Token.t list;
  trailing_phrase_separator_tokens: Token.t list list;
}

type t =
  | Implementation of implementation
  | Interface of interface

type source_file = t

module SourceFile = struct
  type t = source_file

  let syntax_node = function
    | Implementation source_file -> source_file.syntax_node
    | Interface source_file -> source_file.syntax_node

  let structure_items = function
    | Implementation source_file -> Some source_file.items
    | Interface _ -> None

  let signature_items = function
    | Implementation _ -> None
    | Interface source_file -> Some source_file.items

  let phrase_separator_tokens = function
    | Implementation source_file -> source_file.phrase_separator_tokens
    | Interface source_file -> source_file.phrase_separator_tokens

  let trailing_phrase_separator_tokens = function
    | Implementation source_file -> source_file.trailing_phrase_separator_tokens
    | Interface source_file -> source_file.trailing_phrase_separator_tokens

  let kind = function
    | Implementation _ -> `Implementation
    | Interface _ -> `Interface
end

let syntax_node_of_source_file = fun source_file -> SourceFile.syntax_node source_file
