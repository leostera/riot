(** Lossless concrete syntax tree (CST) nodes produced from successful `syn`
    parses.

    This module is intentionally grammar-oriented. Each lifted value keeps a
    handle to the original `syntax_node`, and many nodes also preserve the exact
    `Token.t` values that spelled the construct. That makes the CST suitable for
    refactoring tools, diagnostics, formatting, and fixture-based tests that
    need structured syntax without giving up source fidelity.

    The main families of grammar covered here are:

    - `pattern` for pattern syntax such as `Some x`, `{ field }`, or
      `effect Yield k`
    - `expression` for expression syntax such as `f x`, `match x with`, or
      `let module M = N in ...`
    - `core_type` for core types such as `'a list`, `int -> string`, or
      `(module S with type t = int)`
    - `module_type` and `module_expression` for module-language syntax such as
      `sig ... end`, `functor (X : S) -> T`, `F(X)`, or `F()`
    - `StructureItem.t`, `SignatureItem.t`, and `SourceFile.t` for file-level
      structure

    Some public nodes deliberately retain raw `syntax_node`s for subgrammars
    that parse successfully but are not yet reified into a richer public type.
    When that happens, the documentation calls it out explicitly so consumers
    know which details live in the red tree instead of dedicated fields.
*)
open Std

(** A red-tree node from Ceibo.

    Every major CST value keeps its originating `syntax_node`, which lets tools
    recover spans, inspect retained trivia, or fall back to lower-level syntax
    when the public CST does not expose a dedicated field.
*)
(** A red-tree token from Ceibo.

    Tokens are surfaced directly only when the exact spelling matters to CST
    consumers, for example identifier names, operator symbols, or literal text.
*)
type syntax_node = (Syntax_kind.t, string) Ceibo.Red.syntax_node
(** A green-tree node from Ceibo.

    This appears in the public interface for consumers that need to bridge
    between the immutable green tree and the lifted CST, but most callers stay
    at the `syntax_node` level.
*)
type syntax_token = (Syntax_kind.t, string) Ceibo.Red.syntax_token
type syntax_trivia = (Syntax_kind.t, string) Ceibo.Red.syntax_trivia
(** Thin wrapper around a Ceibo token with helpers for common token-oriented
    queries.

    Use `text` when you care about the source spelling and `span` when you need
    source coordinates.
*)
(** Dotted identifiers and paths.

    The same representation is used for value paths, type constructor paths, and
    module paths whenever the grammar is simply a chain of segments separated by
    dots.

    Examples:

    ```ocaml,norun
    x
    List.map
    ```

    `Map.Make(Key).t` is represented only for the dotted segments that the CST
    lifts directly; richer functor application syntax is modeled elsewhere.
*)
type green_node = (Syntax_kind.t, string) Ceibo.Green.node
module Token: sig
  type fixed_operator =
    | BooleanAnd
    | BooleanOr
    | PipeForward
    | PrefixMinus
    | PrefixNegate
  (** A CST token wrapper.

      This is used throughout the CST anywhere the concrete spelling is part of
      the public shape, for example the `name_token` in `let x = ...` or the
      `operator_token` in `a + b`.
  *)
  type t = {
    syntax_token: syntax_token;
  }
  val syntax_token: t -> syntax_token

  val text: t -> string

  val full_text: t -> string

  val leading_trivia: t -> syntax_trivia list

  val span: t -> Ceibo.Span.t

  val same_text: t -> t -> bool

  val fixed_operator: t -> fixed_operator option

  val is_operator_like_name: t -> bool

  val is_identifier_like_name: t -> bool
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
val trivia_of_syntax_trivia: syntax_trivia -> trivia option

(** An OCaml attribute attached to some surrounding grammar node.

    This covers item, type, pattern, expression, and module-language attributes.
    The `sigil_token` preserves whether the attribute was introduced with a
    single `@`, double `@@`, or floating `@@@`-style sigil.

    Examples:

    ```ocaml,norun
    let x = 1 [@inline]
    type t = int [@@boxed]
    [@@@warning "-32"]
    ```
*)
module Ident: sig
  type t =
    | Ident of { syntax_node: syntax_node; name_token: Token.t }
    (** A single unqualified segment such as `x`, `result`, or `List`. *)
    | Qualified of { syntax_node: syntax_node; prefix: t; dot_token: Token.t; name_token: Token.t }

  (** A dotted path segment appended to an earlier path.

            Examples include `Std.List`, `M.N`, and `Foo.Bar.baz`.
        *)
  val syntax_node: t -> syntax_node

  val segments: t -> Token.t list

  val last_segment: t -> Token.t option

  val name: t -> string option

  val from_string: string -> t

  val equal: t -> t -> bool
end

type attribute = {
  syntax_node: syntax_node;
  sigil_tokens: Token.t list;
  name: Ident.t;
  payload: payload option;
}

(** A PPX extension node.

    Extensions appear in whatever grammar position admits `[%name ...]`,
    `[%%name ...]`, or similar extension forms.

    Examples:

    ```ocaml,norun
    [%sql "select 1"]
    [%foo: int]
    ```
*)
and extension = {
  syntax_node: syntax_node;
  sigil_tokens: Token.t list;
  name: Ident.t;
  payload: payload option;
  attributes: attribute list;
}

(** Payloads attached to attributes and extensions.

    Attribute and extension payloads stay lossless but opaque for now. The CST
    preserves the shell-local token slice exactly and does not commit to any
    OCaml-specific payload grammar.
*)
and payload =
  | Opaque of { tokens: Token.t list }

(** A lossless opaque payload preserved as its raw shell-local token
          slice.

          This covers both foreign mini-languages such as
          `[%sql SELECT * FROM users]` and OCaml-shaped payload bodies we are
          intentionally leaving opaque for now.
      *)
(** A field inside an object type.

    This covers method-like object type members written between `<` and `>`.

    Examples:

    ```ocaml,norun
    < next : int; close : unit -> unit >
    ```
*)
and object_type_field = {
  syntax_node: syntax_node;
  field_name: Token.t;
  colon_token: Token.t;
  field_type: core_type;
  semicolon_token: Token.t option;
}

(** Binder names introduced by explicitly polymorphic type syntax.

    These show up in quantified types such as `'a. 'a -> 'a` and
    `type a b. a -> b -> a`.
*)
and type_binder =
  | Quoted of { syntax_node: syntax_node; name_token: Token.t }
  (** A quoted type binder written with a leading apostrophe.

          Example: `'a` in `'a. 'a -> 'a`.
      *)
  | Bare of { name_token: Token.t }

(** A bare binder reconstructed from `type ... .` syntax.

          Example: `a` in `type a. a -> a`.

          Bare binders do not currently expose their own `syntax_node`; they are
          recovered from the token stream of the surrounding quantified type.
      *)
(** A field inside a record type.

    This covers immutable and mutable record fields declared between `{` and
    `}`. Field-level attributes such as `[@deprecated]` are attached here,
    while attributes nested inside parentheses remain part of `field_type`.

    Examples:

    ```ocaml,norun
    { name : string; mutable count : int }
    { count : int [@deprecated] }
    ```
*)
and record_type_field = {
  syntax_node: syntax_node;
  field_name: Token.t;
  mutable_token: Token.t option;
  colon_token: Token.t;
  field_type: core_type;
  semicolon_token: Token.t option;
  attributes: attribute list;
}

(** A tag inside a polymorphic variant type.

    Examples:

    ```ocaml,norun
    [ `Ok of int | `Error of string ]
    [ `Ready ]
    ```
*)
and poly_variant_tag = {
  syntax_node: syntax_node;
  attributes: attribute list;
  bar_token: Token.t option;
  tag_name: Token.t;
  separator_token: Token.t option;
  payload_type: core_type option;
}

(** The bound marker on a polymorphic variant type.

    Examples:

    ```ocaml,norun
    [ `A | `B ]
    [< `A | `B ]
    [> `A | `B ]
    ```
*)
and poly_variant_bound =
  | Exact
  (** An exact row with no explicit bound marker.

          Example: `[ `A | `B ]`.
      *)
  | UpperBound of { marker_token: Token.t }
  (** An upper-bounded row introduced with `<`.

          Example: `[< `A | `B ]`.
      *)
  | LowerBound of { marker_token: Token.t }

(** A lower-bounded row introduced with `>`.

          Example: `[> `A | `B ]`.
      *)
(** A row field inside a polymorphic variant type.

    This covers both concrete tags and inherited rows.

    Examples:

    ```ocaml,norun
    [ `Ok of int | `Error of string ]
    [ color | `Yellow ]
    ```
*)
and row_field =
  | Tag of poly_variant_tag
  (** A concrete tag row such as `` `Ok of int ``. *)
  | Inherit of { bar_token: Token.t option; syntax_node: syntax_node; type_: core_type }

(** An inherited row type such as `color` in
          `[ color | `Yellow ]`.
      *)
(** A full polymorphic variant row.

    The row keeps the surrounding syntax node, the bound marker, and the row
    fields in source order.
*)
and poly_variant = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  kind: poly_variant_bound;
  fields: row_field list;
  closing_token: Token.t;
}

(** A `constraint ... = ...` attached to a type declaration.

    These correspond to `ptype_cstrs` in the stock parsetree and preserve both
    sides as lifted core types.

    Examples:

    ```ocaml,norun
    type 'a t = 'a list constraint 'a = int
    type ('a, 'b) pair = 'a * 'b constraint 'a = int constraint 'b = string
    ```
*)
and type_constraint = {
  syntax_node: syntax_node;
  left: core_type;
  equals_token: Token.t;
  right: core_type;
}

(** Whether a type declaration is public or explicitly `private`.

    Examples:

    ```ocaml,norun
    type t = int
    type t = private int
    type color = private Red | Blue
    ```
*)
and private_flag =
  | Public
  (** A declaration with no `private` keyword, such as `type t = int`. *)
  | Private of { private_token: Token.t }

(** A declaration introduced with `private`, such as
          `type t = private int`.
      *)
(** A `with type` equation attached to a module type.

    The left-hand side is preserved as a lifted core type so constraints like
    `with type 'a t = ...` or `with type Message.t := ...` keep their target
    structure instead of collapsing to a single token.

    Examples:

    ```ocaml,norun
    S with type t = int
    S with type 'a t = 'a list
    S with type t := string
    ```
*)
and module_type_constraint = {
  syntax_node: syntax_node;
  constrained_type: core_type;
  replacement_type: core_type;
  separator_token: Token.t;
}

(** A package type used in first-class module positions.

    Package types are narrower than general module-type syntax: they consist of
    a module type path plus zero or more `with type` constraints.

    Examples:

    ```ocaml,norun
    S
    Handler with type t = int
    ```
*)
(** A package type used in first-class module positions.

    Package types are narrower than general module-type syntax: they consist of
    a module type path plus zero or more `with type` constraints, optionally
    wrapped in attribute nodes that are still valid in package-type positions.

    Examples:

    ```ocaml,norun
    S
    Handler with type t = int
    T[@foo]
    ```
*)
and package_type = {
  syntax_node: syntax_node;
  module_type_path: Ident.t;
  constraints: module_type_constraint list;
  attribute: attribute option;
}

(** A named functor parameter.

    Examples:

    ```ocaml,norun
    functor (X : S) -> T
    module F (X : S) (Y : T) = ...
    ```
*)
and functor_parameter = {
  syntax_node: syntax_node;
  name_token: Token.t;
  colon_token: Token.t;
  module_type: module_type;
}

(** A named label attached to an arrow parameter type.

    This mirrors the stock parsetree's distinction between regular labeled
    arguments and optional labeled arguments while still preserving any leading
    sigil token that appeared in source.

    Examples:

    ```ocaml,norun
    x:int -> string
    ?state:string -> bool
    ```
*)
and arrow_label =
  | Named of { sigil_token: Token.t option; label_token: Token.t; colon_token: Token.t }
  (** A regular labeled arrow parameter such as `x:int -> ...`.

          The optional `sigil_token` preserves syntaxes that spell the label
          with an explicit `~`.
      *)
  | OptionalNamed of { sigil_token: Token.t; label_token: Token.t; colon_token: Token.t }

(** An optional arrow parameter such as `?state:string -> ...`. *)
(** Core type syntax.

    This family covers the type grammar used in annotations, manifests, record
    fields, object fields, value declarations, and first-class module types. The
    representation aims to surface the common structural forms directly while
    still retaining the original `syntax_node` for details that are only kept in
    the underlying Ceibo tree.
*)
and core_type =
  | Wildcard of { syntax_node: syntax_node; wildcard_token: Token.t }
  (** An anonymous type variable written as `_`.

          Examples include `_` in `type (_, 'a) t` or `_` in
          `let f (_ : _) = ...`.
      *)
  | Var of { syntax_node: syntax_node; sigil_token: Token.t option; name_token: Token.t }
  (** A named type variable such as `'a`, `'state`, `'msg`, or bare `a` in
          locally abstract `type a. ...` syntax. *)
  | Constr of { syntax_node: syntax_node; constructor_path: Ident.t; arguments: core_type list }
  (** A named type constructor, optionally applied to arguments.

          Covered grammar includes plain constructor names and type
          applications such as `int`, `'a list`, `(int, string) result`, or
          `Map.S.t`.
      *)
  | Class of {
      syntax_node: syntax_node;
      hash_token: Token.t;
      class_path: Ident.t;
      arguments: core_type list
    }
  (** A class type written with `#`.

          Examples:

          ```ocaml,norun
          #iterator
          int #iterator
          ```
      *)
  | Alias of {
      syntax_node: syntax_node;
      type_: core_type;
      sigil_token: Token.t option;
      name_token: Token.t
    }
  (** A type aliased with `as`.

          Example: `('a list as 'whole)`.
      *)
  | Attribute of { syntax_node: syntax_node; type_: core_type; attribute: attribute }
  (** A type expression with an attached attribute.

          Example: `int [@boxed]`.
      *)
  | Extension of extension
  (** A PPX extension parsed in type position.

          Example: `[%foo: int]`.
      *)
  | Poly of {
      syntax_node: syntax_node;
      type_keyword_token: Token.t option;
      dot_token: Token.t;
      binders: type_binder list;
      body: core_type
    }
  (** An explicitly quantified type.

          Covered grammar includes both apostrophe binders and locally abstract
          `type` binders.

          Examples:

          ```ocaml,norun
          'a. 'a -> 'a
          type a. a -> a
          ```
      *)
  | Arrow of {
      syntax_node: syntax_node;
      label: arrow_label option;
      parameter_type: core_type;
      result_type: core_type
    }
  (** A function type.

          This covers simple, labeled, and optional arrows.

          Examples:

          ```ocaml,norun
          int -> string
          label:int -> string
          ?state:string -> bool
          ```
      *)
  | Tuple of { syntax_node: syntax_node; elements: core_type list }
  (** A tuple type with two or more elements.

          Example: `int * string * bool`.
      *)
  | Parenthesized of { syntax_node: syntax_node; inner: core_type }
  (** A parenthesized type used for grouping.

          Examples:

          ```ocaml,norun
          (int -> string)
          ('a * 'b) list
          ```
      *)
  | PolyVariant of poly_variant
  (** A polymorphic variant type.

          Examples:

          ```ocaml,norun
          [ `Ok of int | `Error of string ]
          [ `Tick | `Tock ]
          [ color | `Yellow ]
          [> `Ready | `Pending ]
          ```
      *)
  | Record of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      fields: record_type_field list;
      separator_tokens: Token.t list;
      closing_token: Token.t
    }
  (** A record type definition written between `{` and `}`.

          Example: `{ name : string; mutable visits : int }`.
      *)
  | FirstClassModule of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      package_type: package_type;
      closing_token: Token.t
    }
  (** A first-class module type.

          Examples:

          ```ocaml,norun
          (module S)
          (module Handler with type t = int)
          ```
      *)
  | Object of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      fields: object_type_field list;
      closing_token: Token.t
    }

(** An object type written between `<` and `>`.

          Example: `< push : int -> unit; close : unit -> unit >`.
      *)
(** Module type expressions.

    These nodes cover signature-like grammar that appears in module
    declarations, module type declarations, functor parameters, `with type`
    constraints, and first-class module types.
*)
and module_type =
  | Path of Ident.t
  (** A named module type path such as `S`, `Set.S`, or `Driver.Intf`. *)
  | TypeOf of { syntax_node: syntax_node; of_token: Token.t; module_path: Ident.t }
  (** A `module type of` expression.

          Example: `module type of M`.
      *)
  | Signature of { syntax_node: syntax_node; signature_syntax_node: syntax_node }
  (** A raw `sig ... end` module type.

          The signature body is preserved as `signature_syntax_node` rather than
          being reified into a separate public signature-item tree.

          Example: `sig type t val make : unit -> t end`.
      *)
  | Functor of { syntax_node: syntax_node; parameters: functor_parameter list; result: module_type }
  (** A functor module type.

          Examples:

          ```ocaml,norun
          functor (X : S) -> T
          functor (X : S) (Y : T) -> U
          ```
      *)
  | With of { syntax_node: syntax_node; base: module_type; constraints: module_type_constraint list }
  (** A constrained module type using `with`.

          Examples:

          ```ocaml,norun
          S with type t = int
          Handler with type state := string and type item = int
          ```
      *)
  | Parenthesized of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      inner: module_type;
      closing_token: Token.t
    }
  (** A parenthesized module type used for grouping.

          Example: `(S with type t = int)`.
      *)
  | Attribute of { syntax_node: syntax_node; module_type: module_type; attribute: attribute }
  (** A module type with an attached attribute.

          Example: `(module type of M [@foo])`.
      *)
  | Extension of extension

(** A PPX extension parsed in module-type position.

          Example: `[%foo: S]`.
      *)
(** Class type syntax.

    This family covers the type grammar used by `class ... : ...` annotations
    and `class type ... = ...` declarations.
*)
and class_type =
  | Path of Ident.t
  (** A named class type path such as `c`, `Widget.t`, or `Driver.class_t`. *)
  | Signature of { syntax_node: syntax_node; fields: class_type_field list }
  (** An `object ... end` class signature.

          Example:

          ```ocaml,norun
          object
            inherit base
            val mutable state : int
            method run : unit -> unit
            constraint t = int
          end
          ```
      *)
  | Arrow of {
      syntax_node: syntax_node;
      label: arrow_label option;
      parameter_type: core_type;
      result_type: class_type
    }
  (** An arrow-style class type.

          Examples:

          ```ocaml,norun
          int -> object method run : int end
          request -> response -> service
          ```
      *)
  | Parenthesized of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      inner: class_type;
      closing_token: Token.t
    }
  (** A parenthesized class type used for grouping.

          Example: `([%driver])`.
      *)
  | Attribute of { syntax_node: syntax_node; class_type: class_type; attribute: attribute }
  (** A class type with an attached attribute.

          Example: `object method x : int end [@foo]`.
      *)
  | Extension of extension

(** A PPX extension parsed in class-type position.

          Example: `[%foo]`.
      *)
(** Fields inside an `object ... end` class signature. *)
and class_type_field =
  | Inherit of { syntax_node: syntax_node; class_type: class_type }
  (** An inherited class type field.

          Example: `inherit base`.
      *)
  | Value of {
      syntax_node: syntax_node;
      name_token: Token.t;
      colon_token: Token.t;
      type_: core_type;
      modifier_tokens: Token.t list
    }
  (** A value declaration in a class signature.

          Examples:

          ```ocaml,norun
          val x : int
          val mutable state : int
          ```
      *)
  | Method of {
      syntax_node: syntax_node;
      name_token: Token.t;
      colon_token: Token.t;
      type_: core_type;
      modifier_tokens: Token.t list
    }
  (** A method declaration in a class signature.

          Examples:

          ```ocaml,norun
          method run : unit -> unit
          method private close : unit
          ```
      *)
  | Constraint of {
      syntax_node: syntax_node;
      left: core_type;
      equals_token: Token.t;
      right: core_type
    }
  (** A class-type constraint.

          Example: `constraint t = int`.
      *)
  | Attribute of { syntax_node: syntax_node; field: class_type_field; attribute: attribute }
  (** A class-type field with an attached attribute.

          Example: `val x : int [@@foo]`.
      *)
  | Extension of extension

(** A PPX extension parsed as a class-type field.

          Example: `[%%foo]`.
      *)

(** Core type syntax.

    This covers the type grammar that appears in annotations, manifests,
    constructor payloads, object fields, and module constraints.

    Examples:

    ```ocaml,norun
    int
    'a list
    name:string -> age:int -> person
    < close : unit -> unit >
    (module S with type t = int)
    ```

    This view stays faithful to the parsed source. Parentheses, local opens,
    attributes, and extensions remain explicit instead of being normalized away.
*)

(** A single `with`-constraint inside a module type.

    Examples:

    ```ocaml,norun
    S with type t = int
    S with type t := int
    ```
*)

(** A type equality constraint attached to a declaration.

    Example:

    ```ocaml,norun
    type ('a, 'b) t constraint 'a = 'b
    ```
*)
module CoreType: sig
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
  val syntax_node: t -> syntax_node
end

(** A function-arrow label.

    This preserves whether the argument was unlabeled, labeled, or optional.
    The label itself is kept as written rather than normalized into a plain
    string.
*)
module ModuleTypeConstraint: sig
  type t = module_type_constraint = {
    syntax_node: syntax_node;
    constrained_type: core_type;
    replacement_type: core_type;
    separator_token: Token.t;
  }
end

(** A named functor parameter together with its module type.

    Example:

    ```ocaml,norun
    functor (X : S) -> T
    ```
*)
module TypeConstraint: sig
  type t = type_constraint = {
    syntax_node: syntax_node;
    left: core_type;
    equals_token: Token.t;
    right: core_type;
  }
end

(** Module type syntax.

    Examples:

    ```ocaml,norun
    S
    sig type t end
    functor (X : S) -> T
    S with type t = int
    ```

    Signature bodies are still represented by their `signature_syntax_node`
    anchor today. That keeps the CST faithful and lossless without pretending
    signature items are already reified more deeply than they are.
*)
module ArrowLabel: sig
  type t = arrow_label =
    | Named of { sigil_token: Token.t option; label_token: Token.t; colon_token: Token.t }
    | OptionalNamed of { sigil_token: Token.t; label_token: Token.t; colon_token: Token.t }
  val sigil_token: t -> Token.t option

  val label_token: t -> Token.t

  val colon_token: t -> Token.t

  val name: t -> string

  val is_optional: t -> bool
end

(** Class type syntax.

    Examples:

    ```ocaml,norun
    service
    object method run : unit -> unit end
    label:int -> service
    ```
*)
module FunctorParameter: sig
  type t = functor_parameter = {
    syntax_node: syntax_node;
    name_token: Token.t;
    colon_token: Token.t;
    module_type: module_type;
  }
end

(** A field inside a class type signature.

    This covers inherited class types, values, methods, constraints, and the
    attribute/extension shells that can wrap them.
*)
module ModuleType: sig
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
  val syntax_node: t -> syntax_node
end

(** How a string literal is delimited in source.

    `"..."` uses `DoubleQuote`, while OCaml's quoted-string form uses
    `Quoted { marker }`, where `marker` is the identifier between `{` and `|`.

    Examples:

    ```ocaml,norun
    "hello"
    {tag|hello|tag}
    ```
*)
module ClassType: sig
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
  val syntax_node: t -> syntax_node
end

module ClassTypeField: sig
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
  val syntax_node: t -> syntax_node
end

(** The numeric base of an integer literal. *)
type string_delimiter =
  | DoubleQuote
  (** An ordinary double-quoted string such as `"hello"`. *)
  | Quoted of { marker: string }
(** A quoted string such as `{sql|select 1|sql}`.

          `marker` stores the identifier between `{` and `|`.
      *)
(** The explicit sign written in a float exponent. *)
type integer_base =
  | Decimal
  (** Base-10 digits, such as `42` or `1_000`. *)
  | Hexadecimal
  (** Base-16 digits, introduced with `0x` or `0X`. *)
  | Octal
  (** Base-8 digits, introduced with `0o` or `0O`. *)
  | Binary
(** Base-2 digits, introduced with `0b` or `0B`. *)
(** A string literal with delimiter and body details preserved.

    `contents` stores the literal body exactly as written between the
    delimiters. Escape sequences remain escaped.
*)
type exponent_sign =
  | Positive
  (** A `+` exponent sign, as in `1.0e+5`. *)
  | Negative
(** A `-` exponent sign, as in `1.0e-5`. *)
(** An integer literal with its base and optional suffix preserved.

    `digits` stores the exact digits as written, including underscores.

    Examples:

    ```ocaml,norun
    42
    0xff
    0b1111_0000l
    ```
*)
type string_constant = {
  syntax_node: syntax_node;
  literal_token: Token.t;
  delimiter: string_delimiter;
  contents: string;
  terminated: bool;
  attributes: attribute list;
}
(** The exponent part of a floating-point literal.

    Examples include the `e-5` in `1.0e-5` and the `E10` in `3.14E10`.
*)
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
(** A floating-point literal with its written pieces preserved.

    `integral_digits` and `fractional_digits` keep underscores exactly as they
    appeared in source.

    Examples:

    ```ocaml,norun
    3.14
    2.0e-5
    1.2g
    ```
*)
type float_exponent = {
  marker: string;
  sign: exponent_sign option;
  digits: string;
}
(** A character literal with its inner contents preserved exactly as written.

    For example, `contents = "\\n"` for the literal `'\n'`.
*)
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
(** A boolean literal such as `true` or `false`. *)
type char_constant = {
  syntax_node: syntax_node;
  literal_token: Token.t;
  contents: string;
  attributes: attribute list;
}
(** Literal forms accepted directly in pattern and expression position.

    The constructors intentionally preserve the written constant structure
    instead of collapsing everything to a single token.
*)
(** Alias of {!Constant} used for direct literal patterns. *)
type bool_constant = {
  syntax_node: syntax_node;
  literal_token: Token.t;
  value: bool;
  attributes: attribute list;
}

(** Alias for `PatternLiteral.t`. *)
module Constant: sig
  type t =
    | String of string_constant
    (** String literals such as `"hello"` or `{tag|hello|tag}`. *)
    | Int of integer_constant
    (** Integer literals such as `0`, `42`, or `0xffL`. *)
    | Float of float_constant
    (** Floating-point literals such as `3.14` or `2.0e-5`. *)
    | Char of char_constant
    (** Character literals such as `'a'` or `'\n'`. *)
    | Bool of bool_constant
    (** Boolean literals `true` and `false`. *)
    | Unit of { syntax_node: syntax_node; attributes: attribute list }

  (** The unit literal `()`. *)
  val syntax_node: t -> syntax_node
end

module PatternLiteral = Constant

(** Namespace helpers for `type_binder`.

    The constructors mirror `type_binder` exactly, so the binder grammar
    documented above applies here unchanged.
*)
(** Pattern syntax.

    Patterns appear in `let` bindings, function parameters, `match` cases,
    `try ... with` handlers, object self patterns, and effect handlers. This
    type covers the common structural forms directly and uses the record payload
    types below to expose the relevant nested grammar.
*)
type pattern_literal = PatternLiteral.t
module TypeBinder: sig
  type t = type_binder =
    | Quoted of { syntax_node: syntax_node; name_token: Token.t }
    | Bare of { name_token: Token.t }
  val name_token: t -> Token.t

  val name: t -> string

  val text: t -> string

  val is_quoted: t -> bool
end

type pattern =
  | Identifier of identifier_pattern
  (** A binding-name pattern such as `x`, `value`, or `state`. *)
  | Wildcard of wildcard_pattern
  (** A wildcard pattern written as `_`. *)
  | Extension of extension_pattern
  (** A PPX extension parsed in pattern position.

          Example: `[%foo? p]`.
      *)
  | Literal of literal_pattern
  (** A literal pattern such as `"hello"`, `true`, `2112`, or `()`. *)
  | Lazy of lazy_pattern
  (** A lazy pattern.

          Example: `lazy p`.
      *)
  | Exception of exception_pattern
  (** An exception pattern used in handlers.

          Example: `exception Not_found`.
      *)
  | Range of range_pattern
  (** A range pattern.

          Examples include `'a' .. 'z'` and `0 .. 9`.
      *)
  | Operator of operator_pattern
  (** An operator identifier used as a pattern name.

          Example: `( :: )`.
      *)
  | FirstClassModule of first_class_module_pattern
  (** A first-class module unpacking pattern.

          Examples:

          ```ocaml,norun
          (module M)
          (module M : S)
          ```
      *)
  | PolyVariant of poly_variant_pattern
  (** A polymorphic variant tag pattern such as `` `Ok x `` or `` `Done ``. *)
  | PolyVariantInherit of poly_variant_inherit_pattern
  (** A polymorphic variant type inheritance pattern such as `#message`. *)
  | Constructor of constructor_pattern
  (** A constructor pattern such as `Some x`, `Ok (a, b)`, or `M.Error`. *)
  | Tuple of tuple_pattern
  (** A tuple pattern such as `(x, y)` or `(x, y, z)`. *)
  | List of list_pattern
  (** A list pattern such as `[x; y; z]`. *)
  | Array of array_pattern
  (** An array pattern such as `[| x; y |]`. *)
  | Record of record_pattern
  (** A record pattern such as `{ x; y = Some z }`. *)
  | Cons of cons_pattern
  (** A cons pattern written with `::`, such as `x :: xs`. *)
  | Or of or_pattern
  (** An alternative pattern such as `None | Some _`. *)
  | Alias of alias_pattern
  (** A pattern alias introduced with `as`.

          Example: `(Some x as whole)`.
      *)
  | Typed of typed_pattern
  (** A type-constrained pattern.

          Example: `(p : t)`.
      *)
  | Effect of effect_pattern
  (** An effect pattern.

          Example: `effect Yield k`.
      *)
  | LocalOpen of local_open_pattern
  (** A locally opened module path in pattern position.

          Example: `M.(Some x)`.
      *)
  | Parenthesized of parenthesized_pattern

(** A parenthesized pattern used for grouping, such as `(Some x)`. *)
(** Payload for `Pattern.Identifier`.

    This covers simple lowercase binding names and raw identifiers when parsed
    as patterns.
*)
and identifier_pattern = {
  syntax_node: syntax_node;
  name_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Pattern.Wildcard`.

    Carries the original `syntax_node` for `_`.
*)
and wildcard_pattern = {
  syntax_node: syntax_node;
  attributes: attribute list;
}

(** Payload for `Pattern.Literal`.

    This keeps literal patterns like `1`, `"x"`, and `()` in the pattern tree
    while attaching outer `[@attr]` metadata orthogonally.
*)
and literal_pattern = {
  syntax_node: syntax_node;
  literal: pattern_literal;
  attributes: attribute list;
}

(** Payload for `Pattern.Extension`.

    The nested `extension` preserves the `[%foo? ...]` payload, while
    `attributes` keeps any trailing pattern attributes such as
    `([%foo? p] [@bar])`.
*)
and extension_pattern = {
  syntax_node: syntax_node;
  extension: extension;
  attributes: attribute list;
}

(** Payload for `Pattern.Lazy`.

    Covers `lazy p`.
*)
and lazy_pattern = {
  syntax_node: syntax_node;
  pattern: pattern;
  attributes: attribute list;
}

(** Payload for `Pattern.Exception`.

    Covers handler patterns like `exception Exit`.
*)
and exception_pattern = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  pattern: pattern;
  attributes: attribute list;
}

(** Payload for `Pattern.Range`.

    Both endpoints are lifted as parsed constants so tools can reason about the
    written interval structure without reparsing token text.
*)
and range_pattern = {
  syntax_node: syntax_node;
  lower: pattern_literal;
  upper: pattern_literal;
  attributes: attribute list;
}

(** Payload for `Pattern.Operator`.

    This is used when the pattern is spelled as an operator token sequence
    instead of an alphanumeric identifier.
*)
and operator_pattern = {
  syntax_node: syntax_node;
  operator_tokens: Token.t list;
  attributes: attribute list;
}

(** Payload for `Pattern.FirstClassModule`.

    This preserves whether the unpack binds a named module or uses `_` to
    discard it.
*)
and first_class_module_pattern_binding =
  | Named of { name_token: Token.t }
  (** A named unpack binding such as `M` in `(module M : S)`. *)
  | Anonymous of { wildcard_token: Token.t }

(** An anonymous unpack binding such as `_` in `(module _ : S)`. *)
(** Payload for `Pattern.FirstClassModule`.

    This covers patterns of the form `(module M)`, `(module _)`,
    `(module M : S)`, and `(module _ : S)`.
*)
and first_class_module_pattern = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  binding: first_class_module_pattern_binding;
  colon_token: Token.t option;
  package_type: package_type option;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Pattern.PolyVariant`.

    This mirrors OCaml's `Parsetree.Ppat_variant` shape: a variant label plus an
    optional nested payload pattern.

    The `payload` is present for forms like `` `Ok x `` and absent for bare tags
    like `` `Done ``.
*)
and poly_variant_pattern = {
  syntax_node: syntax_node;
  tag_token: Token.t;
  payload: pattern option;
  attributes: attribute list;
}

(** Payload for `Pattern.PolyVariantInherit`.

    This covers inheritance-style polymorphic variant patterns such as
    `#message` or `#M.message`.
*)
and poly_variant_inherit_pattern = {
  syntax_node: syntax_node;
  type_path: Ident.t;
  attributes: attribute list;
}

(** Existential binders introduced by a constructor pattern.

    This covers the `(type ...)` clause in GADT-style patterns such as
    `Closure (type a) ((f, x) : (a -> _) * _)`.
*)
and constructor_pattern_existentials = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  type_keyword_token: Token.t;
  binders: type_binder list;
  closing_token: Token.t;
}

(** Payload for `Pattern.Constructor`.

    `existentials` is present for GADT-style patterns such as
    `Closure (type a) payload`. Constructor arguments are stored in source
    order, so both unary and tupled payloads can be inspected structurally.
*)
and constructor_pattern = {
  syntax_node: syntax_node;
  constructor_path: Ident.t;
  existentials: constructor_pattern_existentials option;
  arguments: pattern list;
  attributes: attribute list;
}

(** Payload for `Pattern.Tuple`.

    Covers tuple patterns such as `(x, y)`, `(~x, y)`, and `(~x, y, ..)`.
*)
and tuple_pattern = {
  syntax_node: syntax_node;
  elements: tuple_pattern_element list;
  open_tail: tuple_pattern_open_tail option;
  attributes: attribute list;
}

(** One element inside a tuple pattern.

    `label_token` is present for labeled components such as `~x` and
    `~state:pattern`. The nested `pattern` keeps the payload pattern after the
    label, or the implicit binder for punning forms like `~x`.
*)
and tuple_pattern_element = {
  label_token: Token.t option;
  pattern: pattern;
}

(** Explicit open tail on a tuple pattern.

    This preserves the trailing `..` in open tuple patterns such as
    `(~x, ~y, ..)`.
*)
and tuple_pattern_open_tail = {
  dotdot_token: Token.t;
}

(** Payload for `Pattern.List`.

    Covers list patterns such as `[x]` and `[head; tail]`.
*)
and list_pattern = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  elements: pattern list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Pattern.Array`.

    Covers array patterns such as `[| x; y |]`.
*)
and array_pattern = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  elements: pattern list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Pattern.Record`.

    Each field may be explicit, as in `{ x = p }`, or punning, as in `{ x }`.
    The `closedness` field preserves whether the pattern is closed or carries
    an explicit `_` wildcard tail.
*)
and record_pattern = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  fields: record_pattern_field list;
  separator_tokens: Token.t list;
  closedness: record_pattern_closedness;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Whether a record pattern is closed or explicitly open.

    Examples:

    ```ocaml,norun
    { user; name }
    { user; _ }
    ```
*)
and record_pattern_closedness =
  | Closed
  (** A closed record pattern such as `{ user; name }`. *)
  | Open of { wildcard_token: Token.t }

(** An open record pattern such as `{ user; _ }`. *)
(** A single field inside a record pattern.

    When `pattern` is `None`, the source used punning syntax such as `{ field }`.
    When it is `Some p`, the source spelled an explicit field pattern such as
    `{ field = p }`.
*)
and record_pattern_field = {
  syntax_node: syntax_node;
  field_path: Ident.t;
  equals_token: Token.t option;
  pattern: pattern option;
}

(** Payload for `Pattern.Cons`.

    Covers `head :: tail`.
*)
and cons_pattern = {
  syntax_node: syntax_node;
  head: pattern;
  tail: pattern;
  attributes: attribute list;
}

(** Payload for `Pattern.Or`.

    Alternatives are preserved in source order from left to right.
*)
and or_pattern = {
  syntax_node: syntax_node;
  alternatives: pattern list;
  separator_tokens: Token.t list;
  attributes: attribute list;
}

(** Payload for `Pattern.Alias`.

    Covers `p as name`.
*)
and alias_pattern = {
  syntax_node: syntax_node;
  pattern: pattern;
  name_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Pattern.Typed`.

    Covers `(p : t)` and related parenthesized type-constrained patterns.
*)
and typed_pattern = {
  syntax_node: syntax_node;
  pattern: pattern;
  colon_token: Token.t;
  type_: core_type;
  attributes: attribute list;
}

(** Payload for `Pattern.Effect`.

    The `effect_pattern` is the matched effect constructor, and `continuation`
    is the continuation binder pattern.
*)
and effect_pattern = {
  syntax_node: syntax_node;
  effect_pattern: pattern;
  continuation: pattern;
  attributes: attribute list;
}

(** Payload for `Pattern.LocalOpen`.

    Covers locally opened patterns such as `M.(Some x)`.
*)
and local_open_pattern = {
  syntax_node: syntax_node;
  module_path: Ident.t;
  dot_token: Token.t;
  opening_token: Token.t option;
  pattern: pattern;
  closing_token: Token.t option;
  attributes: attribute list;
}

(** Payload for `Pattern.Parenthesized`. *)
and parenthesized_pattern = {
  syntax_node: syntax_node;
  inner: pattern;
  attributes: attribute list;
}
module Literal = Constant

(** Alias of {!Constant} used for direct literal expressions. *)
type literal = Literal.t

(** A positional function parameter.

    This covers ordinary unlabeled parameters in `fun` expressions and
    function-like `let` bindings, such as `x`, `_`, or `(x : int)`.
*)
and positional_parameter = {
  syntax_node: syntax_node;
  pattern: pattern;
  name_token: Token.t option;
}

(** A labeled parameter introduced with `~`.

    Examples include `~x` and `~label`.
*)
and labeled_parameter = {
  syntax_node: syntax_node;
  sigil_token: Token.t;
  label_token: Token.t;
  colon_token: Token.t option;
  binding_name_token: Token.t option;
  binding_name_matches_label: bool;
  binding_pattern: pattern option;
}

(** An optional parameter introduced with `?`.

    This covers both plain optional parameters like `?x` and parameters with a
    default such as `?(x = 0)`.

    `default_value = None` represents plain optional parameters such as `?x`.
    `default_value = Some expr` represents defaulted parameters such as
    `?(x = expr)`.
*)
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

(** A locally abstract type parameter in function parameter position.

    Examples:

    ```ocaml,norun
    (type a)
    (type a b)
    ```
*)
and locally_abstract_type_parameter = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  type_keyword_token: Token.t;
  binders: type_binder list;
  closing_token: Token.t;
}

(** Function parameter syntax.

    Parameters appear in `fun` expressions and in function-shaped `let`
    bindings. The CST separates positional, labeled, optional, and locally
    abstract parameters so tooling can reason about the source-level calling
    convention directly.
*)
and parameter =
  | Positional of positional_parameter
  (** An ordinary unlabeled parameter such as `x` or `(x : int)`. *)
  | Labeled of labeled_parameter
  (** A labeled parameter such as `~x`. *)
  | Optional of optional_parameter
  (** An optional parameter such as `?x` or `?(x = 0)`. *)
  | LocallyAbstract of locally_abstract_type_parameter

(** A locally abstract type binder such as `(type a)`. *)
(** Namespace helpers for `parameter`.

    The constructors mirror `parameter` exactly, so the parameter grammar
    documented above applies here unchanged.
*)
(** An exception declaration header.

    This is used both for top-level `exception` items and `let exception ... in`
    expressions. The public CST currently exposes the declared name directly and
    retains any richer payload shape in the underlying `syntax_node`.

    Examples:

    ```ocaml,norun
    exception Panic
    exception Panic of string
    ```
*)
and exception_rhs =
  | Alias of { equals_token: Token.t; alias: Ident.t }
  | Payload of { of_token: Token.t; payload_type: core_type }

and exception_declaration = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  name_token: Token.t;
  rhs: exception_rhs option;
}

(** Expression syntax.

    This is the main term-level grammar family. It covers evaluated expressions,
    control-flow forms, object syntax, record and collection literals, local
    module constructs, and first-class modules.

    Trailing `[@attr]` metadata is attached orthogonally through
    {!Expression.attributes}; it does not introduce a dedicated wrapper
    constructor.
*)
and expression =
  | Path of path_expression
  (** A value path used as an expression, such as `x`, `M.value`, or
          `List.map`. *)
  | Constructor of constructor_expression
  (** A constructor expression such as `None`, `Some value`, or
          `Result.Error message`.

          This mirrors the parsetree's `Pexp_construct` shape: the constructor
          path is preserved directly and the payload remains optional.
      *)
  | Operator of operator_expression
  (** An operator identifier used as an expression, such as `(+)` or `(@@)`. *)
  | Literal of literal
  (** A literal expression such as `"hello"`, `42`, `true`, or `()`. *)
  | Unreachable of unreachable_expression
  (** An unreachable expression written as `.`.

          Example: `| None -> .`.
      *)
  | Extension of extension
  (** A PPX extension parsed in expression position, such as `[%sql "..."]`. *)
  | Object of object_expression
  (** An object expression.

          Example:

          ```ocaml,norun
          object
            method size = 0
          end
          ```
      *)
  | PolyVariant of poly_variant_expression
  (** A polymorphic variant expression such as `` `Ok value `` or `` `Done ``. *)
  | ModulePack of module_pack_expression
  (** A packed first-class module expression.

          Examples:

          ```ocaml,norun
          (module M)
          (module M : S)
          ```
      *)
  | LetModule of let_module_expression
  (** A local module binding expression.

          Example: `let module M = N in body`.
      *)
  | LetException of let_exception_expression
  (** A local exception declaration expression.

          Example: `let exception Panic of string in body`.
      *)
  | Assert of assert_expression
  (** An assertion expression: `assert cond`. *)
  | Lazy of lazy_expression
  (** A lazy expression: `lazy expr`. *)
  | While of while_expression
  (** A `while ... do ... done` loop. *)
  | For of for_expression
  (** A `for ... = ... to|downto ... do ... done` loop. *)
  | Apply of apply_expression
  (** A function application, including labeled and optional arguments. *)
  | MethodCall of method_call_expression
  (** An object method call such as `obj#run`. *)
  | New of new_expression
  (** An object instantiation such as `new queue`. *)
  | Prefix of prefix_expression
  (** A prefix operator application such as `!cell` or `~-x`. *)
  | FieldAccess of field_access_expression
  (** A field access such as `record.field`. *)
  | Index of index_expression
  (** An indexing expression such as `arr.(i)` or `s.[i]`. *)
  | ObjectOverride of object_override_expression
  (** An object override expression such as `{< x = 1; y = 2 >}`.

          This mirrors the parsetree's `Pexp_override` shape: the overridden
          instance variables are modeled directly instead of being reused as
          record fields.
      *)
  | InstanceVariableAssign of instance_variable_assign_expression
  (** An instance-variable assignment inside object syntax, such as
          `count <- count + 1`. *)
  | FieldAssign of field_assign_expression
  (** A record-field assignment such as `record.field <- value`. *)
  | Assign of assign_expression
  (** A general assignment expression such as `r := 1` or `arr.(i) <- x`. *)
  | Infix of infix_expression
  (** An infix operator expression such as `a + b` or `x |> f`. *)
  | TypeAscription of type_ascription_expression
  (** A type-constrained or coerced expression such as `(expr : t)`,
          `(expr :> t)`, `(expr : s :> t)`, or `let value : t = expr`.

          When lifted from a binding annotation, `syntax_node` is the enclosing
          `let` binding because the parser does not currently emit a dedicated
          annotation expression node.
      *)
  | Polymorphic of polymorphic_expression
  (** An explicitly polymorphic binding annotation such as
          `let id : 'a. 'a -> 'a = fun x -> x`.

          This is reconstructed from the surrounding binding or typed
          expression syntax when the annotated type is a quoted `CoreType.Poly`.
      *)
  | Sequence of sequence_expression
  (** A sequence expression such as `e1; e2`. *)
  | Tuple of tuple_expression
  (** A tuple expression such as `(a, b, c)`. *)
  | List of list_expression
  (** A list expression such as `[a; b; c]`. *)
  | Array of array_expression
  (** An array expression such as `[| a; b; c |]`. *)
  | Record of record_expression
  (** A record literal or record update expression. *)
  | LocalOpen of local_open_expression
  (** A local module open expression.

          Covered forms include `M.(expr)` and `let open M in expr`.
      *)
  | Fun of fun_expression
  (** A `fun` expression with explicit parameters and either a direct body
          expression or a nested `function`-style case body. *)
  | Function of function_expression
  (** A `function` expression with case clauses and no explicit
          parameter list. *)
  | LetOperator of let_operator_expression
  (** A binding-operator expression such as
          `let* x = expr in body` or
          `let* x = expr and* y = expr in body`. *)
  | Let of let_expression
  (** A `let ... in ...` or `let rec ... in ...` expression. *)
  | Match of match_expression
  (** A `match ... with ...` expression. *)
  | Try of try_expression
  (** A `try ... with ...` expression. *)
  | If of if_expression
  (** An `if ... then ... else ...` expression. *)
  | Parenthesized of parenthesized_expression

(** A parenthesized expression used for grouping. *)
(** Payload for `Expression.Path`.

    Covers path expressions such as `x`, `M.value`, and `List.map`.
*)
and path_expression = {
  syntax_node: syntax_node;
  path: Ident.t;
  attributes: attribute list;
}

(** Payload for `Expression.Constructor`.

    Constant constructors such as `None` and applied constructors such as
    `Some 42` share this shape. Tupled payloads remain structured under the
    nested `payload` expression, for example as `Expression.Tuple`.
*)
and constructor_expression = {
  syntax_node: syntax_node;
  constructor_path: Ident.t;
  payload: expression option;
  attributes: attribute list;
}

(** Payload for `Expression.Operator`.

    This is used when the expression itself is an operator name rather than an
    infix application.
*)
and operator_expression = {
  syntax_node: syntax_node;
  operator_tokens: Token.t list;
  attributes: attribute list;
}

(** Payload for `Expression.Unreachable`.

    Covers the standalone `.` expression used in impossible branches.
*)
and unreachable_expression = {
  syntax_node: syntax_node;
  dot_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Expression.Object`.

    `self_pattern` is present for forms such as `object (self) ... end`.
*)
and object_expression = {
  syntax_node: syntax_node;
  self_pattern: pattern option;
  members: object_member list;
  attributes: attribute list;
}

(** Members inside an object expression body. *)
and object_member =
  | Method of object_method
  (** A `method` declaration inside `object ... end`. *)
  | Value of object_value
  (** A `val` declaration inside `object ... end`. *)
  | Inherit of object_inherit
  (** An `inherit expr` member. *)
  | Extension of extension
  (** A PPX extension member inside `object ... end`.

          Example: `[%%foo]`.
      *)
  | Initializer of object_initializer

(** An `initializer expr` member. *)
(** The valid definition forms for methods.

    Concrete methods require a body and may optionally carry a type
    annotation. Virtual methods require a type and never carry a body.
*)
and method_definition =
  | ConcreteMethod of { body: expression; type_: (Token.t * core_type) option }
  | VirtualMethod of { virtual_token: Token.t; type_: core_type }

(** The valid definition forms for values.

    Concrete values require a bound expression and may optionally carry a type
    annotation. Virtual values require a type and never carry a bound
    expression.
*)
and value_definition =
  | ConcreteValue of { value: expression; type_: (Token.t * core_type) option }
  | VirtualValue of { virtual_token: Token.t; type_: core_type }

(** Payload for `object_member` methods.

    This covers concrete object methods, including `private` and overriding
    forms such as `method!`.
*)
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

(** Payload for `object_member` values.

    This covers concrete object fields declared with `val`, including
    `mutable` and overriding forms.
*)
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

(** Payload for `object_member` inheritance clauses. *)
and object_inherit = {
  syntax_node: syntax_node;
  attributes: attribute list;
  expression: expression;
}

(** Payload for `object_member` initializers.

    Covers `initializer expr`.
*)
and object_initializer = {
  syntax_node: syntax_node;
  body: expression;
}

(** Payload for `Expression.PolyVariant`.

    The `payload` is present for tagged values such as `` `Ok 1 `` and absent
    for bare tags like `` `Done ``.
*)
and poly_variant_expression = {
  syntax_node: syntax_node;
  tag_token: Token.t;
  payload: expression option;
  attributes: attribute list;
}

(** Payload for `Expression.ModulePack`.

    Covers packed module expressions such as `(module M)` and
    `(module M : S)`.

    `module_expression` preserves the packed module payload directly, for
    example as `ModuleExpression.Path` or `ModuleExpression.Structure`.
    `package_type` carries the optional `: S` ascription without re-encoding it
    as a nested `ModuleExpression.Constraint`.
*)
and module_pack_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  module_expression: module_expression;
  colon_token: Token.t option;
  package_type: package_type option;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Expression.LetModule`.

    Covers local module bindings such as `let module M = struct ... end in body`.
*)
and let_module_expression = {
  syntax_node: syntax_node;
  module_name_token: Token.t;
  equals_token: Token.t;
  module_expression: module_expression;
  body: expression;
  attributes: attribute list;
}

(** Payload for `Expression.LetException`.

    Covers local exception bindings such as
    `let exception Panic of string in body`.
*)
and let_exception_expression = {
  syntax_node: syntax_node;
  exception_declaration: exception_declaration;
  body: expression;
  attributes: attribute list;
}

(** Payload for `Expression.Assert`.

    Covers `assert expr`.
*)
and assert_expression = {
  syntax_node: syntax_node;
  asserted: expression;
  attributes: attribute list;
}

(** Payload for `Expression.Lazy`.

    Covers `lazy expr`.
*)
and lazy_expression = {
  syntax_node: syntax_node;
  body: expression;
  attributes: attribute list;
}

(** Payload for `Expression.While`.

    Covers `while cond do body done`.
*)
and while_expression = {
  syntax_node: syntax_node;
  condition: expression;
  body: expression;
  attributes: attribute list;
}

(** The iteration direction of a `for` loop.

    This keeps the written keyword while also distinguishing ascending and
    descending loops structurally.

    Examples:

    ```ocaml,norun
    for i = 0 to 10 do print_int i done
    for i = 10 downto 0 do print_int i done
    ```
*)
and for_direction =
  | To of { direction_token: Token.t }
  (** An ascending loop written with `to`. *)
  | Downto of { direction_token: Token.t }

(** A descending loop written with `downto`. *)
(** Payload for `Expression.For`.

    `direction` distinguishes `to` and `downto` structurally while preserving
    the original keyword token.
*)
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

(** A single function-application argument. *)
and apply_argument =
  | Positional of expression
  (** An unlabeled argument such as `x` in `f x`. *)
  | Labeled of labeled_apply_argument
  (** A labeled argument such as `~label:value`. *)
  | Optional of optional_apply_argument

(** An optional argument such as `?state:value`. *)
(** Payload for labeled application arguments.

    Covers call-site arguments such as `~limit:10`.
*)
and labeled_apply_argument = {
  syntax_node: syntax_node;
  sigil_token: Token.t;
  label_token: Token.t;
  value: expression option;
}

(** Payload for optional application arguments.

    Covers call-site arguments such as `?state:(Some s)` and shorthand forms
    like `?state`.
*)
and optional_apply_argument = {
  syntax_node: syntax_node;
  sigil_token: Token.t;
  label_token: Token.t;
  value: expression option;
}

(** Payload for `Expression.Apply`.

    The public CST stores a single application step, so chained application such
    as `f x y` appears as nested `Apply` nodes.
*)
and apply_expression = {
  syntax_node: syntax_node;
  callee: expression;
  argument: apply_argument;
  attributes: attribute list;
}

(** Payload for `Expression.MethodCall`.

    Covers `receiver#method_name`.
*)
and method_call_expression = {
  syntax_node: syntax_node;
  receiver: expression;
  hash_token: Token.t;
  method_name: Token.t;
  attributes: attribute list;
}

(** Payload for `Expression.New`.

    Covers `new Class_name`.
*)
and new_expression = {
  syntax_node: syntax_node;
  class_path: Ident.t;
  attributes: attribute list;
}

(** Payload for `Expression.Prefix`.

    Covers prefix operators such as `!cell`, `~-x`, and similar unary forms.
*)
and prefix_expression = {
  syntax_node: syntax_node;
  operator_token: Token.t;
  operand: expression;
  attributes: attribute list;
}

(** Payload for `Expression.FieldAccess`.

    Covers `receiver.field`.
*)
and field_access_expression = {
  syntax_node: syntax_node;
  receiver: expression;
  field_name: Token.t;
  attributes: attribute list;
}

(** Payload for `Expression.Index`.

    Covers both array indexing `arr.(i)` and string indexing `s.[i]`.
*)
and index_expression = {
  syntax_node: syntax_node;
  collection: expression;
  opening_tokens: Token.t list;
  index: expression;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Expression.ObjectOverride`.

    Covers object override syntax written as `{< ... >}`.

    Example:

    ```ocaml,norun
    {< current = next; count = count + 1 >}
    ```
*)
and object_override_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  fields: object_override_field list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Expression.InstanceVariableAssign`.

    This form is reserved for assignments where the target is an object instance
    variable name and the operator is `<-`.
*)
and instance_variable_assign_expression = {
  syntax_node: syntax_node;
  name_token: Token.t;
  operator_token: Token.t;
  value: expression;
  attributes: attribute list;
}

(** Payload for `Expression.FieldAssign`.

    This covers assignments whose target is a field-access expression.

    Examples:

    ```ocaml,norun
    record.field <- value
    state.current.count <- 0
    ```
*)
and field_assign_expression = {
  syntax_node: syntax_node;
  target: field_access_expression;
  operator_token: Token.t;
  value: expression;
  attributes: attribute list;
}

(** Payload for `Expression.Assign`.

    This covers assignment expressions that are not reduced to
    `InstanceVariableAssign` or `FieldAssign`.
*)
and assign_expression = {
  syntax_node: syntax_node;
  target: expression;
  operator_token: Token.t;
  value: expression;
  attributes: attribute list;
}

(** Payload for `Expression.Infix`.

    Covers binary operator applications such as `a + b` and `x |> f`.
*)
and infix_expression = {
  syntax_node: syntax_node;
  left: expression;
  operator_tokens: Token.t list;
  right: expression;
  attributes: attribute list;
}

(** Payload for `Expression.TypeAscription`.

    Covers `(expr : t)` and reconstructed binding annotations such as
    `let value : t = expr`.
*)
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

(** Payload for `Expression.Polymorphic`.

    Covers explicit quoted polymorphic constraints such as:

    ```ocaml,norun
    let id : 'a. 'a -> 'a = fun x -> x
    ```

    The `type_` payload is expected to be a `CoreType.Poly`.
*)
and polymorphic_expression = {
  syntax_node: syntax_node;
  expression: expression;
  colon_token: Token.t;
  type_: core_type;
  attributes: attribute list;
}

(** Payload for `Expression.Sequence`.

    Covers `e1; e2; e3`.
*)
and sequence_expression = {
  syntax_node: syntax_node;
  separator_token: Token.t;
  separator_tokens: Token.t list;
  expressions: expression list;
  attributes: attribute list;
}

(** Payload for `Expression.Tuple`.

    Covers tuple expressions such as `(a, b)` and `(a, b, c)`.
*)
and tuple_expression = {
  syntax_node: syntax_node;
  elements: expression list;
  attributes: attribute list;
}

(** Payload for `Expression.List`.

    Covers list expressions such as `[a; b; c]`.
*)
and list_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  elements: expression list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Payload for `Expression.Array`.

    Covers array expressions such as `[| a; b; c |]`.
*)
and array_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  elements: expression list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Record expression syntax.

    This distinguishes plain record literals from record updates with `with`.
*)
and record_expression =
  | Literal of record_literal_expression
  (** A record literal such as `{ x = 1; y = 2 }`. *)
  | Update of record_update_expression

(** A record update such as `{ base with x = 1 }`. *)
(** Payload for `record_expression` literals. *)
and record_literal_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  fields: record_expression_field list;
  separator_tokens: Token.t list;
  closing_token: Token.t;
  attributes: attribute list;
}

(** Payload for `record_expression` updates. *)
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

(** Whether a record field used `=` or shorthand punning syntax. *)
and record_expression_field_source =
  | Explicit
  (** A field written with `=` such as `{ x = 1 }`. *)
  | Punned

(** A field written in shorthand form such as `{ x }`. *)
(** A single field inside a record literal or record update.

    `field_path` mirrors the label stored by the stock parsetree's
    `Pexp_record` branch, while `field_name` exposes the last path segment
    directly for consumers that only care about the field itself.

    `value` is always present. Shorthand source such as `{ field }` is lifted to
    an explicit `Expression.Path` and marked with `source = Punned`.
*)
and record_expression_field = {
  syntax_node: syntax_node;
  field_path: Ident.t;
  field_name: Token.t;
  equals_token: Token.t option;
  value: expression;
  source: record_expression_field_source;
}

(** A single field inside an object override expression.

    Object overrides update instance variables, so `field_name` is always a
    single identifier token rather than a dotted path.

    When `value` is `None`, the source used shorthand syntax such as
    `{< current >}`.
*)
and object_override_field = {
  syntax_node: syntax_node;
  field_name: Token.t;
  equals_token: Token.t option;
  value: expression option;
}

(** Payload for `Expression.LocalOpen`.

    This preserves whether the source used `let open M in expr` or a prefix
    local open such as `M.(expr)` / `M.[expr]`.
*)
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

(** A `function`-style case body.

    This mirrors the case-bearing branch of the stock parsetree's
    `Pfunction_cases`.

    Example:

    ```ocaml,norun
    function
    | Some value -> value
    | None -> default
    ```
*)
and function_case_body = {
  syntax_node: syntax_node;
  cases: match_case list;
}

(** The body of a `fun` expression.

    `Expression body` covers forms such as `fun x -> x + 1`, while
    `Cases body` covers forms such as `fun x -> function | Some y -> y`
    where the `fun` body itself is a nested `function` expression.
*)
and fun_body =
  | Expression of expression
  (** A direct expression body, such as `fun x -> x + 1`. *)
  | Cases of function_case_body

(** A case list body introduced by `function`, such as
          `fun x -> function | Some y -> y`. *)
(** Payload for `Expression.Fun`.

    Covers `fun` expressions with explicit parameter lists, such as
    `fun x ~label ?opt -> body` or `fun x -> function | Some y -> y`.
*)
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

(** Payload for `Expression.Function`.

    Covers `function` expressions whose payload is only a case list, such as:

    ```ocaml,norun
    function
    | Some value -> value
    | None -> default
    ```
*)
and function_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  cases: match_case list;
  attributes: attribute list;
}

(** A single `let` binding.

    This shape is used both for item-level bindings and for nested `and`
    bindings inside `let ... in ...` expressions. For grouped bindings,
    `and_binding` chains the trailing clauses under the leading binding.
*)
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

(** A single binding-operator clause inside a `let*`/`let+`-style expression.

    `keyword_token` preserves whether the clause started with `let` or `and`,
    while `operator_token` keeps the operator suffix such as `*`, `+`, or `=`.

    These clauses are reconstructed from the surrounding `LET_EXPR` token and
    child-node stream because the parser does not yet emit dedicated clause
    nodes for binding operators.
*)
and binding_operator_binding = {
  keyword_token: Token.t;
  operator_token: Token.t;
  equals_token: Token.t;
  binding_pattern: pattern;
  bound_value: expression;
  and_binding: binding_operator_binding option;
}

(** Payload for `Expression.LetOperator`.

    Covers binding-operator expressions such as:

    ```ocaml,norun
    let* x = read () in
    let* x = read () and* y = load () in
    ```
*)
and let_operator_expression = {
  syntax_node: syntax_node;
  binding: binding_operator_binding;
  in_token: Token.t;
  body: expression;
  attributes: attribute list;
}

(** Payload for `Expression.Let`.

    The first binding is split into `binding_pattern` and `bound_value`, while
    any parameter sugar from `let f x = ...` is preserved in `parameters`, and
    any trailing `and` bindings are exposed as a recursive `and_binding`
    chain.
*)
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

(** Payload for `Expression.Match`.

    Covers `match scrutinee with ...`.
*)
and match_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  with_token: Token.t;
  scrutinee: expression;
  cases: match_case list;
  attributes: attribute list;
}

(** Payload for `Expression.Try`.

    Covers `try body with ...`.
*)
and try_expression = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  with_token: Token.t;
  body: expression;
  cases: match_case list;
  attributes: attribute list;
}

(** A single match or handler case.

    Covers both plain cases like `| p -> e` and guarded cases like
    `| p when cond -> e`.
*)
and match_case = {
  syntax_node: syntax_node;
  bar_token: Token.t option;
  when_token: Token.t option;
  arrow_token: Token.t;
  pattern: pattern;
  guard: expression option;
  body: expression;
}

(** Payload for `Expression.If`.

    Covers `if cond then a else b`, with `else_branch = None` for branchless
    `if` expressions.
*)
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

(** Concrete grouping shell used around `Expression.Parenthesized`.

    OCaml uses ordinary parentheses for `(expr)` and `begin ... end` for a
    second grouping form that should remain distinguishable to CST consumers.
*)
and expression_grouping =
  | Parens
  (** Ordinary parentheses, such as `(expr)`. *)
  | BeginEnd

(** `begin ... end` grouping. *)
(** Payload for `Expression.Parenthesized`.

    Covers grouping syntax around an expression while preserving which concrete
    delimiter family was written.
*)
and parenthesized_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  closing_token: Token.t;
  grouping: expression_grouping;
  inner: expression;
  attributes: attribute list;
}

(** Class expression syntax.

    These nodes cover the grammar accepted to the right of `=` in class
    bindings. They keep class-specific structure distinct from ordinary term
    expressions, while still reusing ordinary `expression` nodes for method
    bodies, field initializers, and `let`-bound values inside the class.
*)
and class_expression =
  | Path of Ident.t
  (** A named class constructor reference such as `c`, `Widget.t`, or
          `Driver.make`.

          Examples:

          ```ocaml,norun
          class direct = c
          class service = Driver.make
          ```
      *)
  | Structure of class_structure
  (** An `object ... end` class structure body.

          Example:

          ```ocaml,norun
          object
            val mutable state = 0
            method run = state
          end
          ```
      *)
  | Fun of class_fun_expression
  (** A function-style class expression.

          Example:

          ```ocaml,norun
          fun x -> object method value = x end
          ```
      *)
  | Apply of class_apply_expression
  (** A class application such as `builder x` or `make ~clock`. *)
  | Let of class_let_expression
  (** A let-bound class expression.

          Example:

          ```ocaml,norun
          let state = ref 0 in object method run = !state end
          ```
      *)
  | Constraint of class_constraint_expression
  (** A class expression constrained by a class type.

          Example:

          ```ocaml,norun
          (object method run = 1 end : object method run : int end)
          ```
      *)
  | LocalOpen of local_open_class_expression
  (** A locally opened class expression such as `M.(builder)` or
          `let open M in object end`.
      *)
  | Parenthesized of parenthesized_class_expression
  (** A parenthesized class expression used for grouping. *)
  | Attribute of {
      syntax_node: syntax_node;
      class_expression: class_expression;
      attribute: attribute
    }
  (** A class expression with an attached attribute. *)
  | Extension of extension

(** A PPX extension parsed in class-expression position.

          Example: `[%driver]`.
      *)
(** The structured payload of `ClassExpression.Structure`. *)
and class_structure = {
  syntax_node: syntax_node;
  self_pattern: pattern option;
  fields: class_field list;
}

(** Fields inside an `object ... end` class structure. *)
and class_field =
  | Method of class_method
  (** A `method` declaration inside a class structure. *)
  | Value of class_value
  (** A `val` declaration inside a class structure. *)
  | Inherit of class_inherit
  (** An `inherit class_expr` field. *)
  | Constraint of class_constraint
  (** A `constraint t = u` field. *)
  | Initializer of class_initializer
  (** An `initializer expr` field. *)
  | Attribute of { syntax_node: syntax_node; field: class_field; attribute: attribute }
  (** A class field with an attached attribute.

          Example: `method run = 1 [@@foo]`.
      *)
  | Extension of extension

(** A PPX extension parsed as a class field.

          Example: `[%%foo]`.
      *)
(** Payload for `ClassField.Method`.

    This covers concrete methods, virtual methods, private methods, and
    overriding methods such as `method!`.
*)
and class_method = {
  syntax_node: syntax_node;
  name_token: Token.t;
  concrete_equals_token: Token.t option;
  virtual_colon_token: Token.t option;
  definition: method_definition;
  modifier_tokens: Token.t list;
}

(** Payload for `ClassField.Value`.

    This covers class fields declared with `val`, including `mutable`,
    `virtual`, and overriding forms.
*)
and class_value = {
  syntax_node: syntax_node;
  name_token: Token.t;
  concrete_equals_token: Token.t option;
  virtual_colon_token: Token.t option;
  definition: value_definition;
  modifier_tokens: Token.t list;
}

(** Payload for `ClassField.Inherit`. *)
and class_inherit = {
  syntax_node: syntax_node;
  class_expression: class_expression;
}

(** Payload for `ClassField.Constraint`.

    Example: `constraint t = int`.
*)
and class_constraint = {
  syntax_node: syntax_node;
  left: core_type;
  equals_token: Token.t;
  right: core_type;
}

(** Payload for `ClassField.Initializer`.

    Covers `initializer expr`.
*)
and class_initializer = {
  syntax_node: syntax_node;
  body: expression;
}

(** Payload for `ClassExpression.Apply`. *)
and class_apply_expression = {
  syntax_node: syntax_node;
  callee: class_expression;
  argument: apply_argument;
}

(** Payload for `ClassExpression.Fun`. *)
and class_fun_expression = {
  syntax_node: syntax_node;
  parameters: parameter list;
  body: class_expression;
}

(** Payload for `ClassExpression.Let`. *)
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

(** Payload for `ClassExpression.Constraint`. *)
and class_constraint_expression = {
  syntax_node: syntax_node;
  class_expression: class_expression;
  colon_token: Token.t;
  class_type: class_type;
}

(** Payload for `ClassExpression.LocalOpen`. *)
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

(** Payload for `ClassExpression.Parenthesized`. *)
and parenthesized_class_expression = {
  syntax_node: syntax_node;
  opening_token: Token.t;
  inner: class_expression;
  closing_token: Token.t;
}

(** Module expression syntax.

    These nodes cover the term-level side of the module language, including
    `struct ... end`, functors, functor application, extension nodes such as
    `[%ext]`, and unpacking first-class modules with `val`.
*)
and module_expression =
  | Path of Ident.t
  (** A module path such as `M`, `Stdlib.List`, or `Driver.Sqlite`. *)
  | Structure of { syntax_node: syntax_node; item_syntax_nodes: syntax_node list }
  (** A raw `struct ... end` expression.

          The contained items are preserved as raw `syntax_node`s rather than a
          separate public structure-item tree.
      *)
  | Functor of {
      syntax_node: syntax_node;
      parameters: functor_parameter list;
      body: module_expression
    }
  (** A functor expression.

          Example: `functor (X : S) -> struct ... end`.
      *)
  | Apply of { syntax_node: syntax_node; callee: module_expression; argument: module_expression }
  (** A functor application such as `F(X)` or `F(X)(Y)`. *)
  | ApplyUnit of { syntax_node: syntax_node; callee: module_expression }
  (** A unit functor application such as `F()` or `F()()`.

          This preserves the stock parsetree distinction between ordinary
          module application and generative application with an empty unit
          argument list.
      *)
  | Constraint of {
      syntax_node: syntax_node;
      module_expression: module_expression;
      colon_token: Token.t;
      module_type: module_type
    }
  (** A module expression constrained by a module type.

          Examples:

          ```ocaml,norun
          module M : S = struct end
          ```

          Declaration-site constraints such as `module M : S = ...` are
          reconstructed from the enclosing declaration node, because the
          successful `Ceibo` parse stores the ascription there instead of on a
          dedicated module-expression node.
      *)
  | ModuleUnpack of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      expression: expression;
      colon_token: Token.t option;
      package_type: package_type option;
      closing_token: Token.t
    }
  (** A first-class module unpacking expression.

          Examples:

          ```ocaml,norun
          (val m)
          (val m : S)
          ```
      *)
  | Parenthesized of {
      syntax_node: syntax_node;
      opening_token: Token.t;
      inner: module_expression;
      closing_token: Token.t
    }
  (** A parenthesized module expression used for grouping. *)
  | Attribute of {
      syntax_node: syntax_node;
      module_expression: module_expression;
      attribute: attribute
    }
  (** A module expression with an attached attribute. *)
  | Extension of extension

(** A PPX extension parsed in module-expression position.

          Example: `[%driver]`.
      *)

(** Expression syntax.

    This is the main value-language tree used by lint rules and other syntax
    consumers. The constructors intentionally preserve surface distinctions such
    as `fun` versus `function`, `record` literals versus updates, and explicit
    parentheses.
*)

(** A member inside an object expression body. *)

(** Class expression syntax.

    Examples:

    ```ocaml,norun
    object method run = () end
    fun x -> object end
    C(arg)
    ```
*)
module Expression: sig
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
  val syntax_node: t -> syntax_node

  val attributes: t -> attribute list
end

module Parameter: sig
  type t = parameter =
    | Positional of positional_parameter
    | Labeled of labeled_parameter
    | Optional of optional_parameter
    | LocallyAbstract of locally_abstract_type_parameter
  val syntax_node: t -> syntax_node

  val sigil_token: t -> Token.t option

  val name_token: t -> Token.t option

  val name: t -> string option

  val is_named: t -> bool

  val binding_name_matches_label: t -> bool

  val default_value: t -> expression option

  val binding_pattern: t -> pattern option
end

(** A field inside a class structure body. *)
module ObjectMember: sig
  type t = object_member =
    | Method of object_method
    | Value of object_value
    | Inherit of object_inherit
    | Extension of extension
    | Initializer of object_initializer
  val syntax_node: t -> syntax_node
end

(** Module expression syntax.

    Examples:

    ```ocaml,norun
    M
    struct let x = 1 end
    functor (X : S) -> F(X)
    (val m : S)
    ```

    Structure bodies currently preserve their top-level item anchors via
    `item_syntax_nodes` rather than exposing a second fully typed item tree
    here.
*)
module ClassExpression: sig
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
  val syntax_node: t -> syntax_node
end

(** Pattern syntax helper namespace.

    This mirrors `pattern` exactly and exists mainly so callers can write
    qualified matches such as `Pattern.Constructor ...` when that reads more
    clearly than matching the raw variant directly.
*)
module ClassField: sig
  type t = class_field =
    | Method of class_method
    | Value of class_value
    | Inherit of class_inherit
    | Constraint of class_constraint
    | Initializer of class_initializer
    | Attribute of { syntax_node: syntax_node; field: class_field; attribute: attribute }
    | Extension of extension
  val syntax_node: t -> syntax_node
end

(** Helper view over `infix_expression`.

    This module is convenient when callers specifically want to work with infix
    operators and use helper accessors such as `operator`.
*)
module ModuleExpression: sig
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
  val syntax_node: t -> syntax_node
end

(** Helper view over `record_expression`.

    The constructors mirror `record_expression` exactly, so the literal/update
    distinction documented above applies here unchanged.
*)
module Pattern: sig
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
  val syntax_node: t -> syntax_node

  val attributes: t -> attribute list
end

(** A type variable as it appears in a declaration parameter list.

    Examples include `'a` in `type 'a t = ...` and `_` in `type _ t = ...`.
*)
module InfixExpression: sig
  type t = infix_expression = {
    syntax_node: syntax_node;
    left: expression;
    operator_tokens: Token.t list;
    right: expression;
    attributes: attribute list;
  }
  val syntax_node: t -> syntax_node

  val left: t -> Expression.t

  val operator_tokens: t -> Token.t list

  val operator: t -> string

  val right: t -> Expression.t

  val attributes: t -> attribute list
end

(** A single type parameter in a type or class declaration.

    This keeps the parameter's declared variance and injectivity flags together
    with the written variable token.

    Examples include `+'a` in `type +'a t = ...`, `!'a`-style injective
    parameters, and `_` in `type _ t = ...`.
*)
module RecordExpression: sig
  type t = record_expression =
    | Literal of record_literal_expression
    | Update of record_update_expression
  val syntax_node: t -> syntax_node
end

(** A full declared type parameter.

    This combines the parameter token with any written variance or injectivity
    markers.

    Examples:

    ```ocaml,norun
    'a
    +'a
    !'a
    _ 
    ```
*)
module TypeVariable: sig
  type t = {
    syntax_node: syntax_node;
    name_token: Token.t;
  }
  val syntax_node: t -> syntax_node

  val name_token: t -> Token.t

  val name: t -> string

  val text: t -> string
end

(** Helper view over `private_flag`.

    This keeps the written `private` keyword when one was present instead of
    flattening the declaration down to a boolean.
*)
module TypeParameterVariance: sig
  type t =
    | Covariant of { marker_token: Token.t }
    (** A `+` variance marker, as in `type +'a t = ...`. *)
    | Contravariant of { marker_token: Token.t }

  (** A `-` variance marker, as in `type -'a sink = ...`. *)
  val marker_token: t -> Token.t
end

(** A field inside a record type definition.

    Covers entries such as `name : string`, `mutable count : int`, and
    `count : int [@deprecated]`.
*)
module TypeParameter: sig
  type t = {
    syntax_node: syntax_node;
    variance: TypeParameterVariance.t option;
    injectivity_token: Token.t option;
    type_variable: TypeVariable.t option;
  }
  val syntax_node: t -> syntax_node

  val variance: t -> TypeParameterVariance.t option

  val injectivity_token: t -> Token.t option

  val type_variable: t -> TypeVariable.t option
end

(** Structured argument forms for variant constructors.

    These correspond to the `of ...` payload on ordinary constructors and the
    arrow-parameter portion of GADT-style constructors.

    Examples:

    ```ocaml,norun
    type t = Pair of int * string
    type t = Person of { name : string }
    type _ expr = Val : 'a -> 'a expr
    ```
*)
module PrivateFlag: sig
  type t = private_flag =
    | Public
    (** A public declaration such as `type t = int`. *)
    | Private of { private_token: Token.t }

  (** A private declaration such as `type t = private int`. *)
  val private_token: t -> Token.t option

  val is_private: t -> bool
end

(** A constructor inside a variant type definition or type extension.

    Ordinary constructors fill `arguments` from their `of ...` payload and keep
    `result_type = None`.

    GADT-style constructors fill `payload_type` with the full signature to the
    right of `:`, derive `arguments` from the arrow parameter portion, and
    expose the final return type via `result_type`.

    Examples:

    ```ocaml,norun
    type t = A | B of int
    type t = Pair of int * string
    type t = Person of { name : string; age : int }
    type _ expr = Int : int expr
    type _ expr = Val : 'a -> 'a expr
    ```
*)
module RecordField: sig
  type t = {
    syntax_node: syntax_node;
    field_name: Token.t;
    mutable_token: Token.t option;
    colon_token: Token.t;
    field_type: core_type;
    semicolon_token: Token.t option;
    attributes: attribute list;
  }
  val syntax_node: t -> syntax_node

  val field_name_token: t -> Token.t

  val mutable_token: t -> Token.t option

  val colon_token: t -> Token.t

  val field_type: t -> core_type

  val semicolon_token: t -> Token.t option

  val name: t -> string

  val attributes: t -> attribute list
end

(** Helper view over `poly_variant_tag`.

    The structure matches `poly_variant_tag` exactly and is typically used while
    inspecting polymorphic variant type definitions.
*)
module ConstructorArguments: sig
  (** Inline record constructor arguments.

            Example:

            ```ocaml,norun
            type t = Person of { name : string; age : int }
            ```
        *)
  type t =
    | Tuple of core_type list
    (** Positional constructor arguments.

            Examples:

            ```ocaml,norun
            type t = Pair of int * string
            type t = Wrapped of (int * string)
            ```
        *)
    | Record of { opening_token: Token.t; fields: RecordField.t list; closing_token: Token.t }

  (** Inline record constructor arguments.

            Example:

            ```ocaml,norun
            type t = Person of { name : string; age : int }
            ```
        *)

  (** Inline record constructor arguments.

            Example:

            ```ocaml,norun
            type t = Person of { name : string; age : int }
            ```
        *)

  (** Inline record constructor arguments.

            Example:

            ```ocaml,norun
            type t = Person of { name : string; age : int }
            ```
        *)
end

(** Helper view over `poly_variant_bound`. *)
module VariantConstructor: sig
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
  val syntax_node: t -> syntax_node

  val attributes: t -> attribute list

  val bar_token: t -> Token.t option

  val constructor_name_token: t -> Token.t

  val separator_token: t -> Token.t option

  val arguments: t -> ConstructorArguments.t option

  val payload_type: t -> core_type option

  val arrow_token: t -> Token.t option

  val result_type: t -> core_type option

  val name: t -> string
end

(** Helper view over `row_field`.

    This distinguishes explicit variant tags from inherited rows such as
    `color` in `[ color | `Yellow ]`.
*)
module PolyVariantTag: sig
  type t = poly_variant_tag = {
    syntax_node: syntax_node;
    attributes: attribute list;
    bar_token: Token.t option;
    tag_name: Token.t;
    separator_token: Token.t option;
    payload_type: core_type option;
  }
  val syntax_node: t -> syntax_node

  val attributes: t -> attribute list

  val bar_token: t -> Token.t option

  val tag_name_token: t -> Token.t

  val separator_token: t -> Token.t option

  val payload_type: t -> core_type option

  val name: t -> string
end

(** Helper view over `poly_variant`. *)
module PolyVariantBound: sig
  type t = poly_variant_bound =
    | Exact
    | UpperBound of { marker_token: Token.t }
    | LowerBound of { marker_token: Token.t }
  val marker_token: t -> Token.t option
end

(** The right-hand side of a `type` declaration.

    This is intentionally a broad summary layer over the successful parse. The
    most common declaration shapes are modeled directly. 
*)
module RowField: sig
  type t = row_field =
    | Tag of poly_variant_tag
    | Inherit of { bar_token: Token.t option; syntax_node: syntax_node; type_: core_type }
  val syntax_node: t -> syntax_node

  val bar_token: t -> Token.t option

  val tag: t -> PolyVariantTag.t option

  val inherited_type: t -> core_type option
end

(** A `type` declaration item.

    Examples:

    ```ocaml,norun
    type t = int
    type ('a, 'b) pair = 'a * 'b
    type t := string
    ```
*)
module PolyVariant: sig
  type t = poly_variant = {
    syntax_node: syntax_node;
    opening_token: Token.t;
    kind: poly_variant_bound;
    fields: row_field list;
    closing_token: Token.t;
  }
  val syntax_node: t -> syntax_node

  val kind: t -> PolyVariantBound.t

  val fields: t -> RowField.t list

  val tags: t -> PolyVariantTag.t list
end

(** A `type ... += ...` extension item.

    Examples:

    ```ocaml,norun
    type _ Effect.t += Yield : unit Effect.t
    type message += Ack
    ```
*)
module TypeDefinition: sig
  (** A polymorphic variant definition such as
            `type t = [ `A | `B of int ]` or
            `type t = [ color | `Yellow ]`.
        *)
  type t =
    | Abstract
    (** An abstract declaration with no manifest.

            Example:

            ```ocaml,norun
            type t
            ```
        *)
    | Alias of { syntax_node: syntax_node; manifest: core_type }
    (** A manifest alias.

            Examples:

            ```ocaml,norun
            type t = int
            type 'a t = 'a list
            type t = Outer.Inner.(request -> response)
            ```
        *)
    | Extensible of { syntax_node: syntax_node }
    (** An extensible variant declaration introduced with `= ..`.

            Example:

            ```ocaml,norun
            type t = ..
            ```
        *)
    | FirstClassModule of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        package_type: package_type;
        closing_token: Token.t
      }
    (** A manifest first-class module type.

            Example: `type handler = (module Handler with type t = int)`.
        *)
    | Object of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        fields: object_type_field list;
        closing_token: Token.t
      }
    (** An object type definition such as `type t = < run : unit -> unit >`. *)
    | Record of {
        syntax_node: syntax_node;
        opening_token: Token.t;
        fields: RecordField.t list;
        closing_token: Token.t
      }
    (** A record type definition such as
            `type t = { name : string; mutable count : int }`. *)
    | Variant of { syntax_node: syntax_node; constructors: VariantConstructor.t list }
    (** A variant definition whose constructors are lifted structurally.

            This includes ordinary constructors like `type t = A | B of int`
            and GADT-style constructors like `type _ expr = Int : int expr`.
        *)
    | PolyVariant of PolyVariant.t

  (** A polymorphic variant definition such as
            `type t = [ `A | `B of int ]` or
            `type t = [ color | `Yellow ]`.
        *)

  (** A polymorphic variant definition such as
            `type t = [ `A | `B of int ]` or
            `type t = [ color | `Yellow ]`.
        *)

  (** A polymorphic variant definition such as
            `type t = [ `A | `B of int ]` or
            `type t = [ color | `Yellow ]`.
        *)
end

(** Helper view over `let_binding`.

    This is useful both for top-level `let` items and for nested `and` bindings
    collected from expression forms.
*)
module TypeDeclaration: sig
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
  val syntax_node: t -> syntax_node

  val keyword_token: t -> Token.t

  val nonrec_token: t -> Token.t option

  val type_name: t -> Ident.t

  val type_params: t -> TypeParameter.t list

  val type_definition: t -> TypeDefinition.t

  (** Preserves the leading manifest alias in declarations such as
      `type t = Base.t = A | B`.
  *)
  val manifest_equals_token: t -> Token.t option

  val manifest_alias: t -> core_type option

  val definition_equals_token: t -> Token.t option

  val destructive_substitution_token: t -> Token.t option

  val private_flag: t -> private_flag

  (** Preserves whether the declaration was written with `private`.

      Examples include `type t = private int` and
      `type color = private Red | Blue`.
  *)
  val constraints: t -> TypeConstraint.t list

  (** Declaration-level attributes such as `type t = int [@@immediate]`. *)
  val attributes: t -> attribute list

  val and_declarations: t -> t list

  val next_and_declaration: t -> t option

  val is_private: t -> bool

  val name_token: t -> Token.t
end

(** A single module binding.

    This covers plain `module` items and the individual bindings nested under a
    recursive module item. `is_recursive` reports whether the binding belongs to
    a `module rec` group.

    Examples:

    ```ocaml,norun
    module M = N
    module F (X : S) : T = struct end
    ```
*)
module TypeExtension: sig
  type t = {
    syntax_node: syntax_node;
    type_name: Ident.t;
    type_params: TypeParameter.t list;
    extension_operator_tokens: Token.t list;
    constructors: VariantConstructor.t list;
  }
  val syntax_node: t -> syntax_node

  val type_name: t -> Ident.t

  val type_params: t -> TypeParameter.t list

  val extension_operator_tokens: t -> Token.t list

  val constructors: t -> VariantConstructor.t list

  val name_token: t -> Token.t
end

(** A recursive module structure item.

    The grouped `declarations` preserve source order for bindings introduced by
    `module rec ... and ...`.

    Example:

    ```ocaml,norun
    module rec A : S = X
    and B : T = Y
    ```
*)
module LetBinding: sig
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
  val syntax_node: t -> syntax_node

  val keyword_token: t -> Token.t

  val rec_token: t -> Token.t option

  val equals_token: t -> Token.t

  val attributes: t -> attribute list

  val binding_pattern: t -> Pattern.t

  (** Derived only when `binding_pattern` has a simple recoverable name. *)
  val binding_name_token: t -> Token.t option

  val name: t -> string

  val parameters: t -> Parameter.t list

  val value: t -> Expression.t

  val and_bindings: t -> t list

  val and_binding: t -> t option

  val value_syntax_node: t -> syntax_node

  val is_recursive: t -> bool

  val is_function: t -> bool
end

(** A `module` item that appears in signatures.

    Example:

    ```ocaml,norun
    module M : S
    ```
*)
module ModuleSignature: sig
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
  val syntax_node: t -> syntax_node

  val keyword_token: t -> Token.t

  val rec_token: t -> Token.t option

  val module_name_token: t -> Token.t

  val functor_parameters: t -> functor_parameter list

  val colon_token: t -> Token.t option

  val equals_token: t -> Token.t option

  val definition: t -> definition

  val module_type: t -> module_type option

  val module_expression: t -> module_expression option

  val and_declarations: t -> t list

  val next_and_declaration: t -> t option

  val is_recursive: t -> bool

  val name: t -> string
end

(** A `module` item that appears in implementations.

    Examples:

    ```ocaml,norun
    module M = N
    module F (X : S) : T = struct end
    ```
*)
module ModuleStructure: sig
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
  val syntax_node: t -> syntax_node

  val keyword_token: t -> Token.t

  val rec_token: t -> Token.t option

  val module_name_token: t -> Token.t

  val functor_parameters: t -> functor_parameter list

  val colon_token: t -> Token.t option

  val equals_token: t -> Token.t

  val module_type: t -> module_type option

  val module_expression: t -> module_expression

  val and_declarations: t -> t list

  val next_and_declaration: t -> t option

  val is_recursive: t -> bool

  val name: t -> string
end

(** An `open` statement item.

    Covers both `open M` and `open! M`.
*)

(** A standalone docstring item.

    This models top-level docstrings that are not attached to any adjacent
    definition and instead contribute directly to the surrounding module or
    signature documentation.
*)
module ModuleTypeDeclaration: sig
  type t = {
    syntax_node: syntax_node;
    module_keyword_token: Token.t;
    type_keyword_token: Token.t;
    module_type_name: Token.t;
    equals_token: Token.t option;
    module_type: module_type option;
  }
  val syntax_node: t -> syntax_node

  val module_keyword_token: t -> Token.t

  val type_keyword_token: t -> Token.t

  val module_type_name_token: t -> Token.t

  val equals_token: t -> Token.t option

  val module_type: t -> module_type option

  val name: t -> string
end

(** A `val` declaration item.

    `name_token` preserves the declared value name exactly as written, including
    operator names such as `( + )`. `type_` is the lifted declaration type.

    Examples:

    ```ocaml,norun
    val make : unit -> t
    val ( + ) : int -> int -> int
    ```
*)
module OpenStatement: sig
  type target =
    | Path of Ident.t
    (** A path-style open target such as `open Std.List`.

            This is the only form accepted in signatures.
        *)
    | ModuleExpression of module_expression
  (** An implementation open target lifted through `module_expression`.

            Examples:

            ```ocaml,norun
            open Std.List
            open Make(Std)
            open struct let value = 1 end
            ```
        *)
  type t = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    target: target;
    bang_token: Token.t option;
  }
  val syntax_node: t -> syntax_node

  val keyword_token: t -> Token.t

  val target: t -> target

  val module_expression: t -> module_expression option

  val module_path: t -> Ident.t option

  val bang_token: t -> Token.t option

  val has_bang: t -> bool
end

module Docstring: sig
  type kind = docstring_kind =
    | Ordinary
    | Section
  type t = docstring = {
    syntax_node: syntax_node;
    docstring_token: Token.t;
    kind: kind;
  }
  val syntax_node: t -> syntax_node

  val token: t -> Token.t

  val kind: t -> kind

  val is_section: t -> bool

  val text: t -> string
end

module Comment: sig
  type t = comment = {
    syntax_node: syntax_node;
    comment_token: Token.t;
  }
  val syntax_node: t -> syntax_node

  val token: t -> Token.t

  val text: t -> string
end

module Trivia: sig
  type t = trivia =
    | Docstring of docstring
    | Comment of comment
  val syntax_node: t -> syntax_node

  val token: t -> Token.t

  val text: t -> string

  val is_docstring: t -> bool

  val is_comment: t -> bool
end

(** Filter a token's leading trivia down to the trivia entries that begin at or
    after the given source offset. *)
val leading_trivia_after: after:int -> Token.t -> trivia list

(** Return the first token's leading trivia for a syntax node, filtered to
    entries that begin at or after the given source offset. *)
val leading_trivia_before_node: after:int -> syntax_node -> trivia list

(** Return boundary trivia between a token and the next syntax node by combining
    the token's remaining leading trivia with the node's first-token leading
    trivia after the token ends. *)
val leading_trivia_after_token_before_node: after:int -> Token.t -> syntax_node -> trivia list

(** Return the span covered by the syntax node's real tokens, excluding any
    leading token-attached trivia. *)
val token_body_span: syntax_node -> Ceibo.Span.t

(** Return the syntax kind for a raw syntax node.

    This keeps diagnostics-only kind labeling behind `Syn.Cst` so downstream
    tools can keep the value typed and stringify it only at the edge. *)
val syntax_kind: syntax_node -> Syntax_kind.t

(** A `val` declaration item.

    Example:

    ```ocaml,norun
    val parse : string -> t
    ```
*)
type value_declaration = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  name_tokens: Token.t list;
  colon_token: Token.t;
  type_: core_type;
  trailing_comment: comment option;
}

(** An `external` declaration item.

    `primitive_name_tokens` preserves primitive names in source order, so
    declarations with bytecode/native pairs remain distinguishable.

    Example:

    ```ocaml,norun
    external strlen : string -> int = "caml_strlen"
    ```
*)
module ValueDeclaration: sig
  type t = value_declaration = {
    syntax_node: syntax_node;
    keyword_token: Token.t;
    name_tokens: Token.t list;
    colon_token: Token.t;
    type_: core_type;
    trailing_comment: comment option;
  }
  val syntax_node: t -> syntax_node

  val keyword_token: t -> Token.t

  val name_tokens: t -> Token.t list

  val colon_token: t -> Token.t

  val type_: t -> core_type

  val trailing_comment: t -> comment option
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

(** A `class` declaration item that appears in signatures.

    Example:

    ```ocaml,norun
    class service : object method run : unit -> unit end
    ```
*)
module ClassDeclaration: sig
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
  val syntax_node: t -> syntax_node

  val keyword_token: t -> Token.t

  val type_params: t -> TypeParameter.t list

  val declaration_extension: t -> extension option

  val declaration_attributes: t -> attribute list

  val class_name_token: t -> Token.t

  val colon_token: t -> Token.t

  val class_type: t -> class_type

  val name: t -> string
end

(** A `class` definition item that appears in implementations.

    Examples:

    ```ocaml,norun
    class service = object method run = () end
    class service : object method run : unit end = object method run = () end
    ```
*)
module ClassDefinition: sig
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
  val syntax_node: t -> syntax_node

  val keyword_token: t -> Token.t

  val type_params: t -> TypeParameter.t list

  val declaration_extension: t -> extension option

  val declaration_attributes: t -> attribute list

  val class_name_token: t -> Token.t

  val colon_token: t -> Token.t option

  val class_type: t -> class_type option

  val equals_token: t -> Token.t

  val class_body: t -> class_expression

  val name: t -> string
end

(** A `class type` declaration item.

    `class_type_body` is always present because class-type declarations are
    definition forms rather than forward declarations.

    Example:

    ```ocaml,norun
    class type service = object method run : unit -> unit end
    ```
*)
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
(** The payload of an `include` item.

    Implementations include module expressions such as `include M` or
    `include F(X)`, while signatures include module types such as `include S`.
*)
type include_target =
  | ModuleExpression of module_expression
  (** An included module expression such as `include M` or `include F(X)`. *)
  | ModuleType of module_type
(** An included module type such as `include S` inside a signature. *)
(** An `include` item.

    This keeps the include target in its native grammar family instead of
    collapsing implementation and interface includes to a shared raw path.
*)
type include_statement = {
  syntax_node: syntax_node;
  keyword_token: Token.t;
  target: include_target;
}

(** Top-level items that can appear in an implementation source file.

    This covers structure items such as `let`, `module`, `type`, floating
    attributes, and standalone expressions.
*)
module Payload: sig
  type t = payload =
    | Opaque of { tokens: Token.t list }
  val tokens: t -> Token.t list
end

(** A parsed implementation source file.

    `items` preserves the original source ordering. Consumers that want to work
    with specific item families should iterate that list directly so
    interleaved `let`, `module`, attribute, and expression items stay in order.
*)
module StructureItem: sig
  type t =
    | TypeDeclaration of TypeDeclaration.t
    (** A `type` declaration item. *)
    | TypeExtension of TypeExtension.t
    (** A `type ... += ...` extension item. *)
    | LetBinding of LetBinding.t
    (** A `let` or `let rec` item. *)
    | Expression of Expression.t
    (** A standalone expression item, typically in implementation files. *)
    | Attribute of attribute
    (** A floating attribute item such as `[@@@warning "-32"]`. *)
    | Extension of extension
    (** A floating extension item. *)
    | ClassDeclaration of ClassDefinition.t
    (** A `class` declaration item. *)
    | ClassTypeDeclaration of class_type_declaration
    (** A `class type` declaration item. *)
    | ModuleDeclaration of ModuleStructure.t
    (** A non-recursive `module` declaration item. *)
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    (** A `module type` declaration item. *)
    | OpenStatement of OpenStatement.t
    (** An `open` item. *)
    | Docstring of Docstring.t
    (** A standalone top-level docstring item. *)
    | Comment of Comment.t
    (** A standalone top-level comment item. *)
    | ExternalDeclaration of external_declaration
    (** An `external` declaration item. *)
    | IncludeStatement of include_statement
    (** An `include` item. *)
    | ExceptionDeclaration of exception_declaration

  (** An `exception` declaration item. *)
  val syntax_node: t -> syntax_node
end

module SignatureItem: sig
  type t =
    | TypeDeclaration of TypeDeclaration.t
    (** A `type` declaration item. *)
    | TypeExtension of TypeExtension.t
    (** A `type ... += ...` extension item. *)
    | Attribute of attribute
    (** A floating attribute item such as `[@@@warning "-32"]`. *)
    | Extension of extension
    (** A floating extension item. *)
    | ClassDeclaration of ClassDeclaration.t
    (** A `class` declaration item. *)
    | ClassTypeDeclaration of class_type_declaration
    (** A `class type` declaration item. *)
    | ModuleDeclaration of ModuleSignature.t
    (** A non-recursive `module` declaration item. *)
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    (** A `module type` declaration item. *)
    | OpenStatement of OpenStatement.t
    (** An `open` item. *)
    | Docstring of Docstring.t
    (** A standalone top-level docstring item. *)
    | Comment of Comment.t
    (** A standalone top-level comment item. *)
    | ValueDeclaration of value_declaration
    (** A `val` declaration item. *)
    | ExternalDeclaration of external_declaration
    (** An `external` declaration item. *)
    | IncludeStatement of include_statement
    (** An `include` item. *)
    | ExceptionDeclaration of exception_declaration

  (** An `exception` declaration item. *)
  val syntax_node: t -> syntax_node
end

(** A parsed interface source file.

    `items` preserves the original source ordering. Consumers that want to find
    particular declaration families should iterate the signature items directly
    instead of relying on flattened projections.
*)
type implementation = {
  syntax_node: syntax_node;
  items: StructureItem.t list;
  phrase_separator_tokens: Token.t list;
  trailing_phrase_separator_tokens: Token.t list list;
}
(** A parsed source file, distinguished by whether the grammar was an
    implementation or an interface. *)
type interface = {
  syntax_node: syntax_node;
  items: SignatureItem.t list;
  phrase_separator_tokens: Token.t list;
  trailing_phrase_separator_tokens: Token.t list list;
}
(** Alias for the full-file CST root. *)
type t =
  | Implementation of implementation
  (** An implementation file such as an `.ml`. *)
  | Interface of interface
(** An interface file such as an `.mli`. *)
(** Namespace helpers for the file-level CST root.

    This provides convenient access to the common projections shared by both
    `Implementation` and `Interface` roots while keeping the ordered item lists
    as the canonical file body.
*)
(** Returns the root `syntax_node` for the parsed source file. *)
type source_file = t
module SourceFile: sig
  type t = source_file
  val syntax_node: t -> syntax_node

  val structure_items: t -> StructureItem.t list option

  val signature_items: t -> SignatureItem.t list option

  val phrase_separator_tokens: t -> Token.t list

  val trailing_phrase_separator_tokens: t -> Token.t list list

  val kind: t -> [
      | `Implementation
      | `Interface
    ]
end

val syntax_node_of_source_file: source_file -> syntax_node
