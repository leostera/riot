open Std

(** Syntax Node Kinds for OCaml

    This module defines all possible node and token kinds that can appear in a
    Ceibo syntax tree for OCaml code.

    # Categories

    The kinds are organized into several categories:

    - **Trivia**: Whitespace, comments, docstrings
    - **Literals**: Numbers, strings, chars, booleans, unit
    - **Expressions**: All OCaml expression forms
    - **Patterns**: Pattern matching constructs
    - **Declarations**: Top-level items (let, type, module, etc.)
    - **Structural**: Helper nodes for tree structure
    - **Error Recovery**: Placeholder nodes for malformed code

    # Usage in Trees

    These kinds are used as the `'kind` type parameter when instantiating Ceibo
    trees:

    ```ocaml (* A tree node for OCaml *) type ocaml_tree = (Syntax_kind.t,
    string) Ceibo.Green.node

    (* Check what kind of node we have *) match Ceibo.Green.kind node with |
    Syntax_kind.LET_EXPR -> (* handle let expression *) | Syntax_kind.IDENT_EXPR
    -> (* handle identifier *) | _ -> (* other cases *) ``` *)
type t =
  (* ===================================================================== *)
  (* TRIVIA - Non-semantic whitespace and comments *)
  (* ===================================================================== *)
  | WHITESPACE
  (** Whitespace between tokens (spaces, tabs, newlines). *)
  | COMMENT
  (** Regular comment: `(* ... *)` *)
  | DOCSTRING
  (** Documentation comment: `(** ... *)` *)
  (* ===================================================================== *)
  (* LITERALS - Atomic values *)
  (* ===================================================================== *)
  | INT_LITERAL
  (** Integer literal: `42`, `0x2A`, `0b101010`, `0o52` *)
  | FLOAT_LITERAL
  (** Floating point literal: `3.14`, `1.0e-10` *)
  | STRING_LITERAL
  (** String literal: `"hello"` *)
  | CHAR_LITERAL
  (** Character literal: `'a'`, `'\n'` *)
  | BOOL_LITERAL
  (** Boolean literal: `true` or `false` *)
  | UNIT_LITERAL
  (** Unit literal: `()` *)
  (* ===================================================================== *)
  (* EXPRESSIONS *)
  (* ===================================================================== *)
  | IDENT_EXPR
  (** Simple identifier: `foo`, `x` *)
  | PATH_EXPR
  (** Module path: `List.map`, `Std.Result.map` *)
  | APPLY_EXPR
  (** Function application: `f x y` *)
  | LABELED_ARG
  (** Labeled argument in call: `~label` or `~label:value` *)
  | OPTIONAL_ARG
  (** Optional argument in call: `?label` or `?label:value` *)
  | INFIX_EXPR
  (** Infix operator: `a + b`, `x :: xs` *)
  | PREFIX_EXPR
  (** Prefix operator: `-x`, `!ref` *)
  | MACRO_EXPR
  (** Function-like macro invocation: `Macro.format! expr` *)
  | IF_EXPR
  (** If expression: `if cond then e1 else e2` *)
  | MATCH_EXPR
  (** Match expression: `match x with ...` *)
  | FUN_EXPR
  (** Anonymous function: `fun x -> x + 1` *)
  | LABELED_PARAM
  (** Labeled parameter: `~label` or `~label:pattern` *)
  | OPTIONAL_PARAM
  (** Optional parameter: `?label` or `?label:pattern` *)
  | OPTIONAL_PARAM_DEFAULT
  (** Optional with default: `?(label = expr)` *)
  | FUNCTION_EXPR
  (** Function with pattern matching: `function | P1 -> e1 | P2 -> e2` *)
  | LET_EXPR
  (** Let expression: `let x = 1 in x + 2` *)
  | LET_REC_EXPR
  (** Recursive let: `let rec f x = ... in ...` *)
  | SEQUENCE_EXPR
  (** Sequence: `e1; e2; e3` *)
  | PAREN_EXPR
  (** Parenthesized expression: `(expr)` *)
  | TUPLE_EXPR
  (** Tuple: `(1, 2, 3)` *)
  | LIST_EXPR
  (** List literal: `[1; 2; 3]` *)
  | ARRAY_EXPR
  (** Array literal: `[|1; 2; 3|]` *)
  | RECORD_EXPR
  (** Record: `{ x = 1; y = 2 }` *)
  | RECORD_UPDATE_EXPR
  (** Record update: `{ r with x = 3 }` *)
  | UNREACHABLE_EXPR
  (** Unreachable expression: `.` *)
  | FIELD_ACCESS_EXPR
  (** Field access: `r.field` *)
  | ARRAY_INDEX_EXPR
  (** Array indexing: `arr.(i)` *)
  | STRING_INDEX_EXPR
  (** String indexing: `s.[i]` *)
  | ASSIGN_EXPR
  (** Assignment: `field <- value` or `arr.(i) <- value` *)
  | CONSTRUCTOR_EXPR
  (** Constructor application: `Some 42`, `Node (l, x, r)` *)
  | POLY_VARIANT_EXPR
  (** Polymorphic variant: `` `Tag ``, `` `Tag value `` *)
  | ASSERT_EXPR
  (** Assertion: `assert condition` *)
  | LAZY_EXPR
  (** Lazy value: `lazy expr` *)
  | WHILE_EXPR
  (** While loop: `while cond do ... done` *)
  | FOR_EXPR
  (** For loop: `for i = 0 to 10 do ... done` *)
  | TRY_EXPR
  (** Exception handling: `try expr with ...` *)
  | TYPED_EXPR
  (** Type annotation: `(expr : typ)` *)
  | COERCE_EXPR
  (** Type coercion: `(expr :> typ)` *)
  | ATTRIBUTE_EXPR
  (** Attribute on expression: `expr [@attr]` or `expr [@@attr]` *)
  | EXTENSION_EXPR
  (** Extension expression: `[%ext ...]` *)
  | OBJECT_EXPR
  (** Object expression: `object ... end` *)
  | OBJECT_SELF
  (** Object self binding: `(self)` in `object (self) ... end` *)
  | OBJECT_METHOD
  (** Object method definition: `method m = expr` *)
  | OBJECT_VAL
  (** Object value definition: `val x = expr` *)
  | OBJECT_INHERIT
  (** Object inheritance clause: `inherit expr` *)
  | OBJECT_UPDATE_EXPR
  (** Object update: `{< field = value >}` *)
  | METHOD_CALL_EXPR
  (** Method call: `obj#method` *)
  | NEW_EXPR
  (** Object instantiation: `new class_name` *)
  | LOCAL_OPEN_EXPR
  (** Local open: `let open Module in expr` *)
  | LET_MODULE_EXPR
  (** Let module: `let module M = ... in expr` *)
  | FIRST_CLASS_MODULE_EXPR
  (** First-class module: `(module M)` or `(module M : S)` *)
  | STRUCT_EXPR
  (** Struct expression: `struct ... end` *)
  | SIG_EXPR
  (** Signature expression: `sig ... end` *)
  | MODULE_PATH
  (** Module path: `A.B.C` *)
  (* ===================================================================== *)
  (* PATTERNS *)
  (* ===================================================================== *)
  | IDENT_PATTERN
  (** Identifier pattern: `x` *)
  | WILDCARD_PATTERN
  (** Wildcard: `_` *)
  | LITERAL_PATTERN
  (** Literal pattern: `42`, `"hello"`, `true` *)
  | CONSTRUCTOR_PATTERN
  (** Constructor pattern: `Some x`, `Node (l, x, r)` *)
  | TUPLE_PATTERN
  (** Tuple pattern: `(x, y, z)` *)
  | LIST_PATTERN
  (** List pattern: `[x; y; z]` *)
  | ARRAY_PATTERN
  (** Array pattern: `[|x; y; z|]` *)
  | CONS_PATTERN
  (** Cons pattern: `x :: xs` *)
  | RECORD_PATTERN
  (** Record pattern: `{ x; y = z }` *)
  | OR_PATTERN
  (** Or pattern: `P1 | P2` *)
  | AS_PATTERN
  (** As pattern: `pattern as name` *)
  | RANGE_PATTERN
  (** Range pattern: `'a' .. 'z'` or `0 .. 9` *)
  | TYPED_PATTERN
  (** Typed pattern: `(pattern : typ)` *)
  | LAZY_PATTERN
  (** Lazy pattern: `lazy pattern` *)
  | EXCEPTION_PATTERN
  (** Exception pattern: `exception E` *)
  | PAREN_PATTERN
  (** Parenthesized pattern: `(pattern)` *)
  | POLY_VARIANT_PATTERN
  (** Polymorphic variant pattern: `` `Tag ``, `` `Tag pattern `` *)
  | POLY_VARIANT_TYPE_PATTERN
  (** Polymorphic variant type pattern: `#type` *)
  | EFFECT_PATTERN
  (** Effect handler pattern: `effect p, k` *)
  | LOCAL_OPEN_PATTERN
  (** Local module open pattern: `Module.(pattern)` *)
  | OPERATOR_PATTERN
  (** Operator pattern: `( + )`, `( let* )`, `( mod )` *)
  | FIRST_CLASS_MODULE_PATTERN
  (** First-class module pattern: `(module M)`, `(module _)`, `(module M : S)`, or `(module _ : S)` *)
  (* ===================================================================== *)
  (* TYPE EXPRESSIONS *)
  (* ===================================================================== *)
  | TYPE_VAR
  (** Type variable: `'a`, `'b` *)
  | TYPE_CONSTR
  (** Type constructor: `int`, `string`, `list` *)
  | TYPE_ALIAS
  (** Type alias binder: `'a list as 'b` *)
  | TYPE_ARROW
  (** Arrow type: `int -> string` *)
  | TYPE_TUPLE
  (** Tuple type: `int * string` *)
  | TYPE_PAREN
  (** Parenthesized type: `(int -> string)` *)
  | TYPE_POLY_VARIANT
  (** Polymorphic variant type: `[`A | `B]` *)
  | POLY_VARIANT_TAG
  (** Polymorphic variant tag: `` `A `` or `` `A of int `` *)
  | TYPE_PARAM
  (** Type parameter: `'a` in type params *)
  | TYPE_PARAMS
  (** Type parameters: `('a, 'b)` *)
  | TYPE_VARIANT_CONSTR
  (** Variant constructor in type def: `A | B of int` *)
  | TYPE_EXTENSIBLE
  (** Extensible variant type: `..` *)
  | TYPE_RECORD
  (** Record type: `{ field1: int; field2: string }` *)
  | TYPE_RECORD_FIELD
  (** Record field in type def: `field: int` *)
  | OBJECT_TYPE
  (** Object type: `< m : int; n : string >` *)
  | OBJECT_TYPE_FIELD
  (** Object type field: `m : int` *)
  | TYPE_CONSTRAINT
  (** Type constraint: `constraint 'a = int` *)
  | POLY_TYPE
  (** Polymorphic type with explicit quantifiers: `'a 'b. int -> 'a -> 'b` *)
  | MODULE_TYPE_EXPR
  (** Module type expression: `S with type t = int` *)
  | FIRST_CLASS_MODULE_TYPE
  (** First-class module type: `(module S)` or `(module S with type t = int)` *)
  | MODULE_TYPE_PATH
  (** Module type path: `Module.Type` *)
  | FUNCTOR_PARAM
  (** Functor parameter: `(X : S)` *)
  | FUNCTOR_TYPE
  (** Functor type: `functor (X : S) -> T` *)
  | MODULE_APPLICATION
  (** Module application: `M(X)` *)
  | MODULE_UNIT_APPLICATION
  (** Unit functor application: `M()` *)
  (* ===================================================================== *)
  (* TOP-LEVEL DECLARATIONS *)
  (* ===================================================================== *)
  | LET_BINDING
  (** Top-level let: `let x = 1` *)
  | LET_REC_BINDING
  (** Recursive let: `let rec f x = ...` *)
  | LET_MUTUAL_DECL
  (** Mutually recursive let bindings: `let f x = ... and g y = ...` *)
  | TYPE_DECL
  (** Type declaration: `type t = ...` *)
  | TYPE_MUTUAL_DECL
  (** Mutually recursive type declarations: `type a = ... and b = ...` *)
  | EXCEPTION_DECL
  (** Exception declaration: `exception E of typ` *)
  | MODULE_DECL
  (** Module declaration: `module M = ...` *)
  | CLASS_DECL
  (** Class declaration: `class c = expr` or `class c : typ` *)
  | CLASS_TYPE_DECL
  (** Class type declaration: `class type c = expr` *)
  | MODULE_TYPE_DECL
  (** Module type declaration: `module type S = ...` *)
  | MODULE_TYPE_OF
  (** Module type of expression: `module type of M` *)
  | OPEN_STMT
  (** Open statement: `open Module` *)
  | INCLUDE_STMT
  (** Include statement: `include Module` *)
  | VAL_DECL
  (** Value declaration: `val name : typ` *)
  | EXTERNAL_DECL
  (** External declaration: `external name : typ = "c_name"` *)
  (* ===================================================================== *)
  (* STRUCTURAL - Tree organization nodes *)
  (* ===================================================================== *)
  | SOURCE_FILE
  (** Root node for a complete source file. *)
  | STRUCTURE
  (** Module structure (implementation). *)
  | SIGNATURE
  (** Module signature (interface). *)
  | MATCH_CASE
  (** Single match case: `| pattern -> expr` *)
  | PATTERN_GUARD
  (** Pattern guard: `when condition` *)
  | RECORD_FIELD
  (** Record field definition or value: `field = expr` *)
  | RECORD_FIELD_PATTERN
  (** Record field in pattern: `field = pattern` *)
  | PARAMETER
  (** Function parameter. *)
  | LOCALLY_ABSTRACT_TYPE_PARAM
  (** Locally abstract type parameter: `(type a b c)` *)
  | ARGUMENT
  (** Function argument. *)
  (* ===================================================================== *)
  (* ERROR RECOVERY - Placeholders for malformed code *)
  (* ===================================================================== *)
  | ERROR
  (** Error placeholder for malformed syntax.

          The parser creates ERROR nodes when it encounters syntax it cannot
          parse, allowing it to continue and report multiple errors. *)
  | MISSING

(** Missing token placeholder.

          When the parser expects a token but doesn't find it, it creates a
          MISSING node (zero-width) to maintain tree structure. *)

(** `to_string kind` converts a syntax kind to a human-readable string.

    Useful for debugging and error messages.

    Example: ```ocaml Syntax_kind.to_string LET_EXPR = "LET_EXPR"
    Syntax_kind.to_string INT_LITERAL = "INT_LITERAL" ``` *)
val to_string: t -> string

(** `from_string str` parses a syntax kind from its string representation.

    Returns [Some kind] if the string matches a valid syntax kind, [None]
    otherwise.

    This is the inverse of [to_string].

    Example: ```ocaml Syntax_kind.from_string "LET_EXPR" = Some LET_EXPR
    Syntax_kind.from_string "INVALID" = None ``` *)
val from_string: string -> t option
