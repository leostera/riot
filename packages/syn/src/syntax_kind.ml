open Std

(* OCaml syntax node kinds for Ceibo green trees *)

type t =
  (* ========================================================================= *)
  (* TRIVIA - Whitespace and comments *)
  (* ========================================================================= *)
  | WHITESPACE
  | COMMENT
  | DOCSTRING
  (* ========================================================================= *)
  (* LITERALS *)
  (* ========================================================================= *)
  | INT_LITERAL
  | FLOAT_LITERAL
  | STRING_LITERAL
  | CHAR_LITERAL
  | BOOL_LITERAL
  | UNIT_LITERAL
  (* ========================================================================= *)
  (* EXPRESSIONS *)
  (* ========================================================================= *)
  | IDENT_EXPR
  | PATH_EXPR
  (* Module.path.to.value *)
  | APPLY_EXPR
  (* f x y *)
  | LABELED_ARG
  (* ~label or ~label:value in function call *)
  | OPTIONAL_ARG
  (* ?label or ?label:value in function call *)
  | INFIX_EXPR
  (* x + y *)
  | PREFIX_EXPR
  (* -x, !ref *)
  | MACRO_EXPR
  (* format!(...) *)
  | IF_EXPR
  (* if c then e1 else e2 *)
  | MATCH_EXPR
  (* match e with ... *)
  | FUN_EXPR
  (* fun x -> e *)
  | LABELED_PARAM
  (* ~label or ~label:pattern *)
  | OPTIONAL_PARAM
  (* ?label or ?label:pattern *)
  | OPTIONAL_PARAM_DEFAULT
  (* ?(label = expr) *)
  | FUNCTION_EXPR
  (* function | p1 -> e1 | ... *)
  | LET_EXPR
  (* let x = e1 in e2 *)
  | LET_REC_EXPR
  (* let rec f x = e1 in e2 *)
  | SEQUENCE_EXPR
  (* e1; e2; e3 *)
  | PAREN_EXPR
  (* (e) *)
  | TUPLE_EXPR
  (* (e1, e2, e3) *)
  | LIST_EXPR
  (* [e1; e2; e3] *)
  | ARRAY_EXPR
  (* [|e1; e2; e3|] *)
  | RECORD_EXPR
  (* { field1 = e1; field2 = e2 } *)
  | RECORD_UPDATE_EXPR
  (* { record with field = e } *)
  | UNREACHABLE_EXPR
  (* . *)
  | FIELD_ACCESS_EXPR
  (* record.field *)
  | ARRAY_INDEX_EXPR
  (* arr.(i) *)
  | STRING_INDEX_EXPR
  (* s.[i] *)
  | ASSIGN_EXPR
  (* field <- value, arr.(i) <- value *)
  | CONSTRUCTOR_EXPR
  (* Some e, Ok value *)
  | POLY_VARIANT_EXPR
  (* `Tag or `Tag value *)
  | ASSERT_EXPR
  (* assert e *)
  | LAZY_EXPR
  (* lazy e *)
  | WHILE_EXPR
  (* while c do e done *)
  | FOR_EXPR
  (* for x = e1 to e2 do e3 done *)
  | TRY_EXPR
  (* try e with | p1 -> e1 | ... *)
  | TYPED_EXPR
  (* (e : t) *)
  | COERCE_EXPR
  (* (e :> t) or (e : t1 :> t2) *)
  | ATTRIBUTE_EXPR
  (* e [@attr] or e [@@attr] *)
  | EXTENSION_EXPR
  (* [%ext ...] *)
  | OBJECT_EXPR
  (* object ... end *)
  | OBJECT_SELF
  (* object (self) ... end *)
  | OBJECT_METHOD
  (* method name = expr *)
  | OBJECT_VAL
  (* val x = expr *)
  | OBJECT_INHERIT
  (* inherit expr *)
  | OBJECT_UPDATE_EXPR
  (* {< field = value >} *)
  | METHOD_CALL_EXPR
  (* obj#method *)
  | NEW_EXPR
  (* new class_name *)
  | LOCAL_OPEN_EXPR
  (* let open Module in expr *)
  | LET_MODULE_EXPR
  (* let module M = ... in expr *)
  | FIRST_CLASS_MODULE_EXPR
  (* (module M) or (module M : S) *)
  | STRUCT_EXPR
  (* struct ... end *)
  | SIG_EXPR
  (* sig ... end *)
  | MODULE_PATH
  (* A.B.C module path *)
  (* ========================================================================= *)
  (* PATTERNS *)
  (* ========================================================================= *)
  | IDENT_PATTERN
  | WILDCARD_PATTERN
  (* _ *)
  | LITERAL_PATTERN
  | CONSTRUCTOR_PATTERN
  (* Some x *)
  | TUPLE_PATTERN
  (* (x, y, z) *)
  | LIST_PATTERN
  (* [x; y; z] *)
  | ARRAY_PATTERN
  (* [|x; y; z|] *)
  | CONS_PATTERN
  (* x :: xs *)
  | RECORD_PATTERN
  (* { field1; field2 = p } *)
  | OR_PATTERN
  (* p1 | p2 *)
  | AS_PATTERN
  (* p as x *)
  | RANGE_PATTERN
  (* 'a' .. 'z' *)
  | TYPED_PATTERN
  (* (p : t) *)
  | LAZY_PATTERN
  (* lazy p *)
  | EXCEPTION_PATTERN
  (* exception p *)
  | PAREN_PATTERN
  (* (p) *)
  | POLY_VARIANT_PATTERN
  (* `Tag or `Tag p *)
  | POLY_VARIANT_TYPE_PATTERN
  (* #type *)
  | EFFECT_PATTERN
  (* effect p, k *)
  | LOCAL_OPEN_PATTERN
  (* Module.(pattern) *)
  | OPERATOR_PATTERN
  (* ( + ), ( let* ), ( mod ) *)
  | FIRST_CLASS_MODULE_PATTERN
  (* (module M), (module _), (module M : S), or (module _ : S) *)
  (* ========================================================================= *)
  (* TYPE EXPRESSIONS *)
  (* ========================================================================= *)
  | TYPE_VAR
  (* 'a, 'b *)
  | TYPE_CONSTR
  (* int, string, list *)
  | TYPE_ALIAS
  (* 'a list as 'b *)
  | TYPE_ARROW
  (* int -> string *)
  | TYPE_TUPLE
  (* int * string *)
  | TYPE_PAREN
  (* (int -> string) *)
  | TYPE_POLY_VARIANT
  (* [`A | `B] *)
  | POLY_VARIANT_TAG
  (* `A or `A of int *)
  | TYPE_PARAM
  (* 'a in type params *)
  | TYPE_PARAMS
  (* ('a, 'b) *)
  | TYPE_VARIANT_CONSTR
  (* A | B of int *)
  | TYPE_EXTENSIBLE
  (* .. for extensible variants *)
  | TYPE_RECORD
  (* { field1: int; field2: string } *)
  | TYPE_RECORD_FIELD
  (* field: int *)
  | OBJECT_TYPE
  (* < m : int; n : string > *)
  | OBJECT_TYPE_FIELD
  (* m : int *)
  | TYPE_CONSTRAINT
  (* constraint 'a = int *)
  | POLY_TYPE
  (* 'a 'b. type - polymorphic type with explicit quantifiers *)
  | MODULE_TYPE_EXPR
  (* S | S with type t = int *)
  | FIRST_CLASS_MODULE_TYPE
  (* (module S) or (module S with type t = int) *)
  | MODULE_TYPE_PATH
  (* Module.Type for type annotations *)
  | FUNCTOR_PARAM
  (* (X : S) in functor declaration *)
  | FUNCTOR_TYPE
  (* functor (X : S) -> T *)
  | MODULE_APPLICATION
  (* M(X) functor application *)
  | MODULE_UNIT_APPLICATION
  (* M() unit functor application *)
  (* ========================================================================= *)
  (* TOP-LEVEL DECLARATIONS *)
  (* ========================================================================= *)
  | LET_BINDING
  (* let x = e *)
  | LET_REC_BINDING
  (* let rec f x = e *)
  | LET_MUTUAL_DECL
  (* let f x = ... and g y = ... *)
  | TYPE_DECL
  (* type t = ... *)
  | TYPE_MUTUAL_DECL
  (* type a = ... and b = ... *)
  | EXCEPTION_DECL
  (* exception E of t *)
  | MODULE_DECL
  (* module M = struct ... end *)
  | CLASS_DECL
  (* class c = expr or class c : typ *)
  | CLASS_TYPE_DECL
  (* class type c = expr *)
  | MODULE_TYPE_DECL
  (* module type S = sig ... end *)
  | MODULE_TYPE_OF
  (* module type of M - get signature of module *)
  | OPEN_STMT
  (* open Module *)
  | INCLUDE_STMT
  (* include Module *)
  | VAL_DECL
  (* val name : type *)
  | EXTERNAL_DECL
  (* external name : type = "c_name" *)
  (* ========================================================================= *)
  (* STRUCTURAL ELEMENTS *)
  (* ========================================================================= *)
  | SOURCE_FILE
  (* Top-level file *)
  | STRUCTURE
  (* struct ... end *)
  | SIGNATURE
  (* sig ... end *)
  | MATCH_CASE
  (* | pattern -> expr *)
  | PATTERN_GUARD
  (* when expr *)
  | RECORD_FIELD
  (* field = expr *)
  | RECORD_FIELD_PATTERN
  (* field = pattern *)
  | PARAMETER
  (* Function parameter *)
  | LOCALLY_ABSTRACT_TYPE_PARAM
  (* (type a b c) in function params *)
  | ARGUMENT
  (* Function argument *)
  (* ========================================================================= *)
  (* ERROR RECOVERY *)
  (* ========================================================================= *)
  | ERROR
  (* Unparseable content *)
  | MISSING

(* Expected but missing token/node *)

let to_string = function
  | WHITESPACE -> "WHITESPACE"
  | COMMENT -> "COMMENT"
  | DOCSTRING -> "DOCSTRING"
  | INT_LITERAL -> "INT_LITERAL"
  | FLOAT_LITERAL -> "FLOAT_LITERAL"
  | STRING_LITERAL -> "STRING_LITERAL"
  | CHAR_LITERAL -> "CHAR_LITERAL"
  | BOOL_LITERAL -> "BOOL_LITERAL"
  | UNIT_LITERAL -> "UNIT_LITERAL"
  | IDENT_EXPR -> "IDENT_EXPR"
  | PATH_EXPR -> "PATH_EXPR"
  | APPLY_EXPR -> "APPLY_EXPR"
  | LABELED_ARG -> "LABELED_ARG"
  | OPTIONAL_ARG -> "OPTIONAL_ARG"
  | INFIX_EXPR -> "INFIX_EXPR"
  | PREFIX_EXPR -> "PREFIX_EXPR"
  | MACRO_EXPR -> "MACRO_EXPR"
  | IF_EXPR -> "IF_EXPR"
  | MATCH_EXPR -> "MATCH_EXPR"
  | FUN_EXPR -> "FUN_EXPR"
  | LABELED_PARAM -> "LABELED_PARAM"
  | OPTIONAL_PARAM -> "OPTIONAL_PARAM"
  | OPTIONAL_PARAM_DEFAULT -> "OPTIONAL_PARAM_DEFAULT"
  | FUNCTION_EXPR -> "FUNCTION_EXPR"
  | LET_EXPR -> "LET_EXPR"
  | LET_REC_EXPR -> "LET_REC_EXPR"
  | SEQUENCE_EXPR -> "SEQUENCE_EXPR"
  | PAREN_EXPR -> "PAREN_EXPR"
  | TUPLE_EXPR -> "TUPLE_EXPR"
  | LIST_EXPR -> "LIST_EXPR"
  | ARRAY_EXPR -> "ARRAY_EXPR"
  | RECORD_EXPR -> "RECORD_EXPR"
  | RECORD_UPDATE_EXPR -> "RECORD_UPDATE_EXPR"
  | UNREACHABLE_EXPR -> "UNREACHABLE_EXPR"
  | FIELD_ACCESS_EXPR -> "FIELD_ACCESS_EXPR"
  | ARRAY_INDEX_EXPR -> "ARRAY_INDEX_EXPR"
  | STRING_INDEX_EXPR -> "STRING_INDEX_EXPR"
  | ASSIGN_EXPR -> "ASSIGN_EXPR"
  | CONSTRUCTOR_EXPR -> "CONSTRUCTOR_EXPR"
  | POLY_VARIANT_EXPR -> "POLY_VARIANT_EXPR"
  | ASSERT_EXPR -> "ASSERT_EXPR"
  | LAZY_EXPR -> "LAZY_EXPR"
  | WHILE_EXPR -> "WHILE_EXPR"
  | FOR_EXPR -> "FOR_EXPR"
  | TRY_EXPR -> "TRY_EXPR"
  | TYPED_EXPR -> "TYPED_EXPR"
  | COERCE_EXPR -> "COERCE_EXPR"
  | ATTRIBUTE_EXPR -> "ATTRIBUTE_EXPR"
  | EXTENSION_EXPR -> "EXTENSION_EXPR"
  | OBJECT_EXPR -> "OBJECT_EXPR"
  | OBJECT_SELF -> "OBJECT_SELF"
  | OBJECT_METHOD -> "OBJECT_METHOD"
  | OBJECT_VAL -> "OBJECT_VAL"
  | OBJECT_INHERIT -> "OBJECT_INHERIT"
  | OBJECT_UPDATE_EXPR -> "OBJECT_UPDATE_EXPR"
  | METHOD_CALL_EXPR -> "METHOD_CALL_EXPR"
  | NEW_EXPR -> "NEW_EXPR"
  | LOCAL_OPEN_EXPR -> "LOCAL_OPEN_EXPR"
  | LET_MODULE_EXPR -> "LET_MODULE_EXPR"
  | FIRST_CLASS_MODULE_EXPR -> "FIRST_CLASS_MODULE_EXPR"
  | STRUCT_EXPR -> "STRUCT_EXPR"
  | SIG_EXPR -> "SIG_EXPR"
  | MODULE_PATH -> "MODULE_PATH"
  | IDENT_PATTERN -> "IDENT_PATTERN"
  | WILDCARD_PATTERN -> "WILDCARD_PATTERN"
  | LITERAL_PATTERN -> "LITERAL_PATTERN"
  | CONSTRUCTOR_PATTERN -> "CONSTRUCTOR_PATTERN"
  | TUPLE_PATTERN -> "TUPLE_PATTERN"
  | LIST_PATTERN -> "LIST_PATTERN"
  | ARRAY_PATTERN -> "ARRAY_PATTERN"
  | CONS_PATTERN -> "CONS_PATTERN"
  | RECORD_PATTERN -> "RECORD_PATTERN"
  | OR_PATTERN -> "OR_PATTERN"
  | AS_PATTERN -> "AS_PATTERN"
  | RANGE_PATTERN -> "RANGE_PATTERN"
  | TYPED_PATTERN -> "TYPED_PATTERN"
  | LAZY_PATTERN -> "LAZY_PATTERN"
  | EXCEPTION_PATTERN -> "EXCEPTION_PATTERN"
  | PAREN_PATTERN -> "PAREN_PATTERN"
  | POLY_VARIANT_PATTERN -> "POLY_VARIANT_PATTERN"
  | POLY_VARIANT_TYPE_PATTERN -> "POLY_VARIANT_TYPE_PATTERN"
  | EFFECT_PATTERN -> "EFFECT_PATTERN"
  | LOCAL_OPEN_PATTERN -> "LOCAL_OPEN_PATTERN"
  | OPERATOR_PATTERN -> "OPERATOR_PATTERN"
  | FIRST_CLASS_MODULE_PATTERN -> "FIRST_CLASS_MODULE_PATTERN"
  | TYPE_VAR -> "TYPE_VAR"
  | TYPE_CONSTR -> "TYPE_CONSTR"
  | TYPE_ALIAS -> "TYPE_ALIAS"
  | TYPE_ARROW -> "TYPE_ARROW"
  | TYPE_TUPLE -> "TYPE_TUPLE"
  | TYPE_PAREN -> "TYPE_PAREN"
  | TYPE_POLY_VARIANT -> "TYPE_POLY_VARIANT"
  | POLY_VARIANT_TAG -> "POLY_VARIANT_TAG"
  | TYPE_PARAM -> "TYPE_PARAM"
  | TYPE_PARAMS -> "TYPE_PARAMS"
  | TYPE_VARIANT_CONSTR -> "TYPE_VARIANT_CONSTR"
  | TYPE_EXTENSIBLE -> "TYPE_EXTENSIBLE"
  | TYPE_RECORD -> "TYPE_RECORD"
  | TYPE_RECORD_FIELD -> "TYPE_RECORD_FIELD"
  | OBJECT_TYPE -> "OBJECT_TYPE"
  | OBJECT_TYPE_FIELD -> "OBJECT_TYPE_FIELD"
  | TYPE_CONSTRAINT -> "TYPE_CONSTRAINT"
  | POLY_TYPE -> "POLY_TYPE"
  | MODULE_TYPE_EXPR -> "MODULE_TYPE_EXPR"
  | FIRST_CLASS_MODULE_TYPE -> "FIRST_CLASS_MODULE_TYPE"
  | MODULE_TYPE_PATH -> "MODULE_TYPE_PATH"
  | FUNCTOR_PARAM -> "FUNCTOR_PARAM"
  | FUNCTOR_TYPE -> "FUNCTOR_TYPE"
  | MODULE_APPLICATION -> "MODULE_APPLICATION"
  | MODULE_UNIT_APPLICATION -> "MODULE_UNIT_APPLICATION"
  | LET_BINDING -> "LET_BINDING"
  | LET_REC_BINDING -> "LET_REC_BINDING"
  | LET_MUTUAL_DECL -> "LET_MUTUAL_DECL"
  | TYPE_DECL -> "TYPE_DECL"
  | TYPE_MUTUAL_DECL -> "TYPE_MUTUAL_DECL"
  | EXCEPTION_DECL -> "EXCEPTION_DECL"
  | MODULE_DECL -> "MODULE_DECL"
  | CLASS_DECL -> "CLASS_DECL"
  | CLASS_TYPE_DECL -> "CLASS_TYPE_DECL"
  | MODULE_TYPE_DECL -> "MODULE_TYPE_DECL"
  | MODULE_TYPE_OF -> "MODULE_TYPE_OF"
  | OPEN_STMT -> "OPEN_STMT"
  | INCLUDE_STMT -> "INCLUDE_STMT"
  | VAL_DECL -> "VAL_DECL"
  | EXTERNAL_DECL -> "EXTERNAL_DECL"
  | SOURCE_FILE -> "SOURCE_FILE"
  | STRUCTURE -> "STRUCTURE"
  | SIGNATURE -> "SIGNATURE"
  | MATCH_CASE -> "MATCH_CASE"
  | PATTERN_GUARD -> "PATTERN_GUARD"
  | RECORD_FIELD -> "RECORD_FIELD"
  | RECORD_FIELD_PATTERN -> "RECORD_FIELD_PATTERN"
  | PARAMETER -> "PARAMETER"
  | LOCALLY_ABSTRACT_TYPE_PARAM -> "LOCALLY_ABSTRACT_TYPE_PARAM"
  | ARGUMENT -> "ARGUMENT"
  | ERROR -> "ERROR"
  | MISSING -> "MISSING"

let from_string = function
  | "WHITESPACE" -> Some WHITESPACE
  | "COMMENT" -> Some COMMENT
  | "DOCSTRING" -> Some DOCSTRING
  | "INT_LITERAL" -> Some INT_LITERAL
  | "FLOAT_LITERAL" -> Some FLOAT_LITERAL
  | "STRING_LITERAL" -> Some STRING_LITERAL
  | "CHAR_LITERAL" -> Some CHAR_LITERAL
  | "BOOL_LITERAL" -> Some BOOL_LITERAL
  | "UNIT_LITERAL" -> Some UNIT_LITERAL
  | "IDENT_EXPR" -> Some IDENT_EXPR
  | "PATH_EXPR" -> Some PATH_EXPR
  | "APPLY_EXPR" -> Some APPLY_EXPR
  | "LABELED_ARG" -> Some LABELED_ARG
  | "OPTIONAL_ARG" -> Some OPTIONAL_ARG
  | "INFIX_EXPR" -> Some INFIX_EXPR
  | "PREFIX_EXPR" -> Some PREFIX_EXPR
  | "MACRO_EXPR" -> Some MACRO_EXPR
  | "IF_EXPR" -> Some IF_EXPR
  | "MATCH_EXPR" -> Some MATCH_EXPR
  | "FUN_EXPR" -> Some FUN_EXPR
  | "LABELED_PARAM" -> Some LABELED_PARAM
  | "OPTIONAL_PARAM" -> Some OPTIONAL_PARAM
  | "OPTIONAL_PARAM_DEFAULT" -> Some OPTIONAL_PARAM_DEFAULT
  | "FUNCTION_EXPR" -> Some FUNCTION_EXPR
  | "LET_EXPR" -> Some LET_EXPR
  | "LET_REC_EXPR" -> Some LET_REC_EXPR
  | "SEQUENCE_EXPR" -> Some SEQUENCE_EXPR
  | "PAREN_EXPR" -> Some PAREN_EXPR
  | "TUPLE_EXPR" -> Some TUPLE_EXPR
  | "LIST_EXPR" -> Some LIST_EXPR
  | "ARRAY_EXPR" -> Some ARRAY_EXPR
  | "RECORD_EXPR" -> Some RECORD_EXPR
  | "RECORD_UPDATE_EXPR" -> Some RECORD_UPDATE_EXPR
  | "UNREACHABLE_EXPR" -> Some UNREACHABLE_EXPR
  | "FIELD_ACCESS_EXPR" -> Some FIELD_ACCESS_EXPR
  | "ARRAY_INDEX_EXPR" -> Some ARRAY_INDEX_EXPR
  | "STRING_INDEX_EXPR" -> Some STRING_INDEX_EXPR
  | "ASSIGN_EXPR" -> Some ASSIGN_EXPR
  | "CONSTRUCTOR_EXPR" -> Some CONSTRUCTOR_EXPR
  | "POLY_VARIANT_EXPR" -> Some POLY_VARIANT_EXPR
  | "ASSERT_EXPR" -> Some ASSERT_EXPR
  | "LAZY_EXPR" -> Some LAZY_EXPR
  | "WHILE_EXPR" -> Some WHILE_EXPR
  | "FOR_EXPR" -> Some FOR_EXPR
  | "TRY_EXPR" -> Some TRY_EXPR
  | "TYPED_EXPR" -> Some TYPED_EXPR
  | "COERCE_EXPR" -> Some COERCE_EXPR
  | "ATTRIBUTE_EXPR" -> Some ATTRIBUTE_EXPR
  | "EXTENSION_EXPR" -> Some EXTENSION_EXPR
  | "OBJECT_EXPR" -> Some OBJECT_EXPR
  | "OBJECT_SELF" -> Some OBJECT_SELF
  | "OBJECT_METHOD" -> Some OBJECT_METHOD
  | "OBJECT_VAL" -> Some OBJECT_VAL
  | "OBJECT_INHERIT" -> Some OBJECT_INHERIT
  | "OBJECT_UPDATE_EXPR" -> Some OBJECT_UPDATE_EXPR
  | "METHOD_CALL_EXPR" -> Some METHOD_CALL_EXPR
  | "NEW_EXPR" -> Some NEW_EXPR
  | "LOCAL_OPEN_EXPR" -> Some LOCAL_OPEN_EXPR
  | "LET_MODULE_EXPR" -> Some LET_MODULE_EXPR
  | "FIRST_CLASS_MODULE_EXPR" -> Some FIRST_CLASS_MODULE_EXPR
  | "STRUCT_EXPR" -> Some STRUCT_EXPR
  | "SIG_EXPR" -> Some SIG_EXPR
  | "IDENT_PATTERN" -> Some IDENT_PATTERN
  | "WILDCARD_PATTERN" -> Some WILDCARD_PATTERN
  | "LITERAL_PATTERN" -> Some LITERAL_PATTERN
  | "CONSTRUCTOR_PATTERN" -> Some CONSTRUCTOR_PATTERN
  | "TUPLE_PATTERN" -> Some TUPLE_PATTERN
  | "LIST_PATTERN" -> Some LIST_PATTERN
  | "ARRAY_PATTERN" -> Some ARRAY_PATTERN
  | "CONS_PATTERN" -> Some CONS_PATTERN
  | "RECORD_PATTERN" -> Some RECORD_PATTERN
  | "OR_PATTERN" -> Some OR_PATTERN
  | "AS_PATTERN" -> Some AS_PATTERN
  | "RANGE_PATTERN" -> Some RANGE_PATTERN
  | "TYPED_PATTERN" -> Some TYPED_PATTERN
  | "LAZY_PATTERN" -> Some LAZY_PATTERN
  | "EXCEPTION_PATTERN" -> Some EXCEPTION_PATTERN
  | "PAREN_PATTERN" -> Some PAREN_PATTERN
  | "POLY_VARIANT_PATTERN" -> Some POLY_VARIANT_PATTERN
  | "POLY_VARIANT_TYPE_PATTERN" -> Some POLY_VARIANT_TYPE_PATTERN
  | "EFFECT_PATTERN" -> Some EFFECT_PATTERN
  | "LOCAL_OPEN_PATTERN" -> Some LOCAL_OPEN_PATTERN
  | "OPERATOR_PATTERN" -> Some OPERATOR_PATTERN
  | "TYPE_VAR" -> Some TYPE_VAR
  | "TYPE_CONSTR" -> Some TYPE_CONSTR
  | "TYPE_ALIAS" -> Some TYPE_ALIAS
  | "TYPE_ARROW" -> Some TYPE_ARROW
  | "TYPE_TUPLE" -> Some TYPE_TUPLE
  | "TYPE_PAREN" -> Some TYPE_PAREN
  | "TYPE_POLY_VARIANT" -> Some TYPE_POLY_VARIANT
  | "POLY_VARIANT_TAG" -> Some POLY_VARIANT_TAG
  | "TYPE_PARAM" -> Some TYPE_PARAM
  | "TYPE_PARAMS" -> Some TYPE_PARAMS
  | "TYPE_VARIANT_CONSTR" -> Some TYPE_VARIANT_CONSTR
  | "TYPE_EXTENSIBLE" -> Some TYPE_EXTENSIBLE
  | "TYPE_RECORD" -> Some TYPE_RECORD
  | "TYPE_RECORD_FIELD" -> Some TYPE_RECORD_FIELD
  | "OBJECT_TYPE" -> Some OBJECT_TYPE
  | "OBJECT_TYPE_FIELD" -> Some OBJECT_TYPE_FIELD
  | "TYPE_CONSTRAINT" -> Some TYPE_CONSTRAINT
  | "POLY_TYPE" -> Some POLY_TYPE
  | "MODULE_TYPE_EXPR" -> Some MODULE_TYPE_EXPR
  | "FUNCTOR_PARAM" -> Some FUNCTOR_PARAM
  | "FUNCTOR_TYPE" -> Some FUNCTOR_TYPE
  | "MODULE_APPLICATION" -> Some MODULE_APPLICATION
  | "MODULE_UNIT_APPLICATION" -> Some MODULE_UNIT_APPLICATION
  | "LET_BINDING" -> Some LET_BINDING
  | "LET_REC_BINDING" -> Some LET_REC_BINDING
  | "LET_MUTUAL_DECL" -> Some LET_MUTUAL_DECL
  | "TYPE_DECL" -> Some TYPE_DECL
  | "TYPE_MUTUAL_DECL" -> Some TYPE_MUTUAL_DECL
  | "EXCEPTION_DECL" -> Some EXCEPTION_DECL
  | "MODULE_DECL" -> Some MODULE_DECL
  | "CLASS_DECL" -> Some CLASS_DECL
  | "CLASS_TYPE_DECL" -> Some CLASS_TYPE_DECL
  | "MODULE_TYPE_DECL" -> Some MODULE_TYPE_DECL
  | "MODULE_TYPE_OF" -> Some MODULE_TYPE_OF
  | "OPEN_STMT" -> Some OPEN_STMT
  | "INCLUDE_STMT" -> Some INCLUDE_STMT
  | "VAL_DECL" -> Some VAL_DECL
  | "EXTERNAL_DECL" -> Some EXTERNAL_DECL
  | "SOURCE_FILE" -> Some SOURCE_FILE
  | "STRUCTURE" -> Some STRUCTURE
  | "SIGNATURE" -> Some SIGNATURE
  | "MATCH_CASE" -> Some MATCH_CASE
  | "PATTERN_GUARD" -> Some PATTERN_GUARD
  | "RECORD_FIELD" -> Some RECORD_FIELD
  | "RECORD_FIELD_PATTERN" -> Some RECORD_FIELD_PATTERN
  | "PARAMETER" -> Some PARAMETER
  | "LOCALLY_ABSTRACT_TYPE_PARAM" -> Some LOCALLY_ABSTRACT_TYPE_PARAM
  | "ARGUMENT" -> Some ARGUMENT
  | "ERROR" -> Some ERROR
  | "MISSING" -> Some MISSING
  | _ -> None
