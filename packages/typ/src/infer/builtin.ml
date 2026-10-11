open Std

(**
   Convert a hard-coded prelude name into the same structured identifier shape
   used for user-written names.

   This is intentionally the only string-to-path step in this module. Everywhere
   else works with `Ast.ident`, so the rest of the checker does not parse or
   split names by hand.
*)
let ident name =
  Model.Surface_path.from_parts [ name ]
  |> Result.expect ~msg:("expected builtin identifier " ^ name)

(* These identifiers are canonical names, not special type constructors. *)

let int_ident = ident "int"

let bool_ident = ident "bool"

let float_ident = ident "float"

let char_ident = ident "char"

let string_ident = ident "string"

let unit_ident = ident "unit"

let exn_ident = ident "exn"

let list_ident = ident "list"

let array_ident = ident "array"

let ref_ident = ident "ref"

let option_ident = ident "option"

let result_ident = ident "result"

let none_ident = ident "None"

let some_ident = ident "Some"

let ok_ident = ident "Ok"

let error_ident = ident "Error"

let raise_ident = ident "raise"

let deref_ident = ident "!"

let assign_ident = ident ":="

(**
   Built-ins are represented as nominal applications with zero or more type
   arguments. That keeps them on the same path as user-defined abstract types:
   `int`, `bool`, and `string` are not separate variants in the type algebra.
*)
let make ?(arguments = []) ident = Ast.Type.Apply { ident; arguments }

let int = make int_ident

let bool = make bool_ident

let float = make float_ident

let char = make char_ident

let string = make string_ident

let unit = make unit_ident

let exn = make exn_ident

let list el = make list_ident ~arguments:[ el ]

let array el = make array_ident ~arguments:[ el ]

let ref_ el = make ref_ident ~arguments:[ el ]

let option el = make option_ident ~arguments:[ el ]

let result ok error = make result_ident ~arguments:[ ok; error ]

(**
   Unit is special only because the source language exposes `()` as syntax, and
   earlier lowering represents that constructor through this canonical name.
*)
let is_unit ident = Model.Surface_path.equal ident unit_ident
