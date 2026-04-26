open Std
open Std.Collections
open Core_types

module SurfacePath = Model.Surface_path

let path_int = SurfacePath.from_name "int"

let path_bool = SurfacePath.from_name "bool"

let path_char = SurfacePath.from_name "char"

let path_string = SurfacePath.from_name "string"

let path_float = SurfacePath.from_name "float"

let path_unit = SurfacePath.from_name "unit"

let path_unit_constructor = SurfacePath.from_name "()"

let path_list = SurfacePath.from_name "list"

let path_array = SurfacePath.from_name "array"

let path_option = SurfacePath.from_name "option"

let path_exn = SurfacePath.from_name "exn"

let path_none = SurfacePath.from_name "None"

let path_some = SurfacePath.from_name "Some"

let path_not = SurfacePath.from_name "not"

let path_plus = SurfacePath.from_name "+"

let path_minus = SurfacePath.from_name "-"

let path_star = SurfacePath.from_name "*"

let path_slash = SurfacePath.from_name "/"

let path_plus_dot = SurfacePath.from_name "+."

let path_minus_dot = SurfacePath.from_name "-."

let path_star_dot = SurfacePath.from_name "*."

let path_slash_dot = SurfacePath.from_name "/."

type builtin = {
  path: SurfacePath.t;
  ty: ty;
}

let generic_var = fun id -> TVar { var = Generic id }

let arrow = fun parameter result -> TArrow (NoLabel, parameter, result)

let builtin_bindings = [
  { path = path_unit_constructor; ty = TUnit };
  { path = path_none; ty = TOption (generic_var 0) };
  { path = path_some; ty = arrow (generic_var 0) (TOption (generic_var 0)) };
  { path = path_not; ty = arrow TBool TBool };
  { path = path_plus; ty = arrow TInt (arrow TInt TInt) };
  { path = path_minus; ty = arrow TInt (arrow TInt TInt) };
  { path = path_star; ty = arrow TInt (arrow TInt TInt) };
  { path = path_slash; ty = arrow TInt (arrow TInt TInt) };
  { path = path_plus_dot; ty = arrow TFloat (arrow TFloat TFloat) };
  { path = path_minus_dot; ty = arrow TFloat (arrow TFloat TFloat) };
  { path = path_star_dot; ty = arrow TFloat (arrow TFloat TFloat) };
  { path = path_slash_dot; ty = arrow TFloat (arrow TFloat TFloat) };
]

let rec lookup_builtin = fun path builtins ->
  match builtins with
  | [] -> None
  | builtin :: rest ->
      if SurfacePath.equal builtin.path path then
        Some builtin.ty
      else
        lookup_builtin path rest
