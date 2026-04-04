open Std

type env = (string * TypeScheme.t) list

type t = {
  prelude: env;
}

let monomorphic = fun ty ->
  TypeScheme.Forall ([], ty)

let polymorphic_eq =
  let lhs = TypeRepr.Var { id = 0; link = None } in
  TypeScheme.Forall ([ 0 ], TypeRepr.Arrow (lhs, TypeRepr.Arrow (lhs, TypeRepr.Bool)))

let default = {
  prelude = [
    ("+", monomorphic (TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Int))));
    ("-", monomorphic (TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Int))));
    ("*", monomorphic (TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Int))));
    ("/", monomorphic (TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Int))));
    ("=", polymorphic_eq);
    ("not", monomorphic (TypeRepr.Arrow (TypeRepr.Bool, TypeRepr.Bool)));
  ];
}
