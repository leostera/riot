open Std.Collections
open Ast

type t = {
  values: (ident * TypeScheme.t) Vector.t;
}

let values t = Vector.iter t.values

let from_env (env: Env.t) =
  let values = Vector.with_capacity ~size:16 in
  Std.Iter.Iterator.for_each (Env.exports env) ~fn:(fun value -> Vector.push values ~value);
  { values }
