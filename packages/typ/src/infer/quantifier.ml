open Std
open Std.Collections
open Ast

(**
   Generalize will freeze any solver variables that are still free (unbound)
   so they can be reused polymorphically later on.

   For example, if we infer `let id x = x` we'll end up with an arrow type like `'0 -> '0`,
   where `'0` is still a mutable solver var. This means that if we store it as is in the environment,
   the first use will bind it forever: `id true` will bind `'0 <- int` and we can't call `id true`.

   So Generalize grabs that unsolved vars and turns it into a stable generic.
   This can be reversed with `instantiate`, which turns a generic back into a
   fresh solver var without affecting the generalized arrow.
*)
let generalize type_ =
  let rec loop type_ =
    match Unifier.resolve type_ with
    | Type.Var { id; link = None } -> Type.Generic id
    | Type.Var { link = Some linked; _ } -> loop linked
    | Type.Generic _ -> type_
    | Type.Tuple parts -> Type.Tuple (List.map parts ~fn:loop)
    | Type.Arrow arrow ->
        let parameter = loop arrow.parameter in
        let result = loop arrow.result in
        Type.Arrow { arrow with parameter; result }
    | Type.Constructor ctr ->
        let arguments = List.map ctr.arguments ~fn:loop in
        Type.Constructor { ctr with arguments }
  in
  TypeScheme.monomorphic (loop type_)

(**
   Instantiate makes a copy of a type with its generics replaced by fresh
   variables so the solver can unify them against concrete types.
*)
let instantiate state t =
  let substitutions = HashMap.with_capacity ~size:8 in
  let fresh_for_generic id =
    match HashMap.get substitutions ~key:id with
    | Some type_ -> type_
    | None ->
        let type_ = State.fresh_var state in
        let _ = HashMap.insert substitutions ~key:id ~value:type_ in
        type_
  in
  let rec loop type_ =
    match Unifier.resolve type_ with
    | Type.Generic id -> fresh_for_generic id
    | Type.Var _ as type_ -> type_
    | Type.Tuple parts -> Type.Tuple (List.map parts ~fn:loop)
    | Type.Arrow arrow ->
        let parameter = loop arrow.parameter in
        let result = loop arrow.result in
        Type.Arrow { arrow with parameter; result }
    | Type.Constructor ctr ->
        let arguments = List.map ctr.arguments ~fn:loop in
        Type.Constructor { ctr with arguments }
  in
  loop TypeScheme.(t.body)
