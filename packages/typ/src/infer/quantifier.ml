open Std
open Std.Collections
open Ast

(**
   Generalize will freeze any solver variables that are still free (unbound)
   so they can be reused polymorphically later on.

   For example, if we infer `let id x = x` we'll end up with an arrow type like `'0 -> '0`,
   where `'0` is still a mutable solver var. This means that if we store it as is in the environment,
   the first use will bind it forever: `id true` will bind `'0 <- int` and we can't call `id true`.

   Generalize grabs those unsolved vars and turns them into stable generics,
   recording exactly which identities belong to the scheme. `instantiate`
   freshens only those identities; rigid generics captured from an outer scope
   remain shared.
*)
let generalize_at_level state ~level type_ =
  let quantifiers = ref [] in
  let generalize variable =
    let id = variable.Type.id in
    if not (List.any !quantifiers ~fn:(TypeVar.equal id)) then
      quantifiers := id :: !quantifiers;
    Type.Generic id
  in
  let rec loop type_ =
    match Unifier.resolve type_ with
    | Type.Var ({ link = None; _ } as variable) ->
        if Int.(State.variable_level state variable > level) then
          generalize variable
        else
          Type.Var variable
    | Type.Var { link = Some linked; _ } -> loop linked
    | Type.Generic _ -> type_
    | Type.Tuple parts -> Type.Tuple (List.map parts ~fn:loop)
    | Type.Arrow arrow ->
        let parameter = loop arrow.parameter in
        let result = loop arrow.result in
        Type.Arrow { arrow with parameter; result }
    | Type.Apply application ->
        let arguments = List.map application.arguments ~fn:loop in
        Type.Apply { application with arguments }
  in
  let scheme = TypeScheme.monomorphic (loop type_) in
  { scheme with quantifier = List.reverse !quantifiers }

let generalize state type_ = generalize_at_level state ~level:(State.current_level state) type_

(** Generalize every free solver variable and record its generic identity. *)
let generalize_all type_ =
  let quantifiers = ref [] in
  let generalize id =
    if not (List.any !quantifiers ~fn:(TypeVar.equal id)) then
      quantifiers := id :: !quantifiers;
    Type.Generic id
  in
  let rec loop type_ =
    match Unifier.resolve type_ with
    | Type.Var { id; link = None } -> generalize id
    | Type.Var { link = Some linked; _ } -> loop linked
    | Type.Generic _ -> type_
    | Type.Tuple parts -> Type.Tuple (List.map parts ~fn:loop)
    | Type.Arrow arrow ->
        Type.Arrow { arrow with parameter = loop arrow.parameter; result = loop arrow.result }
    | Type.Apply application ->
        Type.Apply { application with arguments = List.map application.arguments ~fn:loop }
  in
  let scheme = TypeScheme.monomorphic (loop type_) in
  { scheme with quantifier = List.reverse !quantifiers }

(**
   Instantiate makes a copy of a scheme, replacing only its quantified generic
   identities with fresh variables. Unquantified generics denote rigid variables
   captured by a local scope and must remain shared.
*)
let instantiate state (t: TypeScheme.t) =
  let substitutions = HashMap.with_capacity ~size:8 in
  let fresh_for_generic id =
    match HashMap.get substitutions ~key:id with
    | Some type_ -> type_
    | None ->
        let type_ = State.fresh_var state in
        let _ = HashMap.insert substitutions ~key:id ~value:type_ in
        type_
  in
  let is_quantified id = List.any t.quantifier ~fn:(TypeVar.equal id) in
  let rec loop type_ =
    match Unifier.resolve type_ with
    | Type.Generic id when is_quantified id -> fresh_for_generic id
    | Type.Generic _ as type_ -> type_
    | Type.Var _ as type_ -> type_
    | Type.Tuple parts -> Type.Tuple (List.map parts ~fn:loop)
    | Type.Arrow arrow ->
        let parameter = loop arrow.parameter in
        let result = loop arrow.result in
        Type.Arrow { arrow with parameter; result }
    | Type.Apply application ->
        let arguments = List.map application.arguments ~fn:loop in
        Type.Apply { application with arguments }
  in
  loop TypeScheme.(t.body)
