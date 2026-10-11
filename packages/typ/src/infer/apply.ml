open Std
open Ast

let argument_label_to_type_label = fun __tmp1 ->
  match __tmp1 with
  | Positional _ -> Type.Label.NoLabel
  | Labeled { label; _ } -> Type.Label.Labelled label
  | Optional { label; _ } -> Type.Label.Optional label

let argument_value = fun __tmp1 ->
  match __tmp1 with
  | Positional expr -> Some expr
  | Labeled { value; _ }
  | Optional { value; _ } -> value

(**
   Match the source argument label against an arrow label.

   A required labeled argument can satisfy an optional parameter of the same
   name, which covers ordinary override syntax:

   `{ocaml|let f ?(x = 1) () = x in f ~x:2 ()|ocaml}`

   Supplying `?x` is kept distinct because that syntax passes an optional value
   through rather than overriding with the raw payload.
*)
let label_matches supplied expected =
  match (supplied, expected) with
  | (Type.Label.NoLabel, Type.Label.NoLabel) -> true
  | (Type.Label.Labelled supplied, Type.Label.Labelled expected)
  | (Type.Label.Optional supplied, Type.Label.Optional expected)
  | (Type.Label.Labelled supplied, Type.Label.Optional expected) -> String.equal supplied expected
  | _ -> false

let is_optional_label = fun __tmp1 ->
  match __tmp1 with
  | Type.Label.Optional _ -> true
  | _ -> false

(**
   Rebuild arrows skipped while looking for a later labeled parameter.

   When applying `f ~right:true` to
   `left:'a -> right:'a -> bool -> 'a`, the `left` arrow is skipped during the
   search and then wrapped back around the remaining result. The skipped list is
   stored inside-out (`rightmost :: ... :: leftmost`) so a left fold rebuilds the
   original order.
*)
let rebuild_skipped_arrows skipped result =
  List.fold_left skipped ~init:result ~fn:(fun result arrow -> Type.Arrow { arrow with result })

let infer_argument infer_expression state (arg: argument) =
  match argument_value arg.kind with
  | Some expr -> infer_expression expr
  | None -> State.fresh_var state

(**
   Apply one positional argument.

   Optional parameters can be omitted once the caller supplies a later
   positional argument. Required labeled parameters are different: they may be
   supplied after the positional argument, so they are skipped while searching
   and then rebuilt around the result.

   For example, applying `true` to `right:int -> bool -> int` consumes the
   `bool` arrow and keeps `right:int -> int`.
*)
let check_argument state check_expression (arg: argument) expected =
  match argument_value arg.kind with
  | Some expr -> check_expression expr expected
  | None ->
      let actual = State.fresh_var state in
      Constraint.unify
        state
        ~expected
        ~actual
        ~on_error:(Constraint.argument_constraint_diagnostic arg);
      expected

let apply_positional_argument state ~infer_expression ~check_expression callee arg =
  let rec search skipped type_ =
    match Unifier.resolve type_ with
    | Type.Arrow arrow when Type.Label.equal arrow.label Type.Label.NoLabel ->
        let _ = check_argument state check_expression arg arrow.parameter in
        rebuild_skipped_arrows skipped arrow.result
    | Type.Arrow arrow when is_optional_label arrow.label -> search skipped arrow.result
    | Type.Arrow arrow -> search (arrow :: skipped) arrow.result
    | Type.Var _
    | _ ->
        let arg_type = infer_argument infer_expression state arg in
        let result = State.fresh_var state in
        Constraint.unify
          state
          ~expected:type_
          ~actual:(Ast.Type.arrow arg_type result)
          ~on_error:(Constraint.argument_constraint_diagnostic arg);
        rebuild_skipped_arrows skipped result
  in
  search [] callee

(**
   Apply one labeled argument.

   If the callee type is already an arrow chain, we walk through it until the
   matching label is found, preserving unmatched arrows so partial application
   keeps the remaining parameters. If the callee is still a fresh variable, the
   argument itself teaches the checker that the callee must be a function with
   this labeled parameter.
*)
let apply_labeled_argument state ~infer_expression ~check_expression callee arg label =
  let rec search skipped type_ =
    match Unifier.resolve type_ with
    | Type.Arrow arrow when label_matches label arrow.label ->
        let _ = check_argument state check_expression arg arrow.parameter in
        rebuild_skipped_arrows skipped arrow.result
    | Type.Arrow arrow -> search (arrow :: skipped) arrow.result
    | Type.Var _
    | _ ->
        let arg_type = infer_argument infer_expression state arg in
        let result = State.fresh_var state in
        Constraint.unify
          state
          ~expected:type_
          ~actual:(Ast.Type.arrow ~label arg_type result)
          ~on_error:(Constraint.argument_constraint_diagnostic arg);
        rebuild_skipped_arrows skipped result
  in
  search [] callee

let apply_argument state ~infer_expression ~check_expression callee (arg: argument) =
  let label = argument_label_to_type_label arg.kind in
  match label with
  | Type.Label.NoLabel ->
      apply_positional_argument state ~infer_expression ~check_expression callee arg
  | _ ->
      apply_labeled_argument state ~infer_expression ~check_expression callee arg label

let infer state ~infer_expression ~check_expression (apply: application) =
  let callee = infer_expression apply.callee in
  List.fold_left
    apply.arguments
    ~init:callee
    ~fn:(apply_argument state ~infer_expression ~check_expression)
