open Ast

(**
   Apply a unifier result to mutable checker state.

   The unifier deliberately knows nothing about source locations or diagnostics.
   Each call site passes an `on_error` adapter that explains what source node
   created the constraint. That keeps diagnostics precise without forcing spans
   into every temporary solver type.
*)
let unify (state: State.t) ~expected ~actual ~on_error =
  match Unifier.unify_with_state state ~expected ~actual with
  | Ok () -> ()
  | Error err -> State.add_diagnostic state (on_error err)

(**
   Type annotations produce a more specific diagnostic than a generic mismatch:
   the user wrote a type, so we can point both at the checked expression/pattern
   and at the annotation that introduced the failed constraint.
*)
let annotation_diagnostic (annotation: core_type) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.annotation_mismatch
        ~span:annotation.origin.span
        ~annotation_span:annotation.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:annotation.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

(**
   Expression hints are stored on the expression rather than as standalone AST
   nodes, so this adapter needs both origins: the expression is where the value
   lives, and the hint's core type is where the promised type came from.
*)
let expression_hint_diagnostic (expr: expression) (hint: expression_type_hint) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.annotation_mismatch
        ~span:expr.origin.span
        ~annotation_span:hint.type_.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:expr.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

(**
   Fallback expression constraint diagnostics. These cover ordinary typing
   expectations such as `if` conditions being `bool` or record updates matching
   the inferred record type.
*)
let expression_constraint_diagnostic (expr: expression) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.type_mismatch
        ~span:expr.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:expr.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

(**
   Application arguments have their own origin spans. Using the argument span is
   more useful than pointing at the whole call when a single labeled argument is
   mismatched.
*)
let argument_constraint_diagnostic (arg: argument) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.type_mismatch
        ~span:arg.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:arg.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

(**
   Pattern constraints are usually created while binding a scrutinee or
   constructor payload. The pattern span is therefore the best current location
   for the mismatch.
*)
let pattern_constraint_diagnostic (pat: pattern) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.type_mismatch
        ~span:pat.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:pat.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)
