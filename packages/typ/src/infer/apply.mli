(**
   Function application inference.

   This module owns the argument-matching rules for calls. `Typer` still owns
   expression recursion and passes synthesis/checking callbacks so a known
   parameter type is used to check an argument directly.
*)

(**
   Infer a function application.

   The returned type is the type produced after applying every supplied
   argument. Labeled arguments are matched by label rather than only by
   position, and omitted optional parameters may be skipped when a later
   positional argument is supplied. Arguments check against known arrow
   parameters; they synthesize only when the callee type is still unknown.
*)
val infer:
  State.t ->
  infer_expression:(Ast.expression -> Ast.Type.t) ->
  check_expression:(Ast.expression -> Ast.Type.t -> Ast.Type.t) ->
  Ast.application ->
  Ast.Type.t
