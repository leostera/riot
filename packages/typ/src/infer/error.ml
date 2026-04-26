open Std

type t =
  | TypeMismatch of { expected: Ast.Type.t; actual: Ast.Type.t }
  | SolverFoundInfiniteType of { var: Ast.Type.variable; type_: Ast.Type.t }

let solver_found_infinite_type ~var ~type_ = Error (SolverFoundInfiniteType { var; type_ })

let type_mismatch ~expected ~actual = Error (TypeMismatch { expected; actual })

let to_string err =
  match err with
  | TypeMismatch { expected; actual } -> "Expected "
  ^ Ast.Type.to_string expected
  ^ " but got "
  ^ Ast.Type.to_string actual
  | SolverFoundInfiniteType { var; type_ } -> "Solver found an infinite type when trying to solve "
  ^ (Ast.TypeVar.to_string var.id)
  ^ " in "
  ^ (Ast.Type.to_string type_)
