type t = {
  mutable next_var: Ast.TypeVar.t;
  env: Env.t;
  diagnostics: Diagnostics.t;
}

let create () = {
  next_var = Ast.TypeVar.first;
  diagnostics = Diagnostics.create ();
  env = Env.create ();
}

let env state = state.env

let diagnostics state = state.diagnostics

(** Instantiates a new fresh type variable in the current typing environment. *)
let fresh_var state =
  let id = state.next_var in
  state.next_var <- Ast.TypeVar.next id;
  Ast.Type.Var { id; link = None }
