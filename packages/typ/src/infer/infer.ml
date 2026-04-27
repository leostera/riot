open Std

module State = State
module Unifier = Unifier
module TypeScheme = TypeScheme
module Quantifier = Quantifier
module Typer = Typer
module ModuleInterface = ModuleInterface

type infer_result = {
  intf: ModuleInterface.t;
  diagnostics: Diagnostics.t;
}

let check (ast: Ast.t) =
  let state = State.create () in
  Typer.type_ast state ast;
  let intf = ModuleInterface.from_env (State.env state) in
  let diagnostics = State.diagnostics state in
  { intf; diagnostics }
