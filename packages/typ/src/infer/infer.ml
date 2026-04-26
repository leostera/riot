open Std
module State = State
module Error = Error
module Unifier = Unifier
module Typer = Typer
module ModuleInterface = ModuleInterface

type infer_result = {
  intf: ModuleInterface.t;
  diagnostics: Diagnostics.t;
}

let check (ast: Ast.t) =
  let state = State.create () in
  Typer.type_ast state ast;
  let intf = ModuleInterface.from_env state.env in
  let diagnostics = state.diagnostics in
  { intf; diagnostics }
