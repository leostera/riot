open Std
open Std.Result.Syntax

module TypingContext = Typing_context
module Typings = Module_typings_file

type typing_context = TypingContext.t

let make_typing_context = fun () -> TypingContext.empty

let check = fun ?(typing_context = make_typing_context ()) ~source parse_result ->
  let* ast = Ast.from_parse_result ~source parse_result in
  Ok (Core.check_source_file ~typing_context ast)
