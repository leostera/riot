open Std
module TypingContext = Typing_context
module Typings = File

type typing_context = TypingContext.t

let make_typing_context = fun () -> TypingContext.empty

let check = fun ?(typing_context = make_typing_context ()) ~source parse_result ->
  match Ast.from_parse_result ~source parse_result with
  | Ok ast -> Ok (Core.check_source_file ~typing_context ast)
  | Error diagnostics -> Error diagnostics
