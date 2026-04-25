module TypingContext: module type of Typing_context

module Typings: module type of File

type typing_context = TypingContext.t
val make_typing_context: unit -> typing_context

val check:
  ?typing_context:typing_context -> source:Model.Source.t -> Syn.Parser.parse_result -> Typings.t
