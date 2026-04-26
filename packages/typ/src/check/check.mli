open Std

(** One-shot type checking entrypoint.

    This module is the public facade for the current checker slice. It accepts a
    parsed Syn file, builds the corresponding {!Typ.Ast.t}, runs checking over
    that tree, and returns a {!Typings.t} value containing the checked AST,
    diagnostics, exported bindings, and the updated typing context.

    The checker is deliberately whole-file oriented for now. Callers that want
    editor-style behavior should re-run [check] for the current parse result
    and reuse the returned [typing_context] when they need previously checked
    values in scope. *)

(** Serializable public environment produced and consumed by the checker. *)
module TypingContext: module type of Typing_context

(** Result of checking one source file. *)
module Typings: module type of File

(** Public environment available before checking a file. *)
type typing_context = TypingContext.t

(** [make_typing_context ()] returns an empty checker environment. *)
val make_typing_context: unit -> typing_context

(** [check ?typing_context ~source parse_result] builds [Typ.Ast] from
    [parse_result] and type-checks it.

    [source] is kept in the call shape so AST construction and diagnostics can
    stay source-backed as the package grows.

    Returns [Error diagnostics] when building [Typ.Ast] fails before checking
    can start. Successful results may still contain typing diagnostics in
    [Typings.diagnostics]. *)
val check:
  ?typing_context:typing_context ->
  source:Model.Source.t ->
  Syn.Parser.parse_result ->
  (Typings.t, Diagnostics.Diagnostic.t list) Result.t
