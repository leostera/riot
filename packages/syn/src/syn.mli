open Std

(** OCaml lexer, streaming parser, diagnostics, and Ast views. *)

(** Source-text spans used by tokens, diagnostics, and Ast views. *)
module Span = Span

(** Structured parse error descriptions. *)
module Error: module type of Error

(** Token types produced by the lexer. *)
module Token: module type of Token

(** OCaml keyword definitions. *)
module Keyword: module type of Keyword

(** Low-level character cursor used by the lexer. *)
module Cursor: module type of Cursor

(** Lossless lexer. *)
module Lexer: module type of Lexer

(** Streaming parser lexical and grammar syntax kinds. *)
module SyntaxKind: module type of Syntax_kind

(** Source-backed raw token stream. *)
module RawToken: module type of Raw_token

(** Parser event stream. *)
module Event: module type of Event

(** Vector-backed lossless syntax tree. *)
module SyntaxTree: module type of Syntax_tree

(** Typed views over the lossless syntax tree. *)
module Ast: module type of Ast

(** Ast-driven visitor with internal typed-view memoization. *)
module Visitor: module type of Visitor

(** Structured parser diagnostics. *)
module Diagnostic: module type of Diagnostic

(** Streaming parser. *)
module Parser: module type of Parser

(** Diagnostic pretty-printer. *)
module DiagnosticReporter: module type of Diagnostic_reporter

(** Syntactic module dependency extraction. *)
module Deps: module type of Deps

(** Lex a source slice into token records with token-attached leading trivia. *)
val tokenize: IO.IoVec.IoSlice.t -> Token.t list

(** Parse .mli source from an existing source slice. *)
val parse_interface: IO.IoVec.IoSlice.t -> Parser.parse_result

(** Parse .ml source from an existing source slice. *)
val parse_implementation: IO.IoVec.IoSlice.t -> Parser.parse_result

(** Returns true when a source slice ends in a lexical toplevel [`;;`] phrase terminator. *)
val phrase_complete: IO.IoVec.IoSlice.t -> bool

(** Parse one implementation phrase as a single structure item. *)
val parse_structure_phrase: IO.IoVec.IoSlice.t -> Parser.parse_result

(** Parse one interface phrase as a single signature item. *)
val parse_signature_phrase: IO.IoVec.IoSlice.t -> Parser.parse_result

(**
   Parse an existing source slice with file-kind selection based on the
   filename extension.
*)
val parse: filename:Std.Path.t -> IO.IoVec.IoSlice.t -> Parser.parse_result

(**
   Parse a single source identifier or constructor path.

   Returns `None` unless the input parses cleanly as exactly one identifier-like
   implementation expression. This helper is for tooling boundaries that need
   to construct a structured `Ast.Ident.t` from a known-good string without
   reimplementing identifier parsing.
*)
val parse_ident: string -> Ast.Ident.t option
