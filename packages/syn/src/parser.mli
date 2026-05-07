open Std
open Std.Collections

(**
   Streaming OCaml parser.

   The parser consumes a stable source slice, lexes it, streams syntax events
   into the lossless tree builder, and always returns a tree plus diagnostics.
   Callers should keep string/file/reader conversion at their boundary and pass
   `IO.IoVec.IoSlice.t` here so all token views can point back into the same
   source storage.
*)
type file_kind = [ | `Implementation | `Interface]
(**
   Result of one parser run.

   `tokens` contains the raw token stream, including trivia. `tree` stores only
   significant token leaves in child arrays; each token leaf still references
   the raw range that covers its leading trivia.
*)
type parse_result = {
  (** Source slice used by tokens, diagnostics, and Ast views. *)
  source: IO.IoVec.IoSlice.t;
  (** File grammar selected for this parse. *)
  kind: file_kind;
  (** Raw source-backed tokens, including trivia and EOF. *)
  tokens: Raw_token.stream;
  (** Lossless syntax tree built from parser events. *)
  tree: Syntax_tree.t;
  (** Recoverable syntax diagnostics. Empty means the parse was clean. *)
  diagnostics: Diagnostic.t Vector.t;
}

(** Parse an implementation file body. *)
val parse_implementation: IO.IoVec.IoSlice.t -> parse_result

(** Parse an interface file body. *)
val parse_interface: IO.IoVec.IoSlice.t -> parse_result

(**
   Returns true when the source slice ends in a lexical toplevel phrase
   terminator [`;;`], ignoring trailing trivia and EOF.

   This is intended for REPL-style buffering. It does not mean the phrase is
   syntactically valid; callers should still parse the completed phrase and
   inspect diagnostics.
*)
val phrase_complete: IO.IoVec.IoSlice.t -> bool

(** Parse one implementation phrase as a single structure item. *)
val parse_structure_phrase: IO.IoVec.IoSlice.t -> parse_result

(** Parse one interface phrase as a single signature item. *)
val parse_signature_phrase: IO.IoVec.IoSlice.t -> parse_result

(**
   Parse a source slice, choosing interface grammar for `.mli` filenames and
   implementation grammar otherwise.
*)
val parse: filename:Std.Path.t -> IO.IoVec.IoSlice.t -> parse_result
