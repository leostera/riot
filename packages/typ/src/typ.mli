(**
   Riot's type-checker engine.

   `Typ` is the public facade for the package. It exposes the typed tree, the
   existing one-shot checker path, the new inference prototype, diagnostics,
   and small model types shared by those pieces.
*)

(** Shared model helpers for sources, surface paths, and semantic identities. *)
module Model: module type of Model

(** Typed syntax tree built from `Syn.Ast` and annotated by checking. *)
module Ast: module type of Ast

(** Diagnostic types and diagnostic collection helpers. *)
module Diagnostics: module type of Diagnostics

(** Existing one-shot checker over `Typ.Ast`. *)
module Check: module type of Check

(** New small inference engine being built over `Typ.Ast`. *)
module Infer: module type of Infer

(** Temporary `.mli`-style renderer for checked-file summaries. *)
module SignatureGenerator: module type of Signature_generator

(** Source snapshot accepted by checker entrypoints. *)
type source = Model.Source.t
(** Checked-file result produced by the existing `Typ.Check` path. *)
type checked_source = Check.Typings.t
