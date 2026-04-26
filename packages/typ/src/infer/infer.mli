open Std

(** Mutable inference state.

    This owns allocation for solver variables and other per-run counters. It is
    query-local state for the one-shot checker, not a persisted module
    interface. *)
module State: module type of State

(** Inference errors returned by lower-level solver operations. *)
module Error: module type of Error

(** Type unification engine.

    The unifier works over `Ast.Type.t`, follows solver-variable links, and
    rejects impossible constraints such as infinite types. *)
module Unifier: module type of Unifier

(** Exported summary inferred for one source module. *)
module ModuleInterface: module type of ModuleInterface

(** Result of one inference/checking run. *)
type infer_result = {
  (** Exported module interface produced by checking the file. *)
  intf: ModuleInterface.t;
  (** Structured diagnostics collected during checking. *)
  diagnostics: Diagnostics.t;
}

(** Check a complete `Typ.Ast` in one shot.

    The returned AST annotations are written directly onto the input tree. The
    `ModuleInterface.t` is the exported summary that tests and future cache
    layers can render or persist. *)
val check: Ast.t -> infer_result
