(**
   Checked-file result.

   A value of this type is the current output of the one-shot checker. It keeps
   the input `Ast.t` together with diagnostics and the public information that
   later files or tools can reuse.
*)
type t = {
  (** The `Typ.Ast` built from the Syn parse result and checked by the core. *)
  ast: Ast.t;
  (** Structured diagnostics emitted while checking this file. *)
  diagnostics: Diagnostics.Diagnostic.t list;
  (** Top-level type declarations exported by this file. *)
  type_declarations: Ast.type_declaration list;
  (**
     Top-level value bindings exported directly by this file. Module member
     bindings are stored in `typing_context`.
  *)
  bindings: Typing_context.value_binding list;
  (**
     Environment after checking this file. This includes incoming bindings plus
     public bindings discovered in the current file.
  *)
  typing_context: Typing_context.t;
}

(**
   `empty ~ast ~typing_context` returns a successful empty checked file for an
   empty implementation or interface.
*)
val empty: ast:Ast.t -> typing_context:Typing_context.t -> t

(** `is_ok file` is `true` when `file` has no diagnostics. *)
val is_ok: t -> bool

(** Serializer used by tests and future cache/query payloads. *)
val serializer: t Serde.Ser.t
