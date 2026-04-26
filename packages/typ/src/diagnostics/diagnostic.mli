(** Structured diagnostic payloads emitted by `typ`.

    Diagnostics stay small and source-backed. They are meant to be rendered by
    CLI/LSP layers, serialized in snapshots, and eventually reused by cache
    records without depending on human-readable strings alone. *)

(** Syntax accepted by `syn` but not represented by the current `Typ.Ast`
    slice. *)
type unsupported_syntax = {
  (** Source span for the unsupported syntax. *)
  span: Syn.Ceibo.Span.t;
  (** Syntax kind that triggered the diagnostic. *)
  kind: Syn.SyntaxKind.t;
  (** Short human-readable explanation. *)
  summary: string;
}

(** Type syntax accepted by `syn` but not supported by the current checker
    slice. *)
type unsupported_type = {
  (** Source span for the unsupported type. *)
  span: Syn.Ceibo.Span.t;
  (** Short human-readable explanation. *)
  summary: string;
}

(** Diagnostic emitted while building or checking `Typ.Ast`. *)
type t =
  (** Unsupported syntax node. *)
  | UnsupportedSyntax of unsupported_syntax
  (** Unsupported type expression. *)
  | UnsupportedType of unsupported_type

(** Serializer used by snapshots and future machine-readable reports. *)
val serializer : t Serde.Ser.t
