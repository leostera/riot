(** Structured diagnostic payloads emitted by `typ`.

    Diagnostics stay small and source-backed. They are meant to be rendered by
    CLI/LSP layers, serialized in snapshots, and eventually reused by cache
    records without depending on human-readable strings alone. *)

(** Syntax accepted by `syn` but not represented by the current `Typ.Ast`
    slice. *)
type unsupported_syntax = {
  (** Source span for the unsupported syntax. *)
  span: Ceibo.Span.t;
  (** Syntax kind that triggered the diagnostic. *)
  kind: Syn.SyntaxKind.t;
  (** Short human-readable explanation. *)
  summary: string;
}

(** Type syntax accepted by `syn` but not supported by the current checker
    slice. *)
type unsupported_type = {
  (** Source span for the unsupported type. *)
  span: Ceibo.Span.t;
  (** Short human-readable explanation. *)
  summary: string;
}

(** Failed unification created by a source type annotation.

    This is more specific than a generic type mismatch: the checker knows the
    constraint came from source syntax that promised a type. *)
type annotation_mismatch = {
  (** Source span for the failed annotation constraint. *)
  span: Ceibo.Span.t;
  (** Source span for the annotation that created the constraint. *)
  annotation_span: Ceibo.Span.t;
  (** Type required by the annotation. *)
  expected: string;
  (** Type inferred from the checked expression or pattern. *)
  actual: string;
}

(** Failed unification where a solver variable would contain itself. *)
type infinite_substitution = {
  (** Source span for the constraint that triggered the failure. *)
  span: Ceibo.Span.t;
  (** Solver variable being linked. *)
  var: string;
  (** Type that would make the variable recursive. *)
  type_: string;
}

(** Fallback type mismatch when no richer constraint-site diagnostic exists. *)
type type_mismatch = {
  (** Source span for the failed type constraint. *)
  span: Ceibo.Span.t;
  (** Expected type. *)
  expected: string;
  (** Actual type. *)
  actual: string;
}

(** Diagnostic emitted while building or checking `Typ.Ast`. *)
type t =
  (** Unsupported syntax node. *)
  | UnsupportedSyntax of unsupported_syntax
  (** Unsupported type expression. *)
  | UnsupportedType of unsupported_type
  (** Source annotation does not match the inferred type. *)
  | AnnotationMismatch of annotation_mismatch
  (** Solver variable would be substituted with a type containing itself. *)
  | InfiniteSubstitution of infinite_substitution
  (** Generic type mismatch fallback. *)
  | TypeMismatch of type_mismatch

(** Build an annotation mismatch diagnostic. *)
val annotation_mismatch :
  span:Ceibo.Span.t ->
  annotation_span:Ceibo.Span.t ->
  expected:string ->
  actual:string ->
  t

(** Build an infinite-substitution diagnostic. *)
val infinite_substitution :
  span:Ceibo.Span.t ->
  var:string ->
  type_:string ->
  t

(** Build a generic type mismatch diagnostic. *)
val type_mismatch :
  span:Ceibo.Span.t ->
  expected:string ->
  actual:string ->
  t

(** Human-readable diagnostic summary for tests and debugging. *)
val to_string : t -> string

(** Serializer used by snapshots and future machine-readable reports. *)
val serializer : t Serde.Ser.t
