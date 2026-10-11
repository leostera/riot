(** Structured diagnostic payloads emitted by `typ`.

    Diagnostics stay small and source-backed. They are meant to be rendered by
    CLI/LSP layers, serialized in snapshots, and eventually reused by cache
    records without depending on human-readable strings alone. *)

(** Syntax accepted by `syn` but not represented by the current `Typ.Ast`
    slice. *)
type unsupported_syntax = {
  (** Source span for the unsupported syntax. *)
  span: Syn.Span.t;
  (** Syntax kind that triggered the diagnostic. *)
  kind: Syn.SyntaxKind.t;
  (** Short human-readable explanation. *)
  summary: string;
}

(** Type syntax accepted by `syn` but not supported by the current checker
    slice. *)
type unsupported_type = {
  (** Source span for the unsupported type. *)
  span: Syn.Span.t;
  (** Short human-readable explanation. *)
  summary: string;
}

(** Failed unification created by a source type annotation.

    This is more specific than a generic type mismatch: the checker knows the
    constraint came from source syntax that promised a type. *)
type annotation_mismatch = {
  (** Source span for the failed annotation constraint. *)
  span: Syn.Span.t;
  (** Source span for the annotation that created the constraint. *)
  annotation_span: Syn.Span.t;
  (** Type required by the annotation. *)
  expected: string;
  (** Type inferred from the checked expression or pattern. *)
  actual: string;
}

(** Failed unification where a solver variable would contain itself. *)
type infinite_substitution = {
  (** Source span for the constraint that triggered the failure. *)
  span: Syn.Span.t;
  (** Solver variable being linked. *)
  var: string;
  (** Type that would make the variable recursive. *)
  type_: string;
}

(** Fallback type mismatch when no richer constraint-site diagnostic exists. *)
type type_mismatch = {
  (** Source span for the failed type constraint. *)
  span: Syn.Span.t;
  (** Expected type. *)
  expected: string;
  (** Actual type. *)
  actual: string;
}

(** Name lookup failure in a specific typing namespace. *)
type unresolved_name = {
  (** Source span for the unresolved identifier. *)
  span: Syn.Span.t;
  (** Namespace searched, such as value, constructor, field, type, or module. *)
  namespace: string;
  (** Source spelling of the unavailable name. *)
  name: string;
}

(** Application of a known type constructor with the wrong number of arguments. *)
type type_constructor_arity_mismatch = {
  span: Syn.Span.t;
  name: string;
  expected: int;
  actual: int;
}

(** Optional argument whose default cannot be used by ordinary application.

    This mirrors OCaml warning 16. For now `typ` treats it as a warning-level
    diagnostic, not as a type-checking error. *)
type unerasable_optional_argument = {
  (** Source span for the optional parameter. *)
  span: Syn.Span.t;
  (** Optional argument label without the leading `?`. *)
  label: string;
}

(** Diagnostic severity.

    Errors mean the checker could not fully validate the program. Warnings are
    non-fatal observations attached to otherwise usable typing results. *)
type severity =
  (** Fatal diagnostic for the current checking slice. *)
  | Error
  (** Non-fatal diagnostic. *)
  | Warning

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
  (** Optional argument cannot be erased because no later positional argument exists. *)
  | UnerasableOptionalArgument of unerasable_optional_argument
  (** Lookup failed in a typing namespace. *)
  | UnresolvedName of unresolved_name
  (** Type constructor application has the wrong arity. *)
  | TypeConstructorArityMismatch of type_constructor_arity_mismatch

(** Build an annotation mismatch diagnostic. *)
val annotation_mismatch :
  span:Syn.Span.t ->
  annotation_span:Syn.Span.t ->
  expected:string ->
  actual:string ->
  t

(** Build an infinite-substitution diagnostic. *)
val infinite_substitution :
  span:Syn.Span.t ->
  var:string ->
  type_:string ->
  t

(** Build a generic type mismatch diagnostic. *)
val type_mismatch :
  span:Syn.Span.t ->
  expected:string ->
  actual:string ->
  t

(** Build an unerasable optional argument warning. *)
val unerasable_optional_argument :
  span:Syn.Span.t ->
  label:string ->
  t

(** Build a source-located unresolved-name error. *)
val unresolved_name :
  span:Syn.Span.t ->
  namespace:string ->
  name:string ->
  t

(** Build a type-constructor arity error. *)
val type_constructor_arity_mismatch :
  span:Syn.Span.t ->
  name:string ->
  expected:int ->
  actual:int ->
  t

(** Stable diagnostic identifier for a diagnostic. *)
val id : t -> Error.id

(** Source span covered by the diagnostic. *)
val span : t -> Syn.Span.t

(** Severity for a diagnostic. *)
val severity : t -> severity

(** Short actionable hint shown by human-readable renderers. *)
val hint : t -> string

(** Optional concrete fix suggestion. *)
val fix : t -> string option

(** Human-readable diagnostic summary for tests and debugging. *)
val to_string : t -> string

(** Structured JSON representation for diagnostic fixtures and tooling.

    The shape intentionally mirrors `Syn.Diagnostic.to_json`: each diagnostic
    is an object with a `kind` payload and a source `span`. *)
val to_json : t -> Std.Data.Json.t

(** Serializer used by snapshots and future machine-readable reports. *)
val serializer : t Serde.Ser.t
