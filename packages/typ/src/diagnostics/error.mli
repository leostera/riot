open Std

(** Stable diagnostic identifiers for `typ`.

    The numbered variants live outside `Diagnostic` so diagnostic payloads stay
    focused on source facts. Renderers and tools should use these IDs for
    stable matching and display. *)
type id =
  | T0001_UnsupportedSyntax
  | T0002_UnsupportedType
  | T0003_AnnotationMismatch
  | T0004_InfiniteSubstitution
  | T0005_TypeMismatch
  | T0006_UnerasableOptionalArgument
  | T0007_UnresolvedName
  | T0008_TypeConstructorArityMismatch

(** Render a stable diagnostic code. *)
val id_to_string: id -> string

(** Human-readable name for the diagnostic code. *)
val name: id -> string

(** Explanation shown as the diagnostic hint. *)
val explain: id -> string
