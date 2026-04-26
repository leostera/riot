(**
   Public, serializable type information.

   This module defines the stable type language that leaves the checker. The
   inference engine in `Core` uses a richer mutable representation internally;
   successful bindings are converted into these immutable values before they
   are returned to callers or reused as input to another one-shot check.
*)

(** Function argument labels in public arrow types. *)
type arg_label =
  (** Ordinary positional argument. *)
  | NoLabel
  (** Required labelled argument, for example `~name:int -> ...`. *)
  | Labelled of string
  (** Optional labelled argument, for example `?name:int -> ...`. *)
  | Optional of string
(** Public function-arrow payload. *)
type function_type = {
  (** Argument label accepted by this arrow. *)
  label: arg_label;
  (** Parameter type. *)
  parameter: type_expr;
  (** Result type. *)
  result: type_expr;
}

(** Public nominal type constructor application. *)
and type_constructor = {
  (** Nominal type path, such as `list`, `M.t`, or `Derived.t`. *)
  path: Model.Surface_path.t;
  (** Type arguments applied to `path`. *)
  arguments: type_expr list;
}

(** Public named alias for preserving shared type structure. *)
and alias_type = {
  (** Type being named by the alias variable. *)
  type_: type_expr;
  (** Public type-variable id used by renderers to print `'a`, `'b`, and so on. *)
  id: int;
}

(** Public polymorphic-variant row bound. *)
and poly_variant_bound =
  (** Closed row with exactly the listed tags. *)
  | Exact
  (** Upper-bounded row, rendered as `[< ... ]`. *)
  | Upper
  (** Lower-bounded row, rendered as `[> ... ]`. *)
  | Lower

(** Public polymorphic-variant row field. *)
and poly_variant_field = {
  (** Tag name without the leading backtick. *)
  tag: string;
  (** Optional payload carried by this tag. *)
  payload: type_expr option;
}

(** Public polymorphic-variant row. *)
and poly_variant = {
  (** Row bound inferred for this polymorphic variant. *)
  bound: poly_variant_bound;
  (** Normalized tag set. *)
  fields: poly_variant_field list;
}

(** Public first-class module package `with type` constraint. *)
and package_type_constraint = {
  (** Type member constrained inside a first-class module package. *)
  type_name: Model.Surface_path.t;
  (** Manifest type for `type_name`. *)
  manifest: type_expr;
}

(** Public first-class module package type. *)
and package_type = {
  (**
     Optional local module binder used when package result types depend on the
     unpacked module path.
  *)
  binder: string option;
  (** Module type path, such as `S` in `(module S)`. *)
  module_type: Model.Surface_path.t;
  (** `with type` constraints attached to the package. *)
  constraints: package_type_constraint list;
}

(** Public type-expression language returned by the checker. *)
and type_expr =
  (** Built-in `int`. *)
  | Int
  (** Built-in `bool`. *)
  | Bool
  (** Built-in `char`. *)
  | Char
  (** Built-in `string`. *)
  | String
  (** Built-in `float`. *)
  | Float
  (** Built-in `unit`. *)
  | Unit
  (** `'a list`. *)
  | List of type_expr
  (** `'a option`. *)
  | Option of type_expr
  (** Tuple type. The list is expected to contain at least two elements. *)
  | Tuple of type_expr list
  (** Function arrow. *)
  | Arrow of function_type
  (** Nominal type constructor application. *)
  | TypeConstructor of type_constructor
  (**
     Named alias for a type expression. Used mainly to preserve shared
     polymorphic-variant rows while rendering.
  *)
  | Alias of alias_type
  (** Polymorphic variant row. *)
  | PolyVariant of poly_variant
  (** First-class module package type. *)
  | Package of package_type
  (** Public type variable id. These ids are local to a scheme. *)
  | Var of int
(** Generalized public value type. *)
type scheme = {
  (** Quantified variable ids in stable display order. *)
  forall: int list;
  (** Scheme body. *)
  body: type_expr;
}
(** Exported value binding. *)
type value_binding = {
  (** Fresh checker-local binding identity. *)
  binding_id: Model.Binding_id.t;
  (** Stable source-facing identity, including its surface path. *)
  entity_id: Model.Entity_id.t;
  (** Generalized public type for the binding. *)
  scheme: scheme;
}
(** Public typing environment that can be fed into later one-shot checks. *)
type t = {
  (** Next binding stamp to use when checking another file with this context. *)
  next_binding_stamp: int;
  (** Public values available to later checks. *)
  values: value_binding list;
}

(** Empty public typing context. *)
val empty: t

(** Serializer for one exported value binding. *)
val value_binding_serializer: value_binding Serde.Ser.t

(** Serializer for the public typing context. *)
val serializer: t Serde.Ser.t
