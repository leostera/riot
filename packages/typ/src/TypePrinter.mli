open Std

(** Pretty-printers for prototype types and schemes. *)

val type_to_string: TypeRepr.t -> string

val scheme_to_string: TypeScheme.t -> string
