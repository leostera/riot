type t
val empty: t

val is_empty: t -> bool

val from_name: string -> t

val from_segments: string list -> t

val to_segments: t -> string list

val to_string: t -> string

val equal: t -> t -> bool

val compare: t -> t -> Std.Order.t

val serializer: t Serde.Ser.t
