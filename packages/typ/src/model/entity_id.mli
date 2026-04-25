type t
val resolved: binding_id:Binding_id.t -> surface_path:Surface_path.t -> t

val from_binding_id: Binding_id.t -> t

val binding_id: t -> Binding_id.t

val surface_path: t -> Surface_path.t

val equal: t -> t -> bool

val compare: t -> t -> Std.Order.t

val serializer: t Serde.Ser.t
