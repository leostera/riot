open Std

type t = {
  binding_id: Binding_id.t;
  surface_path: Surface_path.t;
}

let resolved = fun ~binding_id ~surface_path -> { binding_id; surface_path }

let from_binding_id = fun binding_id -> { binding_id; surface_path = Binding_id.name binding_id }

let binding_id = fun value -> value.binding_id

let surface_path = fun value -> value.surface_path

let equal = fun left right ->
  Binding_id.equal left.binding_id right.binding_id
  && Surface_path.equal left.surface_path right.surface_path

let compare = fun left right ->
  match Binding_id.compare left.binding_id right.binding_id with
  | Order.EQ -> Surface_path.compare left.surface_path right.surface_path
  | order -> order

let serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "binding_id" Binding_id.serializer (fun (value: t) -> value.binding_id);
          Serde.Ser.field
            "surface_path"
            Surface_path.serializer
            (fun (value: t) -> value.surface_path);
        ]
    )
