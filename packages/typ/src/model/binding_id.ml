open Std

type t = {
  stamp: int;
  name: Surface_path.t;
}

let local = fun ~stamp ~name -> { stamp; name }

let name = fun value -> value.name

let stamp = fun value -> value.stamp

let equal = fun left right ->
  Int.equal left.stamp right.stamp && Surface_path.equal left.name right.name

let compare = fun left right ->
  match Int.compare left.stamp right.stamp with
  | Order.EQ -> Surface_path.compare left.name right.name
  | order -> order

let to_string = fun value -> Surface_path.to_string value.name ^ "#" ^ Int.to_string value.stamp

let serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "stamp" Serde.Ser.int (fun (value: t) -> value.stamp);
          Serde.Ser.field "name" Surface_path.serializer (fun (value: t) -> value.name);
        ]
    )
