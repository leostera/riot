open Std
open Std.Collections
open Ast
open Model

type t = {
  value_order: Surface_path.t Vector.t;
  values: (Surface_path.t, Type.t) HashMap.t;
}

let create () = {
  value_order = Vector.with_capacity ~size:16;
  values = HashMap.with_capacity ~size:16;
}

let add_value t ~name ~type_ =
  let previous = HashMap.insert t.values ~key:name ~value:type_ in
  if Option.is_none previous then
    Vector.push t.value_order ~value:name;
  previous

let has_value t ~name = HashMap.has_key t.values ~key:name

let get_value t ~name = HashMap.get t.values ~key:name

let values t =
  Vector.iter t.value_order
  |> Iter.Iterator.map
    ~fn:(fun name ->
      let type_ =
        HashMap.get t.values ~key:name
        |> Option.expect ~msg:"Env.value_order contains a name missing from Env.values"
      in
      (name, type_))
