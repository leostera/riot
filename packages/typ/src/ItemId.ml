open Std

type t = int

let compare = Int.compare

let equal = Int.equal

let of_int = fun value -> value

let to_int = fun value -> value

let to_string = fun item_id -> "item#" ^ Int.to_string item_id
