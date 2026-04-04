open Std

type t = int

let compare = Int.compare

let equal = Int.equal

let of_int = fun value -> value

let to_int = fun value -> value

let to_string = fun source_id -> "source#" ^ Int.to_string source_id
