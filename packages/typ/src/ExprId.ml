open Std

type t = int

let compare = Int.compare

let equal = Int.equal

let of_int = fun value -> value

let to_int = fun value -> value

let to_string = fun expr_id -> "expr#" ^ Int.to_string expr_id
