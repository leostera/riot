let read_beta : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_beta (`A true)
