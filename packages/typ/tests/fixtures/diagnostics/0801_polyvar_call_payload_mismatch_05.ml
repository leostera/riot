let read_epsilon : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_epsilon (`A true)
