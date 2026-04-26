let read_gamma : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_gamma (`A true)
