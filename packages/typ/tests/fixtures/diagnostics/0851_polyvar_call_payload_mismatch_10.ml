let read_kappa : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_kappa (`A true)
