let read_zeta : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_zeta (`A true)
