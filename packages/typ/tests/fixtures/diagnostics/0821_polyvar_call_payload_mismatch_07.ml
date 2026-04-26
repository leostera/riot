let read_eta : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_eta (`A true)
