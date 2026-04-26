let read_iota : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_iota (`A true)
