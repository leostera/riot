let read_delta : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_delta (`A true)
