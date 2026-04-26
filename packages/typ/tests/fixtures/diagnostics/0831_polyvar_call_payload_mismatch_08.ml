let read_theta : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_theta (`A true)
