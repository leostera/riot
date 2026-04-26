let read_alpha : [ `A of int ] -> int = function
  | `A x -> x

let _ = read_alpha (`A true)
