let read_alpha : [ `A | `B ] -> int = function
  | `A -> 0
  | `B -> 1

let _ = read_alpha `C
