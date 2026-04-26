let read_epsilon : [ `A | `B ] -> int = function
  | `A -> 4
  | `B -> 5

let _ = read_epsilon `C
