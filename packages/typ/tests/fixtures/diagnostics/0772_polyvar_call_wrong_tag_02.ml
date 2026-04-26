let read_beta : [ `A | `B ] -> int = function
  | `A -> 1
  | `B -> 2

let _ = read_beta `C
