let read_iota : [ `A | `B ] -> int = function
  | `A -> 8
  | `B -> 9

let _ = read_iota `C
