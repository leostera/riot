let read_delta : [ `A | `B ] -> int = function
  | `A -> 3
  | `B -> 4

let _ = read_delta `C
