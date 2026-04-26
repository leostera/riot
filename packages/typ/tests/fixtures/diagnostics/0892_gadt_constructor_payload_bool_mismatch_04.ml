type _ expr_delta =
  | EInt_delta : int -> int expr_delta
  | EBool_delta : bool -> bool expr_delta

let _ : bool expr_delta = EBool_delta 3
