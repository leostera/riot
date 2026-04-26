type _ expr_delta =
  | EInt_delta : int -> int expr_delta
  | EBool_delta : bool -> bool expr_delta

let _ : int expr_delta = EInt_delta true
