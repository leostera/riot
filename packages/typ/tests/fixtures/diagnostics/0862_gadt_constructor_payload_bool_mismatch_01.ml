type _ expr_alpha =
  | EInt_alpha : int -> int expr_alpha
  | EBool_alpha : bool -> bool expr_alpha

let _ : bool expr_alpha = EBool_alpha 0
