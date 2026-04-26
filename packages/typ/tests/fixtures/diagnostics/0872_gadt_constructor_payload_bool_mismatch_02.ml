type _ expr_beta =
  | EInt_beta : int -> int expr_beta
  | EBool_beta : bool -> bool expr_beta

let _ : bool expr_beta = EBool_beta 1
