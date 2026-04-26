type _ expr_beta =
  | EInt_beta : int -> int expr_beta
  | EBool_beta : bool -> bool expr_beta

let _ : int expr_beta = EInt_beta true
