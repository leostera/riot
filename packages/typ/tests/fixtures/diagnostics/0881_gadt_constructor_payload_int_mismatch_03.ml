type _ expr_gamma =
  | EInt_gamma : int -> int expr_gamma
  | EBool_gamma : bool -> bool expr_gamma

let _ : int expr_gamma = EInt_gamma true
