type _ expr_kappa =
  | EInt_kappa : int -> int expr_kappa
  | EBool_kappa : bool -> bool expr_kappa

let _ : bool expr_kappa = EBool_kappa 9
