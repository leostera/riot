type _ expr_zeta =
  | EInt_zeta : int -> int expr_zeta
  | EBool_zeta : bool -> bool expr_zeta

let _ : bool expr_zeta = EBool_zeta 5
