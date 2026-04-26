type _ expr_zeta =
  | EInt_zeta : int -> int expr_zeta
  | EBool_zeta : bool -> bool expr_zeta

let _ : int expr_zeta = EInt_zeta true
