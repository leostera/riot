type _ expr_iota =
  | EInt_iota : int -> int expr_iota
  | EBool_iota : bool -> bool expr_iota

let _ : int expr_iota = EInt_iota true
