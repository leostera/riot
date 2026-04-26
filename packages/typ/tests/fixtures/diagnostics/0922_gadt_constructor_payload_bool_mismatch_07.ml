type _ expr_eta =
  | EInt_eta : int -> int expr_eta
  | EBool_eta : bool -> bool expr_eta

let _ : bool expr_eta = EBool_eta 6
