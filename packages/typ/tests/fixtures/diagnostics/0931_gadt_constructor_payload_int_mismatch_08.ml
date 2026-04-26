type _ expr_theta =
  | EInt_theta : int -> int expr_theta
  | EBool_theta : bool -> bool expr_theta

let _ : int expr_theta = EInt_theta true
