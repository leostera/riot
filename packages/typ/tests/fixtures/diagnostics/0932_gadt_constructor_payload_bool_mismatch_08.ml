type _ expr_theta =
  | EInt_theta : int -> int expr_theta
  | EBool_theta : bool -> bool expr_theta

let _ : bool expr_theta = EBool_theta 7
