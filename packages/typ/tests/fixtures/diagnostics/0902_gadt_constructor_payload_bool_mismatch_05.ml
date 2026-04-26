type _ expr_epsilon =
  | EInt_epsilon : int -> int expr_epsilon
  | EBool_epsilon : bool -> bool expr_epsilon

let _ : bool expr_epsilon = EBool_epsilon 4
