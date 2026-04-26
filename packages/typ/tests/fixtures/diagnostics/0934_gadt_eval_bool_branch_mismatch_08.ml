type _ expr_theta =
  | Int_theta : int -> int expr_theta
  | Bool_theta : bool -> bool expr_theta

let eval_theta : type a. a expr_theta -> a = function
  | Int_theta n -> n
  | Bool_theta b -> 7
