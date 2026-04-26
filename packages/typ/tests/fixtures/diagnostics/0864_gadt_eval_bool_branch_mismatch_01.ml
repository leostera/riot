type _ expr_alpha =
  | Int_alpha : int -> int expr_alpha
  | Bool_alpha : bool -> bool expr_alpha

let eval_alpha : type a. a expr_alpha -> a = function
  | Int_alpha n -> n
  | Bool_alpha b -> 0
