type _ expr_epsilon =
  | Int_epsilon : int -> int expr_epsilon
  | Bool_epsilon : bool -> bool expr_epsilon

let eval_epsilon : type a. a expr_epsilon -> a = function
  | Int_epsilon n -> n
  | Bool_epsilon b -> 4
