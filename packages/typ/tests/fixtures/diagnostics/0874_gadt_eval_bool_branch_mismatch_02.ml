type _ expr_beta =
  | Int_beta : int -> int expr_beta
  | Bool_beta : bool -> bool expr_beta

let eval_beta : type a. a expr_beta -> a = function
  | Int_beta n -> n
  | Bool_beta b -> 1
