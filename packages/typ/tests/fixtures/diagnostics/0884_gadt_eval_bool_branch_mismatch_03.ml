type _ expr_gamma =
  | Int_gamma : int -> int expr_gamma
  | Bool_gamma : bool -> bool expr_gamma

let eval_gamma : type a. a expr_gamma -> a = function
  | Int_gamma n -> n
  | Bool_gamma b -> 2
