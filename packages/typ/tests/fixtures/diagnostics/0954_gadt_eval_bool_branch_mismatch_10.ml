type _ expr_kappa =
  | Int_kappa : int -> int expr_kappa
  | Bool_kappa : bool -> bool expr_kappa

let eval_kappa : type a. a expr_kappa -> a = function
  | Int_kappa n -> n
  | Bool_kappa b -> 9
