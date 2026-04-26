type _ expr_zeta =
  | Int_zeta : int -> int expr_zeta
  | Bool_zeta : bool -> bool expr_zeta

let eval_zeta : type a. a expr_zeta -> a = function
  | Int_zeta n -> n
  | Bool_zeta b -> 5
