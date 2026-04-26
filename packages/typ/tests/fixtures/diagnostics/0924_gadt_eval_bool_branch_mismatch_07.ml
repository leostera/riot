type _ expr_eta =
  | Int_eta : int -> int expr_eta
  | Bool_eta : bool -> bool expr_eta

let eval_eta : type a. a expr_eta -> a = function
  | Int_eta n -> n
  | Bool_eta b -> 6
