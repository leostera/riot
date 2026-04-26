type _ expr_iota =
  | Int_iota : int -> int expr_iota
  | Bool_iota : bool -> bool expr_iota

let eval_iota : type a. a expr_iota -> a = function
  | Int_iota n -> n
  | Bool_iota b -> 8
