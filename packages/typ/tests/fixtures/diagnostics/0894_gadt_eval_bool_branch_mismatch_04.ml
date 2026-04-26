type _ expr_delta =
  | Int_delta : int -> int expr_delta
  | Bool_delta : bool -> bool expr_delta

let eval_delta : type a. a expr_delta -> a = function
  | Int_delta n -> n
  | Bool_delta b -> 3
