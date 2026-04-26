type _ expr_alpha =
  | Int_alpha : int -> int expr_alpha
  | Bool_alpha : bool -> bool expr_alpha

type packed_alpha = Pack_alpha : 'a expr_alpha -> packed_alpha

let bad_alpha = function
  | Pack_alpha (Int_alpha n) -> n
  | Pack_alpha (Bool_alpha b) -> b
