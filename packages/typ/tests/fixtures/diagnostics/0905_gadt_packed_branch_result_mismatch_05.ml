type _ expr_epsilon =
  | Int_epsilon : int -> int expr_epsilon
  | Bool_epsilon : bool -> bool expr_epsilon

type packed_epsilon = Pack_epsilon : 'a expr_epsilon -> packed_epsilon

let bad_epsilon = function
  | Pack_epsilon (Int_epsilon n) -> n
  | Pack_epsilon (Bool_epsilon b) -> b
