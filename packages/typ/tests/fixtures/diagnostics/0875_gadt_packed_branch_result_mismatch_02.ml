type _ expr_beta =
  | Int_beta : int -> int expr_beta
  | Bool_beta : bool -> bool expr_beta

type packed_beta = Pack_beta : 'a expr_beta -> packed_beta

let bad_beta = function
  | Pack_beta (Int_beta n) -> n
  | Pack_beta (Bool_beta b) -> b
