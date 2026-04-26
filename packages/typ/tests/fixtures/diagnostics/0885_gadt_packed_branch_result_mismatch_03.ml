type _ expr_gamma =
  | Int_gamma : int -> int expr_gamma
  | Bool_gamma : bool -> bool expr_gamma

type packed_gamma = Pack_gamma : 'a expr_gamma -> packed_gamma

let bad_gamma = function
  | Pack_gamma (Int_gamma n) -> n
  | Pack_gamma (Bool_gamma b) -> b
