type _ expr_kappa =
  | Int_kappa : int -> int expr_kappa
  | Bool_kappa : bool -> bool expr_kappa

type packed_kappa = Pack_kappa : 'a expr_kappa -> packed_kappa

let bad_kappa = function
  | Pack_kappa (Int_kappa n) -> n
  | Pack_kappa (Bool_kappa b) -> b
