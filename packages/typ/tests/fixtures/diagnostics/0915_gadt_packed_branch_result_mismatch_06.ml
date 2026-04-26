type _ expr_zeta =
  | Int_zeta : int -> int expr_zeta
  | Bool_zeta : bool -> bool expr_zeta

type packed_zeta = Pack_zeta : 'a expr_zeta -> packed_zeta

let bad_zeta = function
  | Pack_zeta (Int_zeta n) -> n
  | Pack_zeta (Bool_zeta b) -> b
