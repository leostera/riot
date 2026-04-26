type _ expr_iota =
  | Int_iota : int -> int expr_iota
  | Bool_iota : bool -> bool expr_iota

type packed_iota = Pack_iota : 'a expr_iota -> packed_iota

let bad_iota = function
  | Pack_iota (Int_iota n) -> n
  | Pack_iota (Bool_iota b) -> b
