type _ expr_eta =
  | Int_eta : int -> int expr_eta
  | Bool_eta : bool -> bool expr_eta

type packed_eta = Pack_eta : 'a expr_eta -> packed_eta

let bad_eta = function
  | Pack_eta (Int_eta n) -> n
  | Pack_eta (Bool_eta b) -> b
