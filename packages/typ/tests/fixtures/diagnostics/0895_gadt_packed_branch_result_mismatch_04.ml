type _ expr_delta =
  | Int_delta : int -> int expr_delta
  | Bool_delta : bool -> bool expr_delta

type packed_delta = Pack_delta : 'a expr_delta -> packed_delta

let bad_delta = function
  | Pack_delta (Int_delta n) -> n
  | Pack_delta (Bool_delta b) -> b
