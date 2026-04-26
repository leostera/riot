type _ expr_theta =
  | Int_theta : int -> int expr_theta
  | Bool_theta : bool -> bool expr_theta

type packed_theta = Pack_theta : 'a expr_theta -> packed_theta

let bad_theta = function
  | Pack_theta (Int_theta n) -> n
  | Pack_theta (Bool_theta b) -> b
