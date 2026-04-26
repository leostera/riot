type _ witness_alpha =
  | WInt_alpha : int witness_alpha
  | WBool_alpha : bool witness_alpha

let bad_alpha : type a. a witness_alpha -> a = function
  | WInt_alpha -> true
  | WBool_alpha -> false
