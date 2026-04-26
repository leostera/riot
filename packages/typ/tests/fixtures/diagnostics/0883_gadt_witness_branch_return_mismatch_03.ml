type _ witness_gamma =
  | WInt_gamma : int witness_gamma
  | WBool_gamma : bool witness_gamma

let bad_gamma : type a. a witness_gamma -> a = function
  | WInt_gamma -> true
  | WBool_gamma -> false
