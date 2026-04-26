type _ witness_beta =
  | WInt_beta : int witness_beta
  | WBool_beta : bool witness_beta

let bad_beta : type a. a witness_beta -> a = function
  | WInt_beta -> true
  | WBool_beta -> false
