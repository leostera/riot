type _ witness_epsilon =
  | WInt_epsilon : int witness_epsilon
  | WBool_epsilon : bool witness_epsilon

let bad_epsilon : type a. a witness_epsilon -> a = function
  | WInt_epsilon -> true
  | WBool_epsilon -> false
