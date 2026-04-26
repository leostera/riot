type _ witness_theta =
  | WInt_theta : int witness_theta
  | WBool_theta : bool witness_theta

let bad_theta : type a. a witness_theta -> a = function
  | WInt_theta -> true
  | WBool_theta -> false
