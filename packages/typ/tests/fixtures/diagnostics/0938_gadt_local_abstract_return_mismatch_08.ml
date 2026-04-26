type _ witness_theta =
  | WInt_theta : int witness_theta
  | WBool_theta : bool witness_theta

let bad_theta : type a. a witness_theta -> a -> int =
  fun w x ->
    match w with
    | WInt_theta -> x
    | WBool_theta -> x
