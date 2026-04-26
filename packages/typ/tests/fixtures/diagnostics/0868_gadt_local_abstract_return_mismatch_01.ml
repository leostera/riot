type _ witness_alpha =
  | WInt_alpha : int witness_alpha
  | WBool_alpha : bool witness_alpha

let bad_alpha : type a. a witness_alpha -> a -> int =
  fun w x ->
    match w with
    | WInt_alpha -> x
    | WBool_alpha -> x
