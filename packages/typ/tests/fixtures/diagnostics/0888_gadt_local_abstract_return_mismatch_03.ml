type _ witness_gamma =
  | WInt_gamma : int witness_gamma
  | WBool_gamma : bool witness_gamma

let bad_gamma : type a. a witness_gamma -> a -> int =
  fun w x ->
    match w with
    | WInt_gamma -> x
    | WBool_gamma -> x
