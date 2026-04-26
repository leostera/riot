type _ witness_beta =
  | WInt_beta : int witness_beta
  | WBool_beta : bool witness_beta

let bad_beta : type a. a witness_beta -> a -> int =
  fun w x ->
    match w with
    | WInt_beta -> x
    | WBool_beta -> x
