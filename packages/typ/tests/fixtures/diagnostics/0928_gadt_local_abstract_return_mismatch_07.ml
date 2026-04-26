type _ witness_eta =
  | WInt_eta : int witness_eta
  | WBool_eta : bool witness_eta

let bad_eta : type a. a witness_eta -> a -> int =
  fun w x ->
    match w with
    | WInt_eta -> x
    | WBool_eta -> x
