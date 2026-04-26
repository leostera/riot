type _ witness_delta =
  | WInt_delta : int witness_delta
  | WBool_delta : bool witness_delta

let bad_delta : type a. a witness_delta -> a -> int =
  fun w x ->
    match w with
    | WInt_delta -> x
    | WBool_delta -> x
