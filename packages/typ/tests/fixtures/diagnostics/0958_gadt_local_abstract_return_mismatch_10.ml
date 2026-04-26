type _ witness_kappa =
  | WInt_kappa : int witness_kappa
  | WBool_kappa : bool witness_kappa

let bad_kappa : type a. a witness_kappa -> a -> int =
  fun w x ->
    match w with
    | WInt_kappa -> x
    | WBool_kappa -> x
