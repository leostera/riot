type _ witness_zeta =
  | WInt_zeta : int witness_zeta
  | WBool_zeta : bool witness_zeta

let bad_zeta : type a. a witness_zeta -> a -> int =
  fun w x ->
    match w with
    | WInt_zeta -> x
    | WBool_zeta -> x
