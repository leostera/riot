let ( + ) (x : int) (y : int) : int = x

type _ witness_zeta =
  | WInt_zeta : int witness_zeta
  | WBool_zeta : bool witness_zeta

let step_zeta : type a. a witness_zeta -> a -> int =
  fun w x ->
    match w with
    | WInt_zeta -> x + 5
    | WBool_zeta -> x + 6
