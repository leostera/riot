let ( + ) (x : int) (y : int) : int = x

type _ witness_gamma =
  | WInt_gamma : int witness_gamma
  | WBool_gamma : bool witness_gamma

let step_gamma : type a. a witness_gamma -> a -> int =
  fun w x ->
    match w with
    | WInt_gamma -> x + 2
    | WBool_gamma -> x + 3
