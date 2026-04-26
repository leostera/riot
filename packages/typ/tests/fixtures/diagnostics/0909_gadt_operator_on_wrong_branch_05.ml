let ( + ) (x : int) (y : int) : int = x

type _ witness_epsilon =
  | WInt_epsilon : int witness_epsilon
  | WBool_epsilon : bool witness_epsilon

let step_epsilon : type a. a witness_epsilon -> a -> int =
  fun w x ->
    match w with
    | WInt_epsilon -> x + 4
    | WBool_epsilon -> x + 5
