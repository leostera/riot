let ( + ) (x : int) (y : int) : int = x

type _ witness_alpha =
  | WInt_alpha : int witness_alpha
  | WBool_alpha : bool witness_alpha

let step_alpha : type a. a witness_alpha -> a -> int =
  fun w x ->
    match w with
    | WInt_alpha -> x + 0
    | WBool_alpha -> x + 1
