let ( + ) (x : int) (y : int) : int = x

type _ witness_beta =
  | WInt_beta : int witness_beta
  | WBool_beta : bool witness_beta

let step_beta : type a. a witness_beta -> a -> int =
  fun w x ->
    match w with
    | WInt_beta -> x + 1
    | WBool_beta -> x + 2
