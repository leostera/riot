let ( + ) (x : int) (y : int) : int = x

type _ witness_delta =
  | WInt_delta : int witness_delta
  | WBool_delta : bool witness_delta

let step_delta : type a. a witness_delta -> a -> int =
  fun w x ->
    match w with
    | WInt_delta -> x + 3
    | WBool_delta -> x + 4
