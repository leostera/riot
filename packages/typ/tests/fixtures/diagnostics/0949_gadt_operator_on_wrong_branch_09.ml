let ( + ) (x : int) (y : int) : int = x

type _ witness_iota =
  | WInt_iota : int witness_iota
  | WBool_iota : bool witness_iota

let step_iota : type a. a witness_iota -> a -> int =
  fun w x ->
    match w with
    | WInt_iota -> x + 8
    | WBool_iota -> x + 9
