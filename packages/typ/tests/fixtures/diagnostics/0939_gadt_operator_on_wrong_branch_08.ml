let ( + ) (x : int) (y : int) : int = x

type _ witness_theta =
  | WInt_theta : int witness_theta
  | WBool_theta : bool witness_theta

let step_theta : type a. a witness_theta -> a -> int =
  fun w x ->
    match w with
    | WInt_theta -> x + 7
    | WBool_theta -> x + 8
