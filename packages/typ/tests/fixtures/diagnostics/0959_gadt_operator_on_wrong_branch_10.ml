let ( + ) (x : int) (y : int) : int = x

type _ witness_kappa =
  | WInt_kappa : int witness_kappa
  | WBool_kappa : bool witness_kappa

let step_kappa : type a. a witness_kappa -> a -> int =
  fun w x ->
    match w with
    | WInt_kappa -> x + 9
    | WBool_kappa -> x + 10
