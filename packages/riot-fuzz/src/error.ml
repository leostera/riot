open Std

type t =
  | Native_error of int
  | Io_error of string
  | Random_error of string
  | Test_error of string
  | Runtime_error of string

let message = fun __tmp1 ->
  match __tmp1 with
  | Native_error code -> "native fuzzing error errno=" ^ Int.to_string code
  | Io_error message -> message
  | Random_error message -> message
  | Test_error message -> message
  | Runtime_error message -> message
