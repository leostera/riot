type t =
  | Native_error of int
  | Io_error of string
  | Random_error of string
  | Test_error of string
  | Runtime_error of string

val message: t -> string
