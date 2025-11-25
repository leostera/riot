open Std

type t =
  | OK
  | Cancelled
  | Unknown
  | InvalidArgument
  | DeadlineExceeded
  | NotFound
  | AlreadyExists
  | PermissionDenied
  | ResourceExhausted
  | FailedPrecondition
  | Aborted
  | OutOfRange
  | Unimplemented
  | Internal
  | Unavailable
  | DataLoss
  | Unauthenticated

let to_int = function
  | OK -> 0
  | Cancelled -> 1
  | Unknown -> 2
  | InvalidArgument -> 3
  | DeadlineExceeded -> 4
  | NotFound -> 5
  | AlreadyExists -> 6
  | PermissionDenied -> 7
  | ResourceExhausted -> 8
  | FailedPrecondition -> 9
  | Aborted -> 10
  | OutOfRange -> 11
  | Unimplemented -> 12
  | Internal -> 13
  | Unavailable -> 14
  | DataLoss -> 15
  | Unauthenticated -> 16

let of_int = function
  | 0 -> Some OK
  | 1 -> Some Cancelled
  | 2 -> Some Unknown
  | 3 -> Some InvalidArgument
  | 4 -> Some DeadlineExceeded
  | 5 -> Some NotFound
  | 6 -> Some AlreadyExists
  | 7 -> Some PermissionDenied
  | 8 -> Some ResourceExhausted
  | 9 -> Some FailedPrecondition
  | 10 -> Some Aborted
  | 11 -> Some OutOfRange
  | 12 -> Some Unimplemented
  | 13 -> Some Internal
  | 14 -> Some Unavailable
  | 15 -> Some DataLoss
  | 16 -> Some Unauthenticated
  | _ -> None

let to_string = function
  | OK -> "OK"
  | Cancelled -> "CANCELLED"
  | Unknown -> "UNKNOWN"
  | InvalidArgument -> "INVALID_ARGUMENT"
  | DeadlineExceeded -> "DEADLINE_EXCEEDED"
  | NotFound -> "NOT_FOUND"
  | AlreadyExists -> "ALREADY_EXISTS"
  | PermissionDenied -> "PERMISSION_DENIED"
  | ResourceExhausted -> "RESOURCE_EXHAUSTED"
  | FailedPrecondition -> "FAILED_PRECONDITION"
  | Aborted -> "ABORTED"
  | OutOfRange -> "OUT_OF_RANGE"
  | Unimplemented -> "UNIMPLEMENTED"
  | Internal -> "INTERNAL"
  | Unavailable -> "UNAVAILABLE"
  | DataLoss -> "DATA_LOSS"
  | Unauthenticated -> "UNAUTHENTICATED"

let to_http_status = function
  | OK -> 200
  | Cancelled -> 499  (* Client closed request *)
  | Unknown -> 500
  | InvalidArgument -> 400
  | DeadlineExceeded -> 504  (* Gateway timeout *)
  | NotFound -> 404
  | AlreadyExists -> 409  (* Conflict *)
  | PermissionDenied -> 403
  | ResourceExhausted -> 429  (* Too many requests *)
  | FailedPrecondition -> 400
  | Aborted -> 409
  | OutOfRange -> 400
  | Unimplemented -> 501
  | Internal -> 500
  | Unavailable -> 503
  | DataLoss -> 500
  | Unauthenticated -> 401

let is_ok = function OK -> true | _ -> false

let is_retriable = function
  | Unavailable | DeadlineExceeded | ResourceExhausted -> true
  | _ -> false

let pp fmt t = Format.fprintf fmt "%s" (to_string t)
