open Std

(** gRPC Status Codes

    Standard status codes used by gRPC as defined in:
    https://grpc.github.io/grpc/core/md_doc_statuscodes.html

    These map to specific HTTP/2 headers:
    - grpc-status: integer status code
    - grpc-message: human-readable error message
*)

(** Status code enumeration *)
type t =
  | OK  (** 0: Success *)
  | Cancelled  (** 1: Operation cancelled (typically by caller) *)
  | Unknown  (** 2: Unknown error *)
  | InvalidArgument  (** 3: Client specified invalid argument *)
  | DeadlineExceeded  (** 4: Deadline expired before operation completed *)
  | NotFound  (** 5: Requested entity not found *)
  | AlreadyExists  (** 6: Entity already exists *)
  | PermissionDenied  (** 7: Caller doesn't have permission *)
  | ResourceExhausted  (** 8: Resource exhausted (quota, out of space, etc) *)
  | FailedPrecondition  (** 9: Operation rejected (system not in required state) *)
  | Aborted  (** 10: Operation aborted (concurrency conflict, transaction abort) *)
  | OutOfRange  (** 11: Operation attempted past valid range *)
  | Unimplemented  (** 12: Operation not implemented/supported *)
  | Internal  (** 13: Internal error (server-side) *)
  | Unavailable  (** 14: Service unavailable (temporary, retriable) *)
  | DataLoss  (** 15: Unrecoverable data loss or corruption *)
  | Unauthenticated  (** 16: Request lacks valid authentication *)

(** Convert status code to integer *)
val to_int : t -> int

(** Convert integer to status code *)
val of_int : int -> t option

(** Convert status code to string name *)
val to_string : t -> string

(** Convert status code to HTTP/2 status for headers *)
val to_http_status : t -> int

(** Check if status indicates success *)
val is_ok : t -> bool

(** Check if status indicates a retriable error *)
val is_retriable : t -> bool

(** Pretty printer *)
val pp : Format.formatter -> t -> unit
