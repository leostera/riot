open Std

(** Parse, lower, and infer one source file through the current prototype lane. *)
val check_source: filename:Path.t -> string -> Check_result.t
