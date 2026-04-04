open Std

(** One-shot batch wrapper built on top of [Session] and [Snapshot].

    This is the compatibility lane for compiler-style callers that want one
    analyzed result for one source string without managing a persistent
    session. *)
val check_source: filename:Path.t -> string -> Check_result.t
