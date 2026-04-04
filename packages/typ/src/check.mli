open Std

(** Backwards-compatible one-shot entrypoint over [Batch.check_source].

    New library consumers should prefer [Session], [Snapshot], and [Query]. *)
val check_source: filename:Path.t -> string -> Check_result.t
