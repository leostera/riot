(** Cross-process build-root lifetime guard. Shared holders may build in
    parallel; cleanup requires an exclusive holder. The persistent lock file
    is a sibling of the build root and must not be unlinked on release. *)
type t

type error =
  | InvalidRoot of Std.Path.t
  | Busy of Std.Path.t
  | IoFailed of { path: Std.Path.t; cause: Std.Fs.error }
  | FileFailed of { path: Std.Path.t; cause: Std.Fs.File.error }

val error_message: error -> string

val path: target_dir_root:Std.Path.t -> (Std.Path.t, error) Std.Result.t

val acquire_shared: target_dir_root:Std.Path.t -> (t, error) Std.Result.t

val acquire_exclusive: target_dir_root:Std.Path.t -> (t, error) Std.Result.t

val release: t -> unit
(** Release exactly once. *)
