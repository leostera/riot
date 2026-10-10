open Global

(** `Std.Fs.File` layers runtime-aware read/write behavior over
    `Kernel.Fs.File`. It never exposes raw descriptors. *)
type t = Kernel.Fs.File.t
type error = Kernel.Fs.File.error
val error_to_string: error -> string

module OpenFlags: sig
  type t = Kernel.Fs.File.open_flag =
    | ReadOnly
    | WriteOnly
    | ReadWrite
    | Create
    | Truncate
    | Append
    | Exclusive
end

val open_with_flags: Path.t -> OpenFlags.t list -> mode:Permissions.t -> (t, error) result

val create: Path.t -> (t, error) result

val create_new: Path.t -> (t, error) result

val open_read: Path.t -> (t, error) result

val open_write: Path.t -> (t, error) result

val open_append: Path.t -> (t, error) result

val open_read_write: Path.t -> (t, error) result

val try_lock_exclusive: t -> (bool, error) result

val try_lock_shared: t -> (bool, error) result
(** Nonblocking advisory shared lock; compatible with other shared holders,
    incompatible with an exclusive holder. *)

val unlock: t -> (unit, error) result

val read: t -> bytes -> offset:int -> len:int -> (int, error) result

val read_to_end: t -> (string, error) result

val read_exact: t -> bytes -> offset:int -> len:int -> (unit, error) result

val read_line: t -> (string, error) result

val write: t -> bytes -> offset:int -> len:int -> (int, error) result

val write_all: t -> string -> (unit, error) result

val write_string: t -> string -> (int, error) result

val metadata: t -> (Metadata.t, error) result

val to_source: t -> Kernel.Async.Source.t

val to_reader: t -> (t, error) IO.Reader.t

val to_writer: t -> (t, error) IO.Writer.t

val close: t -> (unit, error) result
