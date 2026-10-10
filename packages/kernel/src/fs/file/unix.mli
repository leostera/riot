type t
type error =
  | InvalidSlice of { pos: int; len: int; buffer_len: int }
  | System of System_error.t
val error_to_string: error -> string

type kind =
  | RegularFile
  | Directory
  | SymbolicLink
  | CharacterDevice
  | BlockDevice
  | NamedPipe
  | Socket
  | Unknown
module Metadata: sig
  type t
  val file_type: t -> kind

  val is_file: t -> bool

  val is_dir: t -> bool

  val is_symlink: t -> bool

  val permissions: t -> int

  val mode: t -> int

  val len: t -> int64

  val nlink: t -> int

  val uid: t -> int

  val gid: t -> int

  val dev: t -> int

  val ino: t -> int

  val rdev: t -> int

  val accessed_ns: t -> int64

  val modified_ns: t -> int64

  val changed_ns: t -> int64
end

type open_flag =
  | ReadOnly
  | WriteOnly
  | ReadWrite
  | Create
  | Truncate
  | Append
  | Exclusive
type pipe = {
  read_end: t;
  write_end: t;
}
val open_file: Path.t -> flags:open_flag list -> permissions:int -> (t, error) Result.t

val open_read: Path.t -> (t, error) Result.t

val open_write:
  ?create:bool -> ?truncate:bool -> ?append:bool -> ?perm:int -> Path.t -> (t, error) Result.t

val close: t -> (unit, error) Result.t

val try_lock_exclusive: t -> (bool, error) Result.t

val try_lock_shared: t -> (bool, error) Result.t
(** Nonblocking advisory shared lock. Multiple shared holders may coexist;
    returns [Ok false] if an exclusive holder prevents acquisition. *)

val unlock: t -> (unit, error) Result.t

val read: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val write: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val read_vectored: t -> Io.Iovec.t -> (int, error) Result.t

val write_vectored: t -> Io.Iovec.t -> (int, error) Result.t

val pipe: unit -> (pipe, error) Result.t

val create_dir: Path.t -> perm:int -> (unit, error) Result.t

val set_permissions: Path.t -> perm:int -> (unit, error) Result.t

val remove_dir: Path.t -> (unit, error) Result.t

val remove_file: Path.t -> (unit, error) Result.t

val rename: src:Path.t -> dst:Path.t -> (unit, error) Result.t

val hard_link: src:Path.t -> dst:Path.t -> (unit, error) Result.t

val symlink: src:Path.t -> dst:Path.t -> (unit, error) Result.t

val read_link: Path.t -> (Path.t, error) Result.t

val canonicalize: Path.t -> (Path.t, error) Result.t

val metadata: Path.t -> (Metadata.t, error) Result.t

val lstat: Path.t -> (Metadata.t, error) Result.t

val symlink_metadata: Path.t -> (Metadata.t, error) Result.t

val fstat: t -> (Metadata.t, error) Result.t

val exists: Path.t -> (bool, error) Result.t

val is_directory: Path.t -> (bool, error) Result.t

val read_dir_names: Path.t -> (string array, error) Result.t

val copy: src:Path.t -> dst:Path.t -> (unit, error) Result.t

val is_tty: t -> bool

val to_source: t -> Async.Source.t
