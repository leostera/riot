module IoSlice: sig
  type t
  type error = Error.t
  val empty: t

  val create: size:int -> (t, error) Result.t

  val length: t -> int

  val sub: t -> off:int -> len:int -> (t, error) Result.t

  val sub_unchecked: t -> off:int -> len:int -> t

  val shift: t -> int -> (t, error) Result.t

  val shift_unchecked: t -> int -> t

  val split_at: t -> int -> ((t * t), error) Result.t

  val split_at_unchecked: t -> int -> t * t

  val get: t -> at:int -> (char, error) Result.t

  val get_unchecked: t -> at:int -> char

  val set: t -> at:int -> char -> (unit, error) Result.t

  val set_unchecked: t -> at:int -> char -> unit

  val blit: src:t -> src_off:int -> dst:t -> dst_off:int -> len:int -> (unit, error) Result.t

  val blit_unchecked: src:t -> src_off:int -> dst:t -> dst_off:int -> len:int -> unit

  val blit_from_bytes: bytes -> src_off:int -> t -> dst_off:int -> len:int -> (unit, error) Result.t

  val blit_from_bytes_unchecked: bytes -> src_off:int -> t -> dst_off:int -> len:int -> unit

  val blit_from_string:
    string -> src_off:int -> t -> dst_off:int -> len:int -> (unit, error) Result.t

  val blit_from_string_unchecked: string -> src_off:int -> t -> dst_off:int -> len:int -> unit

  val blit_to_bytes: t -> src_off:int -> bytes -> dst_off:int -> len:int -> (unit, error) Result.t

  val blit_to_bytes_unchecked: t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit

  val from_string: ?off:int -> ?len:int -> string -> (t, error) Result.t

  val from_bytes: ?off:int -> ?len:int -> bytes -> (t, error) Result.t

  val starts_with: t -> prefix:string -> bool

  val equal_string: t -> string -> bool

  val index_char: t -> char -> int option

  val index_string: t -> string -> int option

  val to_string: t -> string

  val to_bytes: t -> bytes
end

(** Scatter/gather byte slices for narrow kernel I/O paths. *)
type segment = IoSlice.t
type t
type error = Error.t
val empty: t

val create: ?count:int -> size:int -> unit -> (t, error) Result.t

val with_capacity: int -> (t, error) Result.t

val from_slices: segment array -> t

val from_bytes: bytes -> (t, error) Result.t

val from_string: string -> (t, error) Result.t

val from_bytes_array: bytes array -> (t, error) Result.t

val from_string_array: string array -> (t, error) Result.t

val length: t -> int

val for_each: fn:(segment -> unit) -> t -> unit

val sub: ?pos:int -> len:int -> t -> (t, error) Result.t

val to_bytes: t -> bytes

val to_string: t -> string
