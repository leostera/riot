type segment = {
  buffer: bytes;
  offset: int;
  length: int;
}
type t
val create: ?count:int -> size:int -> unit -> t

val with_capacity: int -> t

val from_bytes: bytes -> t

val from_string: string -> t

val from_bytes_array: bytes array -> t

val from_string_array: string array -> t

val length: t -> int

val for_each: fn:(segment -> unit) -> t -> unit

val sub: ?pos:int -> len:int -> t -> t

val to_bytes: t -> bytes

val to_string: t -> string
