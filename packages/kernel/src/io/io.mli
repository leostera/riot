module Iovec: sig
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
end

module Stdin: sig
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t
  val error_to_string: error -> string

  val read: ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

  val read_vectored: Iovec.t -> (int, error) Result.t

  val to_source: unit -> Async.Source.t
end

module Stdout: sig
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t
  val error_to_string: error -> string

  val write: ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

  val write_vectored: Iovec.t -> (int, error) Result.t

  val flush: unit -> (unit, error) Result.t

  val to_source: unit -> Async.Source.t
end

module Stderr: sig
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t
  val error_to_string: error -> string

  val write: ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

  val write_vectored: Iovec.t -> (int, error) Result.t

  val flush: unit -> (unit, error) Result.t

  val to_source: unit -> Async.Source.t
end
