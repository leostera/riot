open Std

type map
type forkserver
type status =
  | Exited of int
  | Signaled of int
  | Stopped of int
  | Timed_out of int

val map_size: unit -> int

val supported: unit -> bool

val status_to_string: status -> string

val create_map: unit -> (map, Error.t) result

val map_id: map -> int

val reset_map: map -> (unit, Error.t) result

val snapshot_map: map -> bytes

val close_map: map -> (unit, Error.t) result

val start_forkserver:
  program:string ->
  args:string list ->
  ?cwd:Path.t ->
  ?env:(string * string) list ->
  map ->
  (forkserver, Error.t) result

val finish_run: ?timeout_ms:int -> forkserver -> (status, Error.t) result

val start_next_run: forkserver -> (unit, Error.t) result

val stop_forkserver: forkserver -> (unit, Error.t) result
