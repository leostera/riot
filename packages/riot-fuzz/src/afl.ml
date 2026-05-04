open Std

type map

type forkserver

type status =
  | Exited of int
  | Signaled of int
  | Stopped of int
  | Timed_out of int

module Native = struct
  external map_size: unit -> int = "riot_fuzz_afl_map_size"

  external supported: unit -> bool = "riot_fuzz_afl_supported"

  external create_map: unit -> (map, int) result = "riot_fuzz_afl_create_map"

  external map_id: map -> int = "riot_fuzz_afl_map_id"

  external reset_map: map -> (unit, int) result = "riot_fuzz_afl_reset_map"

  external snapshot_map: map -> bytes = "riot_fuzz_afl_snapshot_map"

  external close_map: map -> (unit, int) result = "riot_fuzz_afl_close_map"

  external start_forkserver:
    string ->
    string array ->
    (string * string) array ->
    string option ->
    map ->
    (forkserver, int) result =
    "riot_fuzz_afl_start_forkserver"

  external finish_run: forkserver -> int -> ((int * int), int) result = "riot_fuzz_afl_finish_run"

  external start_next_run: forkserver -> (unit, int) result = "riot_fuzz_afl_start_next_run"

  external stop_forkserver: forkserver -> (unit, int) result = "riot_fuzz_afl_stop_forkserver"
end

let map_size = Native.map_size

let supported = Native.supported

let native = Result.map_err ~fn:(fun code -> Error.Native_error code)

let create_map = fun () ->
  Native.create_map ()
  |> native

let map_id = Native.map_id

let reset_map = fun map ->
  Native.reset_map map
  |> native

let snapshot_map = Native.snapshot_map

let close_map = fun map ->
  Native.close_map map
  |> native

let status_of_raw = fun __tmp1 ->
  match __tmp1 with
  | (0, code) -> Exited code
  | (1, signal) -> Signaled signal
  | (2, signal) -> Stopped signal
  | (_, signal) -> Timed_out signal

let status_to_string = fun __tmp1 ->
  match __tmp1 with
  | Exited code -> "exited(" ^ Int.to_string code ^ ")"
  | Signaled signal -> "signaled(" ^ Int.to_string signal ^ ")"
  | Stopped signal -> "stopped(" ^ Int.to_string signal ^ ")"
  | Timed_out signal -> "timed-out(signal " ^ Int.to_string signal ^ ")"

let start_forkserver = fun ~program ~args ?cwd ?(env = []) map ->
  let cwd = Option.map cwd ~fn:Path.to_string in
  Native.start_forkserver program (Array.from_list args) (Array.from_list env) cwd map
  |> native

let finish_run = fun ?(timeout_ms = - 1) forkserver ->
  Native.finish_run forkserver timeout_ms
  |> native
  |> Result.map ~fn:status_of_raw

let start_next_run = fun forkserver ->
  Native.start_next_run forkserver
  |> native

let stop_forkserver = fun forkserver ->
  Native.stop_forkserver forkserver
  |> native
