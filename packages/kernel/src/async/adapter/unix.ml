open Common
open Prelude

type selector = int

type error =
  | InvalidTimeoutNs of { timeout_ns: int64 }
  | InvalidMaxEvents of { max_events: int }
  | System of System_error.t

type event = {
  fd: int;
  filter: int;
  flags: int;
  token: Token.t;
}

module FFI = struct
  external selector_create: unit -> (selector, int) Result.t = "kernel_new_async_unix_selector_create"

  external selector_close: selector -> (unit, int) Result.t = "kernel_new_async_unix_selector_close"

  external selector_wait:
    max_events:int -> timeout_ns:int64 -> selector -> (event array, int) Result.t
    = "kernel_new_async_unix_selector_wait"

  external selector_apply: selector -> event array -> int array -> (unit, int) Result.t = "kernel_new_async_unix_selector_apply"

  external selector_register_process: selector -> int -> Token.t -> (unit, int) Result.t = "kernel_new_async_unix_selector_register_process"

  external selector_reregister_process: selector -> int -> Token.t -> (unit, int) Result.t = "kernel_new_async_unix_selector_reregister_process"

  external selector_deregister_process: selector -> int -> (unit, int) Result.t = "kernel_new_async_unix_selector_deregister_process"

  external selector_register_timer:
    selector -> int -> (int * int) -> bool -> Token.t -> (unit, int) Result.t
    = "kernel_new_async_unix_selector_register_timer"

  external selector_reregister_timer:
    selector -> int -> (int * int) -> bool -> Token.t -> (unit, int) Result.t
    = "kernel_new_async_unix_selector_reregister_timer"

  external selector_deregister_timer: selector -> int -> (unit, int) Result.t = "kernel_new_async_unix_selector_deregister_timer"

  let create = fun () ->
    Result.map_err (selector_create ()) ~fn:(fun code -> System (System_error.from_code code))

  let wait = fun ~max_events ~timeout_ns selector ->
    Result.map_err
      (selector_wait ~max_events ~timeout_ns selector)
      ~fn:(fun code -> System (System_error.from_code code))

  let close = fun selector ->
    Result.map_err (selector_close selector) ~fn:(fun code -> System (System_error.from_code code))

  let apply = fun selector changes ignored_errors ->
    Result.map_err
      (selector_apply selector changes ignored_errors)
      ~fn:(fun code -> System (System_error.from_code code))

  let register_process = fun selector ~pid ~token ->
    Result.map_err
      (selector_register_process selector pid token)
      ~fn:(fun code -> System (System_error.from_code code))

  let reregister_process = fun selector ~pid ~token ->
    Result.map_err
      (selector_reregister_process selector pid token)
      ~fn:(fun code -> System (System_error.from_code code))

  let deregister_process = fun selector ~pid ->
    Result.map_err
      (selector_deregister_process selector pid)
      ~fn:(fun code -> System (System_error.from_code code))

  let register_timer = fun selector ~timer_id ~timeout_parts ~repeat ~token ->
    Result.map_err
      (selector_register_timer selector timer_id timeout_parts repeat token)
      ~fn:(fun code -> System (System_error.from_code code))

  let reregister_timer = fun selector ~timer_id ~timeout_parts ~repeat ~token ->
    Result.map_err
      (selector_reregister_timer selector timer_id timeout_parts repeat token)
      ~fn:(fun code -> System (System_error.from_code code))

  let deregister_timer = fun selector ~timer_id ->
    Result.map_err
      (selector_deregister_timer selector timer_id)
      ~fn:(fun code -> System (System_error.from_code code))
end

module Kevent = struct
  type t = event

  let make = fun fd ~filter ~flags ~token -> { fd; filter; flags; token }

  let token = fun event -> event.token

  let is_error = fun event -> event.flags land Libc.ev_error != 0

  let is_priority = fun event -> event.filter = Libc.evfilt_proc

  let is_readable = fun event -> event.filter = Libc.evfilt_read || event.filter = Libc.evfilt_timer

  let is_writable = fun event -> event.filter = Libc.evfilt_write

  let is_read_closed = fun event -> is_readable event && event.flags land Libc.ev_eof != 0

  let is_write_closed = fun event -> is_writable event && event.flags land Libc.ev_eof != 0
end

module Selector = struct
  type t = {
    native: selector;
    mutable pending_processes: event list;
  }

  let name = "kqueue"

  let make = fun () ->
    let* native = FFI.create () in
    Result.Ok { native; pending_processes = [] }

  let close = fun selector ->
    let* () = FFI.close selector.native in
    selector.pending_processes <- [];
    Result.Ok ()

  let rec discard_pending_process = fun pid events ->
    match events with
    | [] -> []
    | event :: rest when event.fd = pid -> discard_pending_process pid rest
    | event :: rest -> event :: discard_pending_process pid rest

  let rec take_pending = fun count events acc ->
    match (count, events) with
    | (0, _)
    | (_, []) -> (acc, events, count)
    | (_, event :: rest) ->
        take_pending (count - 1) rest (Event.make (module Kevent) event :: acc)

  let select = fun ?(timeout = 500_000_000L) ?(max_events = 1_024) selector ->
    if timeout < 0L then
      Result.Error (InvalidTimeoutNs { timeout_ns = timeout })
    else if max_events <= 0 then
      Result.Error (InvalidMaxEvents { max_events })
    else
      let pending, remaining, native_limit =
        take_pending max_events selector.pending_processes []
      in
      let* events =
        if native_limit = 0 then
          Result.Ok [||]
        else
          FFI.wait ~max_events:native_limit
            ~timeout_ns:(if pending = [] then timeout else 0L) selector.native
      in
      selector.pending_processes <- remaining;
      let rec to_list index acc =
        if index < 0 then
          acc
        else
          to_list
            (index - 1)
            (Event.make (module Kevent) (Array.get_unchecked events ~at:index) :: acc)
      in
      Result.Ok (to_list (Array.length events - 1) pending)

  let register = fun selector ~fd ~token ~interest ->
    let flags = Libc.(ev_clear lor ev_receipt lor ev_add) in
    let changes =
      match (Interest.is_readable interest, Interest.is_writable interest) with
      | true, true -> [
        Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token;
        Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token;
      ]
      | true, false -> [ Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token ]
      | false, true -> [ Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token ]
      | false, false -> []
    in
    FFI.apply selector.native (Array.from_list changes) [|System_error_code.broken_pipe|]

  let reregister = fun selector ~fd ~token ~interest ->
    let flags = Libc.(ev_clear lor ev_receipt) in
    let write_flags =
      if Interest.is_writable interest then
        flags lor Libc.ev_add
      else
        flags lor Libc.ev_delete
    in
    let read_flags =
      if Interest.is_readable interest then
        flags lor Libc.ev_add
      else
        flags lor Libc.ev_delete
    in
    let changes = [|
      Kevent.make fd ~filter:Libc.evfilt_write ~flags:write_flags ~token;
      Kevent.make fd ~filter:Libc.evfilt_read ~flags:read_flags ~token;
    |] in
    FFI.apply
      selector.native
      changes
      [|System_error_code.broken_pipe; System_error_code.no_such_file_or_directory|]

  let deregister = fun selector ~fd ->
    let flags = Libc.(ev_delete lor ev_receipt) in
    let token = Token.make 0 in
    let changes = [|
      Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token;
      Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token;
    |] in
    FFI.apply selector.native changes [|System_error_code.no_such_file_or_directory|]

  let change_process = fun change selector ~pid ~token ->
    selector.pending_processes <- discard_pending_process pid selector.pending_processes;
    match change selector.native ~pid ~token with
    | Result.Error (System System_error.NoSuchProcess) ->
        (* Exit can win between try_wait and registration. Readiness is a retry
           hint, not an exit status: preserve the caller's token until select,
           reregister, deregister, or close consumes it. Never report success
           with neither a native watch nor a pending hint. *)
        selector.pending_processes <-
          Kevent.make pid ~filter:Libc.evfilt_proc ~flags:0 ~token
          :: selector.pending_processes;
        Result.Ok ()
    | result -> result

  let register_process = fun selector ~pid ~token ->
    change_process FFI.register_process selector ~pid ~token

  let reregister_process = fun selector ~pid ~token ->
    change_process FFI.reregister_process selector ~pid ~token

  let deregister_process = fun selector ~pid ->
    selector.pending_processes <- discard_pending_process pid selector.pending_processes;
    FFI.deregister_process selector.native ~pid

  let register_timer = fun selector ~timer_id ~token ~timeout_parts ~repeat ->
    FFI.register_timer selector.native ~timer_id ~timeout_parts ~repeat ~token

  let reregister_timer = fun selector ~timer_id ~token ~timeout_parts ~repeat ->
    FFI.reregister_timer selector.native ~timer_id ~timeout_parts ~repeat ~token

  let deregister_timer = fun selector ~timer_id -> FFI.deregister_timer selector.native ~timer_id
end
