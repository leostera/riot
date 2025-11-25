open Std

(** # Protocol Detection Handler

    Sniffs the initial bytes of a connection to detect HTTP/1.1 vs HTTP/2,
    then switches to the appropriate protocol handler.

    Detection logic:
    - HTTP/2: Starts with `PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n` (24 bytes)
    - HTTP/1.1: Starts with HTTP method (GET, POST, etc.)

    This handler reads the first few bytes, detects the protocol, and uses
    Socket_pool.Handler.Switch to delegate to the appropriate handler.
*)

type state
type error = [ `Detection_error of string ]

val to_string_error : error -> string

(** Create protocol detector

    @param config Server configuration
    @param handler Request handler (used by both HTTP/1.1 and HTTP/2)
    @return Initial state
*)
val make_handler :
  config:Config.t ->
  handler:(Socket_pool.Connection.t -> Request.t -> Response.t) ->
  unit ->
  state

include Socket_pool.Handler.Intf with type state := state and type error := error
