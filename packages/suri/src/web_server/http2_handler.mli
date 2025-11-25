open Std

(** # HTTP/2 Protocol Handler

    Implements HTTP/2 request parsing, response sending, and stream multiplexing.

    ## Features

    - HTTP/2 frame parsing using reentrant parser
    - Stream multiplexing with concurrent request handling
    - HPACK header compression/decompression
    - Server push support (optional)
    - Flow control

    ## Usage

    HTTP/2 connections are typically established via:
    1. Prior knowledge (direct HTTP/2 connection)
    2. HTTP/1.1 Upgrade (not yet implemented)
    3. TLS ALPN negotiation (requires TLS support)

    For now, HTTP/2 is detected by the connection preface:
    `PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n`
*)

type state
type error = [
  | `Parse_error of Http.Http2.Parser_reader.parse_error
  | `Protocol_error of string
  | `Io_error of string
]

val to_string_error : error -> string

(** Create HTTP/2 handler state

    @param config Server configuration
    @param handler Request handler function (receives parsed request)
    @param sniffed_data Optional data already read during protocol detection
    @return Initial handler state
*)
val make_handler :
  config:Config.t ->
  handler:(Socket_pool.Connection.t -> Request.t -> Response.t) ->
  ?sniffed_data:string ->
  unit ->
  state

include Socket_pool.Handler.Intf with type state := state and type error := error
