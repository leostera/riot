(** # HTTP/1.1 Web Server

    HTTP/1.1 server built on SocketPool with request parsing, response handling,
    and keep-alive connection management.

    ## Example

    ```ocaml

    open Suri.WebServer

    let handler conn req = let uri = Request.uri req in let method_ =
    Request.method_ req in Log.info "%s %s" (Net.Http.Method.to_string method_)
    uri; Response.ok ~body:"Hello, World!" ()

    let () = let config = Config.make () in let handler_state = Http1.make
    ~config ~handler () in SocketPool.start_link ~port:8080 ~handler:(module
    Http1) ~initial_state:handler_state

    ``` *)

module Config = Config
(** Server configuration *)

module Request = Request
(** HTTP request representation *)

module Response = Response
(** HTTP response construction *)

module Http1 = Http1_handler
(** HTTP/1.1 protocol handler *)

module Http2 = Http2_handler
(** HTTP/2 protocol handler *)

module ProtocolDetector = Protocol_detector
(** Auto-detect HTTP/1.1 vs HTTP/2 and switch handlers *)
