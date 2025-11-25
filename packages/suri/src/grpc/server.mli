open Std

(** gRPC Server

    Implements gRPC server over HTTP/2 for handling RPC calls.
    Supports unary and streaming call patterns.
*)

(** gRPC server instance *)
type t

(** Server configuration *)
type config = {
  port : int;  (** Port to listen on *)
  max_message_size : int;  (** Maximum message size (default: 4MB) *)
  max_concurrent_streams : int;  (** Max concurrent HTTP/2 streams (default: 100) *)
  max_frame_size : int;  (** Max HTTP/2 frame size (default: 16KB) *)
}

(** Default configuration *)
val default_config : config

(** Server errors *)
type error =
  | Bind_failed of Net.error  (** Failed to bind to port *)
  | Accept_failed of Net.error  (** Failed to accept connection *)
  | Connection_error of string  (** Connection-level error *)
  | Handler_error of string  (** Handler execution error *)

(** Handler context with request metadata *)
type context = {
  peer : string;  (** Client address *)
  metadata : Grpc.Metadata.t;  (** Request headers/metadata *)
  deadline : float option;  (** Request deadline (if set) *)
}

(** Unary handler: single request -> single response *)
type ('req, 'res) unary_handler =
  context -> 'req -> ('res, Grpc.Status.t * string) Result.t

(** Server streaming handler: single request -> stream of responses *)
type ('req, 'res) server_streaming_handler =
  context -> 'req -> ('res Iter.t, Grpc.Status.t * string) Result.t

(** Client streaming handler: stream of requests -> single response *)
type ('req, 'res) client_streaming_handler =
  context -> 'req Iter.t -> ('res, Grpc.Status.t * string) Result.t

(** Bidirectional streaming handler: stream of requests -> stream of responses *)
type ('req, 'res) bidi_streaming_handler =
  context -> 'req Iter.t -> ('res Iter.t, Grpc.Status.t * string) Result.t

(** Method handler (one of the 4 patterns) *)
type method_handler =
  | Unary of (Protobuf.WireFormat.t, Protobuf.WireFormat.t) unary_handler
  | ServerStreaming of (Protobuf.WireFormat.t, Protobuf.WireFormat.t) server_streaming_handler
  | ClientStreaming of (Protobuf.WireFormat.t, Protobuf.WireFormat.t) client_streaming_handler
  | BidiStreaming of (Protobuf.WireFormat.t, Protobuf.WireFormat.t) bidi_streaming_handler

(** Create a new gRPC server

    @param config Server configuration
    @return New server instance
*)
val create : config -> t

(** Register a method handler

    @param server The server instance
    @param service Service name (e.g., "myapp.UserService")
    @param method_ Method name (e.g., "GetUser")
    @param handler Method handler
*)
val register_method :
  t ->
  service:string ->
  method_:string ->
  handler:method_handler ->
  unit

(** Start the server (blocking)

    Binds to the configured port and starts accepting connections.
    Handles each connection in a separate process.

    @param server The server instance
    @return Error if server fails to start
*)
val start : t -> (unit, error) Result.t

(** Stop the server

    @param server The server instance
*)
val stop : t -> unit
