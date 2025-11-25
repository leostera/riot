open Std

(** gRPC Call Types

    Defines the different RPC patterns supported by gRPC.
    This module only defines types - actual I/O is in blink/suri.
*)

(** RPC method type *)
type method_type =
  | Unary  (** Single request, single response *)
  | ServerStreaming  (** Single request, stream of responses *)
  | ClientStreaming  (** Stream of requests, single response *)
  | BidiStreaming  (** Stream of requests, stream of responses *)

(** Service method definition *)
type method_def = {
  service : string;  (** Service name (e.g., "example.UserService") *)
  method_ : string;  (** Method name (e.g., "GetUser") *)
  method_type : method_type;
}

(** Call configuration *)
type call_config = {
  timeout : Metadata.timeout option;
  metadata : Metadata.t;
  max_message_size : int option;
  compression : Metadata.encoding option;
}

(** Create a method definition for unary RPC *)
val unary_method : service:string -> method_:string -> method_def

(** Create a method definition for server streaming RPC *)
val server_streaming_method : service:string -> method_:string -> method_def

(** Create a method definition for client streaming RPC *)
val client_streaming_method : service:string -> method_:string -> method_def

(** Create a method definition for bidirectional streaming RPC *)
val bidi_streaming_method : service:string -> method_:string -> method_def

(** Check if method has request streaming *)
val is_request_streaming : method_def -> bool

(** Check if method has response streaming *)
val is_response_streaming : method_def -> bool

(** Default call configuration *)
val default_config : call_config

(** Create call config with timeout *)
val with_timeout : call_config -> timeout:Metadata.timeout -> call_config

(** Create call config with additional metadata *)
val with_metadata : call_config -> metadata:Metadata.t -> call_config

(** Create call config with max message size *)
val with_max_message_size : call_config -> max_message_size:int -> call_config

(** Create call config with compression *)
val with_compression : call_config -> compression:Metadata.encoding -> call_config

(** Get full method path for HTTP/2 :path header *)
val method_path : method_def -> string
