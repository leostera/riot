open Std

(** gRPC Protocol Implementation

    This package provides protocol-level types and utilities for gRPC.
    It does NOT provide client/server implementations - those are in:
    - blink: Blink.GRPC for gRPC clients
    - suri: Suri.GRPC for gRPC servers

    Usage:
    {[
      (* Define a method *)
      let get_user = Grpc.Call.unary_method
        ~service:"example.UserService"
        ~method_:"GetUser"

      (* Create metadata *)
      let metadata = Grpc.Metadata.empty
        |> Grpc.Metadata.add ~key:"authorization" ~value:"Bearer token123"

      (* Encode a request message *)
      let request_bytes = (* ... protobuf encoding ... *) in
      let framed = Grpc.Message.encode ~compressed:false ~payload:request_bytes

      (* Parse status from trailers *)
      match Grpc.Metadata.parse_status "0" with
      | Some Grpc.Status.OK -> (* success *)
      | _ -> (* error *)
    ]}
*)

(** {1 Modules} *)

(** Status codes *)
module Status : sig
  type t =
    | OK
    | Cancelled
    | Unknown
    | InvalidArgument
    | DeadlineExceeded
    | NotFound
    | AlreadyExists
    | PermissionDenied
    | ResourceExhausted
    | FailedPrecondition
    | Aborted
    | OutOfRange
    | Unimplemented
    | Internal
    | Unavailable
    | DataLoss
    | Unauthenticated

  val to_int : t -> int
  val of_int : int -> t option
  val to_string : t -> string
  val to_http_status : t -> int
  val is_ok : t -> bool
  val is_retriable : t -> bool
  val pp : Format.formatter -> t -> unit
end

(** Message framing (5-byte header + payload) *)
module Message : sig
  type t = { compressed : bool; payload : bytes }

  val encode : compressed:bool -> payload:bytes -> bytes
  val decode : bytes -> (t * bytes, string) Result.t
  val peek_header : bytes -> (bool * int, string) Result.t
  val default_max_message_size : int
  val validate_size : int -> max_size:int option -> (unit, string) Result.t
end

(** Metadata (HTTP/2 headers) *)
module Metadata : sig
  type t = (string * string) list
  type content_type = Proto | Json | Custom of string
  type encoding = Identity | Gzip | Deflate | Snappy

  type timeout = {
    value : int;
    unit :
      [ `Hours
      | `Minutes
      | `Seconds
      | `Milliseconds
      | `Microseconds
      | `Nanoseconds
      ];
  }

  val empty : t
  val add : t -> key:string -> value:string -> t
  val add_all : t -> (string * string) list -> t
  val remove : t -> key:string -> t
  val get : t -> key:string -> string option
  val get_all : t -> key:string -> string list
  val path : service:string -> method_:string -> string * string
  val content_type : content_type -> string * string
  val timeout : timeout -> string * string
  val encoding : encoding -> string * string
  val status : Status.t -> string * string
  val message : string -> string * string
  val parse_content_type : string -> content_type option
  val parse_timeout : string -> timeout option
  val parse_encoding : string -> encoding option
  val parse_status : string -> Status.t option
  val is_binary : string -> bool
  val encode_binary : bytes -> string
  val decode_binary : string -> (bytes, string) Result.t
  val is_valid_header_name : string -> bool
  val is_reserved : string -> bool
  val to_http_headers : t -> Http.Http2.Hpack.header list
  val of_http_headers : Http.Http2.Hpack.header list -> t
end

(** Call types and configuration *)
module Call : sig
  type method_type =
    | Unary
    | ServerStreaming
    | ClientStreaming
    | BidiStreaming

  type method_def = {
    service : string;
    method_ : string;
    method_type : method_type;
    request_streaming : bool;
    response_streaming : bool;
  }

  type call_config = {
    timeout : Metadata.timeout option;
    metadata : Metadata.t;
    max_message_size : int option;
    compression : Metadata.encoding option;
  }

  val unary_method : service:string -> method_:string -> method_def
  val server_streaming_method : service:string -> method_:string -> method_def
  val client_streaming_method : service:string -> method_:string -> method_def
  val bidi_streaming_method : service:string -> method_:string -> method_def
  val default_config : call_config
  val with_timeout : call_config -> Metadata.timeout -> call_config
  val with_metadata : call_config -> Metadata.t -> call_config
  val with_max_message_size : call_config -> int -> call_config
  val with_compression : call_config -> Metadata.encoding -> call_config
  val method_path : method_def -> string
end

(** Code generation from protobuf services *)
module Codegen : module type of Codegen
