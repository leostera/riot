open Std

(** gRPC Metadata

    Metadata is sent as HTTP/2 headers. gRPC uses specific conventions:

    - Header names are lowercase
    - Header names ending in "-bin" contain base64-encoded binary data
    - Reserved headers start with "grpc-" (e.g., grpc-timeout, grpc-encoding)

    Standard metadata:
    - :method = POST
    - :scheme = http or https
    - :path = /<service>/<method>
    - :authority = <host>[:<port>]
    - content-type = application/grpc[+proto|+json|...]
    - te = trailers
    - grpc-timeout = <timeout-value> (e.g., 10S, 500M, 1H)
    - grpc-encoding = gzip | identity | deflate | snappy
    - grpc-message-type = <message-type> (optional)
    - grpc-status = <status-code> (in trailers)
    - grpc-message = <error-message> (in trailers)
*)

(** Metadata is a list of key-value pairs *)
type t = (string * string) list

(** Content type variants *)
type content_type =
  | Proto  (** application/grpc+proto (default) *)
  | Json  (** application/grpc+json *)
  | Custom of string  (** application/grpc+<custom> *)

(** Encoding/compression method *)
type encoding = Identity | Gzip | Deflate | Snappy

(** Timeout duration *)
type timeout = { value : int; unit : [ `Hours | `Minutes | `Seconds | `Milliseconds | `Microseconds | `Nanoseconds ] }

(** {1 Creating Metadata} *)

(** Empty metadata *)
val empty : t

(** Add a header to metadata *)
val add : t -> key:string -> value:string -> t

(** Add multiple headers *)
val add_all : t -> (string * string) list -> t

(** Remove a header from metadata *)
val remove : t -> key:string -> t

(** Get a header value *)
val get : t -> key:string -> string option

(** Get all values for a header (for repeated headers) *)
val get_all : t -> key:string -> string list

(** {1 Standard Headers} *)

(** Create :path header for a method call *)
val path : service:string -> method_:string -> string * string

(** Create content-type header *)
val content_type : content_type -> string * string

(** Create grpc-timeout header *)
val timeout : timeout -> string * string

(** Create grpc-encoding header *)
val encoding : encoding -> string * string

(** Create grpc-status header (for trailers) *)
val status : Status.t -> string * string

(** Create grpc-message header (for trailers) *)
val message : string -> string * string

(** {1 Parsing} *)

(** Parse content-type header *)
val parse_content_type : string -> content_type option

(** Parse grpc-timeout header

    Format: <value><unit>
    Units: H (hours), M (minutes), S (seconds), m (milliseconds), u (microseconds), n (nanoseconds)
    Example: "10S" = 10 seconds
*)
val parse_timeout : string -> timeout option

(** Parse grpc-encoding header *)
val parse_encoding : string -> encoding option

(** Parse grpc-status from trailers *)
val parse_status : string -> Status.t option

(** {1 Binary Metadata} *)

(** Check if header name indicates binary data (ends with "-bin") *)
val is_binary : string -> bool

(** Encode binary data for metadata (base64) *)
val encode_binary : bytes -> string

(** Decode binary metadata (base64) *)
val decode_binary : string -> (bytes, string) Result.t

(** {1 Validation} *)

(** Check if header name is valid (lowercase, no uppercase, starts with letter or :) *)
val is_valid_header_name : string -> bool

(** Check if header is a reserved gRPC header *)
val is_reserved : string -> bool

(** {1 Conversion} *)

(** Convert metadata to HTTP/2 headers for sending *)
val to_http_headers : t -> Http.Http2.Hpack.header list

(** Convert HTTP/2 headers to metadata *)
val of_http_headers : Http.Http2.Hpack.header list -> t
