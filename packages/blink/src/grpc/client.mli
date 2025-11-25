open Std

(** gRPC Client

    Implements gRPC client over HTTP/2 for making RPC calls.
    Supports unary and streaming call patterns.
*)

(** gRPC client connection *)
type t

(** Connection configuration *)
type config = {
  max_message_size : int;  (** Maximum message size (default: 4MB) *)
  connect_timeout : Time.Duration.t option;  (** Connection timeout *)
  default_timeout : Time.Duration.t option;  (** Default call timeout *)
  user_agent : string;  (** User agent header *)
}

(** Default configuration *)
val default_config : config

(** HTTP/2 protocol errors *)
type http2_protocol_error =
  | Missing_preface  (** Missing or invalid HTTP/2 connection preface *)
  | Settings_not_acked  (** SETTINGS frame not acknowledged *)
  | Invalid_stream_state  (** Operation invalid for current stream state *)
  | Flow_control_error  (** Flow control window exceeded *)
  | Stream_closed  (** Operation on closed stream *)

(** HPACK decoding errors *)
type hpack_error =
  | Invalid_header_index of int  (** Invalid dynamic table index *)
  | Invalid_name_index of int  (** Invalid static table name index *)
  | Unsupported_encoding  (** Unsupported huffman or encoding *)
  | Invalid_decoder_state  (** Decoder in invalid state *)
  | Decode_failed of string  (** Generic decode failure with details *)

(** gRPC message decoding errors *)
type message_error =
  | Message_size_exceeds_maximum of { size : int; max_size : int }
      (** Message exceeds configured maximum *)
  | Invalid_compression_flag of int  (** Unknown compression flag *)
  | Invalid_message_format of string  (** Malformed message with details *)

(** Invalid response errors *)
type invalid_response_error =
  | No_message_in_unary_response  (** Unary call completed without message *)
  | Multiple_messages_in_unary_response  (** Unary call received >1 messages *)
  | Multiple_messages_in_client_streaming_response  (** Client streaming received >1 response messages *)
  | No_message_in_client_streaming_response  (** Client streaming received no response *)
  | Not_awaiting_response  (** Received response when not expecting one *)
  | No_active_stream  (** Operation requires active stream *)
  | Send_side_closed  (** Cannot send after closing send side *)
  | No_active_streaming_call  (** No streaming call in progress *)
  | Cannot_send_on_non_streaming_call  (** Send attempted on unary call *)
  | Not_in_client_streaming_state  (** Operation invalid for current state *)
  | Not_in_bidirectional_streaming_state  (** Operation invalid for current state *)

(** Client errors *)
type error =
  | Connection_failed of Net.error  (** TCP connection failed *)
  | Connection_closed  (** Connection closed unexpectedly *)
  | Http2_frame_error of Http.Http2.Parser_reader.parse_error  (** HTTP/2 frame parsing error *)
  | Http2_protocol_error of http2_protocol_error  (** HTTP/2 protocol violation *)
  | Hpack_decode_error of hpack_error  (** HPACK decoding error *)
  | Message_decode_error of message_error  (** gRPC message decoding error *)
  | Protobuf_decode_error of Protobuf.WireFormat.decode_error  (** Protobuf decoding error *)
  | GRPC_status of Grpc.Status.t * string  (** gRPC status from server *)
  | Timeout  (** Call timeout exceeded *)
  | Invalid_response of invalid_response_error  (** Invalid response *)

(** Call response for unary calls *)
type 'a response = {
  headers : Grpc.Metadata.t;  (** Response headers *)
  message : 'a;  (** Response message *)
  trailers : Grpc.Metadata.t;  (** Response trailers *)
  status : Grpc.Status.t;  (** gRPC status *)
}

(** Streaming response (for server streaming) *)
type 'a stream_response = {
  headers : Grpc.Metadata.t;
  messages : 'a list;  (** Messages received so far *)
  complete : bool;  (** Whether stream is complete *)
  trailers : Grpc.Metadata.t option;  (** Trailers (only when complete) *)
  status : Grpc.Status.t option;  (** Status (only when complete) *)
}

(** Connect to gRPC server

    @param uri Server URI (e.g., "http://localhost:50051")
    @param config Optional configuration
    @return Connection or error
*)
val connect : ?config:config -> Net.Uri.t -> (t, error) Result.t

(** Make a unary gRPC call

    @param conn The connection
    @param service Service name (e.g., "myapp.UserService")
    @param method_ Method name (e.g., "GetUser")
    @param request Request message (protobuf records)
    @param timeout Optional call timeout
    @param metadata Optional call metadata
    @return Response with decoded message or error
*)
val call_unary :
  t ->
  service:string ->
  method_:string ->
  request:Protobuf.WireFormat.t ->
  ?timeout:Time.Duration.t ->
  ?metadata:Grpc.Metadata.t ->
  unit ->
  (Protobuf.WireFormat.t response, error) Result.t

(** Start a server streaming call

    @param conn The connection
    @param service Service name
    @param method_ Method name
    @param request Request message
    @param timeout Optional timeout
    @param metadata Optional metadata
    @return Stream handle or error
*)
val call_server_streaming :
  t ->
  service:string ->
  method_:string ->
  request:Protobuf.WireFormat.t ->
  ?timeout:Time.Duration.t ->
  ?metadata:Grpc.Metadata.t ->
  unit ->
  (Protobuf.WireFormat.t stream_response, error) Result.t

(** Receive next message from server streaming call

    @param conn The connection
    @return Updated stream response or error
*)
val receive_stream :
  t -> (Protobuf.WireFormat.t stream_response, error) Result.t

(** Start a client streaming call

    Sends headers only, allowing multiple messages to be sent afterward.

    @param conn The connection
    @param service Service name
    @param method_ Method name
    @param timeout Optional timeout
    @param metadata Optional metadata
    @return Unit on success or error
*)
val call_client_streaming :
  t ->
  service:string ->
  method_:string ->
  ?timeout:Time.Duration.t ->
  ?metadata:Grpc.Metadata.t ->
  unit ->
  (unit, error) Result.t

(** Send a message on a streaming call

    Works for both client streaming and bidirectional streaming.

    @param conn The connection
    @param message Message to send
    @return Unit on success or error
*)
val send_message :
  t ->
  Protobuf.WireFormat.t ->
  (unit, error) Result.t

(** Finish client streaming and receive response

    Closes the send side with END_STREAM and waits for single response.

    @param conn The connection
    @return Response with message or error
*)
val finish_client_stream :
  t ->
  (Protobuf.WireFormat.t response, error) Result.t

(** Start a bidirectional streaming call

    Sends headers only, allowing interleaved sending and receiving.

    @param conn The connection
    @param service Service name
    @param method_ Method name
    @param timeout Optional timeout
    @param metadata Optional metadata
    @return Unit on success or error
*)
val call_bidi_streaming :
  t ->
  service:string ->
  method_:string ->
  ?timeout:Time.Duration.t ->
  ?metadata:Grpc.Metadata.t ->
  unit ->
  (unit, error) Result.t

(** Receive a single message from bidirectional streaming

    Non-blocking: returns None if no message available yet.

    @param conn The connection
    @return Some message, None if no message yet, or error
*)
val receive_message :
  t ->
  (Protobuf.WireFormat.t option, error) Result.t

(** Close the send side of bidirectional stream

    Sends END_STREAM flag but keeps receiving.

    @param conn The connection
    @return Unit on success or error
*)
val close_send :
  t ->
  (unit, error) Result.t

(** Finish bidirectional streaming

    Waits for trailers and status after both sides are closed.

    @param conn The connection
    @return Final status and trailers or error
*)
val finish_bidi_stream :
  t ->
  (Grpc.Metadata.t * Grpc.Status.t, error) Result.t

(** Close the connection *)
val close : t -> unit
