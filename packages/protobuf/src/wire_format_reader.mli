open Std

(** Protobuf Wire Format Decoder using IO.Reader (Reentrant)

    This decoder is designed for streaming protobuf message decoding:
    - Uses IO.Reader.t instead of bytes directly
    - Maintains state between calls (reentrant)
    - Returns Need_more when data is incomplete
    - Handles nested messages and variable-length fields
*)

(** Decoder state *)
type state

(** Create new decoder state *)
val create : unit -> state

(** Decode errors *)
type decode_error =
  | Unexpected_eof_reading_varint  (** Varint ended prematurely *)
  | Unexpected_eof_reading_i32  (** i32 has fewer than 4 bytes *)
  | Unexpected_eof_reading_i64  (** i64 has fewer than 8 bytes *)
  | Unexpected_eof_reading_length_delimited of int  (** Length-delimited field truncated *)
  | Invalid_wire_type of int  (** Unknown wire type *)
  | Mismatched_group_end_tag of { expected : int; actual : int }  (** Group end tag mismatch *)
  | Unexpected_group_end_tag  (** Group end tag outside of group *)
  | Unsupported_encoding  (** Unsupported encoding (e.g., groups in reentrant decoder) *)

(** Decode result *)
type decode_result =
  | Message of WireFormat.t  (** Successfully decoded complete message *)
  | Need_more  (** Need more data - call again when available *)
  | Error of decode_error  (** Decode error *)

(** Decode a protobuf message from reader.

    This is reentrant - call it multiple times as data arrives.
    The decoder maintains state across calls.

    Example usage:
    {[
      let decoder = Wire_format_reader.create () in
      let reader = IO.Reader.create stream in

      match Wire_format_reader.decode decoder reader with
      | Message msg ->
          (* Process decoded protobuf message *)
          handle_message msg
      | Need_more ->
          (* Wait for more data *)
          yield ();
          retry ()
      | Error (Invalid_wire_type typ) ->
          log_error (format "Invalid wire type: %d" typ)
      | Error e ->
          handle_error e
    ]}

    @param state The decoder state
    @param reader The IO reader
    @return Decode result
*)
val decode : state -> IO.Reader.t -> decode_result

(** Reset decoder state for reuse *)
val reset : state -> unit
