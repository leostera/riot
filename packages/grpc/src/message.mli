open Std

(** gRPC Message Framing

    gRPC uses a simple framing format for messages:

    [Compressed-Flag: 1 byte][Message-Length: 4 bytes][Message: N bytes]

    - Compressed-Flag: 0 = not compressed, 1 = compressed (compression method in metadata)
    - Message-Length: 32-bit big-endian unsigned integer (max 4GB, but typically much smaller)
    - Message: Protobuf-encoded message bytes

    This module handles encoding/decoding of this wire format.
*)

(** Message frame *)
type t = { compressed : bool; payload : bytes }

(** Decode errors *)
type decode_error =
  | Incomplete_header of { have : int }
      (** Need 5 bytes for header, but have fewer *)
  | Message_size_exceeds_maximum of { size : int; max_size : int }
      (** Message size exceeds configured maximum *)
  | Incomplete_message of { need : int; have : int }
      (** Need more bytes to read complete message *)

(** Encode a message into gRPC wire format.

    @param compressed Whether the payload is compressed
    @param payload The message bytes (typically protobuf-encoded)
    @return 5-byte header + payload
*)
val encode : compressed:bool -> payload:bytes -> bytes

(** Decode a gRPC message from wire format.

    @param data The bytes to decode (must be at least 5 bytes)
    @return Result with decoded message and remaining bytes, or error
*)
val decode : bytes -> (t * bytes, decode_error) Result.t

(** Peek at message length without consuming bytes.

    Useful for buffering - tells you how many total bytes needed.

    @param data The bytes to peek (must be at least 5 bytes)
    @return Result with (compressed_flag, message_length), or error
*)
val peek_header : bytes -> (bool * int, decode_error) Result.t

(** Maximum message size (configurable, default 4MB).

    While the wire format allows up to 4GB (32-bit length),
    we enforce a reasonable maximum to prevent DoS attacks.
*)
val default_max_message_size : int

(** Validate message size is within limits.

    @param size The message size to validate
    @param max_size Maximum allowed size (optional, uses default)
    @return Ok () if valid, Error if too large
*)
val validate_size : int -> max_size:int option -> (unit, decode_error) Result.t
