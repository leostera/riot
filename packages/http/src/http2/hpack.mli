open Std

(** HPACK: Header Compression for HTTP/2 (RFC 7541)

    This module implements the HPACK header compression algorithm used by HTTP/2
    to reduce the size of HTTP headers transmitted over the wire.

    HPACK uses:
    - A static table of common header fields (defined in RFC 7541 Appendix A)
    - A dynamic table that evolves based on headers seen in the connection
    - Huffman encoding for string values (defined in RFC 7541 Appendix B)
*)

(** {1 Types} *)

(** A header field is a name-value pair *)
type header = { name : string; value : string }

(** Encoding representation for a header field *)
type encoding_type =
  | Indexed  (** Fully indexed - both name and value from table *)
  | LiteralWithIndexing
      (** Literal with indexing - add to dynamic table after encoding *)
  | LiteralWithoutIndexing  (** Literal without indexing - don't add to table *)
  | LiteralNeverIndexed
      (** Literal never indexed - MUST NOT be added to table (e.g., sensitive data) *)

(** Encoder context maintains the dynamic table state *)
type encoder

(** Decoder context maintains the dynamic table state *)
type decoder

(** {1 Encoder} *)

(** Create a new encoder with the given dynamic table size limit.
    Default is 4096 bytes per RFC 7541. *)
val create_encoder : ?max_dynamic_table_size:int -> unit -> encoder

(** Encode a list of headers into HPACK wire format.

    The encoder will automatically choose the best encoding strategy:
    - Use indexed representation if the header exists in static/dynamic table
    - Use literal with indexing for new headers that should be cached
    - Use literal never indexed for sensitive headers (e.g., authorization)

    @param encoder The encoder context
    @param headers List of headers to encode
    @param sensitive_headers Optional set of header names that should never be indexed
    @return Encoded bytes
*)
val encode :
  encoder ->
  headers:header list ->
  ?sensitive_headers:string list ->
  bytes

(** Encode a single header field *)
val encode_header :
  encoder -> header -> encoding_type:encoding_type -> bytes

(** Update the dynamic table size limit.
    This is used when receiving SETTINGS_HEADER_TABLE_SIZE from the peer. *)
val update_max_table_size : encoder -> int -> unit

(** {1 Decoder} *)

(** Create a new decoder with the given dynamic table size limit.
    Default is 4096 bytes per RFC 7541. *)
val create_decoder : ?max_dynamic_table_size:int -> unit -> decoder

(** Decode HPACK-encoded bytes into a list of headers.

    @param decoder The decoder context
    @param data The HPACK-encoded bytes to decode
    @return Either the decoded headers or an error message
*)
val decode : decoder -> bytes -> (header list, string) Result.t

(** Update the dynamic table size limit.
    This is used when receiving SETTINGS_HEADER_TABLE_SIZE from the peer. *)
val update_max_table_size : decoder -> int -> unit

(** {1 Static Table} *)

(** Lookup a header in the static table by index (1-61).
    Returns None if index is out of range. *)
val static_table_lookup : int -> header option

(** Find the index of a header in the static table.
    Returns None if not found. *)
val static_table_find : name:string -> value:string -> int option

(** Find the index of a header name in the static table (value may differ).
    Returns None if not found. *)
val static_table_find_name : string -> int option

(** {1 Utilities} *)

(** Check if a header name should never be indexed (security-sensitive).
    Examples: authorization, cookie, set-cookie *)
val is_sensitive_header : string -> bool

(** Calculate the size of a header for dynamic table accounting.
    Per RFC 7541: size = length(name) + length(value) + 32 *)
val header_size : header -> int
