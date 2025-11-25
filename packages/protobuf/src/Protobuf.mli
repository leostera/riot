open Std

module ProtofileFormat : sig
  type t

  val parse : string -> (t, string) Result.t
  val print : t -> string
  val to_json : t -> Data.Json.t
end

module DebugFormat : sig
  type t

  val parse : string -> (t, string) Result.t
  val print : t -> string
end

module WireFormat : sig
  type t

  type decode_error =
    | Unexpected_eof_reading_varint
    | Unexpected_eof_reading_i32
    | Unexpected_eof_reading_i64
    | Unexpected_eof_reading_length_delimited of int
    | Invalid_wire_type of int
    | Mismatched_group_end_tag of { expected : int; actual : int }
    | Unexpected_group_end_tag

  val decode : bytes -> (t, decode_error) Result.t
  val encode : t -> bytes
end

module Wire_format_reader : sig
  type state

  val create : unit -> state

  type decode_error =
    | Unexpected_eof_reading_varint
    | Unexpected_eof_reading_i32
    | Unexpected_eof_reading_i64
    | Unexpected_eof_reading_length_delimited of int
    | Invalid_wire_type of int
    | Mismatched_group_end_tag of { expected : int; actual : int }
    | Unexpected_group_end_tag
    | Unsupported_encoding

  type decode_result =
    | Message of WireFormat.t
    | Need_more
    | Error of decode_error

  val decode : state -> IO.Reader.t -> decode_result
  val reset : state -> unit
end

module Codegen : module type of Codegen
