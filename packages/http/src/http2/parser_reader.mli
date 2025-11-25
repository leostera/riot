open Std

(** HTTP/2 Frame Parser using IO.Reader (Reentrant)

    This parser is designed for streaming, non-blocking I/O:
    - Uses IO.Reader.t instead of bytes directly
    - Maintains state between calls (reentrant)
    - Returns Need_more when data is incomplete
    - Never blocks or allocates large buffers upfront
*)

(** Parser configuration *)
type config = {
  max_frame_size : int;  (** Maximum frame size (default: 16384) *)
}

(** Default configuration *)
val default_config : config

(** Parser state - opaque, tracks position in frame parsing *)
type state

(** Create a new parser state *)
val create : ?config:config -> unit -> state

(** Parse errors *)
type parse_error =
  | Incomplete_frame_header  (** Frame header has fewer than 9 bytes *)
  | Frame_size_exceeds_maximum of { size : int; max_size : int }
      (** Frame payload size exceeds configured maximum *)
  | Unknown_frame_type of int  (** Unknown HTTP/2 frame type byte *)
  | Invalid_payload_length of { frame_type : string; expected : int; actual : int }
      (** Payload length doesn't match frame type requirements *)
  | Incomplete_settings_payload  (** SETTINGS payload is not multiple of 6 bytes *)

(** Parse result *)
type parse_result =
  | Frame of Frame.t  (** Successfully parsed a complete frame *)
  | Need_more  (** Need more data - call again with more bytes in reader *)
  | Error of parse_error  (** Parse error *)

(** Parse the next frame from the reader.

    This function is reentrant - you can call it multiple times as data arrives.
    The parser state tracks where it left off.

    Example usage:
    {[
      let parser = Parser_reader.create () in
      let reader = IO.Reader.create stream in

      let rec read_frames () =
        match Parser_reader.parse parser reader with
        | Frame frame ->
            (* Process frame *)
            handle_frame frame;
            read_frames ()  (* Parse next frame *)
        | Need_more ->
            (* Wait for more data, then call again *)
            yield ();
            read_frames ()
        | Error e ->
            (* Handle error *)
            handle_error e
    ]}

    @param state The parser state
    @param reader The IO reader to read from
    @return Parse result
*)
val parse : state -> IO.Reader.t -> parse_result

(** Reset parser state to initial (for connection reuse) *)
val reset : state -> unit

(** Get bytes buffered in parser state (for debugging) *)
val buffered_bytes : state -> int
