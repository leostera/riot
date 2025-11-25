open Std

(** gRPC Message Parser using IO.Reader (Reentrant)

    This parser handles gRPC message framing incrementally:
    - Uses IO.Reader.t for streaming I/O
    - Maintains state between calls (reentrant)
    - Validates message sizes
    - Never blocks or allocates large buffers upfront

    gRPC message format: [1-byte compressed][4-byte length][N-byte payload]
*)

(** Parser state *)
type state

(** Create new parser state *)
val create : ?max_message_size:int -> unit -> state

(** Parse errors *)
type parse_error =
  | Message_size_exceeds_maximum of { size : int; max_size : int }
      (** Message payload size exceeds configured maximum *)

(** Parse result *)
type parse_result =
  | Message of Message.t  (** Successfully parsed complete message *)
  | Need_more  (** Need more data - call again when available *)
  | Error of parse_error  (** Parse error *)

(** Parse the next message from reader.

    This is reentrant - call it multiple times as data arrives.

    Example usage:
    {[
      let parser = Message_reader.create () in
      let reader = IO.Reader.create stream in

      let rec read_messages () =
        match Message_reader.parse parser reader with
        | Message msg ->
            (* Process message *)
            handle_message msg;
            read_messages ()
        | Need_more ->
            (* Wait for more data *)
            yield ();
            read_messages ()
        | Error e ->
            handle_error e
    ]}

    @param state The parser state
    @param reader The IO reader
    @return Parse result
*)
val parse : state -> IO.Reader.t -> parse_result

(** Reset parser state *)
val reset : state -> unit

(** Get bytes buffered in parser *)
val buffered_bytes : state -> int
