open Std

type parse_phase =
  | ReadingHeader of {
      buffer : Buffer.t;  (** Accumulating 5-byte header *)
      bytes_read : int;
    }
  | ReadingPayload of {
      compressed : bool;
      length : int;
      buffer : Buffer.t;
      bytes_read : int;
    }

type state = {
  max_message_size : int;
  phase : parse_phase Cell.t;
}

type parse_error =
  | Message_size_exceeds_maximum of { size : int; max_size : int }

type parse_result =
  | Message of Message.t
  | Need_more
  | Error of parse_error

let create ?(max_message_size = Message.default_max_message_size) () =
  {
    max_message_size;
    phase =
      Cell.create (ReadingHeader { buffer = Buffer.create 5; bytes_read = 0 });
  }

let reset state =
  Cell.set state.phase (ReadingHeader { buffer = Buffer.create 5; bytes_read = 0 })

let buffered_bytes state =
  match Cell.get state.phase with
  | ReadingHeader { bytes_read; _ } -> bytes_read
  | ReadingPayload { bytes_read; _ } -> bytes_read + 5

(** Try to read N bytes from reader into buffer *)
let read_n_bytes reader buffer n =
  let bytes = Bytes.create n in
  match IO.Reader.read reader bytes with
  | Ok bytes_read when bytes_read > 0 ->
      Buffer.add_subbytes buffer bytes 0 bytes_read;
      bytes_read
  | Ok _ -> 0
  | Error _ -> 0

let parse state reader =
  let ( let* ) = Result.and_then in

  match Cell.get state.phase with
  | ReadingHeader { buffer; bytes_read } ->
      (* Try to read 5-byte header *)
      let needed = 5 - bytes_read in
      let actual_read = read_n_bytes reader buffer needed in

      if actual_read = 0 && bytes_read = 0 then Need_more
      else if bytes_read + actual_read < 5 then (
        (* Still incomplete *)
        Cell.set state.phase
          (ReadingHeader { buffer; bytes_read = bytes_read + actual_read });
        Need_more)
      else
        (* Have complete 5-byte header *)
        let header_data = Buffer.contents buffer in
        let compressed = Char.code header_data.[0] <> 0 in

        (* Read 32-bit big-endian length *)
        let b1 = Char.code header_data.[1] in
        let b2 = Char.code header_data.[2] in
        let b3 = Char.code header_data.[3] in
        let b4 = Char.code header_data.[4] in
        let length = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in

        (* Validate size *)
        if length > state.max_message_size then (
          reset state;
          Error (Message_size_exceeds_maximum { size = length; max_size = state.max_message_size }))
        else if length = 0 then (
          (* Zero-length message *)
          reset state;
          Message { compressed; payload = Bytes.empty })
        else (
          (* Need to read payload *)
          Cell.set state.phase
            (ReadingPayload
               {
                 compressed;
                 length;
                 buffer = Buffer.create length;
                 bytes_read = 0;
               });
          (* Recursively try to read payload immediately *)
          parse state reader)
  | ReadingPayload { compressed; length; buffer; bytes_read } ->
      (* Try to read payload *)
      let needed = length - bytes_read in
      let actual_read = read_n_bytes reader buffer needed in

      if actual_read = 0 && bytes_read = 0 then Need_more
      else if bytes_read + actual_read < length then (
        (* Still incomplete *)
        Cell.set state.phase
          (ReadingPayload
             { compressed; length; buffer; bytes_read = bytes_read + actual_read });
        Need_more)
      else
        (* Have complete payload *)
        let payload = Buffer.to_bytes buffer in
        reset state;
        Message { compressed; payload }
