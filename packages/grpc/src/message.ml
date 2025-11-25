open Std

type t = { compressed : bool; payload : bytes }

(** Default maximum message size: 4MB
    This prevents DoS attacks while being large enough for most use cases *)
let default_max_message_size = 4 * 1024 * 1024

let validate_size size ~max_size =
  let limit = Option.value max_size ~default:default_max_message_size in
  if size > limit then
    Error (format "Message size %d exceeds maximum %d" size limit)
  else Ok ()

let encode ~compressed ~payload =
  let payload_len = Bytes.length payload in
  let frame = Bytes.create (5 + payload_len) in

  (* Byte 0: Compressed flag *)
  Bytes.set frame 0 (if compressed then '\x01' else '\x00');

  (* Bytes 1-4: Message length (32-bit big-endian) *)
  Bytes.set frame 1 (Char.chr ((payload_len lsr 24) land 0xFF));
  Bytes.set frame 2 (Char.chr ((payload_len lsr 16) land 0xFF));
  Bytes.set frame 3 (Char.chr ((payload_len lsr 8) land 0xFF));
  Bytes.set frame 4 (Char.chr (payload_len land 0xFF));

  (* Bytes 5+: Payload *)
  Bytes.blit payload 0 frame 5 payload_len;

  frame

let peek_header data =
  if Bytes.length data < 5 then Error "Incomplete message header (need 5 bytes)"
  else
    let compressed = Char.code (Bytes.get data 0) <> 0 in

    (* Read 32-bit big-endian length *)
    let b1 = Char.code (Bytes.get data 1) in
    let b2 = Char.code (Bytes.get data 2) in
    let b3 = Char.code (Bytes.get data 3) in
    let b4 = Char.code (Bytes.get data 4) in

    let length = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in

    Ok (compressed, length)

let decode data =
  let ( let* ) = Result.and_then in

  if Bytes.length data < 5 then
    Error "Incomplete message header (need at least 5 bytes)"
  else
    let* (compressed, length) = peek_header data in

    (* Validate message size to prevent DoS *)
    let* () = validate_size length ~max_size:None in

    let total_length = 5 + length in
    if Bytes.length data < total_length then
      Error
        (format "Incomplete message: need %d bytes, have %d" total_length
           (Bytes.length data))
    else
      let payload = Bytes.sub data 5 length in
      let remaining = Bytes.sub data total_length (Bytes.length data - total_length) in
      Ok ({ compressed; payload }, remaining)
