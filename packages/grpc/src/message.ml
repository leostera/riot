open Std

type t = { compressed : bool; payload : bytes }

type decode_error =
  | Incomplete_header of { have : int }
  | Message_size_exceeds_maximum of { size : int; max_size : int }
  | Incomplete_message of { need : int; have : int }

(** Default maximum message size: 4MB
    This prevents DoS attacks while being large enough for most use cases *)
let default_max_message_size = 4 * 1024 * 1024

let validate_size size ~max_size =
  let limit = Option.value max_size ~default:default_max_message_size in
  if size > limit then
    Error (Message_size_exceeds_maximum { size; max_size = limit })
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
  let have = Bytes.length data in
  if have < 5 then
    Error (Incomplete_header { have })
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

  let have = Bytes.length data in
  if have < 5 then
    Error (Incomplete_header { have })
  else
    let* (compressed, length) = peek_header data in

    (* Validate message size to prevent DoS *)
    let* () = validate_size length ~max_size:None in

    let total_length = 5 + length in
    if have < total_length then
      Error (Incomplete_message { need = total_length; have })
    else
      let payload = Bytes.sub data 5 length in
      let remaining = Bytes.sub data total_length (have - total_length) in
      Ok ({ compressed; payload }, remaining)
