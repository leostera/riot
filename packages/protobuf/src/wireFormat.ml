open Std

type wire_type = WtVarint | WtI64 | WtLen | WtSgroup | WtEgroup | WtI32

type varint_value =
  | Int32 of int32
  | Int64 of int64
  | Uint32 of int32
  | Uint64 of int64
  | Sint32 of int32
  | Sint64 of int64
  | Bool of bool
  | Enum of int32

type i64_value = Fixed64 of int64 | Sfixed64 of int64 | Double of float
type i32_value = Fixed32 of int32 | Sfixed32 of int32 | Float of float

type len_value =
  | String of string
  | Bytes of bytes
  | Message of record list
  | PackedVarint of int64 list
  | PackedI32 of int32 list
  | PackedI64 of int64 list

and value =
  | Varint of varint_value
  | I64 of i64_value
  | I32 of i32_value
  | Len of len_value
  | Group of record list

and record = { field_number : int; value : value }

type t = record list

type decode_error =
  | Unexpected_eof_reading_varint
  | Unexpected_eof_reading_i32
  | Unexpected_eof_reading_i64
  | Unexpected_eof_reading_length_delimited of int
  | Invalid_wire_type of int
  | Mismatched_group_end_tag of { expected : int; actual : int }
  | Unexpected_group_end_tag

module ByteCursor = struct
  type t = { source : bytes; mutable pos : int; length : int }

  let create source = { source; pos = 0; length = Stdlib.Bytes.length source }
  let is_eof t = t.pos >= t.length
  let position t = t.pos
  let remaining t = t.length - t.pos

  let read_byte t =
    if is_eof t then None
    else
      let b = Stdlib.Bytes.get t.source t.pos in
      t.pos <- t.pos + 1;
      Some b

  let read_bytes t n =
    if t.pos + n > t.length then None
    else
      let result = Stdlib.Bytes.sub t.source t.pos n in
      t.pos <- t.pos + n;
      Some result

  let peek_byte t =
    if is_eof t then None else Some (Stdlib.Bytes.get t.source t.pos)
end

module Decoder = struct
  let decode_varint cursor =
    let rec loop acc shift =
      match ByteCursor.read_byte cursor with
      | None -> Error Unexpected_eof_reading_varint
      | Some byte ->
          let b = Char.code byte in
          let value = Int64.of_int (b land 0x7F) in
          let acc = Int64.logor acc (Int64.shift_left value shift) in
          if b land 0x80 = 0 then Ok acc else loop acc (shift + 7)
    in
    loop 0L 0

  let decode_zigzag n =
    let open Int64 in
    logxor (shift_right_logical n 1) (neg (logand n 1L))

  let decode_tag cursor =
    match decode_varint cursor with
    | Error e -> Error e
    | Ok tag_value -> (
        let wire_type_int = Int64.to_int (Int64.logand tag_value 7L) in
        let field_number =
          Int64.to_int (Int64.shift_right_logical tag_value 3)
        in
        match wire_type_int with
        | 0 -> Ok (field_number, WtVarint)
        | 1 -> Ok (field_number, WtI64)
        | 2 -> Ok (field_number, WtLen)
        | 3 -> Ok (field_number, WtSgroup)
        | 4 -> Ok (field_number, WtEgroup)
        | 5 -> Ok (field_number, WtI32)
        | _ -> Error (Invalid_wire_type wire_type_int))

  let decode_i32 cursor =
    match ByteCursor.read_bytes cursor 4 with
    | None -> Error Unexpected_eof_reading_i32
    | Some bytes ->
        let b0 = Int32.of_int (Char.code (Stdlib.Bytes.get bytes 0)) in
        let b1 = Int32.of_int (Char.code (Stdlib.Bytes.get bytes 1)) in
        let b2 = Int32.of_int (Char.code (Stdlib.Bytes.get bytes 2)) in
        let b3 = Int32.of_int (Char.code (Stdlib.Bytes.get bytes 3)) in
        let open Int32 in
        let result =
          logor
            (logor (logor b0 (shift_left b1 8)) (shift_left b2 16))
            (shift_left b3 24)
        in
        Ok result

  let decode_i64 cursor =
    match ByteCursor.read_bytes cursor 8 with
    | None -> Error Unexpected_eof_reading_i64
    | Some bytes ->
        let b0 = Int64.of_int (Char.code (Stdlib.Bytes.get bytes 0)) in
        let b1 = Int64.of_int (Char.code (Stdlib.Bytes.get bytes 1)) in
        let b2 = Int64.of_int (Char.code (Stdlib.Bytes.get bytes 2)) in
        let b3 = Int64.of_int (Char.code (Stdlib.Bytes.get bytes 3)) in
        let b4 = Int64.of_int (Char.code (Stdlib.Bytes.get bytes 4)) in
        let b5 = Int64.of_int (Char.code (Stdlib.Bytes.get bytes 5)) in
        let b6 = Int64.of_int (Char.code (Stdlib.Bytes.get bytes 6)) in
        let b7 = Int64.of_int (Char.code (Stdlib.Bytes.get bytes 7)) in
        let open Int64 in
        let result =
          logor
            (logor
               (logor (logor b0 (shift_left b1 8)) (shift_left b2 16))
               (shift_left b3 24))
            (logor
               (logor (shift_left b4 32) (shift_left b5 40))
               (logor (shift_left b6 48) (shift_left b7 56)))
        in
        Ok result

  let decode_float i32_val = Int32.float_of_bits i32_val
  let decode_double i64_val = Int64.float_of_bits i64_val

  let rec decode_message cursor =
    if ByteCursor.is_eof cursor then Ok []
    else
      match decode_record cursor with
      | Error e -> Error e
      | Ok record -> (
          match decode_message cursor with
          | Error e -> Error e
          | Ok rest -> Ok (record :: rest))

  and decode_record cursor =
    match decode_tag cursor with
    | Error e -> Error e
    | Ok (field_number, wire_type) -> (
        match wire_type with
        | WtVarint -> (
            match decode_varint cursor with
            | Error e -> Error e
            | Ok v -> Ok { field_number; value = Varint (Uint64 v) })
        | WtI64 -> (
            match decode_i64 cursor with
            | Error e -> Error e
            | Ok v -> Ok { field_number; value = I64 (Fixed64 v) })
        | WtI32 -> (
            match decode_i32 cursor with
            | Error e -> Error e
            | Ok v -> Ok { field_number; value = I32 (Fixed32 v) })
        | WtLen -> (
            match decode_varint cursor with
            | Error e -> Error e
            | Ok len_val -> (
                let len = Int64.to_int len_val in
                match ByteCursor.read_bytes cursor len with
                | None ->
                    Error (Unexpected_eof_reading_length_delimited len)
                | Some data -> (
                    let nested_cursor = ByteCursor.create data in
                    match decode_message nested_cursor with
                    | Ok records ->
                        Ok { field_number; value = Len (Message records) }
                    | Error _ -> Ok { field_number; value = Len (Bytes data) }))
            )
        | WtSgroup -> (
            let records = Cell.create [] in
            let rec read_group () =
              match decode_tag cursor with
              | Error e -> Error e
              | Ok (fn, WtEgroup) ->
                  if fn = field_number then Ok (Cell.get records)
                  else Error (Mismatched_group_end_tag { expected = field_number; actual = fn })
              | Ok _ -> (
                  match decode_record cursor with
                  | Error e -> Error e
                  | Ok record ->
                      Cell.set records (Cell.get records @ [ record ]);
                      read_group ())
            in
            match read_group () with
            | Error e -> Error e
            | Ok recs -> Ok { field_number; value = Group recs })
        | WtEgroup -> Error Unexpected_group_end_tag)

  let decode bytes =
    let cursor = ByteCursor.create bytes in
    decode_message cursor
end

module Encoder = struct
  let encode_varint value =
    let rec loop acc v =
      if Int64.compare v 0L = 0 then List.rev acc
      else
        let byte = Int64.to_int (Int64.logand v 0x7FL) in
        let v' = Int64.shift_right_logical v 7 in
        if Int64.compare v' 0L = 0 then List.rev (byte :: acc)
        else loop ((byte lor 0x80) :: acc) v'
    in
    let bytes = loop [] value in
    let result = Stdlib.Bytes.create (List.length bytes) in
    List.iteri (fun i b -> Stdlib.Bytes.set result i (Char.chr b)) bytes;
    result

  let encode_zigzag n =
    let open Int64 in
    logxor (shift_left n 1) (shift_right n 63)

  let encode_tag field_number wire_type =
    let wire_type_int =
      match wire_type with
      | WtVarint -> 0
      | WtI64 -> 1
      | WtLen -> 2
      | WtSgroup -> 3
      | WtEgroup -> 4
      | WtI32 -> 5
    in
    let tag = (field_number lsl 3) lor wire_type_int in
    encode_varint (Int64.of_int tag)

  let encode_i32 value =
    let result = Stdlib.Bytes.create 4 in
    let open Int32 in
    Stdlib.Bytes.set result 0 (Char.chr (to_int (logand value 0xFFl)));
    Stdlib.Bytes.set result 1
      (Char.chr (to_int (logand (shift_right_logical value 8) 0xFFl)));
    Stdlib.Bytes.set result 2
      (Char.chr (to_int (logand (shift_right_logical value 16) 0xFFl)));
    Stdlib.Bytes.set result 3
      (Char.chr (to_int (logand (shift_right_logical value 24) 0xFFl)));
    result

  let encode_i64 value =
    let result = Stdlib.Bytes.create 8 in
    let open Int64 in
    Stdlib.Bytes.set result 0 (Char.chr (to_int (logand value 0xFFL)));
    Stdlib.Bytes.set result 1
      (Char.chr (to_int (logand (shift_right_logical value 8) 0xFFL)));
    Stdlib.Bytes.set result 2
      (Char.chr (to_int (logand (shift_right_logical value 16) 0xFFL)));
    Stdlib.Bytes.set result 3
      (Char.chr (to_int (logand (shift_right_logical value 24) 0xFFL)));
    Stdlib.Bytes.set result 4
      (Char.chr (to_int (logand (shift_right_logical value 32) 0xFFL)));
    Stdlib.Bytes.set result 5
      (Char.chr (to_int (logand (shift_right_logical value 40) 0xFFL)));
    Stdlib.Bytes.set result 6
      (Char.chr (to_int (logand (shift_right_logical value 48) 0xFFL)));
    Stdlib.Bytes.set result 7
      (Char.chr (to_int (logand (shift_right_logical value 56) 0xFFL)));
    result

  let encode_float f = encode_i32 (Int32.bits_of_float f)
  let encode_double f = encode_i64 (Int64.bits_of_float f)

  let rec encode_message records =
    let parts = List.map encode_record records in
    let total_len =
      List.fold_left (fun acc b -> acc + Stdlib.Bytes.length b) 0 parts
    in
    let result = Stdlib.Bytes.create total_len in
    let _ =
      List.fold_left
        (fun pos part ->
          Stdlib.Bytes.blit part 0 result pos (Stdlib.Bytes.length part);
          pos + Stdlib.Bytes.length part)
        0 parts
    in
    result

  and encode_record { field_number; value } =
    match value with
    | Varint (Uint64 v) ->
        let tag = encode_tag field_number WtVarint in
        let data = encode_varint v in
        Stdlib.Bytes.cat tag data
    | Varint (Int64 v) ->
        let tag = encode_tag field_number WtVarint in
        let data = encode_varint v in
        Stdlib.Bytes.cat tag data
    | Varint (Sint64 v) ->
        let tag = encode_tag field_number WtVarint in
        let zigzag = encode_zigzag v in
        let data = encode_varint zigzag in
        Stdlib.Bytes.cat tag data
    | Varint (Uint32 v) ->
        let tag = encode_tag field_number WtVarint in
        let data = encode_varint (Int64.of_int32 v) in
        Stdlib.Bytes.cat tag data
    | Varint (Int32 v) ->
        let tag = encode_tag field_number WtVarint in
        let data = encode_varint (Int64.of_int32 v) in
        Stdlib.Bytes.cat tag data
    | Varint (Sint32 v) ->
        let tag = encode_tag field_number WtVarint in
        let zigzag = encode_zigzag (Int64.of_int32 v) in
        let data = encode_varint zigzag in
        Stdlib.Bytes.cat tag data
    | Varint (Bool b) ->
        let tag = encode_tag field_number WtVarint in
        let data = encode_varint (if b then 1L else 0L) in
        Stdlib.Bytes.cat tag data
    | Varint (Enum e) ->
        let tag = encode_tag field_number WtVarint in
        let data = encode_varint (Int64.of_int32 e) in
        Stdlib.Bytes.cat tag data
    | I64 (Fixed64 v) ->
        let tag = encode_tag field_number WtI64 in
        let data = encode_i64 v in
        Stdlib.Bytes.cat tag data
    | I64 (Sfixed64 v) ->
        let tag = encode_tag field_number WtI64 in
        let data = encode_i64 v in
        Stdlib.Bytes.cat tag data
    | I64 (Double f) ->
        let tag = encode_tag field_number WtI64 in
        let data = encode_double f in
        Stdlib.Bytes.cat tag data
    | I32 (Fixed32 v) ->
        let tag = encode_tag field_number WtI32 in
        let data = encode_i32 v in
        Stdlib.Bytes.cat tag data
    | I32 (Sfixed32 v) ->
        let tag = encode_tag field_number WtI32 in
        let data = encode_i32 v in
        Stdlib.Bytes.cat tag data
    | I32 (Float f) ->
        let tag = encode_tag field_number WtI32 in
        let data = encode_float f in
        Stdlib.Bytes.cat tag data
    | Len (String s) ->
        let tag = encode_tag field_number WtLen in
        let str_bytes = Stdlib.Bytes.of_string s in
        let len =
          encode_varint (Int64.of_int (Stdlib.Bytes.length str_bytes))
        in
        Stdlib.Bytes.cat tag (Stdlib.Bytes.cat len str_bytes)
    | Len (Bytes b) ->
        let tag = encode_tag field_number WtLen in
        let len = encode_varint (Int64.of_int (Stdlib.Bytes.length b)) in
        Stdlib.Bytes.cat tag (Stdlib.Bytes.cat len b)
    | Len (Message records) ->
        let tag = encode_tag field_number WtLen in
        let msg_bytes = encode_message records in
        let len =
          encode_varint (Int64.of_int (Stdlib.Bytes.length msg_bytes))
        in
        Stdlib.Bytes.cat tag (Stdlib.Bytes.cat len msg_bytes)
    | Len (PackedVarint values) ->
        let tag = encode_tag field_number WtLen in
        let packed =
          List.fold_left
            (fun acc v -> Stdlib.Bytes.cat acc (encode_varint v))
            Stdlib.Bytes.empty values
        in
        let len = encode_varint (Int64.of_int (Stdlib.Bytes.length packed)) in
        Stdlib.Bytes.cat tag (Stdlib.Bytes.cat len packed)
    | Len (PackedI32 values) ->
        let tag = encode_tag field_number WtLen in
        let packed =
          List.fold_left
            (fun acc v -> Stdlib.Bytes.cat acc (encode_i32 v))
            Stdlib.Bytes.empty values
        in
        let len = encode_varint (Int64.of_int (Stdlib.Bytes.length packed)) in
        Stdlib.Bytes.cat tag (Stdlib.Bytes.cat len packed)
    | Len (PackedI64 values) ->
        let tag = encode_tag field_number WtLen in
        let packed =
          List.fold_left
            (fun acc v -> Stdlib.Bytes.cat acc (encode_i64 v))
            Stdlib.Bytes.empty values
        in
        let len = encode_varint (Int64.of_int (Stdlib.Bytes.length packed)) in
        Stdlib.Bytes.cat tag (Stdlib.Bytes.cat len packed)
    | Group records ->
        let start_tag = encode_tag field_number WtSgroup in
        let body = encode_message records in
        let end_tag = encode_tag field_number WtEgroup in
        Stdlib.Bytes.cat start_tag (Stdlib.Bytes.cat body end_tag)

  let encode records = encode_message records
end

let decode bytes = Decoder.decode bytes
let encode records = Encoder.encode records
