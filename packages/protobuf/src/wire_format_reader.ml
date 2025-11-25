open Std

type decode_error =
  | Unexpected_eof_reading_varint
  | Unexpected_eof_reading_i32
  | Unexpected_eof_reading_i64
  | Unexpected_eof_reading_length_delimited of int
  | Invalid_wire_type of int
  | Mismatched_group_end_tag of { expected : int; actual : int }
  | Unexpected_group_end_tag
  | Unsupported_encoding  (* Groups not supported in reentrant version *)

type decode_result =
  | Message of WireFormat.t
  | Need_more
  | Error of decode_error

(** Varint decoding state *)
type varint_state = {
  acc : int64;  (** Accumulated value *)
  shift : int;  (** Current bit shift *)
}

(** Decoder phases *)
type decoder_phase =
  | ReadingTag of {
      varint_state : varint_state option;
    }
  | ReadingVarint of {
      field_number : int;
      varint_state : varint_state;
    }
  | ReadingI32 of {
      field_number : int;
      buffer : Buffer.t;
      bytes_read : int;
    }
  | ReadingI64 of {
      field_number : int;
      buffer : Buffer.t;
      bytes_read : int;
    }
  | ReadingLenLength of {
      field_number : int;
      varint_state : varint_state;
    }
  | ReadingLenData of {
      field_number : int;
      length : int;
      buffer : Buffer.t;
      bytes_read : int;
    }
  | MessageComplete

type state = {
  phase : decoder_phase Cell.t;
  records : WireFormat.record list Cell.t;
}

let create () =
  {
    phase = Cell.create (ReadingTag { varint_state = None });
    records = Cell.create [];
  }

let reset state =
  Cell.set state.phase (ReadingTag { varint_state = None });
  Cell.set state.records []

(** Read single byte from reader *)
let read_byte reader =
  let buf = Bytes.create 1 in
  match IO.Reader.read reader buf with
  | Ok n when n = 1 -> Some (Bytes.get buf 0)
  | _ -> None

(** Read N bytes into buffer, returning number actually read *)
let read_n_bytes reader buffer n =
  let bytes = Bytes.create n in
  match IO.Reader.read reader bytes with
  | Ok bytes_read when bytes_read > 0 ->
      Buffer.add_subbytes buffer bytes 0 bytes_read;
      bytes_read
  | _ -> 0

(** Decode varint incrementally - returns (value, complete) *)
let decode_varint_step reader varint_state =
  match read_byte reader with
  | None -> (varint_state, false)  (* Need more data *)
  | Some byte ->
      let b = Char.code byte in
      let value = Int64.of_int (b land 0x7F) in
      let acc = Int64.logor varint_state.acc (Int64.shift_left value varint_state.shift) in
      let complete = (b land 0x80) = 0 in
      ({ acc; shift = varint_state.shift + 7 }, complete)

(** Parse 4-byte i32 from buffer *)
let parse_i32 data =
  if String.length data < 4 then None
  else
    let b0 = Int32.of_int (Char.code data.[0]) in
    let b1 = Int32.of_int (Char.code data.[1]) in
    let b2 = Int32.of_int (Char.code data.[2]) in
    let b3 = Int32.of_int (Char.code data.[3]) in
    let open Int32 in
    let result =
      logor (logor (logor b0 (shift_left b1 8)) (shift_left b2 16)) (shift_left b3 24)
    in
    Some result

(** Parse 8-byte i64 from buffer *)
let parse_i64 data =
  if String.length data < 8 then None
  else
    let b0 = Int64.of_int (Char.code data.[0]) in
    let b1 = Int64.of_int (Char.code data.[1]) in
    let b2 = Int64.of_int (Char.code data.[2]) in
    let b3 = Int64.of_int (Char.code data.[3]) in
    let b4 = Int64.of_int (Char.code data.[4]) in
    let b5 = Int64.of_int (Char.code data.[5]) in
    let b6 = Int64.of_int (Char.code data.[6]) in
    let b7 = Int64.of_int (Char.code data.[7]) in
    let open Int64 in
    let result =
      logor
        (logor (logor (logor b0 (shift_left b1 8)) (shift_left b2 16)) (shift_left b3 24))
        (logor
           (logor (shift_left b4 32) (shift_left b5 40))
           (logor (shift_left b6 48) (shift_left b7 56)))
    in
    Some result

(** Decode tag to (field_number, wire_type) *)
let decode_tag tag_value =
  let wire_type_int = Int64.to_int (Int64.logand tag_value 7L) in
  let field_number = Int64.to_int (Int64.shift_right_logical tag_value 3) in
  match wire_type_int with
  | 0 -> Ok (field_number, WireFormat.WtVarint)
  | 1 -> Ok (field_number, WireFormat.WtI64)
  | 2 -> Ok (field_number, WireFormat.WtLen)
  | 3 -> Ok (field_number, WireFormat.WtSgroup)
  | 4 -> Ok (field_number, WireFormat.WtEgroup)
  | 5 -> Ok (field_number, WireFormat.WtI32)
  | _ -> Error (Invalid_wire_type wire_type_int)

let decode state reader =
  let rec decode_next () =
    match Cell.get state.phase with
    | ReadingTag { varint_state } -> (
        let vs = Option.value varint_state ~default:{ acc = 0L; shift = 0 } in
        let (new_vs, complete) = decode_varint_step reader vs in

        if not complete then (
          Cell.set state.phase (ReadingTag { varint_state = Some new_vs });
          Need_more)
        else
          (* Tag complete - decode it *)
          match decode_tag new_vs.acc with
          | Error e -> Error e
          | Ok (field_number, wire_type) -> (
              match wire_type with
              | WireFormat.WtVarint ->
                  Cell.set state.phase
                    (ReadingVarint { field_number; varint_state = { acc = 0L; shift = 0 } });
                  decode_next ()
              | WireFormat.WtI32 ->
                  Cell.set state.phase
                    (ReadingI32 { field_number; buffer = Buffer.create 4; bytes_read = 0 });
                  decode_next ()
              | WireFormat.WtI64 ->
                  Cell.set state.phase
                    (ReadingI64 { field_number; buffer = Buffer.create 8; bytes_read = 0 });
                  decode_next ()
              | WireFormat.WtLen ->
                  Cell.set state.phase
                    (ReadingLenLength { field_number; varint_state = { acc = 0L; shift = 0 } });
                  decode_next ()
              | WireFormat.WtEgroup ->
                  Error Unexpected_group_end_tag
              | WireFormat.WtSgroup ->
                  (* Groups not fully supported in reentrant version *)
                  Error Unsupported_encoding))

    | ReadingVarint { field_number; varint_state } -> (
        let (new_vs, complete) = decode_varint_step reader varint_state in

        if not complete then (
          Cell.set state.phase (ReadingVarint { field_number; varint_state = new_vs });
          Need_more)
        else (
          (* Varint complete *)
          let record = {
            WireFormat.field_number;
            value = WireFormat.Varint (WireFormat.Uint64 new_vs.acc);
          } in
          let records = Cell.get state.records in
          Cell.set state.records (record :: records);
          Cell.set state.phase (ReadingTag { varint_state = None });
          decode_next ()))

    | ReadingI32 { field_number; buffer; bytes_read } ->
        let needed = 4 - bytes_read in
        let actual_read = read_n_bytes reader buffer needed in

        if actual_read = 0 then Need_more
        else if bytes_read + actual_read < 4 then (
          Cell.set state.phase
            (ReadingI32 { field_number; buffer; bytes_read = bytes_read + actual_read });
          Need_more)
        else (
          (* Have complete 4 bytes *)
          let data = Buffer.contents buffer in
          match parse_i32 data with
          | None -> Error Unexpected_eof_reading_i32
          | Some i32_val ->
              let record = {
                WireFormat.field_number;
                value = WireFormat.I32 (WireFormat.Fixed32 i32_val);
              } in
              let records = Cell.get state.records in
              Cell.set state.records (record :: records);
              Cell.set state.phase (ReadingTag { varint_state = None });
              decode_next ())

    | ReadingI64 { field_number; buffer; bytes_read } ->
        let needed = 8 - bytes_read in
        let actual_read = read_n_bytes reader buffer needed in

        if actual_read = 0 then Need_more
        else if bytes_read + actual_read < 8 then (
          Cell.set state.phase
            (ReadingI64 { field_number; buffer; bytes_read = bytes_read + actual_read });
          Need_more)
        else (
          (* Have complete 8 bytes *)
          let data = Buffer.contents buffer in
          match parse_i64 data with
          | None -> Error Unexpected_eof_reading_i64
          | Some i64_val ->
              let record = {
                WireFormat.field_number;
                value = WireFormat.I64 (WireFormat.Fixed64 i64_val);
              } in
              let records = Cell.get state.records in
              Cell.set state.records (record :: records);
              Cell.set state.phase (ReadingTag { varint_state = None });
              decode_next ())

    | ReadingLenLength { field_number; varint_state } -> (
        let (new_vs, complete) = decode_varint_step reader varint_state in

        if not complete then (
          Cell.set state.phase (ReadingLenLength { field_number; varint_state = new_vs });
          Need_more)
        else (
          (* Length complete - now read that many bytes *)
          let length = Int64.to_int new_vs.acc in
          if length = 0 then (
            (* Zero-length field *)
            let record = {
              WireFormat.field_number;
              value = WireFormat.Len (WireFormat.Bytes Bytes.empty);
            } in
            let records = Cell.get state.records in
            Cell.set state.records (record :: records);
            Cell.set state.phase (ReadingTag { varint_state = None });
            decode_next ())
          else (
            Cell.set state.phase
              (ReadingLenData { field_number; length; buffer = Buffer.create length; bytes_read = 0 });
            decode_next ())))

    | ReadingLenData { field_number; length; buffer; bytes_read } ->
        let needed = length - bytes_read in
        let actual_read = read_n_bytes reader buffer needed in

        if actual_read = 0 then Need_more
        else if bytes_read + actual_read < length then (
          Cell.set state.phase
            (ReadingLenData { field_number; length; buffer; bytes_read = bytes_read + actual_read });
          Need_more)
        else (
          (* Have complete length-delimited data *)
          let data = Buffer.contents buffer in
          let data_bytes = Bytes.of_string data in

          (* Try to decode as nested message, fall back to bytes *)
          let value =
            match WireFormat.decode data_bytes with
            | Ok nested_records -> WireFormat.Len (WireFormat.Message nested_records)
            | Error _ -> WireFormat.Len (WireFormat.Bytes data_bytes)
          in

          let record = { WireFormat.field_number; value } in
          let records = Cell.get state.records in
          Cell.set state.records (record :: records);
          Cell.set state.phase (ReadingTag { varint_state = None });
          decode_next ())

    | MessageComplete ->
        let records = List.rev (Cell.get state.records) in
        reset state;
        Message records
  in

  (* Check if we're at EOF - if so and we have records, complete *)
  match Cell.get state.phase with
  | ReadingTag { varint_state = None } when List.length (Cell.get state.records) > 0 ->
      (* Try to read a byte to see if we're at EOF *)
      (match read_byte reader with
      | None ->
          (* EOF - complete the message *)
          Cell.set state.phase MessageComplete;
          decode_next ()
      | Some byte ->
          (* Have data - put it back conceptually by starting varint decode *)
          let b = Char.code byte in
          let value = Int64.of_int (b land 0x7F) in
          let complete = (b land 0x80) = 0 in
          if complete then (
            (* Single-byte tag *)
            let varint_state = { acc = value; shift = 7 } in
            Cell.set state.phase (ReadingTag { varint_state = Some varint_state });
            decode_next ())
          else (
            let varint_state = { acc = value; shift = 7 } in
            Cell.set state.phase (ReadingTag { varint_state = Some varint_state });
            decode_next ()))
  | ReadingTag { varint_state = None } when List.length (Cell.get state.records) = 0 ->
      (* Empty message at start *)
      decode_next ()
  | _ ->
      (* Continue decoding from current phase *)
      decode_next ()
