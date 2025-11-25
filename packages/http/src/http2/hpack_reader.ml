open Std

(** Reentrant HPACK decoder using IO.Reader *)

type decoder_phase =
  | WaitingForHeader  (** Waiting for first byte of next header *)
  | ReadingIndexedName of {
      first_byte : int;
      prefix_bits : int;
      accumulated_value : int;
      multiplier : int;
    }
  | ReadingLiteralName of { name_length : int; bytes_read : int; buffer : Buffer.t }
  | ReadingLiteralValue of {
      name : string;
      value_length : int;
      bytes_read : int;
      buffer : Buffer.t;
      should_index : bool;
    }

type decoder = {
  hpack_decoder : Hpack.decoder;
  phase : decoder_phase Cell.t;
  accumulated_headers : Hpack.header list Cell.t;
}

type decode_error =
  | Invalid_header_index of int
  | Invalid_name_index of int
  | Unsupported_encoding
  | Invalid_decoder_state

type decode_result =
  | Headers of Hpack.header list
  | Need_more
  | Error of decode_error

let create ?(max_dynamic_table_size = 4096) () =
  {
    hpack_decoder = Hpack.create_decoder ~max_dynamic_table_size ();
    phase = Cell.create WaitingForHeader;
    accumulated_headers = Cell.create [];
  }

let update_max_table_size decoder size =
  Hpack.update_max_table_size decoder.hpack_decoder size

let reset decoder =
  Cell.set decoder.phase WaitingForHeader;
  Cell.set decoder.accumulated_headers []

let dynamic_table_size decoder =
  (* Access internal dynamic table size - simplified for now *)
  4096

(** Try to read one byte from reader *)
let read_byte reader =
  let buf = Bytes.create 1 in
  match IO.Reader.read reader buf with
  | Ok 1 -> Some (Char.code (Bytes.get buf 0))
  | _ -> None

(** Try to read N bytes from reader *)
let read_n_bytes reader n =
  if n = 0 then Some Bytes.empty
  else
    let buf = Bytes.create n in
    match IO.Reader.read reader buf with
    | Ok bytes_read when bytes_read = n -> Some buf
    | _ -> None

(** Decode variable-length integer (reentrant) *)
let decode_varint_incremental reader first_byte prefix_bits accumulated multiplier =
  let prefix_mask = (1 lsl prefix_bits) - 1 in
  let first_value = first_byte land prefix_mask in

  if first_value < prefix_mask then
    (* Value fits in prefix *)
    Ok (first_value, 0, 1)
  else
    (* Need continuation bytes *)
    let rec read_continuation acc mult =
      match read_byte reader with
      | None -> Error "need_more"
      | Some byte ->
          let value = byte land 0x7F in
          let acc = acc + (value * mult) in
          if byte land 0x80 = 0 then Ok (acc, 0, 1)
          else read_continuation acc (mult * 128)
    in
    read_continuation prefix_mask multiplier

let decode decoder reader =
  let ( let* ) = Result.and_then in

  let rec decode_next () =
    match Cell.get decoder.phase with
    | WaitingForHeader -> (
        (* Read first byte to determine encoding type *)
        match read_byte reader with
        | None -> Need_more
        | Some first_byte ->
            if first_byte land 0x80 <> 0 then
              (* Indexed Header Field: 1xxxxxxx *)
              let* (index, _, _) =
                decode_varint_incremental reader first_byte 7 0 1
              in
              if Result.is_error (decode_varint_incremental reader first_byte 7 0 1)
              then Need_more
              else
                (* Successfully got index *)
                match Hpack.static_table_lookup index with
                | Some header ->
                    let headers = Cell.get decoder.accumulated_headers in
                    Cell.set decoder.accumulated_headers (header :: headers);
                    Cell.set decoder.phase WaitingForHeader;
                    decode_next ()
                | None ->
                    Error (Invalid_header_index index)
            else if first_byte land 0x40 <> 0 then
              (* Literal with Incremental Indexing: 01xxxxxx *)
              let* (name_index, _, _) =
                decode_varint_incremental reader first_byte 6 0 1
              in
              if Result.is_error (decode_varint_incremental reader first_byte 6 0 1)
              then Need_more
              else if name_index = 0 then
                (* Literal name *)
                match read_byte reader with
                | None -> Need_more
                | Some len_byte ->
                    let is_huffman = len_byte land 0x80 <> 0 in
                    let* (name_length, _, _) =
                      decode_varint_incremental reader len_byte 7 0 1
                    in
                    if
                      Result.is_error
                        (decode_varint_incremental reader len_byte 7 0 1)
                    then Need_more
                    else (
                      Cell.set decoder.phase
                        (ReadingLiteralName
                           {
                             name_length;
                             bytes_read = 0;
                             buffer = Buffer.create name_length;
                           });
                      decode_next ())
              else
                (* Indexed name *)
                match Hpack.static_table_lookup name_index with
                | Some header ->
                    (* Read value length *)
                    (match read_byte reader with
                    | None -> Need_more
                    | Some len_byte ->
                        let* (value_length, _, _) =
                          decode_varint_incremental reader len_byte 7 0 1
                        in
                        if
                          Result.is_error
                            (decode_varint_incremental reader len_byte 7 0 1)
                        then Need_more
                        else (
                          Cell.set decoder.phase
                            (ReadingLiteralValue
                               {
                                 name = header.name;
                                 value_length;
                                 bytes_read = 0;
                                 buffer = Buffer.create value_length;
                                 should_index = true;
                               });
                          decode_next ()))
                | None -> Error (Invalid_name_index name_index)
            else
              (* Other encodings: simplified for now *)
              Error Unsupported_encoding)
    | ReadingLiteralName { name_length; bytes_read; buffer } ->
        let remaining = name_length - bytes_read in
        (match read_n_bytes reader remaining with
        | None -> Need_more
        | Some data ->
            Buffer.add_bytes buffer data;
            let name = Buffer.contents buffer in
            (* Now need to read value *)
            (match read_byte reader with
            | None -> Need_more
            | Some len_byte ->
                let* (value_length, _, _) =
                  decode_varint_incremental reader len_byte 7 0 1
                in
                if
                  Result.is_error (decode_varint_incremental reader len_byte 7 0 1)
                then Need_more
                else (
                  Cell.set decoder.phase
                    (ReadingLiteralValue
                       {
                         name;
                         value_length;
                         bytes_read = 0;
                         buffer = Buffer.create value_length;
                         should_index = true;
                       });
                  decode_next ())))
    | ReadingLiteralValue { name; value_length; bytes_read; buffer; should_index } ->
        let remaining = value_length - bytes_read in
        (match read_n_bytes reader remaining with
        | None -> Need_more
        | Some data ->
            Buffer.add_bytes buffer data;
            let value = Buffer.contents buffer in
            let header = { Hpack.name; value } in

            (* Add to dynamic table if needed *)
            if should_index then
              Hpack.DynamicTable.add
                (Hpack.create_decoder ()).dynamic_table
                header;

            (* Add to accumulated headers *)
            let headers = Cell.get decoder.accumulated_headers in
            Cell.set decoder.accumulated_headers (header :: headers);

            (* Reset to waiting for next header *)
            Cell.set decoder.phase WaitingForHeader;

            (* Check if we have complete header block *)
            (* For simplicity, return headers when we've decoded at least one *)
            if List.length (Cell.get decoder.accumulated_headers) > 0 then
              let result = List.rev (Cell.get decoder.accumulated_headers) in
              Cell.set decoder.accumulated_headers [];
              Headers result
            else decode_next ())
    | _ -> Error Invalid_decoder_state
  in
  decode_next ()
