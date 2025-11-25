open Std

(** HPACK: Header Compression for HTTP/2 (RFC 7541) *)

type header = { name : string; value : string }

type encoding_type =
  | Indexed
  | LiteralWithIndexing
  | LiteralWithoutIndexing
  | LiteralNeverIndexed

(** {1 Static Table}

    RFC 7541 Appendix A: Static Table Definition
    The static table consists of 61 common HTTP header fields.
    Indexes are 1-based (index 0 is invalid).
*)

let static_table =
  [|
    { name = ":authority"; value = "" };
    { name = ":method"; value = "GET" };
    { name = ":method"; value = "POST" };
    { name = ":path"; value = "/" };
    { name = ":path"; value = "/index.html" };
    { name = ":scheme"; value = "http" };
    { name = ":scheme"; value = "https" };
    { name = ":status"; value = "200" };
    { name = ":status"; value = "204" };
    { name = ":status"; value = "206" };
    { name = ":status"; value = "304" };
    { name = ":status"; value = "400" };
    { name = ":status"; value = "404" };
    { name = ":status"; value = "500" };
    { name = "accept-charset"; value = "" };
    { name = "accept-encoding"; value = "gzip, deflate" };
    { name = "accept-language"; value = "" };
    { name = "accept-ranges"; value = "" };
    { name = "accept"; value = "" };
    { name = "access-control-allow-origin"; value = "" };
    { name = "age"; value = "" };
    { name = "allow"; value = "" };
    { name = "authorization"; value = "" };
    { name = "cache-control"; value = "" };
    { name = "content-disposition"; value = "" };
    { name = "content-encoding"; value = "" };
    { name = "content-language"; value = "" };
    { name = "content-length"; value = "" };
    { name = "content-location"; value = "" };
    { name = "content-range"; value = "" };
    { name = "content-type"; value = "" };
    { name = "cookie"; value = "" };
    { name = "date"; value = "" };
    { name = "etag"; value = "" };
    { name = "expect"; value = "" };
    { name = "expires"; value = "" };
    { name = "from"; value = "" };
    { name = "host"; value = "" };
    { name = "if-match"; value = "" };
    { name = "if-modified-since"; value = "" };
    { name = "if-none-match"; value = "" };
    { name = "if-range"; value = "" };
    { name = "if-unmodified-since"; value = "" };
    { name = "last-modified"; value = "" };
    { name = "link"; value = "" };
    { name = "location"; value = "" };
    { name = "max-forwards"; value = "" };
    { name = "proxy-authenticate"; value = "" };
    { name = "proxy-authorization"; value = "" };
    { name = "range"; value = "" };
    { name = "referer"; value = "" };
    { name = "refresh"; value = "" };
    { name = "retry-after"; value = "" };
    { name = "server"; value = "" };
    { name = "set-cookie"; value = "" };
    { name = "strict-transport-security"; value = "" };
    { name = "transfer-encoding"; value = "" };
    { name = "user-agent"; value = "" };
    { name = "vary"; value = "" };
    { name = "via"; value = "" };
    { name = "www-authenticate"; value = "" };
  |]

let static_table_size = Array.length static_table

let static_table_lookup index =
  if index >= 1 && index <= static_table_size then
    Some static_table.(index - 1)
  else None

let static_table_find ~name ~value =
  let rec loop i =
    if i >= static_table_size then None
    else
      let entry = static_table.(i) in
      if String.equal entry.name name && String.equal entry.value value then
        Some (i + 1)
      else loop (i + 1)
  in
  loop 0

let static_table_find_name name =
  let rec loop i =
    if i >= static_table_size then None
    else if String.equal static_table.(i).name name then Some (i + 1)
    else loop (i + 1)
  in
  loop 0

(** {1 Dynamic Table}

    The dynamic table is a FIFO queue of recently seen headers.
    Entries are evicted when the table exceeds its maximum size.

    Per RFC 7541 Section 4.1:
    - Table size = sum of entry sizes
    - Entry size = length(name) + length(value) + 32 bytes overhead
*)

let header_size header = String.length header.name + String.length header.value + 32

module DynamicTable = struct
  type t = {
    entries : header list Cell.t;
    current_size : int Cell.t;
    max_size : int Cell.t;
  }

  let create max_size =
    { entries = Cell.create []; current_size = Cell.create 0; max_size = Cell.create max_size }

  let size t = Cell.get t.current_size
  let max_size t = Cell.get t.max_size
  let entries t = Cell.get t.entries

  let rec evict_to_fit t new_entry_size =
    let current = Cell.get t.current_size in
    let max = Cell.get t.max_size in
    if current + new_entry_size <= max then ()
    else
      (* Remove oldest entry (last in list) *)
      match List.rev (Cell.get t.entries) with
      | [] -> ()
      | oldest :: rest ->
          let remaining = List.rev rest in
          Cell.set t.entries remaining;
          Cell.set t.current_size (current - header_size oldest);
          evict_to_fit t new_entry_size

  let add t header =
    let entry_size = header_size header in
    (* RFC 7541 Section 4.4: If entry is larger than max size, empty the table *)
    if entry_size > Cell.get t.max_size then (
      Cell.set t.entries [];
      Cell.set t.current_size 0)
    else (
      evict_to_fit t entry_size;
      let new_entries = header :: Cell.get t.entries in
      Cell.set t.entries new_entries;
      Cell.set t.current_size (Cell.get t.current_size + entry_size))

  let lookup t index =
    let entries = Cell.get t.entries in
    if index >= 1 && index <= List.length entries then
      Some (List.nth entries (index - 1))
    else None

  let find t ~name ~value =
    let entries = Cell.get t.entries in
    let rec loop i = function
      | [] -> None
      | hdr :: rest ->
          if String.equal hdr.name name && String.equal hdr.value value then
            Some i
          else loop (i + 1) rest
    in
    loop 1 entries

  let find_name t name =
    let entries = Cell.get t.entries in
    let rec loop i = function
      | [] -> None
      | hdr :: rest ->
          if String.equal hdr.name name then Some i
          else loop (i + 1) rest
    in
    loop 1 entries

  let update_max_size t new_max =
    Cell.set t.max_size new_max;
    (* Evict entries if necessary *)
    evict_to_fit t 0
end

(** {1 Integer Encoding/Decoding}

    RFC 7541 Section 5.1: Integer representation with variable length encoding.

    Integers are encoded with a prefix of N bits:
    - If value < 2^N - 1: encode directly in N bits
    - Otherwise: encode 2^N - 1 in N bits, then encode (value - (2^N - 1)) as a series of bytes
*)

module Integer = struct
  let encode prefix_bits value =
    let max_prefix = (1 lsl prefix_bits) - 1 in
    if value < max_prefix then
      (* Fits in prefix *)
      Bytes.make 1 (Char.chr value)
    else
      (* Doesn't fit, use continuation bytes *)
      let buf = Buffer.create 8 in
      Buffer.add_char buf (Char.chr max_prefix);
      let remaining = Cell.create (value - max_prefix) in
      while Cell.get remaining >= 128 do
        let byte = (Cell.get remaining land 0x7F) lor 0x80 in
        Buffer.add_char buf (Char.chr byte);
        Cell.set remaining (Cell.get remaining lsr 7)
      done;
      Buffer.add_char buf (Char.chr (Cell.get remaining));
      Buffer.to_bytes buf

  let decode prefix_bits first_byte data offset =
    let prefix_mask = (1 lsl prefix_bits) - 1 in
    let first_value = Char.code first_byte land prefix_mask in
    if first_value < prefix_mask then
      (* Value fits in prefix *)
      Ok (first_value, offset)
    else
      (* Need to read continuation bytes *)
      let rec read_continuation acc multiplier pos =
        if pos >= Bytes.length data then
          Error "Incomplete integer encoding"
        else
          let byte = Char.code (Bytes.get data pos) in
          let value = byte land 0x7F in
          let acc = acc + (value * multiplier) in
          if byte land 0x80 = 0 then Ok (acc, pos + 1)
          else read_continuation acc (multiplier * 128) (pos + 1)
      in
      match read_continuation prefix_mask 1 offset with
      | Ok (value, new_offset) -> Ok (value, new_offset)
      | Error e -> Error e
end

(** {1 String Encoding/Decoding}

    RFC 7541 Section 5.2: String literal representation.

    Strings can be encoded either:
    - As plain octets (Huffman bit = 0)
    - Using Huffman encoding (Huffman bit = 1)

    Format: [H bit | 7-bit length prefix] [length bytes] [string data]

    Note: Full Huffman implementation deferred to huffman.ml
*)

module String_ = struct
  let encode ?(use_huffman = false) str =
    let len = String.length str in
    (* TODO: Implement Huffman encoding *)
    (* For now, always use plain encoding *)
    let len_bytes = Integer.encode 7 len in
    let buf = Buffer.create (Bytes.length len_bytes + len) in
    Buffer.add_bytes buf len_bytes;
    Buffer.add_string buf str;
    Buffer.to_bytes buf

  let decode data offset =
    if offset >= Bytes.length data then Error "Incomplete string encoding"
    else
      let first_byte = Bytes.get data offset in
      let is_huffman = Char.code first_byte land 0x80 <> 0 in
      match Integer.decode 7 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (length, new_offset) ->
          if new_offset + length > Bytes.length data then
            Error "String data truncated"
          else
            let str_data = Bytes.sub data new_offset length in
            let str =
              if is_huffman then
                (* TODO: Implement Huffman decoding *)
                Bytes.to_string str_data
              else Bytes.to_string str_data
            in
            Ok (str, new_offset + length)
end

(** {1 Encoder} *)

type encoder = {
  dynamic_table : DynamicTable.t;
  sensitive_headers : string list Cell.t;
}

let create_encoder ?(max_dynamic_table_size = 4096) () =
  {
    dynamic_table = DynamicTable.create max_dynamic_table_size;
    sensitive_headers = Cell.create [ "authorization"; "cookie"; "set-cookie" ];
  }

let update_max_table_size encoder new_size =
  DynamicTable.update_max_size encoder.dynamic_table new_size

let is_sensitive_header name =
  List.mem name [ "authorization"; "cookie"; "set-cookie"; "proxy-authorization" ]

let encode_indexed_header index =
  (* Indexed Header Field: 1xxxxxxx *)
  let prefix_byte = 0x80 in
  let index_bytes = Integer.encode 7 index in
  let result = Bytes.create (Bytes.length index_bytes) in
  Bytes.set result 0 (Char.chr (prefix_byte lor Char.code (Bytes.get index_bytes 0)));
  Bytes.blit index_bytes 1 result 1 (Bytes.length index_bytes - 1);
  result

let encode_literal_with_indexing ~name_index ~value =
  (* Literal Header Field with Incremental Indexing: 01xxxxxx *)
  let buf = Buffer.create 64 in
  match name_index with
  | Some index ->
      (* Name is indexed *)
      let prefix_byte = 0x40 in
      let index_bytes = Integer.encode 6 index in
      Buffer.add_char buf (Char.chr (prefix_byte lor Char.code (Bytes.get index_bytes 0)));
      Buffer.add_bytes buf (Bytes.sub index_bytes 1 (Bytes.length index_bytes - 1));
      Buffer.add_bytes buf (String_.encode value);
      Buffer.to_bytes buf
  | None ->
      (* Name is not indexed *)
      Buffer.add_char buf '\x40';
      (* Index 0 means literal name *)
      Buffer.add_bytes buf (String_.encode value);
      Buffer.to_bytes buf

let encode_literal_without_indexing ~name_index ~value =
  (* Literal Header Field without Indexing: 0000xxxx *)
  let buf = Buffer.create 64 in
  match name_index with
  | Some index ->
      let index_bytes = Integer.encode 4 index in
      Buffer.add_bytes buf index_bytes;
      Buffer.add_bytes buf (String_.encode value);
      Buffer.to_bytes buf
  | None ->
      Buffer.add_char buf '\x00';
      Buffer.add_bytes buf (String_.encode value);
      Buffer.to_bytes buf

let encode_literal_never_indexed ~name_index ~value =
  (* Literal Header Field Never Indexed: 0001xxxx *)
  let buf = Buffer.create 64 in
  let prefix_byte = 0x10 in
  match name_index with
  | Some index ->
      let index_bytes = Integer.encode 4 index in
      Buffer.add_char buf (Char.chr (prefix_byte lor Char.code (Bytes.get index_bytes 0)));
      Buffer.add_bytes buf (Bytes.sub index_bytes 1 (Bytes.length index_bytes - 1));
      Buffer.add_bytes buf (String_.encode value);
      Buffer.to_bytes buf
  | None ->
      Buffer.add_char buf (Char.chr prefix_byte);
      Buffer.add_bytes buf (String_.encode value);
      Buffer.to_bytes buf

let encode_header encoder header ~encoding_type =
  let { name; value } = header in

  (* Try to find exact match in tables *)
  let static_match = static_table_find ~name ~value in
  let dynamic_match = DynamicTable.find encoder.dynamic_table ~name ~value in

  match static_match, dynamic_match with
  | Some index, _ | _, Some index ->
      (* Full match found - use indexed representation *)
      let actual_index = match static_match with
        | Some i -> i
        | None -> static_table_size + Option.get dynamic_match
      in
      encode_indexed_header actual_index
  | None, None ->
      (* No full match - check for name match *)
      let static_name_match = static_table_find_name name in
      let dynamic_name_match = DynamicTable.find_name encoder.dynamic_table name in
      let name_index = match static_name_match, dynamic_name_match with
        | Some i, _ -> Some i
        | None, Some i -> Some (static_table_size + i)
        | None, None -> None
      in

      match encoding_type with
      | Indexed ->
          (* Shouldn't happen - fall back to literal with indexing *)
          let result = encode_literal_with_indexing ~name_index ~value in
          DynamicTable.add encoder.dynamic_table header;
          result
      | LiteralWithIndexing ->
          let result = encode_literal_with_indexing ~name_index ~value in
          DynamicTable.add encoder.dynamic_table header;
          result
      | LiteralWithoutIndexing ->
          encode_literal_without_indexing ~name_index ~value
      | LiteralNeverIndexed ->
          encode_literal_never_indexed ~name_index ~value

let encode encoder ~headers ?(sensitive_headers = []) =
  let buf = Buffer.create 256 in
  List.iter (fun header ->
    let encoding_type =
      if is_sensitive_header header.name || List.mem header.name sensitive_headers then
        LiteralNeverIndexed
      else
        LiteralWithIndexing
    in
    let encoded = encode_header encoder header ~encoding_type in
    Buffer.add_bytes buf encoded
  ) headers;
  Buffer.to_bytes buf

(** {1 Decoder} *)

type decoder = { dynamic_table : DynamicTable.t }

let create_decoder ?(max_dynamic_table_size = 4096) () =
  { dynamic_table = DynamicTable.create max_dynamic_table_size }

let update_max_table_size decoder new_size =
  DynamicTable.update_max_size decoder.dynamic_table new_size

let lookup_header decoder index =
  if index <= static_table_size then
    static_table_lookup index
  else
    DynamicTable.lookup decoder.dynamic_table (index - static_table_size)

let decode_header_block decoder data offset =
  if offset >= Bytes.length data then Ok ([], offset)
  else
    let first_byte = Bytes.get data offset in
    let first_code = Char.code first_byte in

    if first_code land 0x80 <> 0 then
      (* Indexed Header Field: 1xxxxxxx *)
      match Integer.decode 7 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (index, new_offset) -> (
          match lookup_header decoder index with
          | None -> Error (format "Invalid header index: %d" index)
          | Some header -> Ok ([ header ], new_offset))
    else if first_code land 0x40 <> 0 then
      (* Literal with Incremental Indexing: 01xxxxxx *)
      match Integer.decode 6 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (name_index, pos1) -> (
          let* (name, pos2) =
            if name_index = 0 then
              String_.decode data pos1
            else
              match lookup_header decoder name_index with
              | None -> Error (format "Invalid name index: %d" name_index)
              | Some h -> Ok (h.name, pos1)
          in
          match String_.decode data pos2 with
          | Error e -> Error e
          | Ok (value, new_offset) ->
              let header = { name; value } in
              DynamicTable.add decoder.dynamic_table header;
              Ok ([ header ], new_offset))
    else if first_code land 0x20 <> 0 then
      (* Dynamic Table Size Update: 001xxxxx *)
      match Integer.decode 5 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (new_size, new_offset) ->
          DynamicTable.update_max_size decoder.dynamic_table new_size;
          Ok ([], new_offset)
    else if first_code land 0x10 <> 0 then
      (* Literal Never Indexed: 0001xxxx *)
      match Integer.decode 4 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (name_index, pos1) -> (
          let* (name, pos2) =
            if name_index = 0 then
              String_.decode data pos1
            else
              match lookup_header decoder name_index with
              | None -> Error (format "Invalid name index: %d" name_index)
              | Some h -> Ok (h.name, pos1)
          in
          match String_.decode data pos2 with
          | Error e -> Error e
          | Ok (value, new_offset) ->
              Ok ([ { name; value } ], new_offset))
    else
      (* Literal without Indexing: 0000xxxx *)
      match Integer.decode 4 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (name_index, pos1) -> (
          let* (name, pos2) =
            if name_index = 0 then
              String_.decode data pos1
            else
              match lookup_header decoder name_index with
              | None -> Error (format "Invalid name index: %d" name_index)
              | Some h -> Ok (h.name, pos1)
          in
          match String_.decode data pos2 with
          | Error e -> Error e
          | Ok (value, new_offset) ->
              Ok ([ { name; value } ], new_offset))

let decode decoder data =
  let rec decode_all acc offset =
    if offset >= Bytes.length data then Ok (List.rev acc)
    else
      match decode_header_block decoder data offset with
      | Error e -> Error e
      | Ok (headers, new_offset) ->
          decode_all (List.rev_append headers acc) new_offset
  in
  decode_all [] 0
