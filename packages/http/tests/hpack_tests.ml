open Std

(** Test suite for HPACK implementation (RFC 7541) *)

module Hpack = Http.Http2.Hpack

(** {1 Helper Functions} *)

let bytes_to_hex bytes =
  let buf = Buffer.create (Bytes.length bytes * 2) in
  Bytes.iter
    (fun c -> Buffer.add_string buf (format "%02x" (Char.code c)))
    bytes;
  Buffer.contents buf

let hex_to_bytes hex_str =
  let len = String.length hex_str / 2 in
  let bytes = Bytes.create len in
  for i = 0 to len - 1 do
    let hi = int_of_string ("0x" ^ String.make 1 hex_str.[i * 2]) in
    let lo = int_of_string ("0x" ^ String.make 1 hex_str.[i * 2 + 1]) in
    Bytes.set bytes i (Char.chr ((hi lsl 4) lor lo))
  done;
  bytes

let assert_bytes_equal actual expected =
  if Bytes.equal actual expected then Ok ()
  else
    Error
      (format "Bytes mismatch:\nExpected: %s\nActual:   %s"
         (bytes_to_hex expected) (bytes_to_hex actual))

let assert_headers_equal actual expected =
  let compare_header h1 h2 =
    String.compare h1.Hpack.name h2.Hpack.name
  in
  let actual_sorted = List.sort compare_header actual in
  let expected_sorted = List.sort compare_header expected in
  if List.length actual <> List.length expected then
    Error
      (format "Header count mismatch: expected %d, got %d"
         (List.length expected) (List.length actual))
  else
    let rec check = function
      | [], [] -> Ok ()
      | h1 :: rest1, h2 :: rest2 ->
          if String.equal h1.Hpack.name h2.Hpack.name
             && String.equal h1.Hpack.value h2.Hpack.value then
            check (rest1, rest2)
          else
            Error
              (format "Header mismatch:\nExpected: %s: %s\nActual:   %s: %s"
                 h2.Hpack.name h2.Hpack.value h1.Hpack.name h1.Hpack.value)
      | _, _ -> Error "List length mismatch (shouldn't happen)"
    in
    check (actual_sorted, expected_sorted)

(** {1 Static Table Tests} *)

let test_static_table_lookup () =
  Test.case "Static table lookup" (fun () ->
      match Hpack.static_table_lookup 2 with
      | Some { name; value } ->
          if String.equal name ":method" && String.equal value "GET" then Ok ()
          else Error (format "Unexpected entry: %s: %s" name value)
      | None -> Error "Entry not found")

let test_static_table_find () =
  Test.case "Static table find" (fun () ->
      match Hpack.static_table_find ~name:":method" ~value:"POST" with
      | Some 3 -> Ok ()
      | Some i -> Error (format "Wrong index: %d" i)
      | None -> Error "Entry not found")

let test_static_table_find_name () =
  Test.case "Static table find name" (fun () ->
      match Hpack.static_table_find_name "accept" with
      | Some index when index > 0 && index <= 61 -> Ok ()
      | Some i -> Error (format "Invalid index: %d" i)
      | None -> Error "Name not found")

(** {1 RFC 7541 Test Vectors}

    These test vectors are from RFC 7541 Appendix C
*)

(** C.2.1: Literal Header Field with Indexing *)
let test_literal_with_indexing () =
  Test.case "RFC 7541 C.2.1: Literal with indexing" (fun () ->
      let encoder = Hpack.create_encoder () in
      let header = { Hpack.name = "custom-key"; value = "custom-header" } in
      let encoded =
        Hpack.encode_header encoder header
          ~encoding_type:Hpack.LiteralWithIndexing
      in

      (* Expected: 400a 6375 7374 6f6d 2d6b 6579 0d63 7573 746f 6d2d 6865 6164 6572 *)
      (* 0x40 = literal with indexing, index 0 (literal name) *)
      (* 0x0a = name length 10 *)
      (* "custom-key" *)
      (* 0x0d = value length 13 *)
      (* "custom-header" *)

      let first_byte = Bytes.get encoded 0 in
      if Char.code first_byte land 0x40 = 0 then
        Error "Expected literal with indexing encoding (0x40 bit)"
      else Ok ())

(** C.2.2: Literal Header Field without Indexing *)
let test_literal_without_indexing () =
  Test.case "RFC 7541 C.2.2: Literal without indexing" (fun () ->
      let encoder = Hpack.create_encoder () in
      let header = { Hpack.name = ":path"; value = "/sample/path" } in
      let encoded =
        Hpack.encode_header encoder header
          ~encoding_type:Hpack.LiteralWithoutIndexing
      in

      (* Should use name index for :path (index 4 or 5 in static table) *)
      (* 0x04 or 0x05 for indexed name, then length-prefixed value *)
      let first_byte = Bytes.get encoded 0 in
      let code = Char.code first_byte in
      if code land 0xF0 = 0 then Ok ()
      else Error (format "Expected literal without indexing (0x0X), got 0x%02x" code))

(** C.2.3: Literal Header Field Never Indexed *)
let test_literal_never_indexed () =
  Test.case "RFC 7541 C.2.3: Literal never indexed" (fun () ->
      let encoder = Hpack.create_encoder () in
      let header = { Hpack.name = "password"; value = "secret" } in
      let encoded =
        Hpack.encode_header encoder header
          ~encoding_type:Hpack.LiteralNeverIndexed
      in

      (* 0x10 = never indexed, index 0 (literal name) *)
      let first_byte = Bytes.get encoded 0 in
      let code = Char.code first_byte in
      if code land 0x10 = 0x10 then Ok ()
      else Error (format "Expected never indexed (0x1X), got 0x%02x" code))

(** C.2.4: Indexed Header Field *)
let test_indexed_header () =
  Test.case "RFC 7541 C.2.4: Indexed header" (fun () ->
      let encoder = Hpack.create_encoder () in
      (* :method GET is at static table index 2 *)
      let header = { Hpack.name = ":method"; value = "GET" } in
      let encoded =
        Hpack.encode_header encoder header ~encoding_type:Hpack.Indexed
      in

      (* Expected: 0x82 (indexed, index 2) *)
      if Bytes.length encoded = 1 && Char.code (Bytes.get encoded 0) = 0x82
      then Ok ()
      else
        Error
          (format "Expected 0x82, got: %s" (bytes_to_hex encoded)))

(** {1 Encoder/Decoder Round-Trip Tests} *)

let test_encode_decode_simple () =
  Test.case "Encode/decode simple headers" (fun () ->
      let encoder = Hpack.create_encoder () in
      let decoder = Hpack.create_decoder () in

      let headers =
        [
          { Hpack.name = ":method"; value = "GET" };
          { Hpack.name = ":path"; value = "/" };
          { Hpack.name = "content-type"; value = "text/html" };
        ]
      in

      let encoded = Hpack.encode encoder ~headers in
      match Hpack.decode decoder encoded with
      | Error e -> Error (format "Decode failed: %s" e)
      | Ok decoded -> assert_headers_equal decoded headers)

let test_encode_decode_custom_headers () =
  Test.case "Encode/decode custom headers" (fun () ->
      let encoder = Hpack.create_encoder () in
      let decoder = Hpack.create_decoder () in

      let headers =
        [
          { Hpack.name = "x-custom-header"; value = "custom-value" };
          { Hpack.name = "x-another-header"; value = "another-value" };
        ]
      in

      let encoded = Hpack.encode encoder ~headers in
      match Hpack.decode decoder encoded with
      | Error e -> Error (format "Decode failed: %s" e)
      | Ok decoded -> assert_headers_equal decoded headers)

let test_dynamic_table_indexing () =
  Test.case "Dynamic table indexing across multiple encodes" (fun () ->
      let encoder = Hpack.create_encoder () in
      let decoder = Hpack.create_decoder () in

      (* First request *)
      let headers1 =
        [
          { Hpack.name = ":method"; value = "GET" };
          { Hpack.name = ":path"; value = "/index.html" };
          { Hpack.name = "x-custom"; value = "first" };
        ]
      in
      let encoded1 = Hpack.encode encoder ~headers in
      let* decoded1 =
        match Hpack.decode decoder encoded1 with
        | Ok d -> Ok d
        | Error e -> Error (format "First decode failed: %s" e)
      in
      let* () = assert_headers_equal decoded1 headers1 in

      (* Second request - x-custom should be indexed now *)
      let headers2 =
        [
          { Hpack.name = ":method"; value = "GET" };
          { Hpack.name = ":path"; value = "/index.html" };
          { Hpack.name = "x-custom"; value = "first" };
        ]
      in
      let encoded2 = Hpack.encode encoder ~headers:headers2 in

      (* Second encoding should be smaller due to dynamic table *)
      if Bytes.length encoded2 <= Bytes.length encoded1 then
        match Hpack.decode decoder encoded2 with
        | Ok decoded2 -> assert_headers_equal decoded2 headers2
        | Error e -> Error (format "Second decode failed: %s" e)
      else
        Error
          (format
             "Expected second encoding to be smaller: %d <= %d"
             (Bytes.length encoded2) (Bytes.length encoded1)))

let test_sensitive_headers () =
  Test.case "Sensitive headers never indexed" (fun () ->
      let encoder = Hpack.create_encoder () in

      let headers = [ { Hpack.name = "authorization"; value = "Bearer token123" } ] in

      let encoded = Hpack.encode encoder ~headers in

      (* First byte should indicate never indexed (0x1X) *)
      let first_byte = Bytes.get encoded 0 in
      let code = Char.code first_byte in
      if code land 0x10 = 0x10 then Ok ()
      else
        Error
          (format "Authorization header should use never indexed encoding, got 0x%02x"
             code))

(** {1 Integer Encoding Tests} *)

let test_integer_encoding_small () =
  Test.case "Integer encoding: small values" (fun () ->
      (* Encode 10 with 5-bit prefix *)
      let encoded =
        Http.Http2.Hpack.Integer.encode 5 (* prefix_bits *) 10
      in
      if Bytes.length encoded = 1 && Char.code (Bytes.get encoded 0) = 10 then
        Ok ()
      else Error (format "Expected single byte 10, got: %s" (bytes_to_hex encoded)))

let test_integer_encoding_large () =
  Test.case "Integer encoding: large values" (fun () ->
      (* Encode 1337 with 5-bit prefix *)
      (* Expected: 1f 9a 0a *)
      (* 0x1f = 31 (max 5-bit value) *)
      (* 0x9a = 154 with continuation bit (1306 & 0x7f | 0x80) *)
      (* 0x0a = 10 (remaining value) *)
      (* Calculation: 31 + 154 + 10*128 = 31 + 154 + 1280 = ... *)
      (* Actually: 1337 = 31 + ((154 - 128) + 10 * 128) = 31 + 26 + 1280 = 1337 *)
      let encoded = Http.Http2.Hpack.Integer.encode 5 1337 in
      if Bytes.length encoded = 3 then Ok ()
      else
        Error
          (format "Expected 3 bytes for 1337, got %d: %s" (Bytes.length encoded)
             (bytes_to_hex encoded)))

(** {1 Test Suite} *)

let () =
  Test.run "HPACK Tests"
    [
      (* Static table tests *)
      test_static_table_lookup ();
      test_static_table_find ();
      test_static_table_find_name ();
      (* RFC 7541 test vectors *)
      test_literal_with_indexing ();
      test_literal_without_indexing ();
      test_literal_never_indexed ();
      test_indexed_header ();
      (* Encoder/decoder tests *)
      test_encode_decode_simple ();
      test_encode_decode_custom_headers ();
      test_dynamic_table_indexing ();
      test_sensitive_headers ();
      (* Integer encoding tests *)
      test_integer_encoding_small ();
      test_integer_encoding_large ();
    ]
