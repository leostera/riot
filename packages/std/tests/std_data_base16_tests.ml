open Std
open Std.Encoding
open Std.IO

let test_encode_simple = fun _ctx ->
  let encoded = Base16.encode "Hi" in
  if encoded = "4869" then
    Ok ()
  else
    Error ("Expected '4869', got '" ^ encoded ^ "'")

let test_encode_empty = fun _ctx ->
  let encoded = Base16.encode "" in
  if encoded = "" then
    Ok ()
  else
    Error "Empty string should encode to empty string"

let test_encode_bytes = fun _ctx ->
  let bytes = Bytes.of_string "test" in
  let encoded = Base16.encode_bytes bytes in
  if encoded = "74657374" then
    Ok ()
  else
    Error ("Expected '74657374', got '" ^ encoded ^ "'")

let test_encode_lower = fun _ctx ->
  let encoded = Base16.encode_lower "\xAB\xCD" in
  if encoded = "abcd" then
    Ok ()
  else
    Error ("Expected 'abcd', got '" ^ encoded ^ "'")

let test_encode_special_chars = fun _ctx ->
  let encoded = Base16.encode "\x00\xFF" in
  if encoded = "00FF" then
    Ok ()
  else
    Error ("Expected '00FF', got '" ^ encoded ^ "'")

let test_decode_simple = fun _ctx ->
  match Base16.decode "4869" with
  | Ok "Hi" -> Ok ()
  | Ok s -> Error ("Expected 'Hi', got '" ^ s ^ "'")
  | Error _ -> Error "Decode failed"

let test_decode_lowercase = fun _ctx ->
  match Base16.decode "4869" with
  | Ok "Hi" -> Ok ()
  | _ -> Error "Failed to decode lowercase hex"

let test_decode_mixed_case = fun _ctx ->
  match Base16.decode "48aB" with
  | Ok _ -> Ok ()
  | Error _ -> Error "Failed to decode mixed case hex"

let test_decode_invalid_char = fun _ctx ->
  match Base16.decode "4G" with
  | Error `Invalid_base16 -> Ok ()
  | Ok _ -> Error "Should reject invalid hex character"

let test_decode_odd_length = fun _ctx ->
  match Base16.decode "486" with
  | Error `Invalid_base16 -> Ok ()
  | Ok _ -> Error "Should reject odd length string"

let test_roundtrip = fun _ctx ->
  let original = "Hello, World!" in
  let encoded = Base16.encode original in
  match Base16.decode encoded with
  | Ok decoded when decoded = original -> Ok ()
  | _ -> Error "Roundtrip failed"

let test_roundtrip_binary = fun _ctx ->
  let original = "\x00\x01\x02\xFF\xFE\xFD" in
  let encoded = Base16.encode original in
  match Base16.decode encoded with
  | Ok decoded when decoded = original -> Ok ()
  | _ -> Error "Binary roundtrip failed"

let tests =
  Test.[
    case "encode simple" test_encode_simple;
    case "encode empty" test_encode_empty;
    case "encode bytes" test_encode_bytes;
    case "encode lowercase" test_encode_lower;
    case "encode special chars" test_encode_special_chars;
    case "decode simple" test_decode_simple;
    case "decode lowercase" test_decode_lowercase;
    case "decode mixed case" test_decode_mixed_case;
    case "decode invalid char" test_decode_invalid_char;
    case "decode odd length" test_decode_odd_length;
    case "roundtrip" test_roundtrip;
    case "binary roundtrip" test_roundtrip_binary;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"base16" ~tests ~args) ~args:Env.args ()
