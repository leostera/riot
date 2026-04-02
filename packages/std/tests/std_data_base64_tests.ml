open Std
open Std.Encoding
open Std.IO

let test_encode_simple = fun _ctx ->
  let encoded = Base64.encode "Hello" in
  if encoded = "SGVsbG8=" then
    Ok ()
  else
    Error ("Expected 'SGVsbG8=', got '" ^ encoded ^ "'")

let test_encode_empty = fun _ctx ->
  let encoded = Base64.encode "" in
  if encoded = "" then
    Ok ()
  else
    Error "Empty string should encode to empty string"

let test_encode_bytes = fun _ctx ->
  let bytes = Bytes.of_string "test" in
  let encoded = Base64.encode_bytes bytes in
  if encoded = "dGVzdA==" then
    Ok ()
  else
    Error ("Expected 'dGVzdA==', got '" ^ encoded ^ "'")

let test_decode_simple = fun _ctx ->
  match Base64.decode "SGVsbG8=" with
  | Ok "Hello" -> Ok ()
  | Ok s -> Error ("Expected 'Hello', got '" ^ s ^ "'")
  | Error _ -> Error "Decode failed"

let test_decode_invalid_char = fun _ctx ->
  match Base64.decode "SGVsbG8!" with
  | Error `Invalid_base64 -> Ok ()
  | Ok _ -> Error "Should reject invalid Base64 character"

let test_roundtrip = fun _ctx ->
  let original = "Hello, World!" in
  let encoded = Base64.encode original in
  match Base64.decode encoded with
  | Ok decoded when decoded = original -> Ok ()
  | _ -> Error "Roundtrip failed"

let test_roundtrip_binary = fun _ctx ->
  let original = "\x00\x01\x02\xFF\xFE\xFD" in
  let encoded = Base64.encode original in
  match Base64.decode encoded with
  | Ok decoded when decoded = original -> Ok ()
  | _ -> Error "Binary roundtrip failed"

let test_padding = fun _ctx ->
  let encoded = Base64.encode "f" in
  if encoded = "Zg==" then
    Ok ()
  else
    Error ("Expected 'Zg==', got '" ^ encoded ^ "'")

let tests =
  Test.[
    case "encode simple" test_encode_simple;
    case "encode empty" test_encode_empty;
    case "encode bytes" test_encode_bytes;
    case "decode simple" test_decode_simple;
    case "decode invalid char" test_decode_invalid_char;
    case "roundtrip" test_roundtrip;
    case "binary roundtrip" test_roundtrip_binary;
    case "padding" test_padding;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"base64" ~tests ~args) ~args:Env.args ()
