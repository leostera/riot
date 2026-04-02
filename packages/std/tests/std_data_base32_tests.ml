open Std
open Std.Encoding
open Std.Collections
open Std.IO

let test_encode_simple = fun _ctx ->
  let encoded = Base32.encode "Hello" in
  if encoded = "JBSWY3DP" then
    Ok ()
  else
    Error ("Expected 'JBSWY3DP', got '" ^ encoded ^ "'")

let test_encode_empty = fun _ctx ->
  let encoded = Base32.encode "" in
  if encoded = "" then
    Ok ()
  else
    Error "Empty string should encode to empty string"

let test_encode_bytes = fun _ctx ->
  let bytes = Bytes.of_string "test" in
  let encoded = Base32.encode_bytes bytes in
  if String.length encoded > 0 then
    Ok ()
  else
    Error "Bytes encoding failed"

let test_decode_simple = fun _ctx ->
  match Base32.decode "JBSWY3DP" with
  | Ok "Hello" -> Ok ()
  | Ok s -> Error ("Expected 'Hello', got '" ^ s ^ "'")
  | Error _ -> Error "Decode failed"

let test_decode_invalid_char = fun _ctx ->
  match Base32.decode "INVALID!" with
  | Error `Invalid_base32 -> Ok ()
  | Ok _ -> Error "Should reject invalid Base32 character"

let test_roundtrip = fun _ctx ->
  let original = "Hello, World!" in
  let encoded = Base32.encode original in
  match Base32.decode encoded with
  | Ok decoded when decoded = original -> Ok ()
  | _ -> Error "Roundtrip failed"

let test_roundtrip_binary = fun _ctx ->
  let original = "\x00\x01\x02\xFF" in
  let encoded = Base32.encode original in
  match Base32.decode encoded with
  | Ok decoded when decoded = original -> Ok ()
  | _ -> Error "Binary roundtrip failed"

let test_padding = fun _ctx ->
  let encoded = Base32.encode "f" in
  if String.contains encoded "=" then
    Ok ()
  else
    Error "Short strings should have padding"

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
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"base32" ~tests ~args) ~args:Env.args ()
