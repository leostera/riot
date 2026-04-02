open Std
module Base85 = Std.Encoding.Base85
open Std.IO

let test_encode_simple = fun _ctx ->
  let encoded = Base85.encode "Man" in
  if encoded = "9jqo" then
    Ok ()
  else
    Error ("Expected '9jqo', got '" ^ encoded ^ "'")

let test_encode_empty = fun _ctx ->
  let encoded = Base85.encode "" in
  if encoded = "" then
    Ok ()
  else
    Error "Empty string should encode to empty string"

let test_encode_bytes = fun _ctx ->
  let bytes = Bytes.of_string "test" in
  let encoded = Base85.encode_bytes bytes in
  if String.length encoded > 0 then
    Ok ()
  else
    Error "Bytes encoding failed"

let test_encode_zeros = fun _ctx ->
  let encoded = Base85.encode "\x00\x00\x00\x00" in
  if encoded = "z" then
    Ok ()
  else
    Error ("Expected 'z', got '" ^ encoded ^ "'")

let test_decode_simple = fun _ctx ->
  match Base85.decode "9jqo" with
  | Ok "Man" -> Ok ()
  | Ok s -> Error ("Expected 'Man', got '" ^ s ^ "'")
  | Error _ -> Error "Decode failed"

let test_decode_zeros = fun _ctx ->
  match Base85.decode "z" with
  | Ok "\x00\x00\x00\x00" -> Ok ()
  | Ok _ -> Error "Zero block decoded incorrectly"
  | Error _ -> Error "Decode failed"

let test_decode_invalid_char = fun _ctx ->
  match Base85.decode "bad~{" with
  | Error `Invalid_base85 -> Ok ()
  | Ok _ -> Error "Should reject invalid Base85 character"

let test_roundtrip = fun _ctx ->
  let original = "Hello, World!" in
  let encoded = Base85.encode original in
  match Base85.decode encoded with
  | Ok decoded when decoded = original -> Ok ()
  | _ -> Error "Roundtrip failed"

let test_roundtrip_binary = fun _ctx ->
  let original = "\x00\x01\x02\xFF\xFE\xFD" in
  let encoded = Base85.encode original in
  match Base85.decode encoded with
  | Ok decoded when decoded = original -> Ok ()
  | _ -> Error "Binary roundtrip failed"

let tests =
  Test.[
    case "encode simple" test_encode_simple;
    case "encode empty" test_encode_empty;
    case "encode bytes" test_encode_bytes;
    case "encode zeros" test_encode_zeros;
    case "decode simple" test_decode_simple;
    case "decode zeros" test_decode_zeros;
    case "decode invalid char" test_decode_invalid_char;
    case "roundtrip" test_roundtrip;
    case "binary roundtrip" test_roundtrip_binary;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"base85" ~tests ~args) ~args:Env.args ()
