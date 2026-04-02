open Std
module Test = Std.Test

let test_encode_int = fun _ctx ->
  if String.equal (Encoding.Octal.encode_int 0o755) "755" then
    Ok ()
  else
    Error "expected 0o755 to encode as 755"

let test_encode_int64 = fun _ctx ->
  if String.equal (Encoding.Octal.encode_int64 0o1_750L) "1750" then
    Ok ()
  else
    Error "expected 0o1750L to encode as 1750"

let test_encode_negative = fun _ctx ->
  if String.equal (Encoding.Octal.encode_int (-0o755)) "-755" then
    Ok ()
  else
    Error "expected -0o755 to encode as -755"

let test_decode_int = fun _ctx ->
  match Encoding.Octal.decode_int "755" with
  | Ok value when value = 0o755 -> Ok ()
  | Ok _ -> Error "expected 755 to decode to 0o755"
  | Error _ -> Error "expected 755 to decode successfully"

let test_decode_prefixed = fun _ctx ->
  match Encoding.Octal.decode_int32 "0o644" with
  | Ok value when value = 0o644l -> Ok ()
  | Ok _ -> Error "expected 0o644 to decode to 0o644l"
  | Error _ -> Error "expected prefixed octal to decode successfully"

let test_decode_signed = fun _ctx ->
  match Encoding.Octal.decode_int64 "-10" with
  | Ok value when value = (-8L) -> Ok ()
  | Ok _ -> Error "expected -10 to decode to -8L"
  | Error _ -> Error "expected signed octal to decode successfully"

let test_decode_invalid = fun _ctx ->
  match Encoding.Octal.decode_int "8" with
  | Error `Invalid_octal -> Ok ()
  | Ok _ -> Error "expected invalid octal digit to be rejected"

let tests =
  Test.[
    case "octal encode int" test_encode_int;
    case "octal encode int64" test_encode_int64;
    case "octal encode negative" test_encode_negative;
    case "octal decode int" test_decode_int;
    case "octal decode prefixed" test_decode_prefixed;
    case "octal decode signed" test_decode_signed;
    case "octal decode invalid" test_decode_invalid;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"octal" ~tests ~args) ~args:Env.args ()
