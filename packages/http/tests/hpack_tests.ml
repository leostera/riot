open Std
module Hpack = Http.Http2.Hpack

let test_encoder_decoder_roundtrip = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let decoder = Hpack.create_decoder () in
  let headers = [
    { Hpack.name = "content-type"; value = "application/json" };
    { Hpack.name = "content-length"; value = "123" };
  ] in
  let encoded = Hpack.encode encoder ~sensitive_headers:[] () ~headers in
  let decoded = Hpack.decode decoder encoded in
  match decoded with
  | Ok decoded_headers ->
      if List.length decoded_headers = List.length headers then
        Result.Ok ()
      else
        Result.Error ("Header count mismatch: expected "
        ^ Int.to_string (List.length headers)
        ^ ", got "
        ^ Int.to_string (List.length decoded_headers))
  | Error err -> Result.Error ("Decode failed: " ^ err)

let test_static_table_lookup = fun _ctx ->
  match Hpack.static_table_lookup 2 with
  | Some header ->
      if header.name = ":method" && header.value = "GET" then
        Result.Ok ()
      else
        Result.Error ("Static table entry 2 has wrong values: " ^ header.name ^ "=" ^ header.value)
  | None -> Result.Error "Static table lookup failed"

let test_encode_simple_header = fun _ctx ->
  let encoder = Hpack.create_encoder () in
  let headers = [ { Hpack.name = ":method"; value = "GET" } ] in
  let encoded = Hpack.encode encoder ~sensitive_headers:[] () ~headers in
  if IO.Bytes.length encoded > 0 then
    Result.Ok ()
  else
    Result.Error "Encoding produced empty output"

let tests = [
  Test.case "encoder_decoder_roundtrip" test_encoder_decoder_roundtrip;
  Test.case "static_table_lookup" test_static_table_lookup;
  Test.case "encode_simple_header" test_encode_simple_header;
]

let () =
  Miniriot.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"http:hpack" ~tests ~args:Env.args)
    ~args:Env.args
    ()
