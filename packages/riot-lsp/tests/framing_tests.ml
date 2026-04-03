open Std

let parse_roundtrip = fun payload ->
  let encoded = Riot_lsp.Framing.encode payload in
  Riot_lsp.Framing.decode_one encoded

let test_encode_and_decode_roundtrip = fun _ctx ->
  let payload = "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}" in
  match parse_roundtrip payload with
  | Error message -> Error ("expected framing roundtrip to succeed, got: " ^ message)
  | Ok (decoded, rest) ->
      if not (String.equal decoded payload) then
        Error "decoded payload did not match original"
      else if not (String.equal rest "") then
        Error "expected no trailing data after decode"
      else
        Ok ()

let test_decode_handles_lf_headers = fun _ctx ->
  let payload = "{\"hello\":\"world\"}" in
  let framed = "Content-Length: " ^ Int.to_string (String.length payload) ^ "\n\n" ^ payload in
  match Riot_lsp.Framing.decode_one framed with
  | Error message -> Error ("expected LF framing to succeed, got: " ^ message)
  | Ok (decoded, rest) ->
      if not (String.equal decoded payload) then
        Error "decoded LF payload did not match original"
      else if not (String.equal rest "") then
        Error "expected no trailing LF payload data"
      else
        Ok ()

let test_decode_requires_content_length = fun _ctx ->
  match Riot_lsp.Framing.decode_one "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{}" with
  | Ok _ -> Error "expected missing Content-Length to fail"
  | Error _ -> Ok ()

let test_decode_returns_unconsumed_tail = fun _ctx ->
  let first = "{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}" in
  let second = "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}" in
  let framed = Riot_lsp.Framing.encode first ^ Riot_lsp.Framing.encode second in
  match Riot_lsp.Framing.decode_one framed with
  | Error message -> Error ("expected combined decode to succeed, got: " ^ message)
  | Ok (decoded, rest) ->
      if not (String.equal decoded first) then
        Error "decoded first payload did not match"
      else
        match Riot_lsp.Framing.decode_one rest with
        | Error message -> Error ("expected tail decode to succeed, got: " ^ message)
        | Ok (decoded_second, final_rest) ->
            if not (String.equal decoded_second second) then
              Error "decoded second payload did not match"
            else if not (String.equal final_rest "") then
              Error "expected no trailing bytes after second payload"
            else
              Ok ()

let () =
  Actors.run
    ~main:(fun ~args ->
      Test.Cli.main
        ~name:"riot-lsp framing tests"
        ~tests:
          [
            Test.case "encode and decode roundtrip" test_encode_and_decode_roundtrip;
            Test.case "decode handles LF headers" test_decode_handles_lf_headers;
            Test.case "decode requires Content-Length" test_decode_requires_content_length;
            Test.case "decode returns unconsumed tail" test_decode_returns_unconsumed_tail;
          ]
        ~args)
    ~args:Env.args
    ()
