open Std

let live_server_enabled = Env.var Env.Bool ~name:"BLINK_RUN_LIVE_SERVER_TESTS"
|> Option.unwrap_or ~default:false

let case =
  if live_server_enabled then
    Test.case
  else
    Test.skip

let test_large_json_response = fun _ctx ->
  (* Test that we can read JSON responses without truncation.
     This test makes a real HTTP request to an LM Studio instance expected
     to be running on port 1234. We use the chat completions endpoint with
     a large response to verify the body isn't truncated. *)
  let request_body = {|{"max_tokens":2000,"temperature":0.7,"model":"qwen/qwen3-coder-30b","messages":[{"role":"system","content":"You are: TestBot\nWrite a long response with at least 500 words about the history of computing."}]}|} in
  (* Build URI *)
  let uri = Net.Uri.of_string "http://127.0.0.1:1234/v1/chat/completions" |> Result.expect ~msg:"Invalid URI" in
  (* Connect *)
  match Blink.connect uri with
  | Error _err -> Error "Connection failed - is LM Studio running on port 1234?"
  | Ok conn ->
      (* Create POST request *)
      let req = Net.Http.Request.create Net.Http.Method.Post uri in
      let req = Net.Http.Request.with_header req "Content-Type" "application/json" in
      (* Send request with body *)
      let send_result = Blink.request conn req ~body:request_body () in
      match send_result with
      | Error _err -> Error "Request failed"
      | Ok () ->
          (* Read response *)
          match Blink.await conn with
          | Error _err -> Error "Response read failed"
          | Ok (response, body) ->
              let body_len = String.length body in
              let status = Net.Http.Response.status response |> Net.Http.Status.to_int in
              let headers = Net.Http.Response.headers response in
              let content_length_hdr = Net.Http.Header.get headers "content-length" in
              (* Verify we got a successful response *)
              if status < 200 || status >= 300 then
                (Error ("Unexpected status: " ^ string_of_int status ^ ", body: " ^ body))
                (* The key test: verify the response body is complete JSON, not truncated *)
              else if not (String.ends_with ~suffix:"}" body) then
                Error ("Response body truncated! Length: "
                ^ string_of_int body_len
                ^ ", Content-Length: "
                ^ (Option.unwrap_or ~default:"missing" content_length_hdr))
                (* Try to parse as JSON - this will fail if truncated *)
              else
                match Data.Json.of_string body with
                | Error err -> Error ("JSON parse failed (body may be truncated): "
                ^ Data.Json.error_to_string err
                ^ ", body length: "
                ^ string_of_int body_len)
                | Ok json ->
                    (* Verify it has expected structure for OpenAI API *)
                    match json with
                    | Data.Json.Object fields -> (
                        match List.assoc_opt "choices" fields with
                        | Some (Data.Json.Array _choices) ->
                            (* Success! We got a complete JSON response with the expected structure *)
                            Ok ()
                        | Some _ -> Error "'choices' field is not an array"
                        | None ->
                            (* Some error responses may not have choices, which is OK *)
                            Ok ()
                      )
                    | _ -> Error "Response is not a JSON object"

let test_streamed_response = fun _ctx ->
  (* Test that we can read chunked/streamed responses without truncation.
     When "stream": true is set, the server sends Transfer-Encoding: chunked
     with Server-Sent Events (SSE) format data chunks. *)
  let request_body = {|{"stream":true,"max_tokens":100,"temperature":0.7,"model":"qwen/qwen3-coder-30b","messages":[{"role":"user","content":"Count from 1 to 5"}]}|} in
  (* Build URI *)
  let uri = Net.Uri.of_string "http://127.0.0.1:1234/v1/chat/completions" |> Result.expect ~msg:"Invalid URI" in
  (* Connect *)
  match Blink.connect uri with
  | Error _err -> Error "Connection failed - is LM Studio running on port 1234?"
  | Ok conn ->
      (* Create POST request *)
      let req = Net.Http.Request.create Net.Http.Method.Post uri in
      let req = Net.Http.Request.with_header req "Content-Type" "application/json" in
      (* Send request with body *)
      let send_result = Blink.request conn req ~body:request_body () in
      match send_result with
      | Error _err -> Error "Request failed"
      | Ok () ->
          (* Read response, printing each chunk as it arrives *)
          let chunk_count = ref 0 in
          let on_message msgs =
            List.iter
              (fun msg ->
                match msg with
                | Blink.Connection.Data chunk ->
                    chunk_count := !chunk_count + 1;
                    let preview = chunk in
                    Log.info
                      ("Chunk "
                      ^ string_of_int !chunk_count
                      ^ " ("
                      ^ string_of_int (String.length chunk)
                      ^ " bytes): "
                      ^ preview)
                | Blink.Connection.Status status ->
                    Log.info ("Status: " ^ string_of_int (Net.Http.Status.to_int status))
                | Blink.Connection.Headers _headers ->
                    Log.info "Headers received"
                | Blink.Connection.Done ->
                    Log.info "Done!")
              msgs
          in
          match Blink.await ~on_message conn with
          | Error _err -> Error "Response read failed"
          | Ok (response, body) ->
              let body_len = String.length body in
              let status = Net.Http.Response.status response |> Net.Http.Status.to_int in
              let headers = Net.Http.Response.headers response in
              let transfer_encoding_hdr = Net.Http.Header.get headers "transfer-encoding" in
              (* Verify we got a successful response *)
              if status < 200 || status >= 300 then
                Error ("Unexpected status: " ^ string_of_int status)
                (* Verify we got some data *)
              else if body_len < 10 then
                Error ("Response body too short: " ^ string_of_int body_len ^ " bytes")
                (* Verify we got chunked encoding *)
              else
                (
                  match transfer_encoding_hdr with
                  | Some "chunked" -> Ok ()
                  | Some other -> Error ("Expected Transfer-Encoding: chunked, got: " ^ other)
                  | None -> Error "No Transfer-Encoding header (expected chunked)"
                )

let test_sse_parsing = fun _ctx ->
  (* Test the SSE module to parse streaming events *)
  let request_body = {|{"stream":true,"max_tokens":50,"temperature":0.7,"model":"qwen/qwen3-coder-30b","messages":[{"role":"user","content":"Count from 1 to 3"}]}|} in
  let uri = Net.Uri.of_string "http://127.0.0.1:1234/v1/chat/completions" |> Result.expect ~msg:"Invalid URI" in
  match Blink.connect uri with
  | Error _err -> Error "Connection failed - is LM Studio running on port 1234?"
  | Ok conn ->
      let req = Net.Http.Request.create Net.Http.Method.Post uri in
      let req = Net.Http.Request.with_header req "Content-Type" "application/json" in
      let send_result = Blink.request conn req ~body:request_body () in
      match send_result with
      | Error _err -> Error "Request failed"
      | Ok () ->
          (* Use SSE module to parse events *)
          Log.info "Starting to parse SSE events...";
          let iter = Blink.SSE.await conn in
          Log.info "Created SSE iterator";
          (* Try to get first event manually *)
          let first_event = Iter.MutIterator.next iter in
          Log.info
            (
              "First event: " ^ (
                match first_event with
                | Some _ -> "got one"
                | None -> "none"
              )
            );
          let events =
            match first_event with
            | Some e -> e :: (Iter.MutIterator.to_list iter)
            | None -> []
          in
          let event_count = List.length events in
          Log.info ("Received " ^ string_of_int event_count ^ " SSE events");
          (* Verify we got some events *)
          if event_count = 0 then
            Error "No SSE events received"
          else (
            (* Log each event *)
            List.iteri
              (fun i event ->
                Log.info
                  ("SSE Event "
                  ^ string_of_int (i + 1)
                  ^ ": "
                  ^ String.sub event.Blink.SSE.data 0 (min 80 (String.length event.Blink.SSE.data))))
              events;
            (* Verify each event has JSON data *)
            let all_valid_json =
              List.for_all
                (fun event ->
                  match Data.Json.of_string event.Blink.SSE.data with
                  | Ok _ -> true
                  | Error _ -> false)
                events
            in
            if not all_valid_json then
              Error "Some SSE events contained invalid JSON"
            else (
              Log.info ("✓ Parsed " ^ string_of_int event_count ^ " SSE events successfully");
              Ok ()
            )
          )

let tests =
  Test.[
    case "large JSON response without truncation" test_large_json_response;
    case "streamed/chunked response without truncation" test_streamed_response;
    case "SSE event parsing" test_sse_parsing;
  ]

let test_config = {|
[[log.handler]]
type = "stdout"
format = "full"
|}

let () =
  Miniriot.run
    ~main:(fun ~args ->
      (* Enable logging to see chunk messages in streamed response test *)
      Std.Config.load_string test_config;
      ignore (Std.Log.start_link ());
      Test.Cli.main ~name:"blink_large_response" ~tests ~args)
    ~args:Env.args
    ()
