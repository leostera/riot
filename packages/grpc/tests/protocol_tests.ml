open Std

(** Test suite for gRPC protocol implementation *)

module Grpc = Grpc

(** {1 Status Tests} *)

let test_status_to_int () =
  Test.case "Status: to_int" (fun () ->
      if Grpc.Status.to_int Grpc.Status.OK = 0 then Ok ()
      else Error "OK should be 0")

let test_status_of_int () =
  Test.case "Status: of_int" (fun () ->
      match Grpc.Status.of_int 13 with
      | Some Grpc.Status.Internal -> Ok ()
      | _ -> Error "13 should be Internal")

let test_status_is_retriable () =
  Test.case "Status: is_retriable" (fun () ->
      if Grpc.Status.is_retriable Grpc.Status.Unavailable then Ok ()
      else Error "Unavailable should be retriable")

let test_status_to_http () =
  Test.case "Status: to_http_status" (fun () ->
      if Grpc.Status.to_http_status Grpc.Status.NotFound = 404 then Ok ()
      else Error "NotFound should map to 404")

(** {1 Message Framing Tests} *)

let test_message_encode_decode () =
  Test.case "Message: encode/decode round-trip" (fun () ->
      let payload = Bytes.of_string "Hello, gRPC!" in
      let encoded = Grpc.Message.encode ~compressed:false ~payload in

      (* Should be 5-byte header + payload *)
      if Bytes.length encoded <> 5 + Bytes.length payload then
        Error
          (format "Expected length %d, got %d"
             (5 + Bytes.length payload)
             (Bytes.length encoded))
      else
        match Grpc.Message.decode encoded with
        | Error e -> Error (format "Decode failed: %s" e)
        | Ok (msg, remaining) ->
            if
              msg.compressed = false
              && Bytes.equal msg.payload payload
              && Bytes.length remaining = 0
            then Ok ()
            else Error "Decoded message doesn't match")

let test_message_peek_header () =
  Test.case "Message: peek_header" (fun () ->
      let payload = Bytes.of_string "test" in
      let encoded = Grpc.Message.encode ~compressed:true ~payload in

      match Grpc.Message.peek_header encoded with
      | Error e -> Error (format "Peek failed: %s" e)
      | Ok (compressed, length) ->
          if compressed && length = Bytes.length payload then Ok ()
          else
            Error
              (format "Expected compressed=true, length=%d, got compressed=%b, length=%d"
                 (Bytes.length payload) compressed length))

let test_message_size_validation () =
  Test.case "Message: size validation" (fun () ->
      (* Test with very large size *)
      match
        Grpc.Message.validate_size (10 * 1024 * 1024) ~max_size:(Some (4 * 1024 * 1024))
      with
      | Ok () -> Error "Should reject message larger than max size"
      | Error _ -> Ok ())

let test_message_incomplete () =
  Test.case "Message: decode incomplete message" (fun () ->
      let partial = Bytes.of_string "\x00\x00\x00\x00\x10test" in
      (* Header says 16 bytes, but only 4 provided *)
      match Grpc.Message.decode partial with
      | Ok _ -> Error "Should fail on incomplete message"
      | Error msg ->
          if String.contains msg "Incomplete" then Ok ()
          else Error (format "Wrong error message: %s" msg))

(** {1 Metadata Tests} *)

let test_metadata_add_get () =
  Test.case "Metadata: add and get" (fun () ->
      let meta =
        Grpc.Metadata.empty
        |> Grpc.Metadata.add ~key:"x-custom" ~value:"test123"
      in
      match Grpc.Metadata.get meta ~key:"x-custom" with
      | Some "test123" -> Ok ()
      | _ -> Error "Failed to retrieve metadata")

let test_metadata_path () =
  Test.case "Metadata: path helper" (fun () ->
      let key, value =
        Grpc.Metadata.path ~service:"example.UserService" ~method_:"GetUser"
      in
      if String.equal key ":path" && String.equal value "/example.UserService/GetUser"
      then Ok ()
      else Error (format "Unexpected path: %s = %s" key value))

let test_metadata_content_type () =
  Test.case "Metadata: content_type helper" (fun () ->
      let key, value = Grpc.Metadata.content_type Grpc.Metadata.Proto in
      if String.equal key "content-type" && String.equal value "application/grpc+proto"
      then Ok ()
      else Error (format "Unexpected content-type: %s = %s" key value))

let test_metadata_parse_timeout () =
  Test.case "Metadata: parse_timeout" (fun () ->
      match Grpc.Metadata.parse_timeout "10S" with
      | Some t when t.value = 10 && t.unit = `Seconds -> Ok ()
      | Some t ->
          Error (format "Wrong timeout: value=%d, unit=%s" t.value "?")
      | None -> Error "Failed to parse timeout")

let test_metadata_parse_status () =
  Test.case "Metadata: parse_status" (fun () ->
      match Grpc.Metadata.parse_status "14" with
      | Some Grpc.Status.Unavailable -> Ok ()
      | _ -> Error "Failed to parse status")

let test_metadata_binary () =
  Test.case "Metadata: binary encoding" (fun () ->
      let data = Bytes.of_string "binary data \x00\x01\x02" in
      let encoded = Grpc.Metadata.encode_binary data in

      match Grpc.Metadata.decode_binary encoded with
      | Error e -> Error (format "Decode failed: %s" e)
      | Ok decoded ->
          if Bytes.equal data decoded then Ok ()
          else Error "Binary data mismatch")

let test_metadata_is_binary () =
  Test.case "Metadata: is_binary" (fun () ->
      if
        Grpc.Metadata.is_binary "x-trace-bin"
        && not (Grpc.Metadata.is_binary "x-trace")
      then Ok ()
      else Error "is_binary check failed")

let test_metadata_validation () =
  Test.case "Metadata: header name validation" (fun () ->
      if
        Grpc.Metadata.is_valid_header_name "x-custom-header"
        && Grpc.Metadata.is_valid_header_name ":path"
        && not (Grpc.Metadata.is_valid_header_name "X-Invalid")
        && not (Grpc.Metadata.is_valid_header_name "")
      then Ok ()
      else Error "Header name validation failed")

let test_metadata_reserved () =
  Test.case "Metadata: reserved headers" (fun () ->
      if
        Grpc.Metadata.is_reserved "grpc-timeout"
        && not (Grpc.Metadata.is_reserved "x-custom")
      then Ok ()
      else Error "Reserved header check failed")

(** {1 Call Tests} *)

let test_call_method_path () =
  Test.case "Call: method_path" (fun () ->
      let method_def =
        Grpc.Call.unary_method ~service:"example.Service" ~method_:"Method"
      in
      let path = Grpc.Call.method_path method_def in
      if String.equal path "/example.Service/Method" then Ok ()
      else Error (format "Unexpected path: %s" path))

let test_call_config_with_timeout () =
  Test.case "Call: with_timeout" (fun () ->
      let config =
        Grpc.Call.default_config
        |> Grpc.Call.with_timeout { value = 30; unit = `Seconds }
      in
      match config.timeout with
      | Some t when t.value = 30 -> Ok ()
      | _ -> Error "Timeout not set correctly")

let test_call_config_with_metadata () =
  Test.case "Call: with_metadata" (fun () ->
      let config =
        Grpc.Call.default_config
        |> Grpc.Call.with_metadata [ ("x-custom", "value") ]
      in
      match Grpc.Metadata.get config.metadata ~key:"x-custom" with
      | Some "value" -> Ok ()
      | _ -> Error "Metadata not set correctly")

(** {1 Integration Tests} *)

let test_full_request_metadata () =
  Test.case "Integration: full request metadata" (fun () ->
      let method_def =
        Grpc.Call.unary_method ~service:"example.UserService" ~method_:"GetUser"
      in

      let metadata =
        Grpc.Metadata.empty
        |> Grpc.Metadata.add ~key:":method" ~value:"POST"
        |> Grpc.Metadata.add ~key:":scheme" ~value:"https"
        |> Grpc.Metadata.add
             ~key:(fst (Grpc.Metadata.path ~service:method_def.service ~method_:method_def.method_))
             ~value:(snd (Grpc.Metadata.path ~service:method_def.service ~method_:method_def.method_))
        |> Grpc.Metadata.add
             ~key:(fst (Grpc.Metadata.content_type Grpc.Metadata.Proto))
             ~value:(snd (Grpc.Metadata.content_type Grpc.Metadata.Proto))
        |> Grpc.Metadata.add ~key:"te" ~value:"trailers"
      in

      (* Verify required headers are present *)
      match
        ( Grpc.Metadata.get metadata ~key:":path",
          Grpc.Metadata.get metadata ~key:"content-type" )
      with
      | Some path, Some ct ->
          if
            String.equal path "/example.UserService/GetUser"
            && String.equal ct "application/grpc+proto"
          then Ok ()
          else Error "Request metadata incomplete"
      | _ -> Error "Required headers missing")

(** {1 Test Suite} *)

let () =
  Test.run "gRPC Protocol Tests"
    [
      (* Status tests *)
      test_status_to_int ();
      test_status_of_int ();
      test_status_is_retriable ();
      test_status_to_http ();
      (* Message tests *)
      test_message_encode_decode ();
      test_message_peek_header ();
      test_message_size_validation ();
      test_message_incomplete ();
      (* Metadata tests *)
      test_metadata_add_get ();
      test_metadata_path ();
      test_metadata_content_type ();
      test_metadata_parse_timeout ();
      test_metadata_parse_status ();
      test_metadata_binary ();
      test_metadata_is_binary ();
      test_metadata_validation ();
      test_metadata_reserved ();
      (* Call tests *)
      test_call_method_path ();
      test_call_config_with_timeout ();
      test_call_config_with_metadata ();
      (* Integration tests *)
      test_full_request_metadata ();
    ]
