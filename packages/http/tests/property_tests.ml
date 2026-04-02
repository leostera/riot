open Std
open Http
open Propane

(** Property tests for HTTP package using Propane *)
(* ===== HTTP/2 HPACK Property Tests ===== *)

(* Generator for valid HTTP header names (lowercase alphanumeric + hyphens) *)

let header_name_gen =
  Generator.(
    map
      (fun name ->
        if String.length name = 0 then
          "x-header"
        else
          name)
      (string_of char_lowercase)
  )

(* Generator for HTTP header values (printable ASCII) *)

let header_value_gen = Generator.(string_of char_printable)

(* Generator for HTTP headers *)

let header_gen =
  Generator.map
    (fun ((name, value)) -> Http2.Hpack.{ name; value })
    (Generator.pair header_name_gen header_value_gen)

(* Arbitrary for headers *)

let header_arb =
  Arbitrary.make ~print:(fun h -> h.Http2.Hpack.name ^ ": " ^ h.Http2.Hpack.value) header_gen

(* Property: HPACK encoding produces output *)

let hpack_encode_prop =
  property "HPACK encoding produces non-empty output for non-empty headers" Arbitrary.(list header_arb)
    (fun headers ->
      assume (List.length headers > 0);
      assume
        (List.for_all
          (fun h -> String.length h.Http2.Hpack.name > 0 && String.length h.Http2.Hpack.value > 0)
          headers);
      let encoder = Http2.Hpack.create_encoder () in
      let encoded = Http2.Hpack.encode encoder ~sensitive_headers:[] () ~headers in
      IO.Bytes.length encoded > 0)

(* Property: HPACK encoding is deterministic *)

let hpack_deterministic_prop =
  property "HPACK encoding is deterministic" Arbitrary.(list header_arb)
    (fun headers ->
      let encoder1 = Http2.Hpack.create_encoder () in
      let encoder2 = Http2.Hpack.create_encoder () in
      let encoded1 = Http2.Hpack.encode encoder1 ~sensitive_headers:[] () ~headers in
      let encoded2 = Http2.Hpack.encode encoder2 ~sensitive_headers:[] () ~headers in
      IO.Bytes.equal encoded1 encoded2)

(* ===== HTTP/2 Frame Property Tests ===== *)

(* Generator for frame types *)

let frame_type_gen = Generator.one_of
  [
    Generator.return Http2.Frame.Data;
    Generator.return Http2.Frame.Headers;
    Generator.return Http2.Frame.Settings;
    Generator.return Http2.Frame.Ping;
    Generator.return Http2.Frame.Goaway;
    Generator.return Http2.Frame.WindowUpdate;
    Generator.return Http2.Frame.Priority;
    Generator.return Http2.Frame.RstStream;
    Generator.return Http2.Frame.PushPromise;
    Generator.return Http2.Frame.Continuation;
  ]

(* Generator for frame flags *)

let frame_flags_gen =
  let open Generator in
    map2
      (fun ((end_stream, end_headers, padded)) ((priority_flag, ack)) ->
        Http2.Frame.{
          end_stream;
          end_headers;
          padded;
          priority = priority_flag;
          ack;
        })
      (map3 (fun a b c -> (a, b, c)) bool bool bool)
      (map2 (fun a b -> (a, b)) bool bool)

(* Generator for simple DATA frames *)

let data_frame_gen =
  Generator.map
    (fun ((stream_id, payload_data, flags)) ->
      let length = String.length payload_data in
      Http2.Frame.{
        length;
        frame_type = Data;
        flags;
        stream_id;
        payload = DataPayload { data = payload_data; pad_length = None };
      })
    Generator.(triple (int_range 1 100) (string_of char_printable) frame_flags_gen)

(* Generator for SETTINGS frames *)

let settings_frame_gen =
  Generator.map
    (fun flags ->
      Http2.Frame.{
        length = 0;
        frame_type = Settings;
        flags;
        stream_id = 0;
        payload = SettingsPayload [];
      })
    frame_flags_gen

(* Property: Frame serialization produces valid length *)

let frame_length_prop =
  property "Frame serialization includes 9-byte header" Arbitrary.(make settings_frame_gen)
    (fun frame ->
      let serialized = Http2.Serializer.serialize_frame frame in
      String.length serialized >= 9)

(* ===== HTTP/1 Chunked Encoding Property Tests ===== *)

(* Generator for valid chunk sizes (small positive integers) *)

let chunk_size_gen = Generator.int_range 0 100

(* Property: Chunk encoding/decoding round-trip *)

let chunk_roundtrip_prop =
  property "HTTP/1 chunk encode/decode preserves data" Arbitrary.(pair
    (make (Generator.int_range 1 50))
    string)
    (fun ((size, full_data)) ->
      assume (String.length full_data > 0);
      (* Take a substring of the specified size *)
      let actual_size = min size (String.length full_data) in
      let data = String.sub full_data 0 actual_size in
      (* Convert to hex manually for small numbers *)
      let hex_size =
        let n = String.length data in
        if n < 10 then
          Int.to_string n
        else if n < 16 then
          String.make 1 (Char.chr (Char.code 'a' + n - 10))
        else
          Int.to_string n
      in
      let encoded = hex_size ^ "\r\n" ^ data ^ "\r\n" in
      (* Decode the chunk *)
      match Http1.Chunk.parse encoded with
      | Done { value=chunk_result; _ } -> chunk_result.data = data
      | _ -> false)

(* ===== HTTP/1 Request Parser Property Tests ===== *)

(* Generator for HTTP methods *)

let method_gen = Generator.one_of
  [
    Generator.return "GET";
    Generator.return "POST";
    Generator.return "PUT";
    Generator.return "DELETE";
    Generator.return "HEAD";
    Generator.return "OPTIONS";
    Generator.return "PATCH";
  ]

(* Generator for valid URI paths *)

let path_gen =
  Generator.map
    (fun parts ->
      if List.length parts = 0 then
        "/"
      else
        "/" ^ String.concat "/" parts)
    Generator.(list (string_of char_lowercase))

(* Property: Request line parsing succeeds for valid requests *)

let request_parse_prop =
  property "HTTP/1 request parsing succeeds for valid requests" Arbitrary.(make method_gen)
    (fun method_ ->
      let request = method_ ^ " / HTTP/1.1\r\n\r\n" in
      match Http1.Request.parse request with
      | Done { value=parsed; _ } ->
          let parsed_method = Std.Net.Http.Request.method_ parsed |> Std.Net.Http.Method.to_string in
          parsed_method = method_
      | _ -> false)

(* ===== All Property Tests ===== *)

let tests = [
  hpack_encode_prop;
  hpack_deterministic_prop;
  frame_length_prop;
  chunk_roundtrip_prop;
  request_parse_prop;
]

let () =
  Miniriot.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"http:properties" ~tests ~args:Env.args)
    ~args:Env.args
    ()
