open Std

type config = {
  max_message_size : int;
  connect_timeout : Time.Duration.t option;
  default_timeout : Time.Duration.t option;
  user_agent : string;
}

let default_config =
  {
    max_message_size = 4 * 1024 * 1024;  (* 4MB *)
    connect_timeout = None;
    default_timeout = None;
    user_agent = "Riot-Blink-GRPC/0.1.0";
  }

type http2_protocol_error =
  | Missing_preface
  | Settings_not_acked
  | Invalid_stream_state
  | Flow_control_error
  | Stream_closed

type hpack_error =
  | Invalid_header_index of int
  | Invalid_name_index of int
  | Unsupported_encoding
  | Invalid_decoder_state
  | Decode_failed of string

type message_error =
  | Message_size_exceeds_maximum of { size : int; max_size : int }
  | Invalid_compression_flag of int
  | Invalid_message_format of string

type invalid_response_error =
  | No_message_in_unary_response
  | Multiple_messages_in_unary_response
  | Multiple_messages_in_client_streaming_response
  | No_message_in_client_streaming_response
  | Not_awaiting_response
  | No_active_stream
  | Send_side_closed
  | No_active_streaming_call
  | Cannot_send_on_non_streaming_call
  | Not_in_client_streaming_state
  | Not_in_bidirectional_streaming_state

type error =
  | Connection_failed of Net.error
  | Connection_closed
  | Http2_frame_error of Http.Http2.Parser_reader.parse_error
  | Http2_protocol_error of http2_protocol_error
  | Hpack_decode_error of hpack_error
  | Message_decode_error of message_error
  | Protobuf_decode_error of Protobuf.WireFormat.decode_error
  | GRPC_status of Grpc.Status.t * string
  | Timeout
  | Invalid_response of invalid_response_error

type 'a response = {
  headers : Grpc.Metadata.t;
  message : 'a;
  trailers : Grpc.Metadata.t;
  status : Grpc.Status.t;
}

type 'a stream_response = {
  headers : Grpc.Metadata.t;
  messages : 'a list;
  complete : bool;
  trailers : Grpc.Metadata.t option;
  status : Grpc.Status.t option;
}

(** Internal connection state *)
type connection_state =
  | Connected
  | AwaitingResponse of {
      stream_id : int;
      headers : Grpc.Metadata.t Cell.t;
      messages : Protobuf.WireFormat.t list Cell.t;
      trailers : Grpc.Metadata.t Cell.t;
      status : Grpc.Status.t option Cell.t;
      end_stream_received : bool Cell.t;
    }
  | StreamingClient of {
      stream_id : int;
      send_closed : bool Cell.t;  (** Whether send side is closed *)
      headers : Grpc.Metadata.t Cell.t;
      messages : Protobuf.WireFormat.t list Cell.t;
      trailers : Grpc.Metadata.t Cell.t;
      status : Grpc.Status.t option Cell.t;
      end_stream_received : bool Cell.t;
    }
  | StreamingBidi of {
      stream_id : int;
      send_closed : bool Cell.t;
      headers : Grpc.Metadata.t Cell.t;
      messages : Protobuf.WireFormat.t list Cell.t;
      trailers : Grpc.Metadata.t Cell.t;
      status : Grpc.Status.t option Cell.t;
      end_stream_received : bool Cell.t;
    }
  | Closed

type t = {
  config : config;
  uri : Net.Uri.t;
  stream : Net.TcpStream.t;
  reader : IO.Reader.t;
  writer : IO.Writer.t;
  http2_conn : Http.Http2.Connection.t;
  frame_parser : Http.Http2.Parser_reader.state;
  state : connection_state Cell.t;
  next_stream_id : int Cell.t;
}

let connect ?(config = default_config) uri =
  let host = Net.Uri.host uri |> Option.unwrap_or ~default:"localhost" in
  let port = Net.Uri.port uri |> Option.unwrap_or ~default:50051 in

  match Net.Addr.of_host_and_port ~host ~port with
  | Error e -> Error (Connection_failed e)
  | Ok addr -> (
      match Net.TcpStream.connect addr with
      | Error e -> Error (Connection_failed e)
      | Ok tcp_stream ->
          let reader = Net.TcpStream.to_reader tcp_stream in
          let writer = Net.TcpStream.to_writer tcp_stream in

          (* Create HTTP/2 connection *)
          let http2_conn = Http.Http2.Connection.create ~role:Client () in

          (* Send HTTP/2 connection preface: "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" *)
          let preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" in
          let ( let* ) = Result.and_then in
          let* () =
            match IO.write_all writer ~buf:preface with
            | Error e -> Error (Connection_failed e)
            | Ok () -> Ok ()
          in

          (* Send initial SETTINGS frame (empty for now) *)
          let settings_bytes = Bytes.create 9 in
          (* Length = 0 (3 bytes big-endian) *)
          Bytes.set settings_bytes 0 (Char.chr 0);
          Bytes.set settings_bytes 1 (Char.chr 0);
          Bytes.set settings_bytes 2 (Char.chr 0);
          (* Type = SETTINGS (0x4) *)
          Bytes.set settings_bytes 3 (Char.chr 0x04);
          (* Flags = 0 *)
          Bytes.set settings_bytes 4 (Char.chr 0);
          (* Stream ID = 0 (4 bytes big-endian) *)
          Bytes.set settings_bytes 5 (Char.chr 0);
          Bytes.set settings_bytes 6 (Char.chr 0);
          Bytes.set settings_bytes 7 (Char.chr 0);
          Bytes.set settings_bytes 8 (Char.chr 0);

          let* () =
            match IO.write_all writer ~buf:(Bytes.to_string settings_bytes) with
            | Error e -> Error (Connection_failed e)
            | Ok () -> Ok ()
          in

          Ok
            {
              config;
              uri;
              stream = tcp_stream;
              reader;
              writer;
              http2_conn;
              frame_parser = Http.Http2.Parser_reader.create ();
              state = Cell.create Connected;
              next_stream_id = Cell.create 1;  (* Client uses odd stream IDs *)
            })

(** Encode HTTP/2 frame to bytes *)
let encode_frame frame =
  let open Http.Http2.Frame in
  let buf = Buffer.create (9 + frame.length) in

  (* Frame header: 9 bytes *)
  (* Length (24-bit big-endian) *)
  Buffer.add_char buf (Char.chr ((frame.length lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((frame.length lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr (frame.length land 0xFF));

  (* Type *)
  let type_byte =
    match frame.frame_type with
    | Data -> 0x0
    | Headers -> 0x1
    | Priority -> 0x2
    | RstStream -> 0x3
    | Settings -> 0x4
    | PushPromise -> 0x5
    | Ping -> 0x6
    | Goaway -> 0x7
    | WindowUpdate -> 0x8
    | Continuation -> 0x9
  in
  Buffer.add_char buf (Char.chr type_byte);

  (* Flags *)
  let flags_byte =
    (if frame.flags.end_stream then 0x01 else 0) lor
    (if frame.flags.ack then 0x01 else 0) lor
    (if frame.flags.end_headers then 0x04 else 0) lor
    (if frame.flags.padded then 0x08 else 0) lor
    (if frame.flags.priority then 0x20 else 0)
  in
  Buffer.add_char buf (Char.chr flags_byte);

  (* Stream ID (31-bit, big-endian) *)
  Buffer.add_char buf (Char.chr ((frame.stream_id lsr 24) land 0x7F));
  Buffer.add_char buf (Char.chr ((frame.stream_id lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((frame.stream_id lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr (frame.stream_id land 0xFF));

  (* Payload - simplified for now *)
  (match frame.payload with
  | DataPayload { data; _ } -> Buffer.add_string buf data
  | HeadersPayload { header_block_fragment; _ } ->
      Buffer.add_string buf header_block_fragment
  | _ -> ());

  Buffer.contents buf

(** Send HTTP/2 frame *)
let send_frame conn frame =
  let ( let* ) = Result.and_then in
  let frame_bytes = encode_frame frame in
  let* () =
    match IO.write_all conn.writer ~buf:frame_bytes with
    | Ok () -> Ok ()
    | Error e -> Error (Connection_failed e)
  in
  Ok ()

(** Receive and parse next HTTP/2 frame *)
let receive_frame conn =
  match Http.Http2.Parser_reader.parse conn.frame_parser conn.reader with
  | Frame frame -> Ok frame
  | Need_more -> Error Connection_closed  (* EOF or connection closed *)
  | Error e -> Error (Http2_frame_error e)

(** Allocate next stream ID *)
let next_stream_id conn =
  let id = Cell.get conn.next_stream_id in
  Cell.set conn.next_stream_id (id + 2);  (* Client uses odd IDs *)
  id

(** Convert Time.Duration.t to Grpc.Metadata.timeout *)
let duration_to_grpc_timeout (duration : Time.Duration.t) : Grpc.Metadata.timeout =
  let total_millis = Time.Duration.to_millis duration in
  if total_millis < 1000 then
    { value = total_millis; unit = `Milliseconds }
  else
    let total_secs = Time.Duration.to_secs duration in
    if total_secs < 60 then
      { value = total_secs; unit = `Seconds }
    else if total_secs < 3600 then
      { value = total_secs / 60; unit = `Minutes }
    else
      { value = total_secs / 3600; unit = `Hours }

(** Convert Grpc.Message.decode_error to message_error *)
let message_decode_error_to_message_error (e : Grpc.Message.decode_error) : message_error =
  match e with
  | Incomplete_header { have } ->
      Invalid_message_format (format "Incomplete message header: have %d bytes, need 5" have)
  | Message_size_exceeds_maximum { size; max_size } ->
      Message_size_exceeds_maximum { size; max_size }
  | Incomplete_message { need; have } ->
      Invalid_message_format (format "Incomplete message: need %d bytes, have %d" need have)

let call_unary conn ~service ~method_ ~request ?(timeout = conn.config.default_timeout)
    ?(metadata = []) () =
  let ( let* ) = Result.and_then in

  (* Allocate stream ID *)
  let stream_id = next_stream_id conn in

  (* Build request headers *)
  let headers =
    [
      (":method", "POST");
      (":scheme", if Net.Uri.scheme conn.uri = Some "https" then "https" else "http");
      (":path", "/" ^ service ^ "/" ^ method_);
      (":authority", Net.Uri.host conn.uri |> Option.unwrap_or ~default:"localhost");
      ("content-type", "application/grpc+proto");
      ("te", "trailers");
      ("user-agent", conn.config.user_agent);
    ]
  in

  let headers =
    match timeout with
    | Some t -> headers @ [ Grpc.Metadata.timeout (duration_to_grpc_timeout t) ]
    | None -> headers
  in

  let headers = headers @ metadata in

  (* Encode headers with HPACK *)
  let hpack_encoder = Http.Http2.Hpack.create_encoder () in
  let header_block = Http.Http2.Hpack.encode hpack_encoder headers in

  (* Send HEADERS frame *)
  let headers_frame =
    Http.Http2.Frame.{
      length = String.length header_block;
      frame_type = Headers;
      flags = { end_stream = false; end_headers = true; padded = false; priority = false; ack = false };
      stream_id;
      payload =
        HeadersPayload {
          pad_length = None;
          stream_dependency = None;
          weight = None;
          exclusive = false;
          header_block_fragment = header_block;
        };
    }
  in

  let* () = send_frame conn headers_frame in

  (* Encode protobuf request *)
  let request_bytes = Protobuf.WireFormat.encode request in

  (* Encode gRPC message *)
  let grpc_message = Grpc.Message.encode ~compressed:false ~payload:request_bytes in

  (* Send DATA frame with END_STREAM *)
  let data_frame =
    Http.Http2.Frame.{
      length = Bytes.length grpc_message;
      frame_type = Data;
      flags = { end_stream = true; end_headers = false; padded = false; priority = false; ack = false };
      stream_id;
      payload = DataPayload { data = Bytes.to_string grpc_message; pad_length = None };
    }
  in

  let* () = send_frame conn data_frame in

  (* Setup response state *)
  Cell.set conn.state
    (AwaitingResponse {
      stream_id;
      headers = Cell.create [];
      messages = Cell.create [];
      trailers = Cell.create [];
      status = Cell.create None;
      end_stream_received = Cell.create false;
    });

  (* Receive response frames *)
  let rec receive_response () =
    match Cell.get conn.state with
    | AwaitingResponse state_data when not (Cell.get state_data.end_stream_received) -> (
        let* frame = receive_frame conn in

        (* Process frame based on type *)
        match frame.frame_type with
        | Http.Http2.Frame.Headers -> (
            (* Decode HPACK headers *)
            let header_block =
              match frame.payload with
              | Http.Http2.Frame.HeadersPayload { header_block_fragment; _ } -> header_block_fragment
              | _ -> ""
            in

            (* Use non-reentrant HPACK decoder on complete header block *)
            let hpack_decoder = Http.Http2.Hpack.create_decoder () in
            let header_bytes = Bytes.of_string header_block in

            let* hdrs =
              match Http.Http2.Hpack.decode hpack_decoder header_bytes with
              | Ok hdrs -> Ok hdrs
              | Error e -> Error (Hpack_decode_error (Decode_failed e))
            in

            (* Check if this is trailers (has END_STREAM) or initial headers *)
            if frame.flags.end_stream then (
              Cell.set state_data.trailers hdrs;
              Cell.set state_data.end_stream_received true)
            else
              Cell.set state_data.headers hdrs;

            receive_response ())

        | Http.Http2.Frame.Data -> (
            let data_payload =
              match frame.payload with
              | Http.Http2.Frame.DataPayload { data; _ } -> data
              | _ -> ""
            in

            if data_payload = "" then (
              (* Empty DATA frame *)
              if frame.flags.end_stream then
                Cell.set state_data.end_stream_received true;
              receive_response ())
            else (
              (* Decode gRPC message *)
              let data_bytes = Bytes.of_string data_payload in

              let* grpc_msg =
                match Grpc.Message.decode data_bytes with
                | Ok (msg, _remaining) -> Ok msg
                | Error e -> Error (Message_decode_error (message_decode_error_to_message_error e))
              in

              (* Decode protobuf payload *)
              let* protobuf_msg =
                match Protobuf.WireFormat.decode grpc_msg.payload with
                | Ok msg -> Ok msg
                | Error e -> Error (Protobuf_decode_error e)
              in

              let msgs = Cell.get state_data.messages in
              Cell.set state_data.messages (msgs @ [protobuf_msg]);

              if frame.flags.end_stream then
                Cell.set state_data.end_stream_received true;

              receive_response ()))

        | _ ->
            (* Ignore other frame types (SETTINGS ACK, PING, etc.) *)
            receive_response ())

    | AwaitingResponse state_data -> (
        (* END_STREAM received, extract response *)
        let headers = Cell.get state_data.headers in
        let messages = Cell.get state_data.messages in
        let trailers = Cell.get state_data.trailers in

        (* Extract status from trailers *)
        let status_code =
          List.find_map
            (fun (name, value) ->
              if name = "grpc-status" then
                match int_of_string_opt value with
                | Some code -> Grpc.Status.of_int code
                | None -> None
              else None)
            trailers
          |> Option.unwrap_or ~default:Grpc.Status.OK
        in

        let status_message =
          List.find_map
            (fun (name, value) -> if name = "grpc-message" then Some value else None)
            trailers
          |> Option.unwrap_or ~default:""
        in

        (* Check if status is OK *)
        if status_code <> Grpc.Status.OK then
          Error (GRPC_status (status_code, status_message))
        else
          match messages with
          | [ msg ] ->
              Cell.set conn.state Connected;
              Ok { headers; message = msg; trailers; status = status_code }
          | [] ->
              Error (Invalid_response No_message_in_unary_response)
          | _ ->
              Error (Invalid_response Multiple_messages_in_unary_response))

    | Connected ->
        Error (Invalid_response Not_awaiting_response)
    | Closed ->
        Error Connection_closed
  in

  receive_response ()

let call_server_streaming conn ~service ~method_ ~request ?timeout ?metadata () =
  let ( let* ) = Result.and_then in

  (* Similar to call_unary but returns stream handle *)
  let stream_id = next_stream_id conn in

  (* Build request headers *)
  let headers =
    [
      (":method", "POST");
      (":scheme", if Net.Uri.scheme conn.uri = Some "https" then "https" else "http");
      (":path", "/" ^ service ^ "/" ^ method_);
      (":authority", Net.Uri.host conn.uri |> Option.unwrap_or ~default:"localhost");
      ("content-type", "application/grpc+proto");
      ("te", "trailers");
      ("user-agent", conn.config.user_agent);
    ]
  in

  let headers =
    match timeout with
    | Some t -> headers @ [ Grpc.Metadata.timeout (duration_to_grpc_timeout t) ]
    | None -> headers
  in

  let headers =
    match metadata with
    | Some md -> headers @ md
    | None -> headers
  in

  (* Encode and send HEADERS frame *)
  let hpack_encoder = Http.Http2.Hpack.create_encoder () in
  let header_block = Http.Http2.Hpack.encode hpack_encoder headers in

  let headers_frame =
    Http.Http2.Frame.{
      length = String.length header_block;
      frame_type = Headers;
      flags = { end_stream = false; end_headers = true; padded = false; priority = false; ack = false };
      stream_id;
      payload =
        HeadersPayload {
          pad_length = None;
          stream_dependency = None;
          weight = None;
          exclusive = false;
          header_block_fragment = header_block;
        };
    }
  in

  let* () = send_frame conn headers_frame in

  (* Encode and send request message *)
  let request_bytes = Protobuf.WireFormat.encode request in
  let grpc_message = Grpc.Message.encode ~compressed:false ~payload:request_bytes in

  let data_frame =
    Http.Http2.Frame.{
      length = Bytes.length grpc_message;
      frame_type = Data;
      flags = { end_stream = true; end_headers = false; padded = false; priority = false; ack = false };
      stream_id;
      payload = DataPayload { data = Bytes.to_string grpc_message; pad_length = None };
    }
  in

  let* () = send_frame conn data_frame in

  (* Setup streaming state *)
  Cell.set conn.state
    (AwaitingResponse {
      stream_id;
      headers = Cell.create [];
      messages = Cell.create [];
      trailers = Cell.create [];
      status = Cell.create None;
      end_stream_received = Cell.create false;
    });

  Ok {
    headers = [];
    messages = [];
    complete = false;
    trailers = None;
    status = None;
  }

let receive_stream conn =
  let ( let* ) = Result.and_then in

  match Cell.get conn.state with
  | AwaitingResponse state_data -> (
      let* frame = receive_frame conn in

      match frame.frame_type with
      | Http.Http2.Frame.Headers -> (
          (* Initial headers or trailers *)
          let header_block =
            match frame.payload with
            | Http.Http2.Frame.HeadersPayload { header_block_fragment; _ } -> header_block_fragment
            | _ -> ""
          in

          let hpack_decoder = Http.Http2.Hpack.create_decoder () in
          let header_bytes = Bytes.of_string header_block in

          let* hdrs =
            match Http.Http2.Hpack.decode hpack_decoder header_bytes with
            | Ok hdrs -> Ok hdrs
            | Error e -> Error (Hpack_decode_error (Decode_failed e))
          in

          if frame.flags.end_stream then (
            (* Trailers *)
            Cell.set state_data.trailers hdrs;
            Cell.set state_data.end_stream_received true;

            (* Extract status *)
            let status_code =
              List.find_map
                (fun (name, value) ->
                  if name = "grpc-status" then
                    match int_of_string_opt value with
                    | Some code -> Grpc.Status.of_int code
                    | None -> None
                  else None)
                hdrs
              |> Option.unwrap_or ~default:Grpc.Status.OK
            in
            Cell.set state_data.status (Some status_code))
          else
            Cell.set state_data.headers hdrs;

          Ok {
            headers = Cell.get state_data.headers;
            messages = Cell.get state_data.messages;
            complete = Cell.get state_data.end_stream_received;
            trailers = if Cell.get state_data.end_stream_received then Some (Cell.get state_data.trailers) else None;
            status = Cell.get state_data.status;
          })

      | Http.Http2.Frame.Data -> (
          let data_payload =
            match frame.payload with
            | Http.Http2.Frame.DataPayload { data; _ } -> data
            | _ -> ""
          in

          if data_payload = "" then (
            if frame.flags.end_stream then
              Cell.set state_data.end_stream_received true;

            Ok {
              headers = Cell.get state_data.headers;
              messages = Cell.get state_data.messages;
              complete = Cell.get state_data.end_stream_received;
              trailers = if Cell.get state_data.end_stream_received then Some (Cell.get state_data.trailers) else None;
              status = Cell.get state_data.status;
            })
          else (
            (* Decode gRPC message *)
            let data_bytes = Bytes.of_string data_payload in

            let* grpc_msg =
              match Grpc.Message.decode data_bytes with
              | Ok (msg, _remaining) -> Ok msg
              | Error e -> Error (Message_decode_error (Invalid_message_format e))
            in

            (* Decode protobuf *)
            let* protobuf_msg =
              match Protobuf.WireFormat.decode grpc_msg.payload with
              | Ok msg -> Ok msg
              | Error e -> Error (Protobuf_decode_error e)
            in

            let msgs = Cell.get state_data.messages in
            Cell.set state_data.messages (msgs @ [protobuf_msg]);

            if frame.flags.end_stream then
              Cell.set state_data.end_stream_received true;

            Ok {
              headers = Cell.get state_data.headers;
              messages = Cell.get state_data.messages;
              complete = Cell.get state_data.end_stream_received;
              trailers = if Cell.get state_data.end_stream_received then Some (Cell.get state_data.trailers) else None;
              status = Cell.get state_data.status;
            }))

      | _ ->
          (* Ignore other frames, return current state *)
          Ok {
            headers = Cell.get state_data.headers;
            messages = Cell.get state_data.messages;
            complete = Cell.get state_data.end_stream_received;
            trailers = if Cell.get state_data.end_stream_received then Some (Cell.get state_data.trailers) else None;
            status = Cell.get state_data.status;
          })

  | Connected ->
      Error (Invalid_response No_active_stream)
  | Closed ->
      Error Connection_closed

let call_client_streaming conn ~service ~method_ ?timeout ?metadata () =
  let ( let* ) = Result.and_then in

  (* Allocate stream ID *)
  let stream_id = next_stream_id conn in

  (* Build request headers *)
  let headers =
    [
      (":method", "POST");
      (":scheme", if Net.Uri.scheme conn.uri = Some "https" then "https" else "http");
      (":path", "/" ^ service ^ "/" ^ method_);
      (":authority", Net.Uri.host conn.uri |> Option.unwrap_or ~default:"localhost");
      ("content-type", "application/grpc+proto");
      ("te", "trailers");
      ("user-agent", conn.config.user_agent);
    ]
  in

  let headers =
    match timeout with
    | Some t -> headers @ [ Grpc.Metadata.timeout (duration_to_grpc_timeout t) ]
    | None -> headers
  in

  let headers =
    match metadata with
    | Some md -> headers @ md
    | None -> headers
  in

  (* Encode and send HEADERS frame (no END_STREAM - will send messages later) *)
  let hpack_encoder = Http.Http2.Hpack.create_encoder () in
  let header_block = Http.Http2.Hpack.encode hpack_encoder headers in

  let headers_frame =
    Http.Http2.Frame.{
      length = String.length header_block;
      frame_type = Headers;
      flags = { end_stream = false; end_headers = true; padded = false; priority = false; ack = false };
      stream_id;
      payload =
        HeadersPayload {
          pad_length = None;
          stream_dependency = None;
          weight = None;
          exclusive = false;
          header_block_fragment = header_block;
        };
    }
  in

  let* () = send_frame conn headers_frame in

  (* Setup client streaming state *)
  Cell.set conn.state
    (StreamingClient {
      stream_id;
      send_closed = Cell.create false;
      headers = Cell.create [];
      messages = Cell.create [];
      trailers = Cell.create [];
      status = Cell.create None;
      end_stream_received = Cell.create false;
    });

  Ok ()

let send_message conn message =
  let ( let* ) = Result.and_then in

  match Cell.get conn.state with
  | StreamingClient state_data | StreamingBidi state_data when not (Cell.get state_data.send_closed) ->
      (* Encode protobuf message *)
      let message_bytes = Protobuf.WireFormat.encode message in

      (* Encode gRPC message *)
      let grpc_message = Grpc.Message.encode ~compressed:false ~payload:message_bytes in

      (* Send DATA frame (without END_STREAM - stream still open) *)
      let data_frame =
        Http.Http2.Frame.{
          length = Bytes.length grpc_message;
          frame_type = Data;
          flags = { end_stream = false; end_headers = false; padded = false; priority = false; ack = false };
          stream_id = state_data.stream_id;
          payload = DataPayload { data = Bytes.to_string grpc_message; pad_length = None };
        }
      in

      send_frame conn data_frame

  | StreamingClient _ | StreamingBidi _ ->
      Error (Invalid_response Send_side_closed)
  | Connected ->
      Error (Invalid_response No_active_streaming_call)
  | AwaitingResponse _ ->
      Error (Invalid_response Cannot_send_on_non_streaming_call)
  | Closed ->
      Error Connection_closed

let finish_client_stream conn =
  let ( let* ) = Result.and_then in

  match Cell.get conn.state with
  | StreamingClient state_data when not (Cell.get state_data.send_closed) ->
      (* Send empty DATA frame with END_STREAM to close send side *)
      let data_frame =
        Http.Http2.Frame.{
          length = 0;
          frame_type = Data;
          flags = { end_stream = true; end_headers = false; padded = false; priority = false; ack = false };
          stream_id = state_data.stream_id;
          payload = DataPayload { data = ""; pad_length = None };
        }
      in

      let* () = send_frame conn data_frame in
      Cell.set state_data.send_closed true;

      (* Now receive single response *)
      let rec receive_response () =
        if not (Cell.get state_data.end_stream_received) then (
          let* frame = receive_frame conn in

          match frame.frame_type with
          | Http.Http2.Frame.Headers -> (
              let header_block =
                match frame.payload with
                | Http.Http2.Frame.HeadersPayload { header_block_fragment; _ } -> header_block_fragment
                | _ -> ""
              in

              let hpack_decoder = Http.Http2.Hpack.create_decoder () in
              let header_bytes = Bytes.of_string header_block in

              let* hdrs =
                match Http.Http2.Hpack.decode hpack_decoder header_bytes with
                | Ok hdrs -> Ok hdrs
                | Error e -> Error (Hpack_decode_error (Decode_failed e))
              in

              if frame.flags.end_stream then (
                Cell.set state_data.trailers hdrs;
                Cell.set state_data.end_stream_received true)
              else
                Cell.set state_data.headers hdrs;

              receive_response ())

          | Http.Http2.Frame.Data -> (
              let data_payload =
                match frame.payload with
                | Http.Http2.Frame.DataPayload { data; _ } -> data
                | _ -> ""
              in

              if data_payload = "" then (
                if frame.flags.end_stream then
                  Cell.set state_data.end_stream_received true;
                receive_response ())
              else (
                let data_bytes = Bytes.of_string data_payload in

                let* grpc_msg =
                  match Grpc.Message.decode data_bytes with
                  | Ok (msg, _remaining) -> Ok msg
                  | Error e -> Error (Message_decode_error (Invalid_message_format e))
                in

                let* protobuf_msg =
                  match Protobuf.WireFormat.decode grpc_msg.payload with
                  | Ok msg -> Ok msg
                  | Error e -> Error (Protobuf_decode_error e)
                in

                let msgs = Cell.get state_data.messages in
                Cell.set state_data.messages (msgs @ [protobuf_msg]);

                if frame.flags.end_stream then
                  Cell.set state_data.end_stream_received true;

                receive_response ()))

          | _ ->
              receive_response ())
        else (
          (* END_STREAM received, extract response *)
          let headers = Cell.get state_data.headers in
          let messages = Cell.get state_data.messages in
          let trailers = Cell.get state_data.trailers in

          let status_code =
            List.find_map
              (fun (name, value) ->
                if name = "grpc-status" then
                  match int_of_string_opt value with
                  | Some code -> Grpc.Status.of_int code
                  | None -> None
                else None)
              trailers
            |> Option.unwrap_or ~default:Grpc.Status.OK
          in

          let status_message =
            List.find_map
              (fun (name, value) -> if name = "grpc-message" then Some value else None)
              trailers
            |> Option.unwrap_or ~default:""
          in

          if status_code <> Grpc.Status.OK then
            Error (GRPC_status (status_code, status_message))
          else
            match messages with
            | [ msg ] ->
                Cell.set conn.state Connected;
                Ok { headers; message = msg; trailers; status = status_code }
            | [] ->
                Error (Invalid_response No_message_in_client_streaming_response)
            | _ ->
                Error (Invalid_response Multiple_messages_in_client_streaming_response))
      in

      receive_response ()

  | StreamingClient _ ->
      Error (Invalid_response Send_side_closed)
  | _ ->
      Error (Invalid_response Not_in_client_streaming_state)

let call_bidi_streaming conn ~service ~method_ ?timeout ?metadata () =
  let ( let* ) = Result.and_then in

  (* Allocate stream ID *)
  let stream_id = next_stream_id conn in

  (* Build request headers *)
  let headers =
    [
      (":method", "POST");
      (":scheme", if Net.Uri.scheme conn.uri = Some "https" then "https" else "http");
      (":path", "/" ^ service ^ "/" ^ method_);
      (":authority", Net.Uri.host conn.uri |> Option.unwrap_or ~default:"localhost");
      ("content-type", "application/grpc+proto");
      ("te", "trailers");
      ("user-agent", conn.config.user_agent);
    ]
  in

  let headers =
    match timeout with
    | Some t -> headers @ [ Grpc.Metadata.timeout (duration_to_grpc_timeout t) ]
    | None -> headers
  in

  let headers =
    match metadata with
    | Some md -> headers @ md
    | None -> headers
  in

  (* Encode and send HEADERS frame *)
  let hpack_encoder = Http.Http2.Hpack.create_encoder () in
  let header_block = Http.Http2.Hpack.encode hpack_encoder headers in

  let headers_frame =
    Http.Http2.Frame.{
      length = String.length header_block;
      frame_type = Headers;
      flags = { end_stream = false; end_headers = true; padded = false; priority = false; ack = false };
      stream_id;
      payload =
        HeadersPayload {
          pad_length = None;
          stream_dependency = None;
          weight = None;
          exclusive = false;
          header_block_fragment = header_block;
        };
    }
  in

  let* () = send_frame conn headers_frame in

  (* Setup bidirectional streaming state *)
  Cell.set conn.state
    (StreamingBidi {
      stream_id;
      send_closed = Cell.create false;
      headers = Cell.create [];
      messages = Cell.create [];
      trailers = Cell.create [];
      status = Cell.create None;
      end_stream_received = Cell.create false;
    });

  Ok ()

let receive_message conn =
  let ( let* ) = Result.and_then in

  match Cell.get conn.state with
  | StreamingBidi state_data when not (Cell.get state_data.end_stream_received) -> (
      (* Non-blocking receive attempt *)
      let* frame = receive_frame conn in

      match frame.frame_type with
      | Http.Http2.Frame.Headers -> (
          let header_block =
            match frame.payload with
            | Http.Http2.Frame.HeadersPayload { header_block_fragment; _ } -> header_block_fragment
            | _ -> ""
          in

          let hpack_decoder = Http.Http2.Hpack.create_decoder () in
          let header_bytes = Bytes.of_string header_block in

          let* hdrs =
            match Http.Http2.Hpack.decode hpack_decoder header_bytes with
            | Ok hdrs -> Ok hdrs
            | Error e -> Error (Hpack_decode_error (Decode_failed e))
          in

          if frame.flags.end_stream then (
            Cell.set state_data.trailers hdrs;
            Cell.set state_data.end_stream_received true;

            let status_code =
              List.find_map
                (fun (name, value) ->
                  if name = "grpc-status" then
                    match int_of_string_opt value with
                    | Some code -> Grpc.Status.of_int code
                    | None -> None
                  else None)
                hdrs
              |> Option.unwrap_or ~default:Grpc.Status.OK
            in
            Cell.set state_data.status (Some status_code))
          else
            Cell.set state_data.headers hdrs;

          Ok None  (* Headers received, no message yet *))

      | Http.Http2.Frame.Data -> (
          let data_payload =
            match frame.payload with
            | Http.Http2.Frame.DataPayload { data; _ } -> data
            | _ -> ""
          in

          if data_payload = "" then (
            if frame.flags.end_stream then
              Cell.set state_data.end_stream_received true;
            Ok None)
          else (
            let data_bytes = Bytes.of_string data_payload in

            let* grpc_msg =
              match Grpc.Message.decode data_bytes with
              | Ok (msg, _remaining) -> Ok msg
              | Error e -> Error (Message_decode_error (Invalid_message_format e))
            in

            let* protobuf_msg =
              match Protobuf.WireFormat.decode grpc_msg.payload with
              | Ok msg -> Ok msg
              | Error e -> Error (Protobuf_decode_error e)
            in

            let msgs = Cell.get state_data.messages in
            Cell.set state_data.messages (msgs @ [protobuf_msg]);

            if frame.flags.end_stream then
              Cell.set state_data.end_stream_received true;

            Ok (Some protobuf_msg)))

      | _ ->
          (* Ignore other frames *)
          Ok None)

  | StreamingBidi state_data ->
      (* Stream already complete *)
      Ok None
  | _ ->
      Error (Invalid_response Not_in_bidirectional_streaming_state)

let close_send conn =
  let ( let* ) = Result.and_then in

  match Cell.get conn.state with
  | StreamingBidi state_data when not (Cell.get state_data.send_closed) ->
      (* Send empty DATA frame with END_STREAM *)
      let data_frame =
        Http.Http2.Frame.{
          length = 0;
          frame_type = Data;
          flags = { end_stream = true; end_headers = false; padded = false; priority = false; ack = false };
          stream_id = state_data.stream_id;
          payload = DataPayload { data = ""; pad_length = None };
        }
      in

      let* () = send_frame conn data_frame in
      Cell.set state_data.send_closed true;
      Ok ()

  | StreamingBidi _ ->
      Error (Invalid_response Send_side_closed)
  | _ ->
      Error (Invalid_response Not_in_bidirectional_streaming_state)

let finish_bidi_stream conn =
  let ( let* ) = Result.and_then in

  match Cell.get conn.state with
  | StreamingBidi state_data -> (
      (* Wait for END_STREAM if not received yet *)
      let rec wait_for_trailers () =
        if not (Cell.get state_data.end_stream_received) then (
          let* frame = receive_frame conn in

          match frame.frame_type with
          | Http.Http2.Frame.Headers when frame.flags.end_stream -> (
              let header_block =
                match frame.payload with
                | Http.Http2.Frame.HeadersPayload { header_block_fragment; _ } -> header_block_fragment
                | _ -> ""
              in

              let hpack_decoder = Http.Http2.Hpack.create_decoder () in
              let header_bytes = Bytes.of_string header_block in

              let* hdrs =
                match Http.Http2.Hpack.decode hpack_decoder header_bytes with
                | Ok hdrs -> Ok hdrs
                | Error e -> Error (Hpack_decode_error (Decode_failed e))
              in

              Cell.set state_data.trailers hdrs;
              Cell.set state_data.end_stream_received true;

              let status_code =
                List.find_map
                  (fun (name, value) ->
                    if name = "grpc-status" then
                      match int_of_string_opt value with
                      | Some code -> Grpc.Status.of_int code
                      | None -> None
                    else None)
                  hdrs
                |> Option.unwrap_or ~default:Grpc.Status.OK
              in
              Cell.set state_data.status (Some status_code);

              wait_for_trailers ())

          | Http.Http2.Frame.Data when frame.flags.end_stream ->
              Cell.set state_data.end_stream_received true;
              wait_for_trailers ()

          | _ ->
              wait_for_trailers ())
        else
          Ok ()
      in

      let* () = wait_for_trailers () in

      let trailers = Cell.get state_data.trailers in
      let status = Cell.get state_data.status |> Option.unwrap_or ~default:Grpc.Status.OK in

      Cell.set conn.state Connected;

      Ok (trailers, status))

  | _ ->
      Error (Invalid_response Not_in_bidirectional_streaming_state)

let close conn =
  Cell.set conn.state Closed;
  Net.TcpStream.close conn.stream
