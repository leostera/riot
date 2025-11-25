open Std

type config = {
  port : int;
  max_message_size : int;
  max_concurrent_streams : int;
  max_frame_size : int;
}

let default_config = {
  port = 50051;
  max_message_size = 4 * 1024 * 1024;  (* 4MB *)
  max_concurrent_streams = 100;
  max_frame_size = 16 * 1024;  (* 16KB *)
}

type error =
  | Bind_failed of Net.error
  | Accept_failed of Net.error
  | Connection_error of string
  | Handler_error of string

type context = {
  peer : string;
  metadata : Grpc.Metadata.t;
  deadline : float option;
}

type ('req, 'res) unary_handler =
  context -> 'req -> ('res, Grpc.Status.t * string) Result.t

type ('req, 'res) server_streaming_handler =
  context -> 'req -> ('res Iter.t, Grpc.Status.t * string) Result.t

type ('req, 'res) client_streaming_handler =
  context -> 'req Iter.t -> ('res, Grpc.Status.t * string) Result.t

type ('req, 'res) bidi_streaming_handler =
  context -> 'req Iter.t -> ('res Iter.t, Grpc.Status.t * string) Result.t

type method_handler =
  | Unary of (Protobuf.WireFormat.t, Protobuf.WireFormat.t) unary_handler
  | ServerStreaming of (Protobuf.WireFormat.t, Protobuf.WireFormat.t) server_streaming_handler
  | ClientStreaming of (Protobuf.WireFormat.t, Protobuf.WireFormat.t) client_streaming_handler
  | BidiStreaming of (Protobuf.WireFormat.t, Protobuf.WireFormat.t) bidi_streaming_handler

(** Handler registry: maps service/method path to handler *)
type handler_registry = (string, method_handler) Hashtbl.t

type t = {
  config : config;
  registry : handler_registry;
  listener : Net.Socket.listen_socket option Cell.t;
  running : bool Cell.t;
}

let create config = {
  config;
  registry = Hashtbl.create 16;
  listener = Cell.make None;
  running = Cell.make false;
}

let register_method server ~service ~method_ ~handler =
  let path = Format.sprintf "/%s/%s" service method_ in
  Hashtbl.add server.registry path handler

(** Build gRPC method path from service and method *)
let build_path service method_ =
  Format.sprintf "/%s/%s" service method_

(** Parse gRPC method path into service and method *)
let parse_path path =
  match String.split_on_char '/' path with
  | "" :: service :: method_ :: [] -> Some (service, method_)
  | _ -> None

(** Send HTTP/2 HEADERS frame *)
let send_headers writer stream_id headers end_stream =
  let header_block = Http.Http2.Hpack.encode headers in
  let frame = Http.Http2.Frame.Headers {
    stream_id;
    end_stream;
    end_headers = true;
    header_block;
    priority = None;
    pad_length = 0;
  } in
  let encoded = Http.Http2.Frame.encode frame in
  let _ = Net.Socket.send writer encoded in
  ()

(** Send HTTP/2 DATA frame *)
let send_data writer stream_id data end_stream =
  let frame = Http.Http2.Frame.Data {
    stream_id;
    end_stream;
    data;
    pad_length = 0;
  } in
  let encoded = Http.Http2.Frame.encode frame in
  let _ = Net.Socket.send writer encoded in
  ()

(** Send gRPC response headers *)
let send_response_headers writer stream_id ~content_type =
  let headers = [
    (":status", "200");
    ("content-type", content_type);
    ("grpc-encoding", "identity");
  ] in
  send_headers writer stream_id headers false

(** Send gRPC trailers with status *)
let send_trailers writer stream_id status message =
  let status_str = string_of_int (Grpc.Status.to_int status) in
  let headers = [
    ("grpc-status", status_str);
    ("grpc-message", message);
  ] in
  send_headers writer stream_id headers true

(** Handle unary RPC *)
let handle_unary ctx handler writer stream_id request =
  match handler ctx request with
  | Ok response ->
      (* Send response headers *)
      send_response_headers writer stream_id ~content_type:"application/grpc+proto";

      (* Encode and send response message *)
      let payload = Protobuf.WireFormat.encode response in
      let framed = Grpc.Message.encode ~compressed:false ~payload in
      send_data writer stream_id framed false;

      (* Send trailers with OK status *)
      send_trailers writer stream_id Grpc.Status.OK ""

  | Error (status, message) ->
      (* Send response headers *)
      send_response_headers writer stream_id ~content_type:"application/grpc+proto";

      (* Send trailers with error status *)
      send_trailers writer stream_id status message

(** Handle server streaming RPC *)
let handle_server_streaming ctx handler writer stream_id request =
  (* Send response headers immediately *)
  send_response_headers writer stream_id ~content_type:"application/grpc+proto";

  match handler ctx request with
  | Ok response_iter ->
      (* Send each response message *)
      Iter.iter (fun response ->
        let payload = Protobuf.WireFormat.encode response in
        let framed = Grpc.Message.encode ~compressed:false ~payload in
        send_data writer stream_id framed false
      ) response_iter;

      (* Send trailers with OK status *)
      send_trailers writer stream_id Grpc.Status.OK ""

  | Error (status, message) ->
      (* Send trailers with error status *)
      send_trailers writer stream_id status message

(** Handle client streaming RPC *)
let handle_client_streaming ctx handler writer stream_id request_iter =
  match handler ctx request_iter with
  | Ok response ->
      (* Send response headers *)
      send_response_headers writer stream_id ~content_type:"application/grpc+proto";

      (* Encode and send response message *)
      let payload = Protobuf.WireFormat.encode response in
      let framed = Grpc.Message.encode ~compressed:false ~payload in
      send_data writer stream_id framed false;

      (* Send trailers with OK status *)
      send_trailers writer stream_id Grpc.Status.OK ""

  | Error (status, message) ->
      (* Send response headers *)
      send_response_headers writer stream_id ~content_type:"application/grpc+proto";

      (* Send trailers with error status *)
      send_trailers writer stream_id status message

(** Handle bidirectional streaming RPC *)
let handle_bidi_streaming ctx handler writer stream_id request_iter =
  (* Send response headers immediately *)
  send_response_headers writer stream_id ~content_type:"application/grpc+proto";

  match handler ctx request_iter with
  | Ok response_iter ->
      (* Send each response message *)
      Iter.iter (fun response ->
        let payload = Protobuf.WireFormat.encode response in
        let framed = Grpc.Message.encode ~compressed:false ~payload in
        send_data writer stream_id framed false
      ) response_iter;

      (* Send trailers with OK status *)
      send_trailers writer stream_id Grpc.Status.OK ""

  | Error (status, message) ->
      (* Send trailers with error status *)
      send_trailers writer stream_id status message

(** Handle a gRPC call *)
let handle_call server writer stream_id path headers data =
  match Hashtbl.find_opt server.registry path with
  | None ->
      (* Method not found *)
      send_response_headers writer stream_id ~content_type:"application/grpc+proto";
      send_trailers writer stream_id Grpc.Status.Unimplemented
        (Format.sprintf "Method %s not found" path)

  | Some handler ->
      (* Build context *)
      let ctx = {
        peer = "unknown";  (* TODO: get from connection *)
        metadata = Grpc.Metadata.of_http_headers headers;
        deadline = None;  (* TODO: parse from headers *)
      } in

      (* Dispatch to appropriate handler *)
      (match handler with
       | Unary h ->
           (* Decode request *)
           (match Grpc.Message.decode data with
            | Ok (msg, _rest) ->
                (match Protobuf.WireFormat.decode msg.payload with
                 | Ok request ->
                     handle_unary ctx h writer stream_id request
                 | Error _ ->
                     send_response_headers writer stream_id ~content_type:"application/grpc+proto";
                     send_trailers writer stream_id Grpc.Status.InvalidArgument "Failed to decode request")
            | Error _ ->
                send_response_headers writer stream_id ~content_type:"application/grpc+proto";
                send_trailers writer stream_id Grpc.Status.InvalidArgument "Failed to decode message frame")

       | ServerStreaming h ->
           (* Decode request *)
           (match Grpc.Message.decode data with
            | Ok (msg, _rest) ->
                (match Protobuf.WireFormat.decode msg.payload with
                 | Ok request ->
                     handle_server_streaming ctx h writer stream_id request
                 | Error _ ->
                     send_response_headers writer stream_id ~content_type:"application/grpc+proto";
                     send_trailers writer stream_id Grpc.Status.InvalidArgument "Failed to decode request")
            | Error _ ->
                send_response_headers writer stream_id ~content_type:"application/grpc+proto";
                send_trailers writer stream_id Grpc.Status.InvalidArgument "Failed to decode message frame")

       | ClientStreaming _h ->
           (* TODO: Implement client streaming *)
           send_response_headers writer stream_id ~content_type:"application/grpc+proto";
           send_trailers writer stream_id Grpc.Status.Unimplemented "Client streaming not yet implemented"

       | BidiStreaming _h ->
           (* TODO: Implement bidirectional streaming *)
           send_response_headers writer stream_id ~content_type:"application/grpc+proto";
           send_trailers writer stream_id Grpc.Status.Unimplemented "Bidirectional streaming not yet implemented")

(** Handle HTTP/2 connection *)
let handle_connection server socket =
  let reader = IO.Reader.of_socket socket in
  let writer = socket in

  (* Read HTTP/2 preface *)
  let preface = Bytes.create 24 in
  let _ = IO.Reader.read reader preface in

  (* TODO: Validate preface *)

  (* Send SETTINGS frame *)
  let settings_frame = Http.Http2.Frame.Settings {
    ack = false;
    settings = [
      (Http.Http2.Frame.Settings_max_concurrent_streams, server.config.max_concurrent_streams);
      (Http.Http2.Frame.Settings_max_frame_size, server.config.max_frame_size);
    ];
  } in
  let encoded_settings = Http.Http2.Frame.encode settings_frame in
  let _ = Net.Socket.send writer encoded_settings in

  (* Main frame processing loop *)
  let rec process_frames () =
    let frame_parser = Http.Http2.Parser_reader.create () in

    match Http.Http2.Parser_reader.parse frame_parser reader with
    | Http.Http2.Parser_reader.Frame frame ->
        (match frame with
         | Http.Http2.Frame.Headers { stream_id; header_block; end_stream; _ } ->
             (* Decode headers *)
             let headers = Http.Http2.Hpack.decode header_block in

             (* Get :path header *)
             let path = List.assoc_opt ":path" headers |> Option.value ~default:"" in

             if end_stream then
               (* No data, just headers (e.g., for client streaming) *)
               handle_call server writer stream_id path headers (Bytes.empty)
             else
               (* Wait for DATA frame *)
               process_frames ()

         | Http.Http2.Frame.Data { stream_id; data; end_stream; _ } ->
             (* TODO: Accumulate data from multiple frames *)
             (* For now, assume single DATA frame *)

             (* Get path from previous HEADERS frame *)
             (* TODO: Store stream state *)
             let path = "/" in  (* Placeholder *)
             let headers = [] in  (* Placeholder *)

             handle_call server writer stream_id path headers data;

             if not end_stream then
               process_frames ()

         | Http.Http2.Frame.Settings { ack; _ } ->
             if not ack then begin
               (* Send SETTINGS ACK *)
               let ack_frame = Http.Http2.Frame.Settings {
                 ack = true;
                 settings = [];
               } in
               let encoded = Http.Http2.Frame.encode ack_frame in
               let _ = Net.Socket.send writer encoded in
               ()
             end;
             process_frames ()

         | _ ->
             (* Ignore other frame types for now *)
             process_frames ())

    | Http.Http2.Parser_reader.Need_more ->
        process_frames ()

    | Http.Http2.Parser_reader.Error _ ->
        (* Connection error, close *)
        ()
  in

  process_frames ();
  Net.Socket.close socket

let start server =
  (* Bind to port *)
  match Net.Socket.listen ~port:server.config.port ~backlog:128 with
  | Error err ->
      Error (Bind_failed err)
  | Ok listener ->
      Cell.set server.listener (Some listener);
      Cell.set server.running true;

      (* Accept loop *)
      let rec accept_loop () =
        if not (Cell.get server.running) then
          Ok ()
        else
          match Net.Socket.accept listener with
          | Error err ->
              Error (Accept_failed err)
          | Ok client_socket ->
              (* Handle connection in background *)
              (* TODO: Spawn as separate process/fiber *)
              handle_connection server client_socket;
              accept_loop ()
      in
      accept_loop ()

let stop server =
  Cell.set server.running false;
  match Cell.get server.listener with
  | Some listener -> Net.Socket.close_listen_socket listener
  | None -> ()
