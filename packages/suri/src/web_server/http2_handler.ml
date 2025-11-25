open Std

type error = [
  | `Parse_error of Http.Http2.Parser_reader.parse_error
  | `Protocol_error of string
  | `Io_error of string
]

let to_string_error = function
  | `Parse_error err ->
      Format.asprintf "HTTP/2 parse error: %a" Http.Http2.Parser_reader.pp_error err
  | `Protocol_error msg -> Format.sprintf "HTTP/2 protocol error: %s" msg
  | `Io_error msg -> Format.sprintf "IO error: %s" msg

(** Stream state for multiplexed requests *)
type stream_state = {
  stream_id : int;
  headers : (string * string) list Cell.t;
  data_chunks : string list Cell.t;
  end_stream : bool Cell.t;
}

type state = {
  config : Config.t;
  handler : Socket_pool.Connection.t -> Request.t -> Response.t;
  frame_parser : Http.Http2.Parser_reader.state;
  hpack_decoder : Http.Http2.Hpack.decoder;
  hpack_encoder : Http.Http2.Hpack.encoder;
  streams : (int, stream_state) Hashtbl.t;
  preface_verified : bool Cell.t;
  settings_sent : bool Cell.t;
  buffer : string Cell.t;  (** Accumulated data buffer *)
}

let make_handler ~config ~handler ?(sniffed_data = "") () = {
  config;
  handler;
  frame_parser = Http.Http2.Parser_reader.create ();
  hpack_decoder = Http.Http2.Hpack.create_decoder ();
  hpack_encoder = Http.Http2.Hpack.create_encoder ();
  streams = Hashtbl.create 16;
  preface_verified = Cell.make false;
  settings_sent = Cell.make false;
  buffer = Cell.make sniffed_data;
}

(** Verify HTTP/2 connection preface *)
let verify_preface data =
  let preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" in
  if String.length data >= String.length preface then
    String.sub data 0 (String.length preface) = preface
  else
    false

(** Send SETTINGS frame *)
let send_settings conn =
  let settings_frame = Http.Http2.Frame.Settings {
    ack = false;
    settings = [
      (Http.Http2.Frame.Settings_max_concurrent_streams, 100);
      (Http.Http2.Frame.Settings_max_frame_size, 16384);
      (Http.Http2.Frame.Settings_initial_window_size, 65535);
    ];
  } in
  let encoded = Http.Http2.Frame.encode settings_frame in
  match Socket_pool.Connection.send conn encoded with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`Io_error "Connection closed while sending SETTINGS")

(** Send SETTINGS ACK *)
let send_settings_ack conn =
  let ack_frame = Http.Http2.Frame.Settings {
    ack = true;
    settings = [];
  } in
  let encoded = Http.Http2.Frame.encode ack_frame in
  match Socket_pool.Connection.send conn encoded with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`Io_error "Connection closed while sending SETTINGS ACK")

(** Send HTTP/2 HEADERS frame *)
let send_headers conn hpack_encoder stream_id headers end_stream =
  let header_block = Http.Http2.Hpack.encode hpack_encoder headers in
  let frame = Http.Http2.Frame.Headers {
    stream_id;
    end_stream;
    end_headers = true;
    header_block;
    priority = None;
    pad_length = 0;
  } in
  let encoded = Http.Http2.Frame.encode frame in
  match Socket_pool.Connection.send conn encoded with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`Io_error "Connection closed while sending HEADERS")

(** Send HTTP/2 DATA frame *)
let send_data conn stream_id data end_stream =
  let frame = Http.Http2.Frame.Data {
    stream_id;
    end_stream;
    data;
    pad_length = 0;
  } in
  let encoded = Http.Http2.Frame.encode frame in
  match Socket_pool.Connection.send conn encoded with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`Io_error "Connection closed while sending DATA")

(** Convert HTTP/2 headers to Request.t *)
let headers_to_request conn headers body =
  let method_ = List.assoc_opt ":method" headers
    |> Option.map Net.Http.Method.of_string
    |> Option.value ~default:Net.Http.Method.GET
  in
  let uri = List.assoc_opt ":path" headers |> Option.value ~default:"/" in
  let headers = List.filter (fun (k, _) -> not (String.starts_with ~prefix:":" k)) headers in

  Request.make ~method_ ~uri ~headers ~body conn

(** Handle completed stream (all headers and data received *)
let handle_stream conn state stream_id stream =
  if not (Cell.get stream.end_stream) then
    Ok ()  (* Stream not complete yet *)
  else
    let headers = List.rev (Cell.get stream.headers) in
    let body = String.concat "" (List.rev (Cell.get stream.data_chunks)) in

    (* Convert to Request and call handler *)
    let request = headers_to_request conn headers body in
    let response = state.handler conn request in

    (* Send response *)
    let status = Response.status response in
    let response_headers = [
      (":status", string_of_int (Net.Http.Status.to_int status));
    ] @ Response.headers response in

    match send_headers conn state.hpack_encoder stream_id response_headers false with
    | Error e -> Error e
    | Ok () ->
        let body = Response.body response in
        match send_data conn stream_id body true with
        | Error e -> Error e
        | Ok () ->
            (* Clean up stream *)
            Hashtbl.remove state.streams stream_id;
            Ok ()

(** Process a single HTTP/2 frame *)
let process_frame conn state frame =
  match frame with
  | Http.Http2.Frame.Settings { ack; _ } ->
      if not ack then
        (* Client sent SETTINGS, send ACK *)
        send_settings_ack conn
      else
        Ok ()  (* Client ACKed our SETTINGS *)

  | Http.Http2.Frame.Headers { stream_id; header_block; end_stream; _ } ->
      (* Decode headers *)
      (match Http.Http2.Hpack.decode state.hpack_decoder header_block with
       | Ok headers ->
           (* Get or create stream *)
           let stream = match Hashtbl.find_opt state.streams stream_id with
             | Some s -> s
             | None ->
                 let s = {
                   stream_id;
                   headers = Cell.make [];
                   data_chunks = Cell.make [];
                   end_stream = Cell.make false;
                 } in
                 Hashtbl.add state.streams stream_id s;
                 s
           in

           (* Add headers *)
           Cell.set stream.headers (List.rev_append headers (Cell.get stream.headers));

           (* Mark if stream ended *)
           if end_stream then begin
             Cell.set stream.end_stream true;
             handle_stream conn state stream_id stream
           end else
             Ok ()
       | Error _ ->
           Error (`Protocol_error "HPACK decode error"))

  | Http.Http2.Frame.Data { stream_id; data; end_stream; _ } ->
      (* Find stream *)
      (match Hashtbl.find_opt state.streams stream_id with
       | None -> Error (`Protocol_error (Format.sprintf "DATA for unknown stream %d" stream_id))
       | Some stream ->
           (* Add data *)
           Cell.set stream.data_chunks (data :: Cell.get stream.data_chunks);

           (* Mark if stream ended *)
           if end_stream then begin
             Cell.set stream.end_stream true;
             handle_stream conn state stream_id stream
           end else
             Ok ())

  | Http.Http2.Frame.Ping { ack; opaque_data } ->
      if not ack then
        (* Client sent PING, send PONG *)
        let pong_frame = Http.Http2.Frame.Ping { ack = true; opaque_data } in
        let encoded = Http.Http2.Frame.encode pong_frame in
        (match Socket_pool.Connection.send conn encoded with
         | Ok () -> Ok ()
         | Error `Closed -> Error (`Io_error "Connection closed while sending PING"))
      else
        Ok ()  (* PONG received *)

  | _ ->
      (* Ignore other frame types for now *)
      Ok ()

(** Main data handler *)
let handle_data data conn state =
  (* Accumulate data in buffer *)
  let current_buffer = Cell.get state.buffer in
  Cell.set state.buffer (current_buffer ^ data);
  let buffer_data = Cell.get state.buffer in

  (* Verify preface if not yet done *)
  if not (Cell.get state.preface_verified) then begin
    if String.length buffer_data >= 24 then begin
      if verify_preface buffer_data then begin
        Cell.set state.preface_verified true;
        (* Remove preface from buffer *)
        Cell.set state.buffer (String.sub buffer_data 24 (String.length buffer_data - 24));

        (* Send SETTINGS *)
        match send_settings conn with
        | Error e -> Socket_pool.Handler.Error (state, e)
        | Ok () ->
            Cell.set state.settings_sent true;
            Socket_pool.Handler.Continue state
      end else
        Socket_pool.Handler.Error (state, `Protocol_error "Invalid HTTP/2 preface")
    end else
      (* Need more data for preface *)
      Socket_pool.Handler.Continue state
  end else begin
    (* Parse frames *)
    let reader = IO.Reader.of_string (Cell.get state.buffer) in

    let rec parse_frames () =
      match Http.Http2.Parser_reader.parse state.frame_parser reader with
      | Http.Http2.Parser_reader.Frame frame ->
          (match process_frame conn state frame with
           | Ok () -> parse_frames ()
           | Error e -> Socket_pool.Handler.Error (state, e))

      | Http.Http2.Parser_reader.Need_more ->
          (* Update buffer with remaining data *)
          let remaining = IO.Reader.remaining reader in
          Cell.set state.buffer remaining;
          Socket_pool.Handler.Continue state

      | Http.Http2.Parser_reader.Error err ->
          Socket_pool.Handler.Error (state, `Parse_error err)
    in

    parse_frames ()
  end

let handle_connection _conn state =
  Socket_pool.Handler.Continue state

let handle_error _error _conn state =
  Socket_pool.Handler.Close state

let handle_close _conn _state =
  ()

let handle_shutdown _conn state =
  Socket_pool.Handler.Close state

let handle_message _msg _conn state =
  Socket_pool.Handler.Continue state
