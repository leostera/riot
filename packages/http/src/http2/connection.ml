open Std

(** HTTP/2 Connection Management (RFC 9113) *)

type state = Idle | Active | GoingAway | Closed

type stream_state =
  | StreamIdle
  | StreamOpen
  | StreamReservedLocal
  | StreamReservedRemote
  | StreamHalfClosedLocal
  | StreamHalfClosedRemote
  | StreamClosed

type stream = {
  id : int;
  state : stream_state Cell.t;
  window_size : int Cell.t;
  headers : Hpack.header list Cell.t;
  data_chunks : bytes list Cell.t;
}

type settings = {
  header_table_size : int Cell.t;
  enable_push : bool Cell.t;
  max_concurrent_streams : int option Cell.t;
  initial_window_size : int Cell.t;
  max_frame_size : int Cell.t;
  max_header_list_size : int option Cell.t;
}

type config = {
  max_frame_size : int;
  initial_window_size : int;
  max_concurrent_streams : int;
  enable_push : bool;
}

type role = Client | Server

type t = {
  role : role;
  state : state Cell.t;
  streams : (int, stream) Hashtbl.t;
  next_stream_id : int Cell.t;
  connection_window_size : int Cell.t;
  local_settings : settings;
  remote_settings : settings;
  hpack_encoder : Hpack.encoder;
  hpack_decoder : Hpack.decoder;
  last_stream_id : int Cell.t;  (** Last stream ID we initiated *)
  peer_last_stream_id : int Cell.t;  (** Last stream ID peer initiated *)
}

type event =
  | HeadersReceived of { stream_id : int; headers : Hpack.header list; end_stream : bool }
  | DataReceived of { stream_id : int; data : bytes; end_stream : bool }
  | SettingsReceived of Frame.setting list
  | SettingsAckReceived
  | PingReceived of { data : string }
  | PingAckReceived of { data : string }
  | WindowUpdateReceived of { stream_id : int; increment : int }
  | GoawayReceived of {
      last_stream_id : int;
      error_code : Frame.error_code;
      debug_data : string;
    }
  | RstStreamReceived of { stream_id : int; error_code : Frame.error_code }
  | PriorityReceived of {
      stream_id : int;
      dependency : int;
      weight : int;
      exclusive : bool;
    }

(** {1 Constants} *)

(** RFC 9113: Connection preface for clients *)
let client_preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

(** Default settings per RFC 9113 *)
let default_config =
  {
    max_frame_size = 16384;
    initial_window_size = 65535;
    max_concurrent_streams = 100;
    enable_push = true;
  }

let create_settings config =
  {
    header_table_size = Cell.create 4096;
    enable_push = Cell.create config.enable_push;
    max_concurrent_streams = Cell.create (Some config.max_concurrent_streams);
    initial_window_size = Cell.create config.initial_window_size;
    max_frame_size = Cell.create config.max_frame_size;
    max_header_list_size = Cell.create None;
  }

(** {1 Connection Lifecycle} *)

let create ~role ?(config = default_config) () =
  {
    role;
    state = Cell.create Idle;
    streams = Hashtbl.create 16;
    next_stream_id = Cell.create (match role with Client -> 1 | Server -> 2);
    connection_window_size = Cell.create 65535;
    local_settings = create_settings config;
    remote_settings = create_settings config;
    hpack_encoder = Hpack.create_encoder ~max_dynamic_table_size:4096 ();
    hpack_decoder = Hpack.create_decoder ~max_dynamic_table_size:4096 ();
    last_stream_id = Cell.create 0;
    peer_last_stream_id = Cell.create 0;
  }

let send_preface conn =
  (* Create initial SETTINGS frame *)
  let settings_list =
    [
      Frame.HeaderTableSize (Cell.get conn.local_settings.header_table_size);
      Frame.EnablePush (Cell.get conn.local_settings.enable_push);
      Frame.MaxConcurrentStreams
        (Option.value
           (Cell.get conn.local_settings.max_concurrent_streams)
           ~default:100);
      Frame.InitialWindowSize (Cell.get conn.local_settings.initial_window_size);
      Frame.MaxFrameSize (Cell.get conn.local_settings.max_frame_size);
    ]
  in

  let settings_frame =
    {
      Frame.length = 0;  (* Will be calculated during serialization *)
      frame_type = Frame.Settings;
      flags = { Frame.end_stream = false; end_headers = false; padded = false; priority = false; ack = false };
      stream_id = 0;
      payload = Frame.SettingsPayload settings_list;
    }
  in

  let settings_bytes = Serializer.serialize_frame settings_frame in

  match conn.role with
  | Client ->
      (* Client sends preface string + SETTINGS *)
      let buf = Buffer.create (String.length client_preface + Bytes.length settings_bytes) in
      Buffer.add_string buf client_preface;
      Buffer.add_bytes buf settings_bytes;
      Cell.set conn.state Active;
      Buffer.to_bytes buf
  | Server ->
      (* Server sends only SETTINGS *)
      Cell.set conn.state Active;
      settings_bytes

(** {1 Stream Management} *)

let is_valid_stream_id ~role stream_id =
  match role with
  | Client -> stream_id mod 2 = 1  (* Client streams are odd *)
  | Server -> stream_id mod 2 = 0  (* Server streams are even *)

let create_stream conn =
  if Cell.get conn.state <> Active then Error "Connection not active"
  else
    let stream_id = Cell.get conn.next_stream_id in
    Cell.set conn.next_stream_id (stream_id + 2);  (* Skip to next valid ID *)
    Cell.set conn.last_stream_id stream_id;

    let stream =
      {
        id = stream_id;
        state = Cell.create StreamIdle;
        window_size = Cell.create (Cell.get conn.local_settings.initial_window_size);
        headers = Cell.create [];
        data_chunks = Cell.create [];
      }
    in

    Hashtbl.add conn.streams stream_id stream;
    Ok stream_id

let get_stream conn stream_id = Hashtbl.find_opt conn.streams stream_id

let send_headers conn ~stream_id ~headers ~end_stream =
  match get_stream conn stream_id with
  | None -> Error (format "Stream %d not found" stream_id)
  | Some stream -> (
      (* Encode headers using HPACK *)
      let encoded_headers = Hpack.encode conn.hpack_encoder ~headers in

      (* Create HEADERS frame *)
      let frame =
        {
          Frame.length = Bytes.length encoded_headers;
          frame_type = Frame.Headers;
          flags =
            {
              Frame.end_stream;
              end_headers = true;  (* For simplicity, always send complete headers *)
              padded = false;
              priority = false;
              ack = false;
            };
          stream_id;
          payload =
            Frame.HeadersPayload
              {
                pad_length = None;
                stream_dependency = None;
                weight = None;
                exclusive = false;
                header_block_fragment = Bytes.to_string encoded_headers;
              };
        }
      in

      (* Update stream state *)
      if end_stream then
        Cell.set stream.state StreamHalfClosedLocal;

      Ok (Serializer.serialize_frame frame))

let send_data conn ~stream_id ~data ~end_stream =
  match get_stream conn stream_id with
  | None -> Error (format "Stream %d not found" stream_id)
  | Some stream -> (
      let data_len = Bytes.length data in

      (* Check flow control window *)
      let stream_window = Cell.get stream.window_size in
      let conn_window = Cell.get conn.connection_window_size in

      if data_len > stream_window then
        Error (format "Data size %d exceeds stream window %d" data_len stream_window)
      else if data_len > conn_window then
        Error (format "Data size %d exceeds connection window %d" data_len conn_window)
      else
        (* Create DATA frame *)
        let frame =
          {
            Frame.length = data_len;
            frame_type = Frame.Data;
            flags =
              {
                Frame.end_stream;
                end_headers = false;
                padded = false;
                priority = false;
                ack = false;
              };
            stream_id;
            payload = Frame.DataPayload { data = Bytes.to_string data; pad_length = None };
          }
        in

        (* Update flow control windows *)
        Cell.set stream.window_size (stream_window - data_len);
        Cell.set conn.connection_window_size (conn_window - data_len);

        (* Update stream state *)
        if end_stream then
          Cell.set stream.state StreamHalfClosedLocal;

        Ok (Serializer.serialize_frame frame))

let reset_stream conn ~stream_id ~error_code =
  let frame =
    {
      Frame.length = 4;
      frame_type = Frame.RstStream;
      flags =
        {
          Frame.end_stream = false;
          end_headers = false;
          padded = false;
          priority = false;
          ack = false;
        };
      stream_id;
      payload = Frame.RstStreamPayload error_code;
    }
  in

  (match get_stream conn stream_id with
  | Some stream -> Cell.set stream.state StreamClosed
  | None -> ());

  Serializer.serialize_frame frame

(** {1 Flow Control} *)

let send_window_update_connection conn ~increment =
  let frame =
    {
      Frame.length = 4;
      frame_type = Frame.WindowUpdate;
      flags =
        {
          Frame.end_stream = false;
          end_headers = false;
          padded = false;
          priority = false;
          ack = false;
        };
      stream_id = 0;  (* 0 means connection-level *)
      payload = Frame.WindowUpdatePayload increment;
    }
  in

  Cell.set conn.connection_window_size (Cell.get conn.connection_window_size + increment);
  Serializer.serialize_frame frame

let send_window_update_stream conn ~stream_id ~increment =
  let frame =
    {
      Frame.length = 4;
      frame_type = Frame.WindowUpdate;
      flags =
        {
          Frame.end_stream = false;
          end_headers = false;
          padded = false;
          priority = false;
          ack = false;
        };
      stream_id;
      payload = Frame.WindowUpdatePayload increment;
    }
  in

  (match get_stream conn stream_id with
  | Some stream ->
      Cell.set stream.window_size (Cell.get stream.window_size + increment)
  | None -> ());

  Serializer.serialize_frame frame

let connection_window_size conn = Cell.get conn.connection_window_size

let stream_window_size conn ~stream_id =
  match get_stream conn stream_id with
  | Some stream -> Some (Cell.get stream.window_size)
  | None -> None

(** {1 Settings} *)

let update_settings conn settings =
  let frame =
    {
      Frame.length = List.length settings * 6;
      frame_type = Frame.Settings;
      flags =
        {
          Frame.end_stream = false;
          end_headers = false;
          padded = false;
          priority = false;
          ack = false;
        };
      stream_id = 0;
      payload = Frame.SettingsPayload settings;
    }
  in

  Serializer.serialize_frame frame

let send_settings_ack conn =
  let frame =
    {
      Frame.length = 0;
      frame_type = Frame.Settings;
      flags =
        {
          Frame.end_stream = false;
          end_headers = false;
          padded = false;
          priority = false;
          ack = true;
        };
      stream_id = 0;
      payload = Frame.SettingsPayload [];
    }
  in

  Serializer.serialize_frame frame

let local_settings conn = conn.local_settings
let remote_settings conn = conn.remote_settings

(** {1 Connection Control} *)

let send_ping conn ~data =
  let frame =
    {
      Frame.length = 8;
      frame_type = Frame.Ping;
      flags =
        {
          Frame.end_stream = false;
          end_headers = false;
          padded = false;
          priority = false;
          ack = false;
        };
      stream_id = 0;
      payload = Frame.PingPayload data;
    }
  in

  Serializer.serialize_frame frame

let send_ping_ack conn ~data =
  let frame =
    {
      Frame.length = 8;
      frame_type = Frame.Ping;
      flags =
        {
          Frame.end_stream = false;
          end_headers = false;
          padded = false;
          priority = false;
          ack = true;
        };
      stream_id = 0;
      payload = Frame.PingPayload data;
    }
  in

  Serializer.serialize_frame frame

let send_goaway conn ~last_stream_id ~error_code ?(debug_data = "") () =
  Cell.set conn.state GoingAway;

  let frame =
    {
      Frame.length = 8 + String.length debug_data;
      frame_type = Frame.Goaway;
      flags =
        {
          Frame.end_stream = false;
          end_headers = false;
          padded = false;
          priority = false;
          ack = false;
        };
      stream_id = 0;
      payload = Frame.GoawayPayload { last_stream_id; error_code; debug_data };
    }
  in

  Serializer.serialize_frame frame

let state conn = Cell.get conn.state

let close conn =
  Cell.set conn.state Closed;
  Hashtbl.clear conn.streams

(** {1 Frame Processing} *)

let process_settings_frame conn settings_list flags =
  if flags.Frame.ack then
    (* SETTINGS ACK *)
    Ok [ SettingsAckReceived ]
  else
    (* Apply received settings *)
    List.iter
      (function
        | Frame.HeaderTableSize size ->
            Cell.set conn.remote_settings.header_table_size size;
            Hpack.update_max_table_size conn.hpack_encoder size
        | Frame.EnablePush enabled ->
            Cell.set conn.remote_settings.enable_push enabled
        | Frame.MaxConcurrentStreams max ->
            Cell.set conn.remote_settings.max_concurrent_streams (Some max)
        | Frame.InitialWindowSize size ->
            Cell.set conn.remote_settings.initial_window_size size
        | Frame.MaxFrameSize size ->
            Cell.set conn.remote_settings.max_frame_size size
        | Frame.MaxHeaderListSize size ->
            Cell.set conn.remote_settings.max_header_list_size (Some size))
      settings_list;

    Ok [ SettingsReceived settings_list ]

let process_headers_frame conn stream_id payload flags =
  match payload with
  | Frame.HeadersPayload { header_block_fragment; _ } -> (
      (* Decode HPACK-encoded headers *)
      let header_bytes = Bytes.of_string header_block_fragment in
      match Hpack.decode conn.hpack_decoder header_bytes with
      | Error e -> Error (format "HPACK decode error: %s" e)
      | Ok headers ->
          let end_stream = flags.Frame.end_stream in

          (* Create or update stream *)
          let stream =
            match get_stream conn stream_id with
            | Some s -> s
            | None ->
                let s =
                  {
                    id = stream_id;
                    state = Cell.create StreamOpen;
                    window_size = Cell.create (Cell.get conn.remote_settings.initial_window_size);
                    headers = Cell.create [];
                    data_chunks = Cell.create [];
                  }
                in
                Hashtbl.add conn.streams stream_id s;
                s
          in

          Cell.set stream.headers headers;
          if end_stream then Cell.set stream.state StreamHalfClosedRemote;

          Ok [ HeadersReceived { stream_id; headers; end_stream } ])
  | _ -> Error "Invalid HEADERS payload"

let process_data_frame conn stream_id payload flags =
  match payload with
  | Frame.DataPayload { data; _ } ->
      let data_bytes = Bytes.of_string data in
      let end_stream = flags.Frame.end_stream in

      (match get_stream conn stream_id with
      | Some stream ->
          let chunks = Cell.get stream.data_chunks in
          Cell.set stream.data_chunks (chunks @ [ data_bytes ]);
          if end_stream then Cell.set stream.state StreamHalfClosedRemote
      | None -> ());

      Ok [ DataReceived { stream_id; data = data_bytes; end_stream } ]
  | _ -> Error "Invalid DATA payload"

let process_frame conn frame =
  match frame.Frame.frame_type with
  | Frame.Settings ->
      (match frame.payload with
      | Frame.SettingsPayload settings ->
          process_settings_frame conn settings frame.flags
      | _ -> Error "Invalid SETTINGS payload")
  | Frame.Headers ->
      process_headers_frame conn frame.stream_id frame.payload frame.flags
  | Frame.Data ->
      process_data_frame conn frame.stream_id frame.payload frame.flags
  | Frame.WindowUpdate ->
      (match frame.payload with
      | Frame.WindowUpdatePayload increment ->
          if frame.stream_id = 0 then
            (* Connection-level window update *)
            Cell.set conn.connection_window_size
              (Cell.get conn.connection_window_size + increment)
          else
            (* Stream-level window update *)
            (match get_stream conn frame.stream_id with
            | Some stream ->
                Cell.set stream.window_size
                  (Cell.get stream.window_size + increment)
            | None -> ());
          Ok [ WindowUpdateReceived { stream_id = frame.stream_id; increment } ]
      | _ -> Error "Invalid WINDOW_UPDATE payload")
  | Frame.Ping ->
      (match frame.payload with
      | Frame.PingPayload data ->
          if frame.flags.ack then Ok [ PingAckReceived { data } ]
          else Ok [ PingReceived { data } ]
      | _ -> Error "Invalid PING payload")
  | Frame.Goaway ->
      (match frame.payload with
      | Frame.GoawayPayload { last_stream_id; error_code; debug_data } ->
          Cell.set conn.state GoingAway;
          Ok [ GoawayReceived { last_stream_id; error_code; debug_data } ]
      | _ -> Error "Invalid GOAWAY payload")
  | Frame.RstStream ->
      (match frame.payload with
      | Frame.RstStreamPayload error_code ->
          (match get_stream conn frame.stream_id with
          | Some stream -> Cell.set stream.state StreamClosed
          | None -> ());
          Ok [ RstStreamReceived { stream_id = frame.stream_id; error_code } ]
      | _ -> Error "Invalid RST_STREAM payload")
  | Frame.Priority ->
      (match frame.payload with
      | Frame.PriorityPayload { stream_dependency; exclusive; weight } ->
          Ok
            [
              PriorityReceived
                {
                  stream_id = frame.stream_id;
                  dependency = stream_dependency;
                  weight;
                  exclusive;
                };
            ]
      | _ -> Error "Invalid PRIORITY payload")
  | Frame.PushPromise
  | Frame.Continuation ->
      (* TODO: Implement these *)
      Ok []

let process_data conn data =
  let rec process_all events remaining =
    match Parser.parse_frame (Bytes.to_string remaining) with
    | Parser.Done { value = frame; remaining = rest } -> (
        match process_frame conn frame with
        | Error e -> Error e
        | Ok new_events ->
            let rest_bytes = Bytes.of_string rest in
            if Bytes.length rest_bytes > 0 then
              process_all (events @ new_events) rest_bytes
            else Ok (events @ new_events))
    | Parser.Need_more -> Ok events
    | Parser.Error e -> Error e
  in
  process_all [] data
