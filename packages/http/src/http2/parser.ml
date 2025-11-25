open Std

let ( let* ) = Result.and_then

type 'a parse_result =
  | Done of { value : 'a; remaining : string }
  | Need_more
  | Error of string

(* RFC 9113: SETTINGS_MAX_FRAME_SIZE default is 16384, max is 16777215 *)
(* For security, we enforce a configurable maximum *)
let default_max_frame_size = 16384 (* 16 KB *)
let absolute_max_frame_size = 16777215 (* ~16 MB, RFC maximum *)

type config = { max_frame_size : int }

let default_config = { max_frame_size = default_max_frame_size }

let read_uint24_be data offset =
  if offset + 3 > String.length data then None
  else
    let b0 = Char.code data.[offset] in
    let b1 = Char.code data.[offset + 1] in
    let b2 = Char.code data.[offset + 2] in
    Some ((b0 lsl 16) lor (b1 lsl 8) lor b2)

let read_uint32_be data offset =
  if offset + 4 > String.length data then None
  else
    let b0 = Char.code data.[offset] in
    let b1 = Char.code data.[offset + 1] in
    let b2 = Char.code data.[offset + 2] in
    let b3 = Char.code data.[offset + 3] in
    Some ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3)

let read_uint16_be data offset =
  if offset + 2 > String.length data then None
  else
    let b0 = Char.code data.[offset] in
    let b1 = Char.code data.[offset + 1] in
    Some ((b0 lsl 8) lor b1)

let read_uint8 data offset =
  if offset >= String.length data then None else Some (Char.code data.[offset])

let int_to_frame_type = function
  | 0x0 -> Some Frame.Data
  | 0x1 -> Some Frame.Headers
  | 0x2 -> Some Frame.Priority
  | 0x3 -> Some Frame.RstStream
  | 0x4 -> Some Frame.Settings
  | 0x5 -> Some Frame.PushPromise
  | 0x6 -> Some Frame.Ping
  | 0x7 -> Some Frame.Goaway
  | 0x8 -> Some Frame.WindowUpdate
  | 0x9 -> Some Frame.Continuation
  | _ -> None

let parse_flags frame_type flags_byte =
  let end_headers = flags_byte land 0x04 <> 0 in
  let padded = flags_byte land 0x08 <> 0 in
  let priority = flags_byte land 0x20 <> 0 in
  let bit_0_set = flags_byte land 0x01 <> 0 in
  let end_stream, ack =
    match frame_type with
    | Frame.Settings | Frame.Ping -> (false, bit_0_set)
    | Frame.Data | Frame.Headers -> (bit_0_set, false)
    | _ -> (false, false)
  in
  { Frame.end_stream; end_headers; padded; priority; ack }

let parse_frame_header ?(config = default_config) data =
  if String.length data < 9 then Need_more
  else
    match read_uint24_be data 0 with
    | None -> Error "Failed to read length"
    | Some length -> (
        (* Security fix: Validate frame size to prevent memory exhaustion DoS *)
        if length > config.max_frame_size then
          Error
            (format "Frame size %d exceeds maximum allowed %d" length
               config.max_frame_size)
        else (
        match read_uint8 data 3 with
        | None -> Error "Failed to read frame type"
        | Some type_byte -> (
            match int_to_frame_type type_byte with
            | None -> Error (format "Unknown frame type: 0x%x" type_byte)
            | Some frame_type -> (
                match read_uint8 data 4 with
                | None -> Error "Failed to read flags"
                | Some flags_byte -> (
                    let flags = parse_flags frame_type flags_byte in
                    match read_uint32_be data 5 with
                    | None -> Error "Failed to read stream ID"
                    | Some stream_id_raw ->
                        let stream_id = stream_id_raw land 0x7FFFFFFF in
                        Done
                          {
                            value = (length, frame_type, flags, stream_id);
                            remaining =
                              String.sub data 9 (String.length data - 9);
                          })))))

let parse_data_payload length flags data =
  let has_padding = flags.Frame.padded in
  if has_padding then
    match read_uint8 data 0 with
    | None -> Need_more
    | Some pad_length ->
        if length < 1 + pad_length then Error "Invalid padding length"
        else if String.length data < length then Need_more
        else
          let data_length = length - pad_length - 1 in
          let payload_data = String.sub data 1 data_length in
          let remaining =
            String.sub data length (String.length data - length)
          in
          Done
            {
              value =
                Frame.DataPayload
                  { data = payload_data; pad_length = Some pad_length };
              remaining;
            }
  else if String.length data < length then Need_more
  else
    let payload_data = String.sub data 0 length in
    let remaining = String.sub data length (String.length data - length) in
    Done
      {
        value = Frame.DataPayload { data = payload_data; pad_length = None };
        remaining;
      }

let parse_priority_fields data offset =
  match read_uint32_be data offset with
  | None -> None
  | Some dep_raw -> (
      let exclusive = dep_raw land 0x80000000 <> 0 in
      let stream_dependency = dep_raw land 0x7FFFFFFF in
      match read_uint8 data (offset + 4) with
      | None -> None
      | Some weight -> Some (stream_dependency, exclusive, weight + 1))

let parse_headers_payload length flags data =
  let has_padding = flags.Frame.padded in
  let has_priority = flags.Frame.priority in

  let pad_length_opt, offset =
    if has_padding then
      match read_uint8 data 0 with None -> (None, 0) | Some pl -> (Some pl, 1)
    else (None, 0)
  in

  let priority_info, offset =
    if has_priority then
      match parse_priority_fields data offset with
      | None -> (None, offset)
      | Some (dep, excl, w) -> (Some (dep, excl, w), offset + 5)
    else (None, offset)
  in

  let pad_length = match pad_length_opt with Some p -> p | None -> 0 in
  let header_fragment_length = length - offset - pad_length in

  if header_fragment_length < 0 then Error "Invalid HEADERS frame length"
  else if String.length data < length then Need_more
  else
    let header_block_fragment = String.sub data offset header_fragment_length in
    let remaining = String.sub data length (String.length data - length) in
    let stream_dependency, weight, exclusive =
      match priority_info with
      | Some (dep, excl, w) -> (Some dep, Some w, excl)
      | None -> (None, None, false)
    in
    Done
      {
        value =
          Frame.HeadersPayload
            {
              pad_length = pad_length_opt;
              stream_dependency;
              weight;
              exclusive;
              header_block_fragment;
            };
        remaining;
      }

let parse_priority_payload length data =
  if length <> 5 then Error "PRIORITY frame must be 5 bytes"
  else if String.length data < 5 then Need_more
  else
    match parse_priority_fields data 0 with
    | None -> Error "Failed to parse PRIORITY payload"
    | Some (stream_dependency, exclusive, weight) ->
        let remaining = String.sub data 5 (String.length data - 5) in
        Done
          {
            value =
              Frame.PriorityPayload { stream_dependency; exclusive; weight };
            remaining;
          }

let parse_rst_stream_payload length data =
  if length <> 4 then Error "RST_STREAM frame must be 4 bytes"
  else if String.length data < 4 then Need_more
  else
    match read_uint32_be data 0 with
    | None -> Error "Failed to read error code"
    | Some error_code_int ->
        let error_code =
          match Frame.int_to_error_code error_code_int with
          | Some ec -> ec
          | None -> Frame.ProtocolError
        in
        let remaining = String.sub data 4 (String.length data - 4) in
        Done { value = Frame.RstStreamPayload error_code; remaining }

let parse_settings_payload length flags data =
  if flags.Frame.ack && length <> 0 then
    Error "SETTINGS ACK must have zero length"
  else if length mod 6 <> 0 then
    Error "SETTINGS frame length must be multiple of 6"
  else if String.length data < length then Need_more
  else
    let rec parse_settings offset acc =
      if offset >= length then Ok (List.rev acc)
      else
        match read_uint16_be data offset with
        | None -> Result.Error "Failed to read setting ID"
        | Some id -> (
            match read_uint32_be data (offset + 2) with
            | None -> Result.Error "Failed to read setting value"
            | Some value -> (
                let setting =
                  match id with
                  | 0x1 -> Some (Frame.HeaderTableSize value)
                  | 0x2 -> Some (Frame.EnablePush (value <> 0))
                  | 0x3 -> Some (Frame.MaxConcurrentStreams value)
                  | 0x4 -> Some (Frame.InitialWindowSize value)
                  | 0x5 -> Some (Frame.MaxFrameSize value)
                  | 0x6 -> Some (Frame.MaxHeaderListSize value)
                  | _ -> None
                in
                match setting with
                | Some s -> parse_settings (offset + 6) (s :: acc)
                | None -> parse_settings (offset + 6) acc))
    in
    match parse_settings 0 [] with
    | Ok settings ->
        let remaining = String.sub data length (String.length data - length) in
        Done { value = Frame.SettingsPayload settings; remaining }
    | Result.Error msg -> Error msg

let parse_push_promise_payload length flags data =
  let has_padding = flags.Frame.padded in
  let pad_length_opt, offset =
    if has_padding then
      match read_uint8 data 0 with None -> (None, 0) | Some pl -> (Some pl, 1)
    else (None, 0)
  in

  if String.length data < offset + 4 then Need_more
  else
    match read_uint32_be data offset with
    | None -> Error "Failed to read promised stream ID"
    | Some promised_stream_id_raw ->
        let promised_stream_id = promised_stream_id_raw land 0x7FFFFFFF in
        let offset = offset + 4 in
        let pad_length = match pad_length_opt with Some p -> p | None -> 0 in
        let header_fragment_length = length - offset - pad_length in

        if header_fragment_length < 0 then
          Error "Invalid PUSH_PROMISE frame length"
        else if String.length data < length then Need_more
        else
          let header_block_fragment =
            String.sub data offset header_fragment_length
          in
          let remaining =
            String.sub data length (String.length data - length)
          in
          Done
            {
              value =
                Frame.PushPromisePayload
                  {
                    pad_length = pad_length_opt;
                    promised_stream_id;
                    header_block_fragment;
                  };
              remaining;
            }

let parse_ping_payload length data =
  if length <> 8 then Error "PING frame must be 8 bytes"
  else if String.length data < 8 then Need_more
  else
    let opaque_data = String.sub data 0 8 in
    let remaining = String.sub data 8 (String.length data - 8) in
    Done { value = Frame.PingPayload opaque_data; remaining }

let parse_goaway_payload length data =
  if length < 8 then Error "GOAWAY frame must be at least 8 bytes"
  else if String.length data < length then Need_more
  else
    match read_uint32_be data 0 with
    | None -> Error "Failed to read last stream ID"
    | Some last_stream_id_raw -> (
        let last_stream_id = last_stream_id_raw land 0x7FFFFFFF in
        match read_uint32_be data 4 with
        | None -> Error "Failed to read error code"
        | Some error_code_int ->
            let error_code =
              match Frame.int_to_error_code error_code_int with
              | Some ec -> ec
              | None -> Frame.ProtocolError
            in
            let debug_data = String.sub data 8 (length - 8) in
            let remaining =
              String.sub data length (String.length data - length)
            in
            Done
              {
                value =
                  Frame.GoawayPayload { last_stream_id; error_code; debug_data };
                remaining;
              })

let parse_window_update_payload length data =
  if length <> 4 then Error "WINDOW_UPDATE frame must be 4 bytes"
  else if String.length data < 4 then Need_more
  else
    match read_uint32_be data 0 with
    | None -> Error "Failed to read window size increment"
    | Some increment_raw ->
        let increment = increment_raw land 0x7FFFFFFF in
        if increment = 0 then Error "WINDOW_UPDATE increment must be non-zero"
        else
          let remaining = String.sub data 4 (String.length data - 4) in
          Done { value = Frame.WindowUpdatePayload increment; remaining }

let parse_continuation_payload length data =
  if String.length data < length then Need_more
  else
    let header_block_fragment = String.sub data 0 length in
    let remaining = String.sub data length (String.length data - length) in
    Done { value = Frame.ContinuationPayload header_block_fragment; remaining }

let parse_payload length frame_type flags data =
  match frame_type with
  | Frame.Data -> parse_data_payload length flags data
  | Frame.Headers -> parse_headers_payload length flags data
  | Frame.Priority -> parse_priority_payload length data
  | Frame.RstStream -> parse_rst_stream_payload length data
  | Frame.Settings -> parse_settings_payload length flags data
  | Frame.PushPromise -> parse_push_promise_payload length flags data
  | Frame.Ping -> parse_ping_payload length data
  | Frame.Goaway -> parse_goaway_payload length data
  | Frame.WindowUpdate -> parse_window_update_payload length data
  | Frame.Continuation -> parse_continuation_payload length data

let parse_frame data =
  match parse_frame_header data with
  | Error msg -> Error msg
  | Need_more -> Need_more
  | Done { value = length, frame_type, flags, stream_id; remaining } -> (
      match parse_payload length frame_type flags remaining with
      | Error msg -> Error msg
      | Need_more -> Need_more
      | Done { value = payload; remaining } ->
          Done
            {
              value = { Frame.length; frame_type; flags; stream_id; payload };
              remaining;
            })
