open Std

type config = { max_frame_size : int }

let default_config = { max_frame_size = 16384 }

(** Parser phases - what we're currently trying to parse *)
type parse_phase =
  | ReadingFrameHeader of {
      buffer : Buffer.t;  (** Accumulating 9-byte header *)
      bytes_read : int;
    }
  | ReadingFramePayload of {
      header : Frame.t;  (** Parsed header *)
      buffer : Buffer.t;  (** Accumulating payload *)
      bytes_read : int;
      total_length : int;
    }

type state = {
  config : config;
  phase : parse_phase Cell.t;
}

type parse_error =
  | Incomplete_frame_header
  | Frame_size_exceeds_maximum of { size : int; max_size : int }
  | Unknown_frame_type of int
  | Invalid_payload_length of { frame_type : string; expected : int; actual : int }
  | Incomplete_settings_payload

type parse_result =
  | Frame of Frame.t
  | Need_more
  | Error of parse_error

let create ?(config = default_config) () =
  {
    config;
    phase =
      Cell.create
        (ReadingFrameHeader { buffer = Buffer.create 9; bytes_read = 0 });
  }

let reset state =
  Cell.set state.phase
    (ReadingFrameHeader { buffer = Buffer.create 9; bytes_read = 0 })

let buffered_bytes state =
  match Cell.get state.phase with
  | ReadingFrameHeader { bytes_read; _ } -> bytes_read
  | ReadingFramePayload { bytes_read; _ } -> bytes_read + 9

(** Read exactly N bytes from reader into buffer, returning number actually read *)
let read_n_bytes reader buffer n =
  let bytes = Bytes.create n in
  match IO.Reader.read reader bytes with
  | Ok bytes_read when bytes_read > 0 ->
      Buffer.add_subbytes buffer bytes 0 bytes_read;
      bytes_read
  | Ok _ -> 0  (* EOF or no data available *)
  | Error _ -> 0  (* Read error treated as no data *)

(** Parse 9-byte frame header from buffer *)
let parse_frame_header_bytes config data =
  if String.length data < 9 then Error Incomplete_frame_header
  else
    (* Read length (24-bit big-endian) *)
    let b0 = Char.code data.[0] in
    let b1 = Char.code data.[1] in
    let b2 = Char.code data.[2] in
    let length = (b0 lsl 16) lor (b1 lsl 8) lor b2 in

    (* Security: Validate frame size *)
    if length > config.max_frame_size then
      Error (Frame_size_exceeds_maximum { size = length; max_size = config.max_frame_size })
    else
      (* Read type *)
      let type_byte = Char.code data.[3] in
      let frame_type_opt =
        match type_byte with
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
      in

      match frame_type_opt with
      | None -> Error (Unknown_frame_type type_byte)
      | Some frame_type ->
          (* Read flags *)
          let flags_byte = Char.code data.[4] in
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
          let flags = { Frame.end_stream; end_headers; padded; priority; ack } in

          (* Read stream ID (31-bit, ignore reserved bit) *)
          let s0 = Char.code data.[5] in
          let s1 = Char.code data.[6] in
          let s2 = Char.code data.[7] in
          let s3 = Char.code data.[8] in
          let stream_id_raw = (s0 lsl 24) lor (s1 lsl 16) lor (s2 lsl 8) lor s3 in
          let stream_id = stream_id_raw land 0x7FFFFFFF in

          Ok
            {
              Frame.length;
              frame_type;
              flags;
              stream_id;
              payload = Frame.DataPayload { data = ""; pad_length = None };
              (* Placeholder *)
            }

(** Parse frame payload based on frame type *)
let parse_payload frame payload_data =
  match frame.Frame.frame_type with
  | Frame.Data ->
      Ok
        {
          frame with
          payload = Frame.DataPayload { data = payload_data; pad_length = None };
        }
  | Frame.Headers ->
      Ok
        {
          frame with
          payload =
            Frame.HeadersPayload
              {
                pad_length = None;
                stream_dependency = None;
                weight = None;
                exclusive = false;
                header_block_fragment = payload_data;
              };
        }
  | Frame.Settings ->
      (* Parse settings pairs: each is 6 bytes (2-byte ID + 4-byte value) *)
      let rec parse_settings offset acc =
        if offset >= String.length payload_data then Ok (List.rev acc)
        else if offset + 6 > String.length payload_data then
          Error Incomplete_settings_payload
        else
          let id =
            (Char.code payload_data.[offset] lsl 8)
            lor Char.code payload_data.[offset + 1]
          in
          let value =
            (Char.code payload_data.[offset + 2] lsl 24)
            lor (Char.code payload_data.[offset + 3] lsl 16)
            lor (Char.code payload_data.[offset + 4] lsl 8)
            lor Char.code payload_data.[offset + 5]
          in
          let setting_opt =
            match id with
            | 0x1 -> Some (Frame.HeaderTableSize value)
            | 0x2 -> Some (Frame.EnablePush (value <> 0))
            | 0x3 -> Some (Frame.MaxConcurrentStreams value)
            | 0x4 -> Some (Frame.InitialWindowSize value)
            | 0x5 -> Some (Frame.MaxFrameSize value)
            | 0x6 -> Some (Frame.MaxHeaderListSize value)
            | _ -> None
          in
          match setting_opt with
          | Some setting -> parse_settings (offset + 6) (setting :: acc)
          | None -> parse_settings (offset + 6) acc
      in
      let* settings = parse_settings 0 [] in
      Ok { frame with payload = Frame.SettingsPayload settings }
  | Frame.Ping ->
      if String.length payload_data <> 8 then
        Error (Invalid_payload_length { frame_type = "PING"; expected = 8; actual = String.length payload_data })
      else Ok { frame with payload = Frame.PingPayload payload_data }
  | Frame.WindowUpdate ->
      if String.length payload_data <> 4 then
        Error (Invalid_payload_length { frame_type = "WINDOW_UPDATE"; expected = 4; actual = String.length payload_data })
      else
        let increment =
          (Char.code payload_data.[0] lsl 24)
          lor (Char.code payload_data.[1] lsl 16)
          lor (Char.code payload_data.[2] lsl 8)
          lor Char.code payload_data.[3]
        in
        let increment = increment land 0x7FFFFFFF in
        Ok { frame with payload = Frame.WindowUpdatePayload increment }
  | Frame.RstStream ->
      if String.length payload_data <> 4 then
        Error (Invalid_payload_length { frame_type = "RST_STREAM"; expected = 4; actual = String.length payload_data })
      else
        let code =
          (Char.code payload_data.[0] lsl 24)
          lor (Char.code payload_data.[1] lsl 16)
          lor (Char.code payload_data.[2] lsl 8)
          lor Char.code payload_data.[3]
        in
        let error_code =
          match code with
          | 0x0 -> Frame.NoError
          | 0x1 -> Frame.ProtocolError
          | 0x2 -> Frame.InternalError
          | 0x3 -> Frame.FlowControlError
          | 0x4 -> Frame.SettingsTimeout
          | 0x5 -> Frame.StreamClosed
          | 0x6 -> Frame.FrameSizeError
          | 0x7 -> Frame.RefusedStream
          | 0x8 -> Frame.Cancel
          | 0x9 -> Frame.CompressionError
          | 0xa -> Frame.ConnectError
          | 0xb -> Frame.EnhanceYourCalm
          | 0xc -> Frame.InadequateSecurity
          | 0xd -> Frame.Http11Required
          | _ -> Frame.InternalError
        in
        Ok { frame with payload = Frame.RstStreamPayload error_code }
  | Frame.Goaway ->
      if String.length payload_data < 8 then
        Error (Invalid_payload_length { frame_type = "GOAWAY"; expected = 8; actual = String.length payload_data })
      else
        let last_stream_id =
          ((Char.code payload_data.[0] lsl 24)
          lor (Char.code payload_data.[1] lsl 16)
          lor (Char.code payload_data.[2] lsl 8)
          lor Char.code payload_data.[3])
          land 0x7FFFFFFF
        in
        let error_code_int =
          (Char.code payload_data.[4] lsl 24)
          lor (Char.code payload_data.[5] lsl 16)
          lor (Char.code payload_data.[6] lsl 8)
          lor Char.code payload_data.[7]
        in
        let error_code =
          match error_code_int with
          | 0x0 -> Frame.NoError
          | 0x1 -> Frame.ProtocolError
          | 0x2 -> Frame.InternalError
          | _ -> Frame.InternalError
        in
        let debug_data =
          if String.length payload_data > 8 then
            String.sub payload_data 8 (String.length payload_data - 8)
          else ""
        in
        Ok
          {
            frame with
            payload = Frame.GoawayPayload { last_stream_id; error_code; debug_data };
          }
  | Frame.Priority | Frame.PushPromise | Frame.Continuation ->
      (* Simplified: return placeholder *)
      Ok
        {
          frame with
          payload = Frame.DataPayload { data = payload_data; pad_length = None };
        }

let parse state reader =
  let ( let* ) = Result.and_then in
  match Cell.get state.phase with
  | ReadingFrameHeader { buffer; bytes_read } ->
      (* Try to read more header bytes *)
      let needed = 9 - bytes_read in
      let actual_read = read_n_bytes reader buffer needed in

      if actual_read = 0 && bytes_read = 0 then
        (* No data available and nothing buffered *)
        Need_more
      else if bytes_read + actual_read < 9 then (
        (* Still incomplete, update state *)
        Cell.set state.phase
          (ReadingFrameHeader
             { buffer; bytes_read = bytes_read + actual_read });
        Need_more)
      else
        (* Have complete 9-byte header *)
        let header_data = Buffer.contents buffer in
        match parse_frame_header_bytes state.config header_data with
        | Error e -> Error e
        | Ok frame_header ->
            if frame_header.length = 0 then
              (* Zero-length frame - complete immediately *)
              let* complete_frame = parse_payload frame_header "" in
              reset state;
              Frame complete_frame
            else (
              (* Need to read payload *)
              Cell.set state.phase
                (ReadingFramePayload
                   {
                     header = frame_header;
                     buffer = Buffer.create frame_header.length;
                     bytes_read = 0;
                     total_length = frame_header.length;
                   });
              (* Recursively try to read payload immediately *)
              parse state reader)
  | ReadingFramePayload { header; buffer; bytes_read; total_length } ->
      (* Try to read more payload bytes *)
      let needed = total_length - bytes_read in
      let actual_read = read_n_bytes reader buffer needed in

      if actual_read = 0 && bytes_read = 0 then Need_more
      else if bytes_read + actual_read < total_length then (
        (* Still incomplete *)
        Cell.set state.phase
          (ReadingFramePayload
             {
               header;
               buffer;
               bytes_read = bytes_read + actual_read;
               total_length;
             });
        Need_more)
      else
        (* Have complete payload *)
        let payload_data = Buffer.contents buffer in
        match parse_payload header payload_data with
        | Error e ->
            reset state;
            Error e
        | Ok complete_frame ->
            reset state;
            Frame complete_frame
