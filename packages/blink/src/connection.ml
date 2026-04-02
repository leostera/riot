open Std
open Std.IO

type message =
  | Data of string
  | Done
  | Headers of Net.Http.Header.t
  | Status of Net.Http.Status.t

type response_state =
  | Waiting_for_headers
  | Reading_fixed_body of { length: int; received: int }
  | Reading_chunked_body
  | Complete

type t =
  | Conn: {
      protocol: (module Protocol.Intf);
      writer: ('socket, 'err) IO.Writer.t;
      reader: ('socket, 'err) IO.Reader.t;
      uri: Net.Uri.t;
      mutable buffer: Buffer.t;
      mutable state: response_state;
      mutable response: Net.Http.Response.t option;
      of_io_error: 'err -> Error.t;
    } -> t

let make : type socket err. reader:(socket, err) IO.Reader.t ->
writer:(socket, err) IO.Writer.t ->
of_io_error:(err -> Error.t) ->
uri:Net.Uri.t ->
t = fun ~reader ~writer ~of_io_error ~uri ->
  Conn {
    protocol = (module Protocol.Http1);
    reader;
    writer;
    uri;
    buffer = Buffer.create 4_096;
    state = Waiting_for_headers;
    response = None;
    of_io_error;
  }

let request = fun (Conn conn) req ?body () ->
  let method_ = Net.Http.Request.method_ req in
  let version = Net.Http.Request.version req in
  let headers = Net.Http.Request.headers req in
  let resource = Net.Uri.path_and_query conn.uri in
  let request_line = (Net.Http.Method.to_string method_)
  ^ " "
  ^ resource
  ^ " "
  ^ (Net.Http.Version.to_string version)
  ^ "\r\n" in
  let headers =
    (
      headers |> fun h ->
        Net.Http.Header.add h "host"
          (Net.Uri.host conn.uri |> Option.unwrap_or ~default:"localhost")
    )
    |> fun h ->
      Net.Http.Header.add h "user-agent" "Riot-Blink/0.2.0"
  in
  let headers =
    match body with
    | Some b ->
        Net.Http.Header.add headers "content-length"
          (String.length b |> Int.to_string)
    | None -> headers
  in
  let headers_str = Net.Http.Header.to_list headers
  |> List.map (fun ((name, value)) -> name ^ ": " ^ value ^ "\r\n")
  |> String.concat "" in
  let request = request_line ^ headers_str ^ "\r\n" in
  let full_request =
    match body with
    | Some b -> request ^ b
    | None -> request
  in
  match IO.write_all conn.writer ~buf:full_request with
  | Ok () ->
      conn.state <- Waiting_for_headers;
      conn.response <- None;
      Buffer.clear conn.buffer;
      Ok ()
  | Error e -> Error (conn.of_io_error e)

let read_more = fun (Conn conn) ->
  let chunk = Bytes.create 4_096 in
  match IO.read conn.reader chunk with
  | Ok 0 ->
      Error Error.Eof
  | Ok n ->
      Buffer.add_subbytes conn.buffer chunk 0 n;
      Ok ()
  | Error e ->
      Error (conn.of_io_error e)

let stream = fun (Conn conn as c) ->
  match conn.state with
  | Complete ->
      Ok [ Done ]
  | Waiting_for_headers ->
      let rec try_parse () =
        let data = Buffer.contents conn.buffer in
        match Http.Http1.Response.parse data with
        | Http.Http1.Common.Done { value=response; remaining } ->
            let status = Net.Http.Response.status response in
            let headers = Net.Http.Response.headers response in
            conn.response <- Some response;
            Buffer.clear conn.buffer;
            Buffer.add_string conn.buffer remaining;
            let transfer_encoding = Net.Http.Header.get headers "transfer-encoding" in
            let content_length = Net.Http.Header.get headers "content-length" in
            conn.state <- (
              match transfer_encoding with
              | Some "chunked" -> Reading_chunked_body
              | _ -> (
                  match content_length with
                  | Some len -> (
                      try
                        let length = int_of_string len in
                        Reading_fixed_body { length; received = 0 }
                      with
                      | _ -> Complete
                    )
                  | None -> Complete
                )
            );
            Ok [ Status status; Headers headers ]
        | Http.Http1.Common.Need_more -> (
            match read_more c with
            | Ok () -> try_parse ()
            | Error e -> Error e
          )
        | Http.Http1.Common.Error msg ->
            Error (Error.Parse_error msg)
      in
      try_parse ()
  | Reading_fixed_body { length; received } -> (
      let data = Buffer.contents conn.buffer in
      let available = String.length data in
      let remaining = length - received in
      if remaining <= 0 then
        (
          (* Body complete but buffer empty *)
          conn.state <- Complete;
          Ok [ Done ]
        )
      else if available >= remaining then
        (
          (* We have enough data in buffer to complete the body *)
          let body_data = String.sub data 0 remaining in
          let leftover = String.sub data remaining (available - remaining) in
          Buffer.clear conn.buffer;
          Buffer.add_string conn.buffer leftover;
          conn.state <- Complete;
          Ok [ Data body_data; Done ]
        )
      else if available > 0 then
        (
          (* Partial data available, consume it and continue *)
          Buffer.clear conn.buffer;
          conn.state <- Reading_fixed_body { length; received = received + available };
          Ok [ Data data ]
        )
      else
        match read_more c with
        | Ok () -> Ok []
        | Error e -> Error e
    )
  | Reading_chunked_body ->
      let rec parse_chunks acc =
        let data = Buffer.contents conn.buffer in
        match Http.Http1.Chunk.parse data with
        | Http.Http1.Common.Done { value={ data=chunk_data; remaining }; _ } ->
            Buffer.clear conn.buffer;
            Buffer.add_string conn.buffer remaining;
            if chunk_data = "" then
              (
                conn.state <- Complete;
                Ok (List.rev (Done :: acc))
              )
            else
              (* Return the chunk immediately for streaming support *)
              Ok (List.rev (Data chunk_data :: acc))
        | Http.Http1.Common.Need_more -> (
            match read_more c with
            | Ok () -> parse_chunks acc
            | Error e ->
                if List.length acc > 0 then
                  Ok (List.rev acc)
                else
                  Error e
          )
        | Http.Http1.Common.Error msg ->
            Error (Error.Parse_error msg)
      in
      parse_chunks []

let messages = fun ?(on_message = fun _ -> ()) conn ->
  let rec loop acc =
    match stream conn with
    | Error e -> Error e
    | Ok msgs ->
        on_message msgs;
        let acc = List.rev_append (List.rev msgs) acc in
        if List.mem Done msgs then
          Ok (List.rev acc)
        else
          loop acc
  in
  loop []

let await = fun ?(on_message = fun _ -> ()) (Conn conn as c) ->
  match messages ~on_message c with
  | Error e -> Error e
  | Ok msgs ->
      let response = conn.response
      |> Option.unwrap_or ~default:(Net.Http.Response.create (Net.Http.Status.of_int 500)) in
      let body_chunks =
        List.filter_map
          (
            function
            | Data chunk -> Some chunk
            | _ -> None
          )
          msgs
      in
      let body = String.concat "" body_chunks in
      Ok (response, body)

let close = fun _conn -> ()

(* Reader/writer don't need explicit close *)
