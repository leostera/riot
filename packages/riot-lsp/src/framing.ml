open Std

let ( let* ) = Result.and_then

let strip_line_ending = fun line ->
  let len = String.length line in
  if len >= 2 && String.sub line (len - 2) 2 = "\r\n" then
    String.sub line 0 (len - 2)
  else if len >= 1 && line.[len - 1] = '\n' then
    String.sub line 0 (len - 1)
  else
    line

let header_separator = fun payload ->
  let len = String.length payload in
  let rec loop index =
    if index + 3 < len && String.sub payload index 4 = "\r\n\r\n" then
      Some (index, 4)
    else if index + 1 < len && String.sub payload index 2 = "\n\n" then
      Some (index, 2)
    else if index >= len then
      None
    else
      loop (index + 1)
  in
  loop 0

let parse_content_length = fun headers ->
  let parse_int = fun value ->
    try
      Some (int_of_string value)
    with
    | Failure _ -> None
  in
  let rec loop = function
    | [] -> Error "missing Content-Length header"
    | header :: rest -> (
        match String.split_on_char ':' header with
        | name :: value_parts when String.equal (String.lowercase_ascii (String.trim name)) "content-length" -> (
            match parse_int (String.trim (String.concat ":" value_parts)) with
            | Some length when length >= 0 -> Ok length
            | Some _ -> Error "Content-Length must be non-negative"
            | None -> Error "invalid Content-Length header"
          )
        | _ -> loop rest
      )
  in
  loop headers

let encode = fun payload ->
  "Content-Length: " ^ Int.to_string (String.length payload) ^ "\r\n\r\n" ^ payload

let decode_one = fun framed ->
  match header_separator framed with
  | None -> Error "missing header/body separator"
  | Some (header_end, separator_len) ->
      let headers =
        String.sub framed 0 header_end
        |> String.split_on_char '\n'
        |> List.map strip_line_ending
        |> List.filter (fun line -> not (String.equal line ""))
      in
      let payload_start = header_end + separator_len in
      let body = String.sub framed payload_start (String.length framed - payload_start) in
      let* content_length = parse_content_length headers in
      if String.length body < content_length then
        Error "payload shorter than Content-Length"
      else
        let payload = String.sub body 0 content_length in
        let rest = String.sub body content_length (String.length body - content_length) in
        Ok (payload, rest)

let read = fun file ->
  let rec read_headers acc =
    match Fs.File.read_line file with
    | Error error -> Error (IO.error_message error)
    | Ok "" ->
        if List.is_empty acc then
          Ok None
        else
          Error "unexpected EOF while reading LSP headers"
    | Ok raw_line ->
        let line = strip_line_ending raw_line in
        if String.equal line "" then
          Ok (Some (List.rev acc))
        else
          read_headers (line :: acc)
  in
  let* headers_opt = read_headers [] in
  match headers_opt with
  | None -> Ok None
  | Some headers ->
      let* content_length = parse_content_length headers in
      let buffer = IO.Bytes.create content_length in
      let* () = Fs.File.read_exact file buffer ~offset:0 ~len:content_length |> Result.map_error IO.error_message in
      Ok (Some (IO.Bytes.to_string buffer))

let write = fun file payload ->
  Fs.File.write_all file (encode payload) |> Result.map_error IO.error_message
