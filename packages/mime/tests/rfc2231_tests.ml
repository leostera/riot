open Std
open Mime

let test_simple_filename = fun _ctx ->
  let headers = [ ("Content-Disposition", "attachment; filename=\"test.txt\"") ] in
  let body = "test content" in
  match parse ~headers ~body with
  | Ok (SinglePart part) -> (
      match get_filename part with
      | Some "test.txt" -> Ok ()
      | Some other -> Error ("Expected 'test.txt', got '" ^ other ^ "'")
      | None -> Error "No filename found"
    )
  | _ -> Error "Parse failed"

let test_rfc2231_encoded = fun _ctx ->
  let headers = [ ("Content-Disposition", "attachment; filename*=utf-8'en'Hello%20World.txt"); ] in
  let body = "test content" in
  match parse ~headers ~body with
  | Ok (SinglePart part) -> (
      match get_filename part with
      | Some "Hello World.txt" -> Ok ()
      | Some other -> Error ("Expected 'Hello World.txt', got '" ^ other ^ "'")
      | None -> Error "No filename found"
    )
  | _ -> Error "Parse failed"

let test_rfc2231_continuation = fun _ctx ->
  let headers = [ (
      "Content-Disposition",
      "attachment; filename*0=\"Long\"; filename*1=\"Filename\"; \
         filename*2=\".pdf\""
    ); ]
  in
  let body = "test content" in
  match parse ~headers ~body with
  | Ok (SinglePart part) -> (
      match get_filename part with
      | Some "LongFilename.pdf" -> Ok ()
      | Some other -> Error ("Expected 'LongFilename.pdf', got '" ^ other ^ "'")
      | None -> Error "No filename found"
    )
  | _ -> Error "Parse failed"

let test_base64_encoding = fun _ctx ->
  let headers = [ ("Content-Type", "text/plain"); ("Content-Transfer-Encoding", "base64") ] in
  let body = "SGVsbG8gV29ybGQ=" in
  match parse ~headers ~body with
  | Ok (SinglePart part) -> (
      match get_decoded_content part with
      | Ok "Hello World" -> Ok ()
      | Ok other -> Error ("Expected 'Hello World', got '" ^ other ^ "'")
      | Error e -> Error ("Decode failed: " ^ e)
    )
  | _ -> Error "Parse failed"

let test_quoted_printable = fun _ctx ->
  let headers = [
    ("Content-Type", "text/plain");
    ("Content-Transfer-Encoding", "quoted-printable");
  ] in
  let body = "Hello=20World=21" in
  match parse ~headers ~body with
  | Ok (SinglePart part) -> (
      match get_decoded_content part with
      | Ok "Hello World!" -> Ok ()
      | Ok other -> Error ("Expected 'Hello World!', got '" ^ other ^ "'")
      | Error e -> Error ("Decode failed: " ^ e)
    )
  | _ -> Error "Parse failed"

let test_nested_multipart = fun _ctx ->
  let headers = [ ("Content-Type", "multipart/mixed; boundary=outer") ] in
  let body = "--outer\r\n\
     Content-Type: multipart/alternative; boundary=inner\r\n\
     \r\n\
     --inner\r\n\
     Content-Type: text/plain\r\n\
     \r\n\
     Plain text\r\n\
     --inner\r\n\
     Content-Type: text/html\r\n\
     \r\n\
     <html>HTML</html>\r\n\
     --inner--\r\n\
     --outer\r\n\
     Content-Type: application/pdf\r\n\
     Content-Disposition: attachment; filename=\"doc.pdf\"\r\n\
     \r\n\
     PDF content\r\n\
     --outer--"
  in
  match parse ~headers ~body with
  | Ok (MultiPart { parts; _ }) -> (
      if List.length parts != 2 then
        Error ("Expected 2 top-level parts, got " ^ string_of_int (List.length parts))
      else
        match List.nth_opt parts 0 with
        | Some (MultiPart { parts=inner_parts; _ }) ->
            if List.length inner_parts = 2 then
              Ok ()
            else
              Error ("Expected 2 inner parts, got " ^ string_of_int (List.length inner_parts))
        | Some (SinglePart _) -> Error "First part is SinglePart, expected MultiPart"
        | None -> Error "No first part found"
    )
  | Ok (SinglePart _) -> Error "Expected MultiPart, got SinglePart"
  | Error e -> Error ("Parse failed: " ^ e)

let test_content_type_parsing = fun _ctx ->
  let headers = [ ("Content-Type", "text/html; charset=utf-8") ] in
  let body = "test" in
  match parse ~headers ~body with
  | Ok (SinglePart part) -> (
      match get_content_type part with
      | Some ct ->
          if ct.media_type = "text" && ct.subtype = "html" then
            match List.assoc_opt "charset" ct.parameters with
            | Some "utf-8" -> Ok ()
            | Some other -> Error ("Expected charset=utf-8, got " ^ other)
            | None -> Error "No charset parameter found"
          else
            Error ("Expected text/html, got " ^ ct.media_type ^ "/" ^ ct.subtype)
      | None -> Error "No Content-Type found"
    )
  | _ -> Error "Parse failed"

let test_encoding_detection = fun _ctx ->
  let headers = [ ("Content-Transfer-Encoding", "base64") ] in
  let body = "test" in
  match parse ~headers ~body with
  | Ok (SinglePart part) -> (
      match get_encoding part with
      | Some Base64 -> Ok ()
      | Some other -> Error "Expected Base64 encoding"
      | None -> Error "No encoding found"
    )
  | _ -> Error "Parse failed"

let tests =
  let open Test in [
    case "Simple filename parameter" test_simple_filename;
    case "RFC 2231 encoded filename with charset" test_rfc2231_encoded;
    case "RFC 2231 parameter continuation" test_rfc2231_continuation;
    case "Base64 Content-Transfer-Encoding" test_base64_encoding;
    case "Quoted-Printable Content-Transfer-Encoding" test_quoted_printable;
    case "Nested multipart parsing" test_nested_multipart;
    case "Content-Type parameter parsing" test_content_type_parsing;
    case "Encoding variant detection" test_encoding_detection;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"mime" ~tests ~args) ~args:Env.args ()
