open Global
open IO
open Collections
open Result.Syntax

type t =
  | Element of {
      name: string;
      attrs: (string * string) list;
      children: t list;
    }
  | Text of string
  | CData of string

type error =
  | Parse_error of {
      message: string;
      offset: int;
    }

let escape_xml = fun str ->
  let buf = Buffer.create ~size:(String.length str) in
  String.for_each
    ~fn:(fun c ->
      match c with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    str;
  Buffer.contents buf

let element = fun name ?(attrs = []) children -> Element { name; attrs; children }

let text = fun str -> Text (escape_xml str)

let cdata = fun str -> CData str

let attr = fun key -> function
  | Element { attrs; _ } ->
      List.find attrs ~fn:(fun (name, _value) -> String.equal name key)
      |> Option.map ~fn:(fun (_name, value) -> value)
  | _ -> None

let children = fun __tmp1 ->
  match __tmp1 with
  | Element { children; _ } -> children
  | Text _
  | CData _ -> []

let child_elements = fun ?name node ->
  children node
  |> List.filter
    ~fn:(fun child ->
      match (name, child) with
      | (None, Element _) -> true
      | (Some expected, Element { name; _ }) -> String.equal expected name
      | _ -> false)

let starts_at = fun value ~at ~needle ->
  let value_len = String.length value in
  let needle_len = String.length needle in
  if at < 0 then
    false
  else if at > value_len then
    false
  else if needle_len > value_len - at then
    false
  else
    let rec loop index =
      if index >= needle_len then
        true
      else if
        Char.equal
          (String.get_unchecked value ~at:(at + index))
          (String.get_unchecked needle ~at:index)
      then
        loop (index + 1)
      else
        false
    in
    loop 0

let find_substring = fun ?(from = 0) value needle ->
  let value_len = String.length value in
  let needle_len = String.length needle in
  let rec loop index =
    if needle_len = 0 then
      Some index
    else if index + needle_len > value_len then
      None
    else if starts_at value ~at:index ~needle then
      Some index
    else
      loop (index + 1)
  in
  loop from

let decode_xml = fun value ->
  let builder = StringBuilder.create ~size:(String.length value) in
  let rec loop index =
    if index >= String.length value then
      StringBuilder.contents builder
    else if starts_at value ~at:index ~needle:"&lt;" then (
      StringBuilder.add_char builder '<';
      loop (index + 4)
    ) else if starts_at value ~at:index ~needle:"&gt;" then (
      StringBuilder.add_char builder '>';
      loop (index + 4)
    ) else if starts_at value ~at:index ~needle:"&amp;" then (
      StringBuilder.add_char builder '&';
      loop (index + 5)
    ) else if starts_at value ~at:index ~needle:"&quot;" then (
      StringBuilder.add_char builder '"';
      loop (index + 6)
    ) else if starts_at value ~at:index ~needle:"&apos;" then (
      StringBuilder.add_char builder '\'';
      loop (index + 6)
    ) else (
      StringBuilder.add_char builder (String.get_unchecked value ~at:index);
      loop (index + 1)
    )
  in
  loop 0

let rec text_content = fun __tmp1 ->
  match __tmp1 with
  | Text value -> decode_xml value
  | CData value -> value
  | Element { children; _ } ->
      children
      |> List.map ~fn:text_content
      |> String.concat ""

let rec to_string = fun ?(indent = 0) ->
  fun __tmp1 ->
    match __tmp1 with
    | Text str -> str
    | CData str -> "<![CDATA[" ^ str ^ "]]>"
    | Element { name; attrs; children } ->
        let spaces = String.make ~len:(indent * 2) ~char:' ' in
        let attrs_str =
          if attrs = [] then
            ""
          else
            " "
            ^ String.concat " " (List.map attrs ~fn:(fun (k, v) -> k ^ "=\"" ^ escape_xml v ^ "\""))
        in
        if children = [] then
          spaces ^ "<" ^ name ^ attrs_str ^ "></" ^ name ^ ">"
        else
          let children_str =
            String.concat "\n" (List.map children ~fn:(to_string ~indent:(indent + 1)))
          in
          spaces ^ "<" ^ name ^ attrs_str ^ ">\n" ^ children_str ^ "\n" ^ spaces ^ "</" ^ name ^ ">"

let declaration = {|<?xml version="1.0" encoding="UTF-8"?>|}

type parser = {
  source: string;
  length: int;
  mutable offset: int;
}

let parse_error = fun parser message ->
  Error (Parse_error { message; offset = parser.offset })

let at_end = fun parser -> parser.offset >= parser.length

let parser_starts_with = fun parser needle -> starts_at parser.source ~at:parser.offset ~needle

let current = fun parser ->
  if at_end parser then
    None
  else
    Some (String.get_unchecked parser.source ~at:parser.offset)

let advance = fun parser count -> parser.offset <- parser.offset + count

let is_space = fun __tmp1 ->
  match __tmp1 with
  | ' '
  | '\n'
  | '\r'
  | '\t' -> true
  | _ -> false

let skip_spaces = fun parser ->
  while
    match current parser with
    | Some char when is_space char -> advance parser 1; true
    | _ -> false
  do
    ()
  done

let consume_until = fun parser terminator ->
  match find_substring ~from:parser.offset parser.source terminator with
  | None -> parse_error parser ("expected " ^ terminator)
  | Some end_offset ->
      parser.offset <- end_offset + String.length terminator;
      Ok ()

let rec skip_ignored = fun parser ->
  skip_spaces parser;
  if parser_starts_with parser "<?" then (
    let* () = consume_until parser "?>" in
    skip_ignored parser
  ) else if parser_starts_with parser "<!--" then (
    let* () = consume_until parser "-->" in
    skip_ignored parser
  ) else
    Ok ()

let is_name_end = fun __tmp1 ->
  match __tmp1 with
  | ' '
  | '\n'
  | '\r'
  | '\t'
  | '/'
  | '>'
  | '=' -> true
  | _ -> false

let parse_name = fun parser ->
  let start = parser.offset in
  let rec loop () =
    match current parser with
    | Some char when not (is_name_end char) ->
        advance parser 1;
        loop ()
    | _ -> ()
  in
  loop ();
  if parser.offset = start then
    parse_error parser "expected XML name"
  else
    Ok (String.sub parser.source ~offset:start ~len:(parser.offset - start))

let expect_char = fun parser expected ->
  match current parser with
  | Some actual when Char.equal actual expected ->
      advance parser 1;
      Ok ()
  | _ -> parse_error parser ("expected '" ^ String.from_char expected ^ "'")

let parse_quoted_value = fun parser ->
  match current parser with
  | Some ('"' as quote)
  | Some ('\'' as quote) ->
      advance parser 1;
      let start = parser.offset in
      (
        match find_substring ~from:start parser.source (String.from_char quote) with
        | None -> parse_error parser "unterminated attribute value"
        | Some end_offset ->
            parser.offset <- end_offset + 1;
            Ok (
              String.sub parser.source ~offset:start ~len:(end_offset - start)
              |> decode_xml
            )
      )
  | _ -> parse_error parser "expected quoted attribute value"

let parse_attribute = fun parser ->
  let* name = parse_name parser in
  skip_spaces parser;
  let* () = expect_char parser '=' in
  skip_spaces parser;
  let* value = parse_quoted_value parser in
  Ok (name, value)

let rec parse_attributes = fun parser acc ->
  skip_spaces parser;
  if parser_starts_with parser "/>" then (
    advance parser 2;
    Ok (List.reverse acc, true)
  ) else if parser_starts_with parser ">" then (
    advance parser 1;
    Ok (List.reverse acc, false)
  ) else
    let* attr = parse_attribute parser in
    parse_attributes parser (attr :: acc)

let parse_text_node = fun parser ->
  let start = parser.offset in
  match find_substring ~from:start parser.source "<" with
  | None ->
      parser.offset <- parser.length;
      Ok (Text (String.sub parser.source ~offset:start ~len:(parser.length - start)))
  | Some end_offset ->
      parser.offset <- end_offset;
      Ok (Text (String.sub parser.source ~offset:start ~len:(end_offset - start)))

let parse_cdata = fun parser ->
  let start = parser.offset + String.length "<![CDATA[" in
  match find_substring ~from:start parser.source "]]>" with
  | None -> parse_error parser "unterminated CDATA section"
  | Some end_offset ->
      parser.offset <- end_offset + 3;
      Ok (CData (String.sub parser.source ~offset:start ~len:(end_offset - start)))

let rec parse_node = fun parser ->
  if parser_starts_with parser "<![CDATA[" then
    parse_cdata parser
  else if parser_starts_with parser "<" then
    parse_element parser
  else
    parse_text_node parser

and parse_element = fun parser ->
  let* () = expect_char parser '<' in
  if parser_starts_with parser "/" then
    parse_error parser "unexpected closing tag"
  else
    let* name = parse_name parser in
    let* (attrs, self_closing) = parse_attributes parser [] in
    if self_closing then
      Ok (Element { name; attrs; children = [] })
    else
      let* children = parse_children parser ~closing_name:name [] in
      Ok (Element { name; attrs; children })

and parse_children = fun parser ~closing_name acc ->
  if at_end parser then
    parse_error parser ("missing closing tag for " ^ closing_name)
  else if parser_starts_with parser "</" then (
    advance parser 2;
    let* actual_name = parse_name parser in
    skip_spaces parser;
    let* () = expect_char parser '>' in
    if String.equal actual_name closing_name then
      Ok (List.reverse acc)
    else
      parse_error parser ("expected closing tag for " ^ closing_name ^ ", got " ^ actual_name)
  ) else if parser_starts_with parser "<?" then (
    let* () = consume_until parser "?>" in
    parse_children parser ~closing_name acc
  ) else if parser_starts_with parser "<!--" then (
    let* () = consume_until parser "-->" in
    parse_children parser ~closing_name acc
  ) else
    let* child = parse_node parser in
    parse_children parser ~closing_name (child :: acc)

let from_string = fun source ->
  let parser = { source; length = String.length source; offset = 0 } in
  let* () = skip_ignored parser in
  let* document = parse_node parser in
  let* () = skip_ignored parser in
  if at_end parser then
    Ok document
  else
    parse_error parser "unexpected content after XML document"

let error_message = fun __tmp1 ->
  match __tmp1 with
  | Parse_error { message; offset } ->
      message ^ " at byte offset " ^ Int.to_string offset
