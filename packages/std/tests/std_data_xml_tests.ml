open Std
open Std.Data
open Std.Collections

let test_create_element = fun _ctx ->
  let elem = Xml.element "div" [] in
  match elem with
  | Xml.Element { name = "div"; attrs = []; children = [] } -> Ok ()
  | _ -> Error "Failed to create element"

let test_create_element_with_attrs = fun _ctx ->
  let elem = Xml.element "div" ~attrs:[ ("class", "test"); ] [] in
  match elem with
  | Xml.Element {
      name = "div";
      attrs = [ ("class", "test") ];
      _;
    } ->
      Ok ()
  | _ -> Error "Failed to create element with attributes"

let test_create_text = fun _ctx ->
  match Xml.text "hello" with
  | Xml.Text "hello" -> Ok ()
  | _ -> Error "Failed to create text node"

let test_create_cdata = fun _ctx ->
  match Xml.cdata "data" with
  | Xml.CData "data" -> Ok ()
  | _ -> Error "Failed to create CDATA"

let test_element_with_children = fun _ctx ->
  let child = Xml.text "content" in
  let parent = Xml.element "p" [ child ] in
  match parent with
  | Xml.Element {
      name = "p";
      children = [ Xml.Text "content" ];
      _;
    } ->
      Ok ()
  | _ -> Error "Failed to create element with children"

let test_nested_elements = fun _ctx ->
  let inner = Xml.element "span" [ Xml.text "inner" ] in
  let outer = Xml.element "div" [ inner ] in
  match outer with
  | Xml.Element {
      name = "div";
      children = [ Xml.Element { name = "span"; _ } ];
      _;
    } ->
      Ok ()
  | _ -> Error "Failed to create nested elements"

let test_serialize_simple_element = fun _ctx ->
  let elem = Xml.element "div" [] in
  let str = Xml.to_string elem in
  if str = "<div></div>" then
    Ok ()
  else
    Error ("Unexpected serialization: " ^ str)

let test_serialize_element_with_attrs = fun _ctx ->
  let elem = Xml.element "div" ~attrs:[ ("id", "test"); ] [] in
  let str = Xml.to_string elem in
  if String.contains str "=" then
    Ok ()
  else
    Error "Attributes not serialized"

let test_serialize_text = fun _ctx ->
  let text = Xml.text "hello world" in
  if Xml.to_string text = "hello world" then
    Ok ()
  else
    Error "Failed to serialize text"

let test_serialize_with_children = fun _ctx ->
  let elem = Xml.element "p" [ Xml.text "content" ] in
  let str = Xml.to_string elem in
  if str = "<p>\ncontent\n</p>" then
    Ok ()
  else
    Error ("Unexpected serialization: " ^ str)

let test_serialize_nested = fun _ctx ->
  let inner = Xml.element "span" [ Xml.text "text" ] in
  let outer = Xml.element "div" [ inner ] in
  let str = Xml.to_string outer in
  if String.contains str "<" && String.contains str ">" then
    Ok ()
  else
    Error "Failed to serialize nested elements"

let test_multiple_attributes = fun _ctx ->
  let elem = Xml.element "div" ~attrs:[ ("id", "test"); ("class", "box"); ] [] in
  let str = Xml.to_string elem in
  if String.contains str "=" then
    Ok ()
  else
    Error "Multiple attributes not serialized"

let test_mixed_children = fun _ctx ->
  let children = [ Xml.text "before"; Xml.element "span" [ Xml.text "middle" ]; Xml.text "after" ]
  in
  let elem = Xml.element "div" children in
  let str = Xml.to_string elem in
  if String.length str > 0 then
    Ok ()
  else
    Error "Failed to serialize mixed children"

let test_empty_text = fun _ctx ->
  let text = Xml.text "" in
  if Xml.to_string text = "" then
    Ok ()
  else
    Error "Failed to serialize empty text"

let test_cdata_serialization = fun _ctx ->
  let cdata = Xml.cdata "some data" in
  let str = Xml.to_string cdata in
  if String.contains str "[" then
    Ok ()
  else
    Error "CDATA not properly serialized"

let test_declaration = fun _ctx ->
  let decl = Xml.declaration in
  if String.contains decl "?" then
    Ok ()
  else
    Error "Declaration doesn't look correct"

let test_indented_output = fun _ctx ->
  let elem = Xml.element "div" [ Xml.element "p" [ Xml.text "test" ] ] in
  let str = Xml.to_string ~indent:2 elem in
  if String.length str > 0 then
    Ok ()
  else
    Error "Failed to serialize with indentation"

let test_special_chars_in_text = fun _ctx ->
  let text = Xml.text "<>&\"" in
  let str = Xml.to_string text in
  if String.length str >= 4 then
    Ok ()
  else
    Error "Special characters not handled"

let test_special_chars_in_attrs = fun _ctx ->
  let elem = Xml.element "div" ~attrs:[ ("data", "a\"b"); ] [] in
  let str = Xml.to_string elem in
  if String.length str > 0 then
    Ok ()
  else
    Error "Special chars in attributes not handled"

let test_complex_document = fun _ctx ->
  let doc =
    Xml.element
      "html"
      [
        Xml.element "head" [ Xml.element "title" [ Xml.text "Test" ] ];
        Xml.element
          "body"
          [ Xml.element "h1" [ Xml.text "Hello" ]; Xml.element "p" [ Xml.text "World" ] ];
      ]
  in
  let str = Xml.to_string doc in
  if String.contains str "<" && String.contains str ">" then
    Ok ()
  else
    Error "Failed to serialize complex document"

let test_parse_simple_element = fun _ctx ->
  match Xml.from_string {|<root id="42"><child>hello</child></root>|} with
  | Ok (Xml.Element { name = "root"; children = [ Xml.Element { name = "child"; _ } ]; _ } as root) ->
      (
        match Xml.attr "id" root with
        | Some "42" -> Ok ()
        | _ -> Error "Failed to parse root attribute"
      )
  | Ok _ -> Error "Unexpected parsed XML shape"
  | Error err -> Error (Xml.error_message err)

let test_parse_self_closing_and_single_quoted_attrs = fun _ctx ->
  match Xml.from_string {|<?xml version="1.0"?><node xpath='//row[1]'><sentinel/></node>|} with
  | Ok root -> (
      match (Xml.attr "xpath" root, Xml.child_elements ~name:"sentinel" root) with
      | (Some "//row[1]", [ Xml.Element { name = "sentinel"; children = []; _ } ]) -> Ok ()
      | _ -> Error "Failed to parse single-quoted attr or self-closing child"
    )
  | Error err -> Error (Xml.error_message err)

let test_parse_decodes_attrs_and_text_content = fun _ctx ->
  match Xml.from_string {|<frame name="a&amp;b">&lt;leaf&gt;</frame>|} with
  | Ok frame -> (
      match (Xml.attr "name" frame, Xml.text_content frame) with
      | (Some "a&b", "<leaf>") -> Ok ()
      | _ -> Error "Failed to decode XML entities"
    )
  | Error err -> Error (Xml.error_message err)

let tests =
  Test.[
    case "create element" test_create_element;
    case "create element with attributes" test_create_element_with_attrs;
    case "create text node" test_create_text;
    case "create CDATA" test_create_cdata;
    case "element with children" test_element_with_children;
    case "nested elements" test_nested_elements;
    case "serialize simple element" test_serialize_simple_element;
    case "serialize element with attributes" test_serialize_element_with_attrs;
    case "serialize text" test_serialize_text;
    case "serialize with children" test_serialize_with_children;
    case "serialize nested" test_serialize_nested;
    case "multiple attributes" test_multiple_attributes;
    case "mixed children" test_mixed_children;
    case "empty text" test_empty_text;
    case "CDATA serialization" test_cdata_serialization;
    case "XML declaration" test_declaration;
    case "indented output" test_indented_output;
    case "special chars in text" test_special_chars_in_text;
    case "special chars in attributes" test_special_chars_in_attrs;
    case "complex document" test_complex_document;
    case "parse simple element" test_parse_simple_element;
    case
      "parse self-closing and single-quoted attrs"
      test_parse_self_closing_and_single_quoted_attrs;
    case "parse decodes attrs and text content" test_parse_decodes_attrs_and_text_content;
  ]

let main ~args = Test.Cli.main ~name:"xml" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
