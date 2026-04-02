open Std
module Uri = Std.Net.Uri

(* ==================== Percent Encoding Tests ==================== *)

let test_percent_encode_basic = fun _ctx ->
  let encoded = Uri.percent_encode "Hello World" in
  if encoded = "Hello%20World" then
    Ok ()
  else
    Error ("Expected 'Hello%20World', got '" ^ encoded ^ "'")

let test_percent_encode_email = fun _ctx ->
  let encoded = Uri.percent_encode "test@example.com" in
  if encoded = "test%40example.com" then
    Ok ()
  else
    Error ("Expected 'test%40example.com', got '" ^ encoded ^ "'")

let test_percent_encode_unreserved = fun _ctx ->
  (* Should NOT encode unreserved chars *)
  let encoded = Uri.percent_encode "abc-._~123" in
  if encoded = "abc-._~123" then
    Ok ()
  else
    Error ("Unreserved chars should not be encoded, got '" ^ encoded ^ "'")

let test_percent_encode_special = fun _ctx ->
  let encoded = Uri.percent_encode "100%" in
  if encoded = "100%25" then
    Ok ()
  else
    Error ("Expected '100%25', got '" ^ encoded ^ "'")

let test_percent_encode_slash = fun _ctx ->
  let encoded = Uri.percent_encode "/path/to/file" in
  if String.contains encoded "%" then
    Ok ()
  else
    Error ("Slashes should be encoded, got '" ^ encoded ^ "'")

let test_percent_encode_ampersand = fun _ctx ->
  let encoded = Uri.percent_encode "a&b" in
  if encoded = "a%26b" then
    Ok ()
  else
    Error ("Expected 'a%26b', got '" ^ encoded ^ "'")

(* ==================== Percent Decoding Tests ==================== *)

let test_percent_decode_basic = fun _ctx ->
  let decoded = Uri.percent_decode "Hello%20World" in
  if decoded = "Hello World" then
    Ok ()
  else
    Error ("Expected 'Hello World', got '" ^ decoded ^ "'")

let test_percent_decode_email = fun _ctx ->
  let decoded = Uri.percent_decode "test%40example.com" in
  if decoded = "test@example.com" then
    Ok ()
  else
    Error ("Expected 'test@example.com', got '" ^ decoded ^ "'")

let test_percent_decode_percent = fun _ctx ->
  let decoded = Uri.percent_decode "100%25" in
  if decoded = "100%" then
    Ok ()
  else
    Error ("Expected '100%', got '" ^ decoded ^ "'")

let test_percent_decode_uppercase = fun _ctx ->
  let decoded = Uri.percent_decode "%2B%2F%3D" in
  if decoded = "+/=" then
    Ok ()
  else
    Error ("Expected '+/=', got '" ^ decoded ^ "'")

let test_percent_decode_lowercase = fun _ctx ->
  let decoded = Uri.percent_decode "%2b%2f%3d" in
  if decoded = "+/=" then
    Ok ()
  else
    Error ("Expected '+/=', got '" ^ decoded ^ "'")

let test_percent_decode_utf8 = fun _ctx ->
  (* UTF-8 encoded é (C3 A9) *)
  let decoded = Uri.percent_decode "%C3%A9" in
  if decoded = "é" then
    Ok ()
  else
    Error ("Expected 'é', got '" ^ decoded ^ "'")

let test_percent_decode_invalid = fun _ctx ->
  (* Invalid sequences left as-is *)
  let decoded = Uri.percent_decode "%ZZ" in
  if decoded = "%ZZ" then
    Ok ()
  else
    Error ("Invalid sequences should be preserved, got '" ^ decoded ^ "'")

let test_percent_decode_incomplete = fun _ctx ->
  let decoded = Uri.percent_decode "%2" in
  if decoded = "%2" then
    Ok ()
  else
    Error ("Incomplete sequences should be preserved, got '" ^ decoded ^ "'")

let test_percent_decode_mixed = fun _ctx ->
  let decoded = Uri.percent_decode "Hello%20%2B%20World" in
  if decoded = "Hello + World" then
    Ok ()
  else
    Error ("Expected 'Hello + World', got '" ^ decoded ^ "'")

(* ==================== Form Encoding Tests ==================== *)

let test_form_encode_space = fun _ctx ->
  let encoded = Uri.form_encode "Hello World" in
  if encoded = "Hello+World" then
    Ok ()
  else
    Error ("Expected 'Hello+World', got '" ^ encoded ^ "'")

let test_form_encode_special = fun _ctx ->
  let encoded = Uri.form_encode "test@example.com" in
  if encoded = "test%40example.com" then
    Ok ()
  else
    Error ("Expected 'test%40example.com', got '" ^ encoded ^ "'")

let test_form_encode_unreserved = fun _ctx ->
  let encoded = Uri.form_encode "abc-._~123" in
  if encoded = "abc-._~123" then
    Ok ()
  else
    Error ("Unreserved chars should not be encoded, got '" ^ encoded ^ "'")

let test_form_encode_ampersand = fun _ctx ->
  let encoded = Uri.form_encode "a&b" in
  if encoded = "a%26b" then
    Ok ()
  else
    Error ("Expected 'a%26b', got '" ^ encoded ^ "'")

(* ==================== Form Decoding Tests ==================== *)

let test_form_decode_plus = fun _ctx ->
  let decoded = Uri.form_decode "Hello+World" in
  if decoded = "Hello World" then
    Ok ()
  else
    Error ("Expected 'Hello World', got '" ^ decoded ^ "'")

let test_form_decode_percent = fun _ctx ->
  let decoded = Uri.form_decode "test%40example.com" in
  if decoded = "test@example.com" then
    Ok ()
  else
    Error ("Expected 'test@example.com', got '" ^ decoded ^ "'")

let test_form_decode_mixed = fun _ctx ->
  let decoded = Uri.form_decode "Hello+World%21" in
  if decoded = "Hello World!" then
    Ok ()
  else
    Error ("Expected 'Hello World!', got '" ^ decoded ^ "'")

let test_form_decode_multiple_plus = fun _ctx ->
  let decoded = Uri.form_decode "a+b+c" in
  if decoded = "a b c" then
    Ok ()
  else
    Error ("Expected 'a b c', got '" ^ decoded ^ "'")

(* ==================== Round-trip Tests ==================== *)

let test_roundtrip_percent = fun _ctx ->
  let original = "Hello World! test@example.com 100%" in
  let encoded = Uri.percent_encode original in
  let decoded = Uri.percent_decode encoded in
  if decoded = original then
    Ok ()
  else
    Error ("Round-trip failed: '" ^ original ^ "' -> '" ^ encoded ^ "' -> '" ^ decoded ^ "'")

let test_roundtrip_form = fun _ctx ->
  let original = "Hello World! test@example.com" in
  let encoded = Uri.form_encode original in
  let decoded = Uri.form_decode encoded in
  if decoded = original then
    Ok ()
  else
    Error ("Round-trip failed: '" ^ original ^ "' -> '" ^ encoded ^ "' -> '" ^ decoded ^ "'")

let test_roundtrip_special_chars = fun _ctx ->
  let original = "a&b=c/d?e#f" in
  let encoded = Uri.form_encode original in
  let decoded = Uri.form_decode encoded in
  if decoded = original then
    Ok ()
  else
    Error "Round-trip with special chars failed"

(* ==================== Query Parsing Tests ==================== *)

let test_query_parse_simple = fun _ctx ->
  let params = Uri.Query.parse "page=1&limit=10" in
  match (Uri.Query.get params "page", Uri.Query.get params "limit") with
  | Some "1", Some "10" -> Ok ()
  | _ -> Error "Failed to parse simple query"

let test_query_parse_encoded = fun _ctx ->
  let params = Uri.Query.parse "name=John%20Doe&email=test%40example.com" in
  match (Uri.Query.get params "name", Uri.Query.get params "email") with
  | Some "John Doe", Some "test@example.com" -> Ok ()
  | _ -> Error "Failed to decode query parameters"

let test_query_parse_plus = fun _ctx ->
  let params = Uri.Query.parse "query=hello+world" in
  match Uri.Query.get params "query" with
  | Some "hello world" -> Ok ()
  | _ -> Error "Failed to decode plus as space"

let test_query_parse_empty_value = fun _ctx ->
  let params = Uri.Query.parse "key1=&key2=value" in
  match (Uri.Query.get params "key1", Uri.Query.get params "key2") with
  | Some "", Some "value" -> Ok ()
  | _ -> Error "Failed to parse empty value"

let test_query_parse_no_value = fun _ctx ->
  let params = Uri.Query.parse "flag&key=value" in
  match (Uri.Query.get params "flag", Uri.Query.get params "key") with
  | Some "", Some "value" -> Ok ()
  | _ -> Error "Failed to parse parameter without value"

let test_query_parse_special_chars = fun _ctx ->
  let params = Uri.Query.parse "path=%2Fhome%2Fuser&equals=a%3Db" in
  match (Uri.Query.get params "path", Uri.Query.get params "equals") with
  | Some "/home/user", Some "a=b" -> Ok ()
  | _ -> Error "Failed to decode special characters"

let test_query_parse_multiple_same_key = fun _ctx ->
  let params = Uri.Query.parse "tag=red&tag=blue&tag=green" in
  let tags = Uri.Query.get_all params "tag" in
  if List.length tags = 3 && List.mem "red" tags && List.mem "blue" tags && List.mem "green" tags then
    Ok ()
  else
    Error "Failed to handle multiple values for same key"

let test_query_parse_empty = fun _ctx ->
  let params = Uri.Query.parse "" in
  if List.length params = 0 then
    Ok ()
  else
    Error "Empty query should parse to []"

(* ==================== Query to_string Tests ==================== *)

let test_query_to_string_simple = fun _ctx ->
  let params = [ ("page", "1"); ("sort", "name") ] in
  let query = Uri.Query.to_string params in
  if query = "page=1&sort=name" then
    Ok ()
  else
    Error ("Expected 'page=1&sort=name', got '" ^ query ^ "'")

let test_query_to_string_encoded = fun _ctx ->
  let params = [ ("name", "John Doe"); ("email", "test@example.com") ] in
  let query = Uri.Query.to_string params in
  if query = "name=John+Doe&email=test%40example.com" then
    Ok ()
  else
    Error ("Expected 'name=John+Doe&email=test%40example.com', got '" ^ query ^ "'")

let test_query_to_string_special = fun _ctx ->
  let params = [ ("query", "a=b&c=d") ] in
  let query = Uri.Query.to_string params in
  (* = and & should be encoded *)
  if String.contains query "%" then
    Ok ()
  else
    Error ("Special chars should be encoded, got '" ^ query ^ "'")

let test_query_to_string_empty_value = fun _ctx ->
  let params = [ ("key1", ""); ("key2", "value") ] in
  let query = Uri.Query.to_string params in
  if query = "key1&key2=value" then
    Ok ()
  else
    Error ("Expected 'key1&key2=value', got '" ^ query ^ "'")

let test_query_to_string_unreserved = fun _ctx ->
  let params = [ ("key", "abc-._~123") ] in
  let query = Uri.Query.to_string params in
  if query = "key=abc-._~123" then
    Ok ()
  else
    Error ("Unreserved chars should not be encoded, got '" ^ query ^ "'")

(* ==================== Query Round-trip Tests ==================== *)

let test_query_roundtrip = fun _ctx ->
  let original = [
    ("name", "John Doe");
    ("email", "test@example.com");
    ("note", "Hello & goodbye!");
  ] in
  let query_str = Uri.Query.to_string original in
  let parsed = Uri.Query.parse query_str in
  (* Check each param *)
  match (Uri.Query.get parsed "name", Uri.Query.get parsed "email", Uri.Query.get parsed "note") with
  | Some "John Doe", Some "test@example.com", Some "Hello & goodbye!" -> Ok ()
  | _ -> Error "Round-trip failed - values don't match"

let test_query_roundtrip_empty_value = fun _ctx ->
  let original = [ ("key1", ""); ("key2", "value") ] in
  let query_str = Uri.Query.to_string original in
  let parsed = Uri.Query.parse query_str in
  match (Uri.Query.get parsed "key1", Uri.Query.get parsed "key2") with
  | Some "", Some "value" -> Ok ()
  | _ -> Error "Round-trip with empty value failed"

let test_query_roundtrip_special_delimiters = fun _ctx ->
  let original = [ ("a", "b&c=d"); ("e", "f?g#h") ] in
  let query_str = Uri.Query.to_string original in
  let parsed = Uri.Query.parse query_str in
  match (Uri.Query.get parsed "a", Uri.Query.get parsed "e") with
  | Some "b&c=d", Some "f?g#h" -> Ok ()
  | _ -> Error "Round-trip with special delimiters failed"

(* ==================== RFC 3986 Reserved Characters Tests ==================== *)

let test_reserved_gen_delims = fun _ctx ->
  (* gen-delims = ":" / "/" / "?" / "#" / "[" / "]" / "@" *)
  let tests = [
    ("%3A", ":");
    ("%2F", "/");
    ("%3F", "?");
    ("%23", "#");
    ("%5B", "[");
    ("%5D", "]");
    ("%40", "@");
  ] in
  let results =
    List.map (fun ((enc, dec)) -> Uri.percent_decode enc = dec) tests
  in
  if List.for_all (fun x -> x) results then
    Ok ()
  else
    Error "Failed to decode gen-delims"

let test_reserved_sub_delims = fun _ctx ->
  (* sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "=" *)
  let tests = [
    ("%21", "!");
    ("%24", "$");
    ("%26", "&");
    ("%27", "'");
    ("%28", "(");
    ("%29", ")");
    ("%2A", "*");
    ("%2B", "+");
    ("%2C", ",");
    ("%3B", ";");
    ("%3D", "=");
  ]
  in
  let results =
    List.map (fun ((enc, dec)) -> Uri.percent_decode enc = dec) tests
  in
  if List.for_all (fun x -> x) results then
    Ok ()
  else
    Error "Failed to decode sub-delims"

(* ==================== Filename/Path Encoding Tests ==================== *)

let test_percent_decode_filename_with_spaces = fun _ctx ->
  let decoded = Uri.percent_decode "Screenshot%202025-11-05%20at%2018.19.04.png" in
  if decoded = "Screenshot 2025-11-05 at 18.19.04.png" then
    Ok ()
  else
    Error ("Expected 'Screenshot 2025-11-05 at 18.19.04.png', got '" ^ decoded ^ "'")

let test_percent_decode_path_with_spaces = fun _ctx ->
  let decoded = Uri.percent_decode "/browse/assets/Screenshot%202025-11-05%20at%2018.19.04.png" in
  if decoded = "/browse/assets/Screenshot 2025-11-05 at 18.19.04.png" then
    Ok ()
  else
    Error ("Expected path with spaces, got '" ^ decoded ^ "'")

let test_percent_decode_relative_path_with_spaces = fun _ctx ->
  let decoded = Uri.percent_decode "assets/Screenshot%202025-11-05%20at%2018.19.04.png" in
  if decoded = "assets/Screenshot 2025-11-05 at 18.19.04.png" then
    Ok ()
  else
    Error ("Expected relative path with spaces, got '" ^ decoded ^ "'")

let test_percent_decode_preserves_length = fun _ctx ->
  let input = "Screenshot%202025-11-05%20at%2018.19.04.png" in
  let decoded = Uri.percent_decode input in
  let expected_length = String.length "Screenshot 2025-11-05 at 18.19.04.png" in
  if String.length decoded = expected_length then
    Ok ()
  else
    Error (String.concat
      ""
      [
        "Length mismatch: expected ";
        string_of_int expected_length;
        " but got ";
        string_of_int (String.length decoded);
        " for '";
        decoded;
        "'"
      ])

let test_percent_decode_multiple_spaces = fun _ctx ->
  let decoded = Uri.percent_decode "file%20with%20many%20spaces.txt" in
  if decoded = "file with many spaces.txt" then
    Ok ()
  else
    Error ("Expected 'file with many spaces.txt', got '" ^ decoded ^ "'")

(* ==================== URI Parsing with Percent-Encoded Paths ==================== *)

let test_uri_parse_path_with_percent_encoding = fun _ctx ->
  match Uri.of_string "/browse/assets/Screenshot%202025-11-05%20at%2018.19.04.png" with
  | Ok uri ->
      let path = Uri.path uri in
      if path = "/browse/assets/Screenshot%202025-11-05%20at%2018.19.04.png" then
        Ok ()
      else
        Error (String.concat "" [ "Expected full path with %20, got '"; path; "'" ])
  | Error _ -> Error "Failed to parse URI with percent-encoded path"

let test_uri_parse_path_preserves_encoding = fun _ctx ->
  match Uri.of_string "/path/with%20spaces/file%2Bname.txt" with
  | Ok uri ->
      let reconstructed = Uri.to_string uri in
      if reconstructed = "/path/with%20spaces/file%2Bname.txt" then
        Ok ()
      else
        Error (String.concat "" [ "Path encoding not preserved, got '"; reconstructed; "'" ])
  | Error _ -> Error "Failed to parse path with percent encoding"

let test_uri_parse_and_decode_path = fun _ctx ->
  (* Test the full workflow: parse URI -> get path -> decode *)
  match Uri.of_string "/files/my%20document.pdf" with
  | Ok uri ->
      let encoded_path = Uri.path uri in
      let decoded_path = Uri.percent_decode encoded_path in
      if decoded_path = "/files/my document.pdf" then
        Ok ()
      else
        Error (String.concat "" [ "Expected decoded path with spaces, got '"; decoded_path; "'" ])
  | Error _ -> Error "Failed to parse URI"

let test_uri_roundtrip_with_encoded_path = fun _ctx ->
  let original = "/api/users/John%20Doe/profile" in
  match Uri.of_string original with
  | Ok uri ->
      let reconstructed = Uri.to_string uri in
      if reconstructed = original then
        Ok ()
      else
        Error (String.concat "" [ "Roundtrip failed: '"; original; "' -> '"; reconstructed; "'" ])
  | Error _ -> Error "Failed to parse URI with encoded path"

(* ==================== Integration Tests ==================== *)

let test_uri_with_encoded_query = fun _ctx ->
  match Uri.of_string "https://example.com/search?q=hello+world&filter=name%3DJohn" with
  | Ok uri -> (
      match Uri.query uri with
      | Some query_str ->
          let params = Uri.Query.parse query_str in
          (
            match (Uri.Query.get params "q", Uri.Query.get params "filter") with
            | Some "hello world", Some "name=John" -> Ok ()
            | _ -> Error "Failed to decode query in full URI"
          )
      | None -> Error "No query found in URI"
    )
  | Error _ -> Error "Failed to parse URI"

let test_full_roundtrip = fun _ctx ->
  (* Create URI with encoded query *)
  let params = [ ("name", "John Doe"); ("page", "1") ] in
  let query_str = Uri.Query.to_string params in
  let uri_str = "https://example.com/api?" ^ query_str in
  match Uri.of_string uri_str with
  | Ok uri -> (
      match Uri.query uri with
      | Some q ->
          let parsed = Uri.Query.parse q in
          (
            match (Uri.Query.get parsed "name", Uri.Query.get parsed "page") with
            | Some "John Doe", Some "1" -> Ok ()
            | _ -> Error "Full round-trip failed"
          )
      | None -> Error "No query in parsed URI"
    )
  | Error _ -> Error "Failed to parse URI"

(* ==================== Test Suite ==================== *)

let tests =
  Test.[
    case "percent_encode basic" test_percent_encode_basic;
    case "percent_encode email" test_percent_encode_email;
    case "percent_encode unreserved" test_percent_encode_unreserved;
    case "percent_encode special" test_percent_encode_special;
    case "percent_encode slash" test_percent_encode_slash;
    case "percent_encode ampersand" test_percent_encode_ampersand;
    case "percent_decode basic" test_percent_decode_basic;
    case "percent_decode email" test_percent_decode_email;
    case "percent_decode percent" test_percent_decode_percent;
    case "percent_decode uppercase" test_percent_decode_uppercase;
    case "percent_decode lowercase" test_percent_decode_lowercase;
    case "percent_decode utf8" test_percent_decode_utf8;
    case "percent_decode invalid" test_percent_decode_invalid;
    case "percent_decode incomplete" test_percent_decode_incomplete;
    case "percent_decode mixed" test_percent_decode_mixed;
    case "form_encode space" test_form_encode_space;
    case "form_encode special" test_form_encode_special;
    case "form_encode unreserved" test_form_encode_unreserved;
    case "form_encode ampersand" test_form_encode_ampersand;
    case "form_decode plus" test_form_decode_plus;
    case "form_decode percent" test_form_decode_percent;
    case "form_decode mixed" test_form_decode_mixed;
    case "form_decode multiple plus" test_form_decode_multiple_plus;
    case "roundtrip percent" test_roundtrip_percent;
    case "roundtrip form" test_roundtrip_form;
    case "roundtrip special chars" test_roundtrip_special_chars;
    case "query parse simple" test_query_parse_simple;
    case "query parse encoded" test_query_parse_encoded;
    case "query parse plus" test_query_parse_plus;
    case "query parse empty value" test_query_parse_empty_value;
    case "query parse no value" test_query_parse_no_value;
    case "query parse special chars" test_query_parse_special_chars;
    case "query parse multiple same key" test_query_parse_multiple_same_key;
    case "query parse empty" test_query_parse_empty;
    case "query to_string simple" test_query_to_string_simple;
    case "query to_string encoded" test_query_to_string_encoded;
    case "query to_string special" test_query_to_string_special;
    case "query to_string empty value" test_query_to_string_empty_value;
    case "query to_string unreserved" test_query_to_string_unreserved;
    case "query roundtrip" test_query_roundtrip;
    case "query roundtrip empty value" test_query_roundtrip_empty_value;
    case "query roundtrip special delimiters" test_query_roundtrip_special_delimiters;
    case "reserved gen-delims" test_reserved_gen_delims;
    case "reserved sub-delims" test_reserved_sub_delims;
    case "percent_decode filename with spaces" test_percent_decode_filename_with_spaces;
    case "percent_decode path with spaces" test_percent_decode_path_with_spaces;
    case "percent_decode relative path with spaces" test_percent_decode_relative_path_with_spaces;
    case "percent_decode preserves length" test_percent_decode_preserves_length;
    case "percent_decode multiple spaces" test_percent_decode_multiple_spaces;
    case "uri parse path with percent encoding" test_uri_parse_path_with_percent_encoding;
    case "uri parse path preserves encoding" test_uri_parse_path_preserves_encoding;
    case "uri parse and decode path" test_uri_parse_and_decode_path;
    case "uri roundtrip with encoded path" test_uri_roundtrip_with_encoded_path;
    case "uri with encoded query" test_uri_with_encoded_query;
    case "full roundtrip" test_full_roundtrip;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"net_uri" ~tests ~args) ~args:Env.args ()
