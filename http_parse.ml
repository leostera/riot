#use http;;

open Std

module Json = Std.Data.Json
module Request = Std.Net.Http.Request
module Header = Std.Net.Http.Header
module Method = Std.Net.Http.Method
module Version = Std.Net.Http.Version
module Uri = Std.Net.Uri
module Parser = Http.Http1.Request
module Common = Http.Http1.Common

let headers_to_json headers =
  Header.to_list headers
  |> Std.List.map ~fn:(fun (name, value) ->
    Json.obj [
      ("name", Json.string name);
      ("value", Json.string value);
    ])
  |> Json.array

let body_to_json request =
  match Request.body_string request with
  | Some body -> Json.string body
  | None -> Json.null

let request_to_json request ~remaining =
  Json.obj [
    ("method", Json.string (Method.to_string (Request.method_ request)));
    ("uri", Json.string (Uri.to_string (Request.uri request)));
    ("version", Json.string (Version.to_string (Request.version request)));
    ("headers", headers_to_json (Request.headers request));
    ("body", body_to_json request);
    ("remaining", Json.string remaining);
  ]

let input =
  match args with
  | request :: _ -> request
  | _ ->
      "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"

let output =
  match Parser.parse_head input with
  | Common.Done { value; remaining } ->
      Json.obj [
        ("ok", Json.bool true);
        ("request", request_to_json value ~remaining);
      ]
  | Common.Need_more ->
      Json.obj [
        ("ok", Json.bool false);
        ("error", Json.string "need_more");
      ]
  | Common.Error error ->
      Json.obj [
        ("ok", Json.bool false);
        ("error", Json.string (Common.error_to_string error));
      ]

let () = Std.println (Json.to_string_pretty output)
