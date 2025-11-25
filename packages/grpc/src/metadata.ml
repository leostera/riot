open Std

type t = (string * string) list

type content_type = Proto | Json | Custom of string

type encoding = Identity | Gzip | Deflate | Snappy

type timeout = {
  value : int;
  unit :
    [ `Hours
    | `Minutes
    | `Seconds
    | `Milliseconds
    | `Microseconds
    | `Nanoseconds
    ];
}

(** {1 Creating Metadata} *)

let empty = []

let add metadata ~key ~value = (key, value) :: metadata

let add_all metadata headers = headers @ metadata

let remove metadata ~key = List.filter (fun (k, _) -> not (String.equal k key)) metadata

let get metadata ~key =
  match List.find_opt (fun (k, _) -> String.equal k key) metadata with
  | Some (_, v) -> Some v
  | None -> None

let get_all metadata ~key =
  List.filter_map
    (fun (k, v) -> if String.equal k key then Some v else None)
    metadata

(** {1 Standard Headers} *)

let path ~service ~method_ = (":path", format "/%s/%s" service method_)

let content_type = function
  | Proto -> ("content-type", "application/grpc+proto")
  | Json -> ("content-type", "application/grpc+json")
  | Custom suffix -> ("content-type", format "application/grpc+%s" suffix)

let timeout t =
  let unit_str =
    match t.unit with
    | `Hours -> "H"
    | `Minutes -> "M"
    | `Seconds -> "S"
    | `Milliseconds -> "m"
    | `Microseconds -> "u"
    | `Nanoseconds -> "n"
  in
  ("grpc-timeout", format "%d%s" t.value unit_str)

let encoding = function
  | Identity -> ("grpc-encoding", "identity")
  | Gzip -> ("grpc-encoding", "gzip")
  | Deflate -> ("grpc-encoding", "deflate")
  | Snappy -> ("grpc-encoding", "snappy")

let status code = ("grpc-status", string_of_int (Status.to_int code))

let message msg = ("grpc-message", msg)

(** {1 Parsing} *)

let parse_content_type str =
  if String.starts_with ~prefix:"application/grpc" str then
    if String.equal str "application/grpc" || String.equal str "application/grpc+proto"
    then Some Proto
    else if String.equal str "application/grpc+json" then Some Json
    else
      (* Extract suffix after "application/grpc+" *)
      let prefix_len = String.length "application/grpc+" in
      if String.length str > prefix_len then
        Some (Custom (String.sub str prefix_len (String.length str - prefix_len)))
      else None
  else None

let parse_timeout str =
  let len = String.length str in
  if len < 2 then None
  else
    let unit_char = str.[len - 1] in
    let value_str = String.sub str 0 (len - 1) in
    match int_of_string_opt value_str with
    | None -> None
    | Some value -> (
        match unit_char with
        | 'H' -> Some { value; unit = `Hours }
        | 'M' -> Some { value; unit = `Minutes }
        | 'S' -> Some { value; unit = `Seconds }
        | 'm' -> Some { value; unit = `Milliseconds }
        | 'u' -> Some { value; unit = `Microseconds }
        | 'n' -> Some { value; unit = `Nanoseconds }
        | _ -> None)

let parse_encoding = function
  | "identity" -> Some Identity
  | "gzip" -> Some Gzip
  | "deflate" -> Some Deflate
  | "snappy" -> Some Snappy
  | _ -> None

let parse_status str =
  match int_of_string_opt str with
  | None -> None
  | Some code -> Status.of_int code

(** {1 Binary Metadata} *)

let is_binary name = String.ends_with ~suffix:"-bin" name

let encode_binary bytes = Data.Base64.encode_bytes bytes

let decode_binary str =
  try Ok (Data.Base64.decode_string str) with _ -> Error "Invalid base64 encoding"

(** {1 Validation} *)

let is_valid_header_name name =
  if String.length name = 0 then false
  else
    let first_char = name.[0] in
    (* Must start with : or lowercase letter *)
    if first_char = ':' || (first_char >= 'a' && first_char <= 'z') then
      (* Rest must be lowercase, digits, - or _ *)
      let rec check_rest i =
        if i >= String.length name then true
        else
          let c = name.[i] in
          if
            (c >= 'a' && c <= 'z')
            || (c >= '0' && c <= '9')
            || c = '-' || c = '_'
          then check_rest (i + 1)
          else false
      in
      check_rest 1
    else false

let is_reserved name = String.starts_with ~prefix:"grpc-" name

(** {1 Conversion} *)

let to_http_headers metadata =
  List.map
    (fun (name, value) -> { Http.Http2.Hpack.name; value })
    metadata

let of_http_headers headers =
  List.map
    (fun (header : Http.Http2.Hpack.header) -> (header.name, header.value))
    headers
