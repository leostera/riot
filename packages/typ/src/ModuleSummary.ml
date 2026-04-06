open Std

type t = {
  module_name: string;
  source_hash: Crypto.hash;
  summary: PersistedSummary.t;
}

let make = fun ~module_name ~source_hash ~summary -> { module_name; source_hash; summary }

let module_name = fun summary -> summary.module_name

let source_hash = fun summary -> summary.source_hash

let summary = fun summary -> summary.summary

let exports = fun summary -> PersistedSummary.exports summary.summary

let type_decls = fun summary -> PersistedSummary.type_decls summary.summary

let json_type_name = function
  | Data.Json.Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"

let error_expected = fun expected actual ->
  Error ("expected " ^ expected ^ " but got " ^ json_type_name actual)

let get_object = function
  | Data.Json.Object fields -> Ok fields
  | other -> error_expected "object" other

let get_string = function
  | Data.Json.String value -> Ok value
  | other -> error_expected "string" other

let field = fun name fields ->
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error _ as err -> err

let hash_of_hex = fun hex ->
  match Encoding.Hex.decode_bytes hex with
  | Ok bytes -> Ok (Crypto.Hash.of_bytes bytes)
  | Error `Invalid_base16 -> Error ("invalid source_hash hex digest " ^ hex)

module Json = struct
  let to_json = fun summary ->
    Data.Json.Object [
      ("module_name", Data.Json.String summary.module_name);
      ("source_hash", Data.Json.String (Crypto.Digest.hex summary.source_hash));
      ("summary", PersistedSummary.Json.to_json summary.summary);
    ]

  let of_json = fun json ->
    let* fields = get_object json in
    let* module_name_json = field "module_name" fields in
    let* source_hash_json = field "source_hash" fields in
    let* summary_json = field "summary" fields in
    let* module_name = get_string module_name_json in
    let* source_hash_hex = get_string source_hash_json in
    let* source_hash = hash_of_hex source_hash_hex in
    let* summary = PersistedSummary.Json.of_json summary_json in
    Ok { module_name; source_hash; summary }
end
