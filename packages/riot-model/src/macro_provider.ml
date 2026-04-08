open Std

type t = {
  package_name: string;
  package_path: Path.t;
  source_path: Path.t;
  module_name: string;
  module_path: string list;
  macros: string list;
}

let make = fun ?module_path ?(macros = []) ~package_name ~package_path ~source_path () ->
  let default_module_path =
    [ Module_name.(of_string package_name |> to_string) ]
  in
  {
    package_name;
    package_path;
    source_path;
    module_name =
      Module_name.(of_string package_name |> to_string);
    module_path = Option.unwrap_or ~default:default_module_path module_path;
    macros;
  }

let parse_module_path = fun package_name provider_items ->
  let validate_segments = fun segments ->
    if segments = [] then
      Error "macro provider 'module_path' must contain at least one segment"
    else
      Ok segments
  in
  match List.assoc_opt "module_path" provider_items with
  | None ->
      Ok [ Module_name.(of_string package_name |> to_string) ]
  | Some (Data.Toml.String module_path) ->
      module_path
      |> String.split_on_char '.'
      |> List.filter (fun segment -> not (String.equal segment ""))
      |> validate_segments
  | Some (Data.Toml.Array items) ->
      let rec loop acc = function
        | [] -> validate_segments (List.rev acc)
        | Data.Toml.String segment :: rest -> loop (segment :: acc) rest
        | _ -> Error "macro provider 'module_path' must be a string or string array"
      in
      loop [] items
  | Some _ -> Error "macro provider 'module_path' must be a string or string array"

let parse_macro_names = fun provider_items ->
  match List.assoc_opt "macros" provider_items with
  | None -> Error "macro provider must declare a 'macros' array"
  | Some (Data.Toml.Array items) ->
      let rec loop acc = function
        | [] ->
            let macros = List.rev acc |> List.sort_uniq String.compare in
            if macros = [] then
              Error "macro provider 'macros' must contain at least one string"
            else
              Ok macros
        | Data.Toml.String macro_name :: rest -> loop (macro_name :: acc) rest
        | _ -> Error "macro provider 'macros' must be an array of strings"
      in
      loop [] items
  | Some _ -> Error "macro provider 'macros' must be an array of strings"

let parse_provider = fun provider_toml ~package_name ~package_path ->
  match provider_toml with
  | Data.Toml.Table provider_items -> (
      match List.assoc_opt "path" provider_items with
      | Some (Data.Toml.String source_path) -> (
          match parse_module_path package_name provider_items, parse_macro_names provider_items with
          | Ok module_path, Ok macros -> Ok [
            make
              ~module_path
              ~macros
              ~package_name
              ~package_path
              ~source_path:(Path.(package_path / Path.v source_path))
              ()
          ]
          | Error err, _
          | _, Error err -> Error err
        )
      | None -> Error "macro provider must declare a 'path'"
      | Some _ -> Error "macro provider 'path' must be a string"
    )
  | _ -> Error "[riot.macro.provider] must be a table"

let parse_from_toml = fun items ~package_name ~package_path ->
  match List.assoc_opt "riot" items with
  | Some (Data.Toml.Table riot_items) -> (
      match List.assoc_opt "macro" riot_items with
      | Some (Data.Toml.Table macro_items) -> (
          match List.assoc_opt "provider" macro_items with
          | Some provider_toml -> parse_provider provider_toml ~package_name ~package_path
          | None -> Ok []
        )
      | Some _ -> Error "[riot.macro] must be a table"
      | None -> Ok []
    )
  | Some _ -> Error "[riot] must be a table"
  | None -> Ok []

let fingerprint = fun provider ->
  String.concat
    ":"
    [
      provider.package_name;
      provider.module_name;
      Path.to_string provider.package_path;
      Path.to_string provider.source_path;
      String.concat "." provider.module_path;
      String.concat "," provider.macros;
    ]

let compare = fun left right ->
  String.compare (fingerprint left) (fingerprint right)

let to_json = fun provider ->
  Data.Json.Object [
    ("package_name", Data.Json.String provider.package_name);
    ("package_path", Data.Json.String (Path.to_string provider.package_path));
    ("source_path", Data.Json.String (Path.to_string provider.source_path));
    ("module_name", Data.Json.String provider.module_name);
    ("module_path", Data.Json.Array (List.map (fun segment -> Data.Json.String segment) provider.module_path));
    ("macros", Data.Json.Array (List.map (fun macro_name -> Data.Json.String macro_name) provider.macros));
  ]

let of_json = function
  | Data.Json.Object fields -> (
      match (
        List.assoc_opt "package_name" fields,
        List.assoc_opt "package_path" fields,
        List.assoc_opt "source_path" fields,
        List.assoc_opt "module_name" fields,
        List.assoc_opt "module_path" fields,
        List.assoc_opt "macros" fields
      ) with
      | Some (Data.Json.String package_name), Some (Data.Json.String package_path), Some (Data.Json.String source_path), Some (Data.Json.String module_name), Some (Data.Json.Array module_path), Some (Data.Json.Array macros) -> (
          let decode_strings label values =
            let rec loop acc = function
              | [] -> Ok (List.rev acc)
              | Data.Json.String value :: rest -> loop (value :: acc) rest
              | _ -> Error ("invalid macro provider " ^ label ^ " payload")
            in
            loop [] values
          in
          match decode_strings "module_path" module_path, decode_strings "macros" macros with
          | Ok module_path, Ok macros -> Ok {
            package_name;
            package_path = Path.v package_path;
            source_path = Path.v source_path;
            module_name;
            module_path;
            macros;
          }
          | Error err, _
          | _, Error err -> Error err
        )
      | Some (Data.Json.String package_name), Some (Data.Json.String package_path), Some (Data.Json.String source_path), Some (Data.Json.String module_name), None, None -> Ok {
        package_name;
        package_path = Path.v package_path;
        source_path = Path.v source_path;
        module_name
        ;
        module_path = [ module_name ];
        macros = [];
      }
      | _ -> Error "invalid macro provider payload"
    )
  | _ -> Error "macro provider payload must be an object"
