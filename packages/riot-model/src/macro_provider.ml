open Std

type t = {
  package_name: string;
  package_path: Path.t;
  source_path: Path.t;
  module_name: string;
}

let make = fun ~package_name ~package_path ~source_path ->
  {
    package_name;
    package_path;
    source_path;
    module_name =
      Module_name.(of_string package_name |> to_string);
  }

let fingerprint = fun provider ->
  String.concat
    ":"
    [
      provider.package_name;
      provider.module_name;
      Path.to_string provider.package_path;
      Path.to_string provider.source_path;
    ]

let compare = fun left right ->
  String.compare (fingerprint left) (fingerprint right)

let to_json = fun provider ->
  Data.Json.Object [
    ("package_name", Data.Json.String provider.package_name);
    ("package_path", Data.Json.String (Path.to_string provider.package_path));
    ("source_path", Data.Json.String (Path.to_string provider.source_path));
    ("module_name", Data.Json.String provider.module_name);
  ]

let of_json = function
  | Data.Json.Object fields -> (
      match (
        List.assoc_opt "package_name" fields,
        List.assoc_opt "package_path" fields,
        List.assoc_opt "source_path" fields,
        List.assoc_opt "module_name" fields
      ) with
      | Some (Data.Json.String package_name), Some (Data.Json.String package_path), Some (Data.Json.String source_path), Some (Data.Json.String module_name) -> Ok {
        package_name;
        package_path = Path.v package_path;
        source_path = Path.v source_path;
        module_name
      }
      | _ -> Error "invalid macro provider payload"
    )
  | _ -> Error "macro provider payload must be an object"
