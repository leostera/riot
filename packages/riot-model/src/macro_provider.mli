open Std

type t = {
  package_name: string;
  package_path: Path.t;
  source_path: Path.t;
  module_name: string;
  module_path: string list;
  macros: string list;
}
val make:
  ?module_path:string list ->
  ?macros:string list ->
  package_name:string ->
  package_path:Path.t ->
  source_path:Path.t ->
  unit ->
  t

val parse_from_toml:
  (string * Data.Toml.value) list ->
  package_name:string ->
  package_path:Path.t ->
  (t list, string) result

val fingerprint: t -> string

val compare: t -> t -> int

val to_json: t -> Data.Json.t

val of_json: Data.Json.t -> (t, string) result
