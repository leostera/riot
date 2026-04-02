open Std

type t = {
  package_name: string;
  package_path: Path.t;
  source_path: Path.t;
  module_name: string;
}
val make: package_name:string -> package_path:Path.t -> source_path:Path.t -> t

val fingerprint: t -> string

val compare: t -> t -> int

val to_json: t -> Data.Json.t

val of_json: Data.Json.t -> (t, string) result
