open Std

type t = {
  package_name: string;
  package_path: Path.t;
  source_path: Path.t;
  module_name: string;
}

val make:
  package_name:string ->
  package_path:Path.t ->
  source_path:Path.t ->
  t
