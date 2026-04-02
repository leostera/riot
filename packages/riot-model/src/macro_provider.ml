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
    module_name = Module_name.(of_string package_name |> to_string);
  }
