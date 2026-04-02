open Global

type fixture = {
  path: Path.t;
  relpath: string;
  name: string;
}

type t = {
  suite_name: string;
  test_name: string;
  test_index: int;
  source_file: string option;
  binary_path: string option;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: fixture option;
}

let with_fixture = fun ctx fixture -> { ctx with fixture = Some fixture }
