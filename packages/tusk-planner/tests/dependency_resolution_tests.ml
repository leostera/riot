open Std

module Test = Std.Test

let make_package name =
  Tusk_model.Package.
    {
      name;
      path = Path.v ".";
      relative_path = Path.v ".";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources = { src = []; native = []; tests = []; examples = []; bench = [] };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
    }

let test_transitive_closure_dependency_first_order () =
  let dep_c =
    Tusk_planner.Dependency.
      {
        package = make_package "c";
        artifact_dir = Path.v "/cache/c";
        depset = [];
        hash = Crypto.hash_string "c";
      }
  in
  let dep_b =
    Tusk_planner.Dependency.
      {
        package = make_package "b";
        artifact_dir = Path.v "/cache/b";
        depset = [ dep_c ];
        hash = Crypto.hash_string "b";
      }
  in
  let dep_a =
    Tusk_planner.Dependency.
      {
        package = make_package "a";
        artifact_dir = Path.v "/cache/a";
        depset = [ dep_b; dep_c ];
        hash = Crypto.hash_string "a";
      }
  in
  let names =
    Tusk_planner.Dependency.transitive_closure [ dep_a ]
    |> List.map (fun d -> d.Tusk_planner.Dependency.package.name)
  in
  if names = [ "c"; "b"; "a" ] then Ok ()
  else Error ("unexpected order: " ^ String.concat "," names)

let test_library_cmxa_uses_store_location () =
  let dep =
    Tusk_planner.Dependency.
      {
        package = make_package "std";
        artifact_dir = Path.v "/tmp/cache/abcd";
        depset = [];
        hash = Crypto.hash_string "std";
      }
  in
  let expected = Path.v "/tmp/cache/abcd/std.cmxa" in
  let got = Tusk_planner.Dependency.library_cmxa dep in
  if Path.equal expected got then Ok ()
  else
    Error
      ("expected " ^ Path.to_string expected ^ " got " ^ Path.to_string got)

let tests =
  Test.
    [
      case "transitive closure order and dedup"
        test_transitive_closure_dependency_first_order;
      case "library cmxa path from artifact_dir"
        test_library_cmxa_uses_store_location;
    ]

let name = "Planner Dependency Resolution Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
