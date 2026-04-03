open Std
module Test = Std.Test
module G = Std.Graph.SimpleGraph

let make_package = fun name ->
  Riot_model.Package.make ~name ~path:(Path.v ".") ~relative_path:(Path.v ".") ()

let test_transitive_closure_dependency_first_order = fun _ctx ->
  let dep_c =
    Riot_planner.Dependency.{
      package = make_package "c";
      artifact_dir = Path.v "/cache/c";
      depset = [];
      hash = Crypto.hash_string "c"
    } in
  let dep_b =
    Riot_planner.Dependency.{
      package = make_package "b";
      artifact_dir = Path.v "/cache/b";
      depset = [ dep_c ];
      hash = Crypto.hash_string "b"
    } in
  let dep_a =
    Riot_planner.Dependency.{
      package = make_package "a";
      artifact_dir = Path.v "/cache/a";
      depset = [ dep_b; dep_c ];
      hash = Crypto.hash_string "a"
    } in
  let names = Riot_planner.Dependency.transitive_closure [ dep_a ]
  |> List.map (fun d -> d.Riot_planner.Dependency.package.name) in
  if names = [ "c"; "b"; "a" ] then
    Ok ()
  else
    Error ("unexpected order: " ^ String.concat "," names)

let test_library_cmxa_uses_store_location = fun _ctx ->
  let dep =
    Riot_planner.Dependency.{
      package = make_package "std";
      artifact_dir = Path.v "/tmp/cache/abcd";
      depset = [];
      hash = Crypto.hash_string "std"
    } in
  let expected =
    Path.(dep.artifact_dir / Riot_model.Module_name.(of_string dep.package.name |> cmxa)) in
  let got = Riot_planner.Dependency.library_cmxa dep in
  if Path.equal expected got then
    Ok ()
  else
    Error ("expected " ^ Path.to_string expected ^ " got " ^ Path.to_string got)

let test_module_graph_prefers_implementation_when_interface_exists = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_graph_prefers_impl"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected src dir creation to succeed" in
        let _ = Fs.write "type t\nval x : t\n" Path.(src_dir / Path.v "foo.mli")
        |> Result.expect ~msg:"expected foo.mli write to succeed" in
        let _ = Fs.write "type t = int\nlet x = 1\n" Path.(src_dir / Path.v "foo.ml")
        |> Result.expect ~msg:"expected foo.ml write to succeed" in
        let _ = Fs.write "val y : Foo.t\n" Path.(src_dir / Path.v "bar.mli") |> Result.expect ~msg:"expected bar.mli write to succeed" in
        let package = Riot_model.Package.make ~name:"pkg" ~path:package_root ~relative_path:(Path.v "pkg")
          ~sources:{
            src = [ Path.v "src/foo.mli"; Path.v "src/foo.ml"; Path.v "src/bar.mli" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace =
          Riot_model.Workspace.{
            root = tmpdir;
            target_dir_root =
              Path.(tmpdir / Path.v "target");
            packages = [ package ];
            dependencies = [];
            dev_dependencies = [];
            build_dependencies = [];
            profile_overrides = [];
          }
        in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"expected toolchain init to succeed" in
        let graph_builder = Riot_planner.Module_graph.create
          Riot_planner.Module_graph.{
            root = package_root;
            source_dir = Path.v "src";
            allowed_source_files = package.sources.src;
            namespace = "Pkg";
            package;
            toolchain;
            workspace;
          }
        in
        match Riot_planner.Module_graph.wire_dependencies graph_builder with
        | Error err -> Error (Riot_planner.Planning_error.to_string err)
        | Ok () ->
            let graph = Riot_planner.Module_graph.graph graph_builder in
            let find_node_id expected_kind expected_name =
              let matches ((_id, (node: Riot_planner.Module_node.t G.node))) =
                match node.value.kind with
                | Riot_planner.Module_node.ML mod_ when expected_kind = `implementation ->
                    String.equal
                      (Riot_model.Module.module_name mod_ |> Riot_model.Module_name.to_string)
                      expected_name
                | Riot_planner.Module_node.MLI mod_ when expected_kind = `interface ->
                    String.equal
                      (Riot_model.Module.module_name mod_ |> Riot_model.Module_name.to_string)
                      expected_name
                | _ -> false
              in
              match List.find_opt matches (G.map graph ~fn:(fun x -> x)) with
              | Some (node_id, _) -> Ok node_id
              | None -> Error ("expected node not found: " ^ expected_name)
            in
            match (
              find_node_id `interface "Bar",
              find_node_id `implementation "Foo",
              find_node_id `interface "Foo"
            ) with
            | Ok bar_mli_id, Ok foo_ml_id, Ok foo_mli_id -> (
                match G.get_node graph bar_mli_id with
                | None -> Error "expected bar.mli node to exist"
                | Some node ->
                    let depends_on_impl = List.exists (G.Node_id.eq foo_ml_id) node.deps in
                    let depends_on_intf = List.exists (G.Node_id.eq foo_mli_id) node.deps in
                    if depends_on_impl && not depends_on_intf then
                      Ok ()
                    else
                      Error "expected bar.mli to depend on foo.ml implementation node only"
              )
            | (Error msg, _, _)
            | (_, Error msg, _)
            | (_, _, Error msg) -> Error msg)
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.[
    case "transitive closure order and dedup" test_transitive_closure_dependency_first_order;
    case "library cmxa path from artifact_dir" test_library_cmxa_uses_store_location;
    case "module graph prefers implementation when interface exists" test_module_graph_prefers_implementation_when_interface_exists;
  ]

let name = "Planner Dependency Resolution Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
