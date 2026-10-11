open Std
open Riot_model

module Test = Std.Test
module G = Graph.SimpleGraph

let test_toolchain =
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"Failed to initialize toolchain"

let planner_artifacts_version = "planner-artifacts:v32"

let legacy_planner_artifacts_version = "planner-artifacts:v19"

let explicit_root_library_path_fix_planner_artifacts_version = "planner-artifacts:v12"

let nested_sibling_dependency_fix_planner_artifacts_version = "planner-artifacts:v13"

let make_test_workspace = fun tmpdir packages ->
  Riot_model.Workspace.make_realized
    ~root:tmpdir
    ~packages
    ~target_dir:(Path.v "target")
    ()

let make_package = fun tmpdir name ->
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let _ = Fs.create_dir_all pkg_dir in
  Riot_model.Package.make
    ~name:(
      Package_name.from_string name
      |> Result.expect ~msg:("expected valid package name: " ^ name)
    )
    ~path:pkg_dir
    ~relative_path:(Path.v name)
    ()

let workspace_dependency = fun name ->
  Riot_model.Package.{
    name =
      Package_name.from_string name
      |> Result.expect ~msg:("expected valid dependency name: " ^ name);
    source =
      {
        workspace = true;
        builtin = false;
        path = None;
        source_locator = None;
        ref_ = None;
        version = None;
      };
  }

let clone_workspace_with_target = fun (workspace: Riot_model.Workspace.t) ~target_dir ->
  Riot_model.Workspace.make
    ?name:workspace.name
    ~root:workspace.root
    ~packages:workspace.packages
    ~dependencies:workspace.dependencies
    ~dev_dependencies:workspace.dev_dependencies
    ~build_dependencies:workspace.build_dependencies
    ~profile_overrides:workspace.profile_overrides
    ~target_dir
    ()

let find_package_by_name = fun (workspace: Riot_model.Workspace.t) name ->
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Dev workspace
  |> List.find
    ~fn:(fun (pkg: Riot_model.Package.t) ->
      Package_name.equal
        pkg.name
        (
          Package_name.from_string name
          |> Result.expect ~msg:("expected valid package name: " ^ name)
        ))

let build_unit_key = fun
  ?(target = Riot_model.Target.host ()) ?(profile = Riot_model.Profile.debug) package artifact ->
  ({
    package = package.Riot_model.Package.name;
    artifact;
    target;
    profile;
  }: Riot_planner.Build_unit.key)

let library_unit = fun ?target ?profile package ->
  let package = Riot_model.Package.for_scope Riot_model.Package.Normal package in
  let key = build_unit_key ?target ?profile package Riot_planner.Build_unit.Library in
  Riot_planner.Build_unit.from_artifact
    ~package
    ~artifact:key.artifact
    ~target:key.target
    ~profile:key.profile

let binary_artifact_for_path = fun name path ->
  let path = Path.to_string path in
  if String.starts_with ~prefix:"tests/" path then
    Riot_planner.Build_unit.TestBinary { name }
  else if String.starts_with ~prefix:"examples/" path then
    Riot_planner.Build_unit.ExampleBinary { name }
  else if String.starts_with ~prefix:"bench/" path then
    Riot_planner.Build_unit.BenchBinary { name }
  else
    Riot_planner.Build_unit.RuntimeBinary { name }

let binary_unit = fun ?target ?profile package binary_name ->
  match List.find
    package.Riot_model.Package.binaries
    ~fn:(fun binary -> String.equal binary.name binary_name) with
  | None -> Error ("binary not found in package: " ^ binary_name)
  | Some binary ->
      match Riot_model.Package.for_binary ~binary_name package with
      | None -> Error ("package could not be projected for binary: " ^ binary_name)
      | Some package ->
          let key =
            build_unit_key
              ?target
              ?profile
              package
              (binary_artifact_for_path binary.name binary.path)
          in
          Ok (Riot_planner.Build_unit.from_artifact
            ~package
            ~artifact:key.artifact
            ~target:key.target
            ~profile:key.profile)

let synthetic_tool_unit = fun
  ?(target = Riot_model.Target.host ()) ?(profile = Riot_model.Profile.debug) package name ->
  let package = Riot_model.Package.for_scope Riot_model.Package.Normal package in
  let key =
    build_unit_key ~target ~profile package (Riot_planner.Build_unit.SyntheticTool { name })
  in
  Riot_planner.Build_unit.from_artifact
    ~package
    ~artifact:key.artifact
    ~target:key.target
    ~profile:key.profile

let create_unit_graph = fun
  ?(roots = None)
  ?(targets = [Riot_model.Target.host ()])
  ?(profile = Riot_model.Profile.debug)
  ?(kind = Riot_planner.Build_unit_graph.Runtime)
  ?(synthetic_tools = [])
  workspace ->
  Riot_planner.Build_unit_graph.create
    workspace
    Riot_planner.Build_unit_graph.{
      roots;
      targets;
      profile;
      kind;
      synthetic_tools;
    }

let all_dev_artifacts = Riot_model.Package.{ tests = true; examples = true; benches = true }

let expect_unit_graph = fun result ->
  result
  |> Result.expect ~msg:"build unit graph should build"

let runtime_unit_graph = fun workspace ->
  create_unit_graph workspace
  |> expect_unit_graph

let dev_unit_graph = fun workspace ->
  create_unit_graph ~kind:(Riot_planner.Build_unit_graph.Dev all_dev_artifacts) workspace
  |> expect_unit_graph

let synthetic_tool_graph = fun workspace package name ->
  create_unit_graph
    ~roots:(Some [])
    ~synthetic_tools:[
      Riot_planner.Build_unit_graph.{ package = package.Riot_model.Package.name; name };
    ]
    workspace
  |> expect_unit_graph

let unit_from_graph = fun unit_graph unit_key ->
  Riot_planner.Build_unit_graph.find unit_graph unit_key
  |> Option.map ~fn:Riot_planner.Build_unit_graph.node_value

let dependency_of_plan_result = fun store result ->
  match result with
  | Riot_planner.Package_planner.Cached { package; artifact; depset; _ } ->
      Some Riot_planner.Dependency.{
        package;
        artifact_dir = Riot_store.Store.hash_dir_of store artifact.input_hash;
        depset;
        input_hash = artifact.input_hash;
        output_hash = artifact.output_hash;
      }
  | Riot_planner.Package_planner.Planned { package; hash; depset; _ } ->
      Some Riot_planner.Dependency.{
        package;
        artifact_dir = Riot_store.Store.hash_dir_of store hash;
        depset;
        input_hash = hash;
        output_hash = hash;
      }

let dependency_of_required_plan_result = fun store result ->
  match dependency_of_plan_result store result with
  | Some dependency -> dependency
  | None -> panic "planned package did not produce a dependency summary"

let plan_build_unit_raw = fun
  ~workspace ~store ~(unit:Riot_planner.Build_unit.t) ~depset ~build_ctx ->
  Riot_planner.Package_planner.plan_build_unit
    ~on_source_analyzed:(fun _ -> ())
    ~workspace
    ~toolchain:test_toolchain
    ~store
    ~unit
    ~depset
    ~build_ctx

let plan_build_unit = fun ~workspace ~store ~unit ~depset ~build_ctx ->
  plan_build_unit_raw ~workspace ~store ~unit ~depset ~build_ctx
  |> Result.map_err ~fn:Riot_planner.Planning_error.to_string

let plan_build_unit_from_graph_raw = fun ~workspace ~store ~unit_graph ~unit_key ~build_ctx ->
  match Riot_planner.Build_unit_graph.topological_sort unit_graph with
  | Error cycle ->
      Error (Riot_planner.Planning_error.GraphBuildFailed {
        reason = "build unit graph contains a cycle: "
        ^ String.concat ", " (List.map cycle ~fn:Riot_planner.Build_unit.key_to_string);
      })
  | Ok units ->
      let planned = Collections.HashMap.create () in
      let dependency_for_key key =
        Collections.HashMap.get planned ~key:(Riot_planner.Build_unit.package_key key)
      in
      let rec loop units =
        match units with
        | [] ->
            Error (Riot_planner.Planning_error.GraphBuildFailed {
              reason = "build unit graph node not found: "
              ^ Riot_planner.Build_unit.key_to_string unit_key;
            })
        | unit :: rest ->
            let depset =
              Riot_planner.Build_unit_graph.dependencies
                unit_graph
                (Riot_planner.Build_unit.key unit)
              |> List.filter_map ~fn:dependency_for_key
            in
            (
              match plan_build_unit_raw ~workspace ~store ~unit ~depset ~build_ctx with
              | Error _ as err -> err
              | Ok result ->
                  let dependency = dependency_of_required_plan_result store result in
                  let _ =
                    Collections.HashMap.insert
                      planned
                      ~key:(Riot_planner.Build_unit.package_key (Riot_planner.Build_unit.key unit))
                      ~value:dependency
                  in
                  if
                    Riot_planner.Build_unit.equal_key (Riot_planner.Build_unit.key unit) unit_key
                  then
                    Ok result
                  else
                    loop rest
            )
      in
      loop units

let plan_build_unit_from_graph = fun ~workspace ~store ~unit_graph ~unit_key ~build_ctx ->
  plan_build_unit_from_graph_raw ~workspace ~store ~unit_graph ~unit_key ~build_ctx
  |> Result.map_err ~fn:Riot_planner.Planning_error.to_string

let describe_plan_result = fun __tmp1 ->
  match __tmp1 with
  | Riot_planner.Package_planner.Cached _ -> "Cached"
  | Riot_planner.Package_planner.Planned _ -> "Planned"

let persist_dummy_artifact = fun
  ~tmpdir ~store ~(package:Riot_model.Package.t) ~scope_name ~hash ->
  let sandbox_dir = Path.(tmpdir / Path.v ("sandbox-" ^ scope_name)) in
  let output = Path.(sandbox_dir / Path.v (scope_name ^ ".stamp")) in
  let _ =
    Fs.create_dir_all sandbox_dir
    |> Result.expect ~msg:"expected sandbox dir creation to succeed"
  in
  let _ =
    Fs.write scope_name output
    |> Result.expect ~msg:"expected sandbox marker write to succeed"
  in
  Riot_store.Store.save
    store
    ~package:(Package_name.to_string package.name)
    ~input_hash:hash
    ~sandbox_dir
    ~outs:[ output ]
  |> Result.map ~fn:(fun _artifact -> ())
  |> Result.map_err ~fn:Riot_store.Store.error_message

let test_package_input_hash_tracks_dependency_output_hash = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dependency_output_hash_test"
    (fun tmpdir ->
      let package = make_package tmpdir "app" in
      let dep_package = make_package tmpdir "dep" in
      let workspace = make_test_workspace tmpdir [ dep_package; package ] in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let make_dep = fun output_hash ->
        Riot_planner.Dependency.{
          package = dep_package;
          artifact_dir = Path.v "/cache/dep";
          depset = [];
          input_hash = Crypto.hash_string "same-dep-input";
          output_hash;
        }
      in
      let first =
        Riot_planner.Package_planner.compute_input_hash
          ~planner_version:planner_artifacts_version
          ~package
          ~depset:[ make_dep (Crypto.hash_string "dep-output-a") ]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      let second =
        Riot_planner.Package_planner.compute_input_hash
          ~planner_version:planner_artifacts_version
          ~package
          ~depset:[ make_dep (Crypto.hash_string "dep-output-b") ]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      if Crypto.Hash.equal first second then
        Error "package input hash should change when dependency output hash changes"
      else
        Ok ()) with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_planned_sandbox_files_link_dependency_native_objects = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_sandbox_files"
    (fun tmpdir ->
      let dep_dir = Path.(tmpdir / Path.v "dep") in
      let package_dir = Path.(tmpdir / Path.v "app") in
      let _ =
        Fs.create_dir_all Path.(dep_dir / Path.v "src")
        |> Result.expect ~msg:"create dep src dir failed"
      in
      let _ =
        Fs.create_dir_all Path.(package_dir / Path.v "src")
        |> Result.expect ~msg:"create app src dir failed"
      in
      let _ =
        Fs.write "void dep_stubs(void) {}\n" Path.(dep_dir / Path.v "src/dep_stubs.c")
        |> Result.expect ~msg:"write dependency native source failed"
      in
      let _ =
        Fs.write "let value = 1\n" Path.(package_dir / Path.v "src/app.ml")
        |> Result.expect ~msg:"write app source failed"
      in
      let dep =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "dep"
            |> Result.expect ~msg:"expected valid package name: dep"
          )
          ~path:dep_dir
          ~relative_path:(Path.v "dep")
          ~sources:{
            src = [];
            native = [ Path.v "src/dep_stubs.c" ];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "app"
            |> Result.expect ~msg:"expected valid package name: app"
          )
          ~path:package_dir
          ~relative_path:(Path.v "app")
          ~library:{ path = Path.v "src/app.ml" }
          ~dependencies:[ workspace_dependency "dep" ]
          ~sources:{
            src = [ Path.v "src/app.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let workspace = make_test_workspace tmpdir [ dep; package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let dep_hash = Crypto.hash_string "dep-artifact" in
      let dep_artifact_dir = Riot_store.Store.hash_dir_of store dep_hash in
      let artifact_sandbox = Path.(tmpdir / Path.v "dep-artifact-sandbox") in
      let dep_object = Path.(artifact_sandbox / Path.v "dep_stubs.o") in
      let manifest_only_object = Path.(artifact_sandbox / Path.v "transitive_extra.o") in
      let _ =
        Fs.create_dir_all artifact_sandbox
        |> Result.expect ~msg:"create dependency artifact sandbox failed"
      in
      let _ =
        Fs.write "dep object\n" dep_object
        |> Result.expect ~msg:"write dependency object failed"
      in
      let _ =
        Fs.write "manifest only object\n" manifest_only_object
        |> Result.expect ~msg:"write manifest-only object failed"
      in
      let _ =
        Riot_store.Store.save
          store
          ~package:"dep"
          ~input_hash:dep_hash
          ~sandbox_dir:artifact_sandbox
          ~outs:[ dep_object; manifest_only_object ]
        |> Result.expect ~msg:"save dependency artifact failed"
      in
      let dependency =
        Riot_planner.Dependency.{
          package = dep;
          artifact_dir = dep_artifact_dir;
          depset = [];
          input_hash = dep_hash;
          output_hash = dep_hash;
        }
      in
      let unit = library_unit package in
      match plan_build_unit ~workspace ~store ~unit ~depset:[ dependency ] ~build_ctx with
      | Error err -> Error err
      | Ok (Riot_planner.Package_planner.Cached _) -> Error "expected package to be planned"
      | Ok (Riot_planner.Package_planner.Planned { sandbox_files; _ }) ->
          let copied_app_source =
            List.any
              sandbox_files
              ~fn:(fun (file: Riot_planner.Sandbox_file.t) ->
                match file.mode with
                | Copy ->
                    Path.equal file.source Path.(package_dir / Path.v "src/app.ml")
                    && Path.equal file.destination (Path.v "src/app.ml")
                | Link
                | Reference -> false)
          in
          let linked_dep_object =
            List.any
              sandbox_files
              ~fn:(fun (file: Riot_planner.Sandbox_file.t) ->
                match file.mode with
                | Link ->
                    Path.equal file.source Path.(dep_artifact_dir / Path.v "dep_stubs.o")
                    && Path.equal file.destination (Path.v "dep_stubs.o")
                | Copy
                | Reference -> false)
          in
          let linked_manifest_only_object =
            List.any
              sandbox_files
              ~fn:(fun (file: Riot_planner.Sandbox_file.t) ->
                Path.equal
                  file.destination
                  (Path.v "transitive_extra.o"))
          in
          if not copied_app_source then
            Error "expected package source to be marked Copy"
          else if not linked_dep_object then
            Error "expected dependency native object to be marked Link"
          else if linked_manifest_only_object then
            Error "dependency artifact manifest objects should not be linked unless declared by native sources"
          else
            Ok ()) with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let source_buckets_of_files = fun files ->
  List.fold_left
    files
    ~init:Riot_model.Package.{
      src = [];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ~fn:(fun buckets (relpath, _) ->
      let path = Path.v relpath in
      if String.starts_with ~prefix:"src/" relpath then
        { buckets with src = path :: buckets.src }
      else if String.starts_with ~prefix:"tests/" relpath then
        { buckets with tests = path :: buckets.tests }
      else if String.starts_with ~prefix:"examples/" relpath then
        { buckets with examples = path :: buckets.examples }
      else if String.starts_with ~prefix:"bench/" relpath then
        { buckets with bench = path :: buckets.bench }
      else
        buckets)
  |> fun buckets ->
    {
      buckets with
      src = List.reverse buckets.src;
      tests = List.reverse buckets.tests;
      examples = List.reverse buckets.examples;
      bench = List.reverse buckets.bench;
    }

let write_package_files = fun ~package_dir files ->
  List.for_each
    files
    ~fn:(fun (relpath, content) ->
      let path = Path.(package_dir / Path.v relpath) in
      let parent =
        Path.parent path
        |> Option.unwrap_or ~default:package_dir
      in
      let _ =
        Fs.create_dir_all parent
        |> Result.expect ~msg:("create parent dir failed for " ^ relpath)
      in
      let _ =
        Fs.write content path
        |> Result.expect ~msg:("write failed for " ^ relpath)
      in
      ())

let make_package_with_files = fun ~library ~tmpdir ~package_name ~files ~binaries ->
  let package_dir = Path.(tmpdir / Path.v package_name) in
  let _ =
    Fs.create_dir_all package_dir
    |> Result.expect ~msg:"create package dir failed"
  in
  let () = write_package_files ~package_dir files in
  Riot_model.Package.make
    ~name:(
      Package_name.from_string package_name
      |> Result.expect ~msg:("expected valid package name: " ^ package_name)
    )
    ~path:package_dir
    ~relative_path:(Path.v package_name)
    ?library
    ~binaries:(List.map
      binaries
      ~fn:(fun (name, path) -> Riot_model.Package.{ name; path = Path.v path }))
    ~sources:(source_buckets_of_files files)
    ()

let find_compile_cmx = fun actions source ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | action :: rest ->
        match action with
        | Riot_planner.Action.CompileImplementation { source = compile_source; outputs; _ } when Path.equal
          compile_source
          source -> (
            match List.find
              outputs
              ~fn:(fun output -> String.ends_with ~suffix:".cmx" (Path.to_string output)) with
            | Some _ as output -> output
            | None -> loop rest
          )
        | _ -> loop rest
  in
  loop actions

let find_create_executable_named = fun action_graph name ->
  List.find
    (Riot_planner.Action_graph.to_action_list action_graph)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Riot_planner.Action.CreateExecutable { outputs; _ } ->
          List.any outputs ~fn:(fun output -> String.equal (Path.to_string output) name)
      | _ -> false)

let path_list_to_string = fun paths -> String.concat ", " (List.map paths ~fn:Path.to_string)

let binary_main = fun expression -> "let main ~args:_ =\n  " ^ expression ^ ";\n  Ok ()\n"

let plan_single_binary_source = fun ~tmpdir source_text ->
  let package =
    make_package_with_files
      ~library:None
      ~tmpdir
      ~package_name:"entry-demo"
      ~files:[ ("src/main.ml", source_text); ]
      ~binaries:[ ("entry-demo", "src/main.ml"); ]
  in
  let workspace = make_test_workspace tmpdir [ package ] in
  let store = Riot_store.Store.create ~workspace in
  let unit_graph = runtime_unit_graph workspace in
  let build_ctx =
    Riot_model.Build_ctx.make
      ~session_id:(Riot_model.Session_id.make ())
      ~profile:Riot_model.Profile.debug
      ()
  in
  match binary_unit package "entry-demo" with
  | Error err -> Error (Riot_planner.Planning_error.GraphBuildFailed { reason = err })
  | Ok unit ->
      plan_build_unit_from_graph_raw
        ~workspace
        ~store
        ~unit_graph
        ~unit_key:(Riot_planner.Build_unit.key unit)
        ~build_ctx

let has_compile_implementation_for_source = fun actions source ->
  List.any
    actions
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Riot_planner.Action.CompileImplementation { source = compile_source; _ } ->
          Path.equal compile_source source
      | _ -> false)

let combine_action_graphs = fun graphs ->
  let combined = Riot_planner.Action_graph.create () in
  List.for_each
    graphs
    ~fn:(fun graph ->
      Riot_planner.Action_graph.nodes graph
      |> List.for_each
        ~fn:(fun (node: Riot_planner.Action_node.t) ->
          ignore
            (Riot_planner.Action_graph.add_node combined (G.value node))));
  combined

let plan_dev_package_actions_with_library = fun ~library ~tmpdir ~package_name ~files ~binaries ->
  let package = make_package_with_files ~library ~tmpdir ~package_name ~files ~binaries in
  let workspace = make_test_workspace tmpdir [ package ] in
  let store = Riot_store.Store.create ~workspace in
  let unit_graph = dev_unit_graph workspace in
  let session_id = Riot_model.Session_id.make () in
  let profile = Riot_model.Profile.debug in
  let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
  let rec plan_binaries acc binaries =
    match binaries with
    | [] -> Ok (package, combine_action_graphs (List.reverse acc))
    | (binary_name, _) :: rest ->
        match binary_unit package binary_name with
        | Error _ as err -> err
        | Ok unit ->
            match plan_build_unit_from_graph
              ~workspace
              ~store
              ~unit_graph
              ~unit_key:(Riot_planner.Build_unit.key unit)
              ~build_ctx with
            | Error _ as err -> err
            | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) ->
                plan_binaries (action_graph :: acc) rest
            | Ok result ->
                Error ("expected dev binary plan to return Planned, got "
                ^ describe_plan_result result)
  in
  plan_binaries [] binaries

let plan_dev_package_actions = fun ~tmpdir ~package_name ~files ~binaries ->
  plan_dev_package_actions_with_library
    ~library:None
    ~tmpdir
    ~package_name
    ~files
    ~binaries

let plan_runtime_package_actions = fun ~tmpdir ~package_name ~files ~binaries ->
  let package = make_package_with_files ~library:None ~tmpdir ~package_name ~files ~binaries in
  let workspace = make_test_workspace tmpdir [ package ] in
  let store = Riot_store.Store.create ~workspace in
  let unit_graph = runtime_unit_graph workspace in
  let session_id = Riot_model.Session_id.make () in
  let profile = Riot_model.Profile.debug in
  let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
  let runtime_binaries =
    List.filter
      binaries
      ~fn:(fun (_, path) ->
        not
          (String.starts_with ~prefix:"tests/" path
          || String.starts_with ~prefix:"examples/" path
          || String.starts_with ~prefix:"bench/" path))
  in
  let rec plan_binaries acc binaries =
    match binaries with
    | [] -> Ok (package, combine_action_graphs (List.reverse acc))
    | (binary_name, _) :: rest ->
        match binary_unit package binary_name with
        | Error _ as err -> err
        | Ok unit ->
            match plan_build_unit_from_graph
              ~workspace
              ~store
              ~unit_graph
              ~unit_key:(Riot_planner.Build_unit.key unit)
              ~build_ctx with
            | Error _ as err -> err
            | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) ->
                plan_binaries (action_graph :: acc) rest
            | Ok result ->
                Error ("expected runtime binary plan to return Planned, got "
                ^ describe_plan_result result)
  in
  match package.library with
  | Some _ ->
      let unit = library_unit package in
      (
        match plan_build_unit_from_graph
          ~workspace
          ~store
          ~unit_graph
          ~unit_key:(Riot_planner.Build_unit.key unit)
          ~build_ctx with
        | Error _ as err -> err
        | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) -> (
            match plan_binaries [ action_graph ] runtime_binaries with
            | Ok _ as ok -> ok
            | Error _ as err -> err
          )
        | Ok result ->
            Error ("expected runtime library plan to return Planned, got "
            ^ describe_plan_result result)
      )
  | None -> plan_binaries [] runtime_binaries

let module_node_label = fun (node: Riot_planner.Module_node.t G.node) ->
  match (G.value node).kind with
  | Riot_planner.Module_node.ML mod_ -> "ML(" ^ Riot_model.Module.namespaced_name mod_ ^ ")"
  | Riot_planner.Module_node.MLI mod_ -> "MLI(" ^ Riot_model.Module.namespaced_name mod_ ^ ")"
  | Riot_planner.Module_node.Library { name; _ } -> "Library(" ^ name ^ ")"
  | Riot_planner.Module_node.Binary { name; _ } -> "Binary(" ^ name ^ ")"
  | Riot_planner.Module_node.Native { files } ->
      "Native(" ^ String.concat ", " (List.map files ~fn:Path.to_string) ^ ")"
  | Riot_planner.Module_node.PackageDependency { root_module; _ } ->
      "PackageDependency(" ^ root_module ^ ")"
  | Riot_planner.Module_node.C ->
      "C(" ^ Riot_planner.Module_node.file_to_string (G.value node).file ^ ")"
  | Riot_planner.Module_node.H ->
      "H(" ^ Riot_planner.Module_node.file_to_string (G.value node).file ^ ")"
  | Riot_planner.Module_node.Root -> "Root"
  | Riot_planner.Module_node.Other value -> "Other(" ^ value ^ ")"

let module_dependency_labels = fun graph (node: Riot_planner.Module_node.t G.node) ->
  List.filter_map
    (G.deps node)
    ~fn:(fun dep_id ->
      G.get_node graph dep_id
      |> Option.map ~fn:module_node_label)

let find_module_node_by_label = fun graph label ->
  match G.topo_sort graph with
  | Ok nodes ->
      List.find
        nodes
        ~fn:(fun (node: Riot_planner.Module_node.t G.node) ->
          String.equal
            (module_node_label node)
            label)
  | Error _ -> None

let find_library_node = fun graph ->
  match G.topo_sort graph with
  | Ok nodes ->
      List.find
        nodes
        ~fn:(fun (node: Riot_planner.Module_node.t G.node) ->
          match (G.value node).kind with
          | Riot_planner.Module_node.Library _ -> true
          | _ -> false)
  | Error _ -> None

let find_create_library_objects = fun action_graph ->
  match List.find
    (Riot_planner.Action_graph.to_action_list action_graph)
    ~fn:(fun action ->
      match action with
      | Riot_planner.Action.CreateLibrary _ -> true
      | _ -> false) with
  | Some (Riot_planner.Action.CreateLibrary { objects; _ }) ->
      Ok (List.map objects ~fn:Path.to_string)
  | Some _ -> Error "expected CreateLibrary action"
  | None -> Error "missing CreateLibrary action"

let find_create_library_node = fun action_graph ->
  List.find
    (Riot_planner.Action_graph.nodes action_graph)
    ~fn:(fun (node: Riot_planner.Action_node.t) ->
      List.any
        (G.value node).actions
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Riot_planner.Action.CreateLibrary _ -> true
          | _ -> false))

let find_action_node_by_source = fun action_graph source ->
  List.find
    (Riot_planner.Action_graph.nodes action_graph)
    ~fn:(fun (node: Riot_planner.Action_node.t) ->
      List.any
        (G.value node).actions
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Riot_planner.Action.CompileInterface { source = action_source; _ }
          | Riot_planner.Action.CompileImplementation { source = action_source; _ } ->
              Path.equal action_source source
          | _ -> false))

let dependency_output_names = fun action_graph (node: Riot_planner.Action_node.t) ->
  List.filter_map
    (G.deps node)
    ~fn:(fun dep_id ->
      match G.get_node (Riot_planner.Action_graph.graph action_graph) dep_id with
      | Some dep_node ->
          List.head (G.value dep_node).outs
          |> Option.map ~fn:Path.to_string
      | None -> None)

let dependency_output_names_flat = fun action_graph (node: Riot_planner.Action_node.t) ->
  List.flat_map
    (G.deps node)
    ~fn:(fun dep_id ->
      match G.get_node (Riot_planner.Action_graph.graph action_graph) dep_id with
      | Some dep_node -> List.map (G.value dep_node).outs ~fn:Path.to_string
      | None -> [])

let require_order = fun items ~before ~after ->
  let rec find_index needle index items =
    match items with
    | [] -> None
    | item :: rest ->
        if String.equal item needle then
          Some index
        else
          find_index needle (index + 1) rest
  in
  match (find_index before 0 items, find_index after 0 items) with
  | (Some before_index, Some after_index) ->
      if before_index < after_index then
        Ok ()
      else
        Error ("expected " ^ before ^ " before " ^ after ^ " in [" ^ String.concat ", " items ^ "]")
  | (None, _) -> Error ("missing " ^ before ^ " in [" ^ String.concat ", " items ^ "]")
  | (_, None) -> Error ("missing " ^ after ^ " in [" ^ String.concat ", " items ^ "]")

let object_name_of_module_node = fun (node: Riot_planner.Module_node.t G.node) ->
  match (G.value node).kind with
  | Riot_planner.Module_node.ML mod_ -> Some (Path.to_string (Riot_model.Module.cmx mod_))
  | Riot_planner.Module_node.MLI _
  | Riot_planner.Module_node.Library _
  | Riot_planner.Module_node.Binary _
  | Riot_planner.Module_node.Native _
  | Riot_planner.Module_node.PackageDependency _
  | Riot_planner.Module_node.C
  | Riot_planner.Module_node.H
  | Riot_planner.Module_node.Root
  | Riot_planner.Module_node.Other _ -> None

let validate_create_library_topological_order = fun graph objects ->
  let object_position needle =
    List.find (List.enumerate objects) ~fn:(fun (_, object_) -> String.equal object_ needle)
    |> Option.map ~fn:(fun (index, _) -> index)
  in
  match G.topo_sort graph with
  | Error cycle_ids ->
      Error ("module graph unexpectedly contains cycle: "
      ^ String.concat ", " (List.map cycle_ids ~fn:G.Node_id.to_string))
  | Ok nodes ->
      let violations =
        List.filter_map
          nodes
          ~fn:(fun (node: Riot_planner.Module_node.t G.node) ->
            match object_name_of_module_node node with
            | None -> None
            | Some node_object ->
                match object_position node_object with
                | None -> None
                | Some node_index ->
                    let bad_dependencies =
                      List.filter_map
                        (G.deps node)
                        ~fn:(fun dep_id ->
                          match G.get_node graph dep_id with
                          | None -> None
                          | Some dep_node ->
                              match object_name_of_module_node dep_node with
                              | None -> None
                              | Some dep_object ->
                                  match object_position dep_object with
                                  | Some dep_index when dep_index < node_index -> None
                                  | Some dep_index ->
                                      Some (dep_object
                                      ^ "@"
                                      ^ Int.to_string dep_index
                                      ^ " after "
                                      ^ node_object
                                      ^ "@"
                                      ^ Int.to_string node_index)
                                  | None -> Some (dep_object ^ " missing for " ^ node_object))
                    in
                    if List.is_empty bad_dependencies then
                      None
                    else
                      Some (node_object, bad_dependencies))
      in
      match violations with
      | [] -> Ok ()
      | (node_object, bad_dependencies) :: _ ->
          Error ("CreateLibrary object order violates module dependency order for "
          ^ node_object
          ^ ": "
          ^ String.concat "; " bad_dependencies)

let move_item_to_front = fun needle items ->
  let (matches, rest) =
    List.fold_right
      items
      ~init:([], [])
      ~fn:(fun item (matches, rest) ->
        if String.equal item needle then
          (item :: matches, rest)
        else
          (matches, item :: rest))
  in
  matches @ rest

let rewrite_create_library_objects_json = fun json ~rewrite ->
  let open Std.Data.Json in
  let rewrite_action action_json =
    match action_json with
    | Object _ -> (
        match (
          get_field "type" action_json,
          get_field "outputs" action_json,
          get_field "objects" action_json,
          get_field "includes" action_json
        ) with
        | (Some (String "CreateLibrary"), Some outputs, Some (Array object_jsons), Some includes) ->
            let objects =
              List.filter_map
                object_jsons
                ~fn:(fun __tmp1 ->
                  match __tmp1 with
                  | String path -> Some path
                  | _ -> None)
            in
            Object [
              ("type", String "CreateLibrary");
              ("outputs", outputs);
              ("objects", Array (List.map (rewrite objects) ~fn:(fun path -> String path)));
              ("includes", includes);
            ]
        | _ -> action_json
      )
    | _ -> action_json
  in
  let rewrite_node node_json =
    match node_json with
    | Object _ -> (
        match (
          get_field "id" node_json,
          get_field "actions" node_json,
          get_field "outputs" node_json,
          get_field "sources" node_json,
          get_field "package" node_json,
          get_field "package_path" node_json,
          get_field "package_relative_path" node_json,
          get_field "hash" node_json,
          get_field "dependencies" node_json
        ) with
        | (
            Some id,
            Some (Array actions),
            Some outputs,
            Some sources,
            Some package,
            Some package_path,
            Some package_relative_path,
            Some hash,
            Some dependencies
          ) ->
            Object [
              ("id", id);
              ("actions", Array (List.map actions ~fn:rewrite_action));
              ("outputs", outputs);
              ("sources", sources);
              ("package", package);
              ("package_path", package_path);
              ("package_relative_path", package_relative_path);
              ("hash", hash);
              ("dependencies", dependencies);
            ]
        | _ -> node_json
      )
    | _ -> node_json
  in
  match json with
  | Object _ -> (
      match get_field "nodes" json with
      | Some (Array nodes) -> Object [ ("nodes", Array (List.map nodes ~fn:rewrite_node)); ]
      | _ -> json
    )
  | _ -> json

let rewrite_plan_bundle_action_graph = fun bundle ~rewrite ->
  let open Std.Data.Json in
  match bundle with
  | Object _ -> (
      match (
        get_field "version" bundle,
        get_field "package" bundle,
        get_field "module_graph" bundle,
        get_field "action_graph" bundle
      ) with
      | (Some version, Some package, Some module_graph, Some action_graph) ->
          Object [
            ("version", version);
            ("package", package);
            ("module_graph", module_graph);
            ("action_graph", rewrite_create_library_objects_json action_graph ~rewrite);
          ]
      | _ -> bundle
    )
  | _ -> bundle

let render_module_graph_dependency_walk = fun graph ->
  let render_dependencies deps =
    match deps with
    | [] -> "  deps: []"
    | _ -> "  deps:\n" ^ String.concat "\n" (List.map deps ~fn:(fun dep -> "    - " ^ dep))
  in
  match G.topo_sort graph with
  | Error cycle_ids ->
      "cycle: " ^ String.concat ", " (List.map cycle_ids ~fn:G.Node_id.to_string) ^ "\n"
  | Ok nodes ->
      nodes
      |> List.map
        ~fn:(fun (node: Riot_planner.Module_node.t G.node) ->
          module_node_label node ^ "\n" ^ render_dependencies (module_dependency_labels graph node))
      |> String.concat "\n\n"

let load_repo_workspace = fun () ->
  let manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan manager (Path.v ".") with
  | Error err ->
      Error ("workspace scan failed: " ^ Riot_model.Workspace_manager.scan_error_message err)
  | Ok (workspace_manifest, errors) ->
      if List.is_empty errors then
        Ok (Riot_model.Workspace.make_realized
          ?name:workspace_manifest.name
          ~root:workspace_manifest.root
          ~packages:(Riot_model.Workspace_manifest.realize_packages
            ~intent:Riot_model.Package.Dev
            workspace_manifest)
          ~dependencies:workspace_manifest.dependencies
          ~dev_dependencies:workspace_manifest.dev_dependencies
          ~build_dependencies:workspace_manifest.build_dependencies
          ~profile_overrides:workspace_manifest.profile_overrides
          ~target_dir:workspace_manifest.target_dir_root
          ())
      else
        Error ("workspace scan produced load errors: "
        ^ String.concat "; " (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string))

let test_kernel_input_hash_is_not_empty_digest = fun _ctx ->
  match load_repo_workspace () with
  | Error _ as err -> err
  | Ok workspace ->
      match find_package_by_name workspace "kernel" with
      | None -> Error "kernel package not found in workspace"
      | Some package ->
          let session_id = Riot_model.Session_id.make () in
          let profile = Riot_model.Profile.debug in
          let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
          let hash =
            Riot_planner.Package_planner.compute_input_hash
              ~package
              ~depset:[]
              ~workspace
              ~profile
              ~build_ctx
              ~toolchain:test_toolchain
              ()
          in
          let empty_hash = Crypto.Sha256.hash_string "" in
          if Std.Crypto.Hash.compare hash empty_hash = Std.Order.EQ then
            Error ("expected kernel input hash to differ from empty digest, got "
            ^ Std.Crypto.Digest.hex hash)
          else
            Ok ()

let test_dev_scope_test_binaries_include_private_helpers = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dev_scope_tests_helpers"
    (fun tmpdir ->
      match plan_dev_package_actions
        ~tmpdir
        ~package_name:"devscope-tests-demo"
        ~binaries:[ ("foo_tests", "tests/foo_tests.ml"); ]
        ~files:[
          ("tests/helper.ml", "let value = 1\n");
          ("tests/foo_tests.ml", binary_main "ignore Helper.value");
        ] with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          let actions = Riot_planner.Action_graph.to_action_list action_graph in
          let helper_source = Path.v "tests/helper.ml" in
          let test_source = Path.v "tests/foo_tests.ml" in
          let helper_cmx = find_compile_cmx actions helper_source in
          let test_cmx = find_compile_cmx actions test_source in
          match (find_create_executable_named action_graph "foo_tests", helper_cmx, test_cmx) with
          | (
              Some (Riot_planner.Action.CreateExecutable { objects; _ }),
              Some helper_cmx,
              Some test_cmx
            ) ->
              let has object_ = List.any objects ~fn:(Path.equal object_) in
              if not (has helper_cmx && has test_cmx) then
                Error "expected test executable to link both helper and root objects"
              else
                Ok ()
          | _ ->
              Error "expected CreateExecutable action and compile outputs for tests/foo_tests.ml and tests/helper.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dev_scope_no_library_tests_include_package_named_helpers = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dev_scope_no_library_package_helpers"
    (fun tmpdir ->
      match plan_dev_package_actions
        ~tmpdir
        ~package_name:"hello-world"
        ~binaries:[ ("hello_world_tests", "tests/hello_world_tests.ml"); ]
        ~files:[
          ("src/main.ml", binary_main "ignore (Hello_world.hello ())");
          ("src/hello_world.mli", "val hello : unit -> string\n");
          ("src/hello_world.ml", "let hello () = \"Hello from hello-world\"\n");
          ("tests/hello_world_tests.ml", binary_main "ignore (Hello_world.hello ())");
        ] with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          let actions = Riot_planner.Action_graph.to_action_list action_graph in
          let helper_source = Path.v "src/hello_world.ml" in
          let test_source = Path.v "tests/hello_world_tests.ml" in
          let helper_cmx = find_compile_cmx actions helper_source in
          let test_cmx = find_compile_cmx actions test_source in
          match (
            find_create_executable_named action_graph "hello_world_tests",
            helper_cmx,
            test_cmx
          ) with
          | (
              Some (Riot_planner.Action.CreateExecutable { objects; libraries; _ }),
              Some helper_cmx,
              Some test_cmx
            ) ->
              let has object_ = List.any objects ~fn:(Path.equal object_) in
              if not (has helper_cmx && has test_cmx) then
                Error ("expected test executable to link package-named helper and test root objects; objects: "
                ^ path_list_to_string objects)
              else if
                List.any
                  libraries
                  ~fn:(fun library ->
                    String.ends_with
                      ~suffix:"Hello_world.cmxa"
                      (Path.to_string library))
              then
                Error ("did not expect no-library test executable to link missing package archive; libraries: "
                ^ path_list_to_string libraries)
              else
                Ok ()
          | _ ->
              Error "expected CreateExecutable action and compile outputs for tests/hello_world_tests.ml and src/hello_world.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dev_scope_tests_can_import_own_runtime_library_root = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dev_scope_own_runtime_root"
    (fun tmpdir ->
      match plan_dev_package_actions_with_library
        ~tmpdir
        ~package_name:"self-lib"
        ~library:(Some Riot_model.Package.{ path = Path.v "src/self_lib.ml" })
        ~binaries:[ ("self_lib_tests", "tests/self_lib_tests.ml"); ]
        ~files:[
          ("src/self_lib.ml", "let value = 1\n");
          ("tests/self_lib_tests.ml", binary_main "ignore Self_lib.value");
        ] with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          match find_create_executable_named action_graph "self_lib_tests" with
          | Some (Riot_planner.Action.CreateExecutable { libraries; _ }) ->
              if
                List.any
                  libraries
                  ~fn:(fun library ->
                    String.ends_with
                      ~suffix:"Self_lib.cmxa"
                      (Path.to_string library))
              then
                Ok ()
              else
                Error ("expected dev test executable to link its runtime package library; libraries: "
                ^ path_list_to_string libraries)
          | _ -> Error "expected CreateExecutable action for tests/self_lib_tests.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dev_scope_example_can_use_opened_own_runtime_library_exports = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dev_scope_open_own_runtime_exports"
    (fun tmpdir ->
      match plan_dev_package_actions_with_library
        ~tmpdir
        ~package_name:"stdish"
        ~library:(Some Riot_model.Package.{ path = Path.v "src/stdish.ml" })
        ~binaries:[ ("demo", "examples/demo.ml"); ]
        ~files:[
          ("src/stdish.ml", "module Env = struct\n  let args = []\nend\n");
          ("examples/demo.ml", "open Stdish\nlet main ~args:_ =\n  ignore Env.args;\n  Ok ()\n");
        ] with
      | Error err -> Error ("expected opened own runtime library exports to resolve, got: " ^ err)
      | Ok (_package, action_graph) ->
          match find_create_executable_named action_graph "demo" with
          | Some (Riot_planner.Action.CreateExecutable { libraries; _ }) ->
              if
                List.any
                  libraries
                  ~fn:(fun library ->
                    String.ends_with
                      ~suffix:"Stdish.cmxa"
                      (Path.to_string library))
              then
                Ok ()
              else
                Error ("expected opened self-library example to link Stdish.cmxa; libraries: "
                ^ path_list_to_string libraries)
          | _ -> Error "expected CreateExecutable action for examples/demo.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dev_scope_multiple_binaries_keep_opened_own_runtime_library_exports = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dev_scope_open_own_runtime_exports_multiple"
    (fun tmpdir ->
      match plan_dev_package_actions_with_library
        ~tmpdir
        ~package_name:"stdish"
        ~library:(Some Riot_model.Package.{ path = Path.v "src/stdish.ml" })
        ~binaries:[ ("aaa_tests", "tests/aaa_tests.ml"); ("zzz_tests", "tests/zzz_tests.ml"); ]
        ~files:[
          ("src/stdish.ml", "module Agent = struct\n  let value = 1\nend\n");
          (
            "tests/aaa_tests.ml",
            "open Stdish\nlet main ~args:_ =\n  ignore Agent.value;\n  Ok ()\n"
          );
          (
            "tests/zzz_tests.ml",
            "open Stdish\nlet main ~args:_ =\n  ignore Agent.value;\n  Ok ()\n"
          );
        ] with
      | Error err ->
          Error ("expected both dev binaries to resolve opened own runtime exports, got: " ^ err)
      | Ok (_package, action_graph) ->
          if Option.is_none (find_create_executable_named action_graph "aaa_tests") then
            Error "expected CreateExecutable action for tests/aaa_tests.ml"
          else if Option.is_none (find_create_executable_named action_graph "zzz_tests") then
            Error "expected CreateExecutable action for tests/zzz_tests.ml"
          else
            Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dev_scope_record_local_open_keeps_opened_runtime_exports = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dev_scope_record_local_open_runtime_exports"
    (fun tmpdir ->
      match plan_dev_package_actions_with_library
        ~tmpdir
        ~package_name:"kernelish"
        ~library:(Some Riot_model.Package.{ path = Path.v "src/kernelish.ml" })
        ~binaries:[ ("process_tests", "tests/process_tests.ml"); ]
        ~files:[
          (
            "src/kernelish.ml",
            {ocaml|module Process = struct
  module Stdin = struct type t = Null end
  module Stdout = struct type t = Null end
  module Stderr = struct type t = Null end
end
|ocaml}
          );
          (
            "tests/process_tests.ml",
            {ocaml|open Kernelish

let main ~args:_ =
  let _stdio =
    Kernelish.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null }
  in
  Ok ()
|ocaml}
          );
        ] with
      | Error err ->
          Error ("expected record local-open exports to resolve through Kernelish, got: " ^ err)
      | Ok (_package, action_graph) ->
          if Option.is_some (find_create_executable_named action_graph "process_tests") then
            Ok ()
          else
            Error "expected CreateExecutable action for tests/process_tests.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_direct_dependency_exports_prefer_direct_depset_package = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_direct_dependency_exports_prefer_direct"
    (fun tmpdir ->
      let stdish =
        make_package_with_files
          ~library:(Some Riot_model.Package.{ path = Path.v "src/stdish.ml" })
          ~tmpdir
          ~package_name:"stdish"
          ~files:[ ("src/stdish.ml", "module Env = struct\n  let args = []\nend\n"); ]
          ~binaries:[]
      in
      let stale_stdish =
        make_package_with_files
          ~library:(Some Riot_model.Package.{ path = Path.v "src/stdish.ml" })
          ~tmpdir
          ~package_name:"stale-stdish"
          ~files:[ ("src/stdish.ml", "let unrelated = ()\n"); ]
          ~binaries:[]
      in
      let stale_stdish =
        Riot_model.Package.make
          ~name:stdish.name
          ~path:stale_stdish.path
          ~relative_path:(Path.v "stale-stdish")
          ~library:Riot_model.Package.{ path = Path.v "src/stdish.ml" }
          ~sources:stale_stdish.sources
          ()
      in
      let colors =
        make_package_with_files
          ~library:(Some Riot_model.Package.{ path = Path.v "src/colors.ml" })
          ~tmpdir
          ~package_name:"colors"
          ~files:[ ("src/colors.ml", "let marker = ()\n"); ]
          ~binaries:[]
      in
      let app_dir = Path.(tmpdir / Path.v "app") in
      let () =
        Fs.create_dir_all app_dir
        |> Result.expect ~msg:"create app package dir failed"
      in
      let () =
        write_package_files
          ~package_dir:app_dir
          [
            ("tests/app_tests.ml", "open Stdish\nlet main ~args:_ =\n  ignore Env.args;\n  Ok ()\n");
          ]
      in
      let app =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "app"
            |> Result.expect ~msg:"expected valid package name: app"
          )
          ~path:app_dir
          ~relative_path:(Path.v "app")
          ~dependencies:[ workspace_dependency "colors"; workspace_dependency "stdish"; ]
          ~binaries:[
            Riot_model.Package.{ name = "app_tests"; path = Path.v "tests/app_tests.ml" };
          ]
          ~sources:(source_buckets_of_files
            [
              (
                "tests/app_tests.ml",
                "open Stdish\nlet main ~args:_ =\n  ignore Env.args;\n  Ok ()\n"
              );
            ])
          ()
      in
      let workspace = make_test_workspace tmpdir [ stdish; colors; app ] in
      let store = Riot_store.Store.create ~workspace in
      let build_ctx =
        Riot_model.Build_ctx.make
          ~session_id:(Riot_model.Session_id.make ())
          ~profile:Riot_model.Profile.debug
          ()
      in
      let dependency package ~name ?(depset = []) () =
        Riot_planner.Dependency.{
          package;
          artifact_dir = Path.(tmpdir / Path.v ("artifact-" ^ name));
          depset;
          input_hash = Crypto.hash_string ("input:" ^ name);
          output_hash = Crypto.hash_string ("output:" ^ name);
        }
      in
      let stale_stdish_dependency = dependency stale_stdish ~name:"stale-stdish" () in
      let colors_dependency =
        dependency colors ~name:"colors" ~depset:[ stale_stdish_dependency ] ()
      in
      let stdish_dependency = dependency stdish ~name:"stdish" () in
      match binary_unit app "app_tests" with
      | Error err -> Error err
      | Ok unit ->
          match plan_build_unit
            ~workspace
            ~store
            ~unit
            ~depset:[ colors_dependency; stdish_dependency ]
            ~build_ctx with
          | Ok (Riot_planner.Package_planner.Planned _) -> Ok ()
          | Ok result ->
              Error ("expected direct dependency exports to plan, got "
              ^ describe_plan_result result)
          | Error err ->
              Error ("expected direct dependency exports to prefer direct depset package, got: "
              ^ err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dev_scope_example_binaries_include_private_helpers = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dev_scope_examples_helpers"
    (fun tmpdir ->
      match plan_dev_package_actions
        ~tmpdir
        ~package_name:"devscope-examples-demo"
        ~binaries:[ ("demo", "examples/demo.ml"); ]
        ~files:[
          ("examples/helper.ml", "let value = 1\n");
          ("examples/demo.ml", binary_main "ignore Helper.value");
        ] with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          let actions = Riot_planner.Action_graph.to_action_list action_graph in
          let helper_source = Path.v "examples/helper.ml" in
          let example_source = Path.v "examples/demo.ml" in
          let helper_cmx = find_compile_cmx actions helper_source in
          let example_cmx = find_compile_cmx actions example_source in
          match (find_create_executable_named action_graph "demo", helper_cmx, example_cmx) with
          | (
              Some (Riot_planner.Action.CreateExecutable { objects; _ }),
              Some helper_cmx,
              Some example_cmx
            ) ->
              let has object_ = List.any objects ~fn:(Path.equal object_) in
              if not (has helper_cmx && has example_cmx) then
                Error "expected example executable to link both helper and root objects"
              else
                Ok ()
          | _ ->
              Error "expected CreateExecutable action and compile outputs for examples/demo.ml and examples/helper.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dev_scope_bench_binaries_include_private_helpers = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dev_scope_bench_helpers"
    (fun tmpdir ->
      match plan_dev_package_actions
        ~tmpdir
        ~package_name:"devscope-bench-demo"
        ~binaries:[ ("foo_bench", "bench/foo_bench.ml"); ]
        ~files:[
          ("bench/helper.ml", "let value = 1\n");
          ("bench/foo_bench.ml", binary_main "ignore Helper.value");
        ] with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          let actions = Riot_planner.Action_graph.to_action_list action_graph in
          let helper_source = Path.v "bench/helper.ml" in
          let bench_source = Path.v "bench/foo_bench.ml" in
          let helper_cmx = find_compile_cmx actions helper_source in
          let bench_cmx = find_compile_cmx actions bench_source in
          match (find_create_executable_named action_graph "foo_bench", helper_cmx, bench_cmx) with
          | (
              Some (Riot_planner.Action.CreateExecutable { objects; _ }),
              Some helper_cmx,
              Some bench_cmx
            ) ->
              let has object_ = List.any objects ~fn:(Path.equal object_) in
              if not (has helper_cmx && has bench_cmx) then
                Error "expected bench executable to link both helper and root objects"
              else
                Ok ()
          | _ ->
              Error "expected CreateExecutable action and compile outputs for bench/foo_bench.ml and bench/helper.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_dev_scope_keeps_private_helpers_separated_by_root = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_dev_scope_multiple_roots"
    (fun tmpdir ->
      match plan_dev_package_actions
        ~tmpdir
        ~package_name:"devscope-multi-root-demo"
        ~binaries:[
          ("foo_tests", "tests/foo_tests.ml");
          ("demo", "examples/demo.ml");
          ("foo_bench", "bench/foo_bench.ml");
        ]
        ~files:[
          ("tests/test_helper.ml", "let value = 1\n");
          ("tests/foo_tests.ml", binary_main "ignore Test_helper.value");
          ("examples/example_helper.ml", "let value = 2\n");
          ("examples/demo.ml", binary_main "ignore Example_helper.value");
          ("bench/bench_helper.ml", "let value = 3\n");
          ("bench/foo_bench.ml", binary_main "ignore Bench_helper.value");
        ] with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          let actions = Riot_planner.Action_graph.to_action_list action_graph in
          let test_helper = find_compile_cmx actions (Path.v "tests/test_helper.ml") in
          let test_root = find_compile_cmx actions (Path.v "tests/foo_tests.ml") in
          let example_helper = find_compile_cmx actions (Path.v "examples/example_helper.ml") in
          let example_root = find_compile_cmx actions (Path.v "examples/demo.ml") in
          let bench_helper = find_compile_cmx actions (Path.v "bench/bench_helper.ml") in
          let bench_root = find_compile_cmx actions (Path.v "bench/foo_bench.ml") in
          match (
            find_create_executable_named action_graph "foo_tests",
            find_create_executable_named action_graph "demo",
            find_create_executable_named action_graph "foo_bench",
            test_helper,
            test_root,
            example_helper,
            example_root,
            bench_helper,
            bench_root
          ) with
          | (
              Some (Riot_planner.Action.CreateExecutable { objects = test_objects; _ }),
              Some (Riot_planner.Action.CreateExecutable { objects = example_objects; _ }),
              Some (Riot_planner.Action.CreateExecutable { objects = bench_objects; _ }),
              Some test_helper,
              Some test_root,
              Some example_helper,
              Some example_root,
              Some bench_helper,
              Some bench_root
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              if not (has test_helper test_objects && has test_root test_objects) then
                Error "expected test executable to link only its own helper closure"
              else if has example_helper test_objects || has bench_helper test_objects then
                Error "did not expect test executable to link example or bench helpers"
              else if
                not (has example_helper example_objects && has example_root example_objects)
              then
                Error "expected example executable to link only its own helper closure"
              else if has test_helper example_objects || has bench_helper example_objects then
                Error "did not expect example executable to link test or bench helpers"
              else if not (has bench_helper bench_objects && has bench_root bench_objects) then
                Error "expected bench executable to link only its own helper closure"
              else if has test_helper bench_objects || has example_helper bench_objects then
                Error "did not expect bench executable to link test or example helpers"
              else
                Ok ()
          | _ -> Error "expected CreateExecutable actions and compile outputs for all dev roots") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_runtime_scope_excludes_dev_only_roots = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_runtime_scope_ignores_dev_roots"
    (fun tmpdir ->
      match plan_runtime_package_actions
        ~tmpdir
        ~package_name:"runtime-scope-demo"
        ~binaries:[ ("foo_tests", "tests/foo_tests.ml"); ]
        ~files:[
          ("src/runtime_scope_demo.ml", "module Public = struct end\n");
          ("tests/helper.ml", "let value = 1\n");
          ("tests/foo_tests.ml", "let () = ignore Helper.value\n");
          ("examples/example_helper.ml", "let value = 2\n");
          ("examples/demo.ml", "let () = ignore Example_helper.value\n");
          ("bench/bench_helper.ml", "let value = 3\n");
          ("bench/foo_bench.ml", "let () = ignore Bench_helper.value\n");
        ] with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          let actions = Riot_planner.Action_graph.to_action_list action_graph in
          if has_compile_implementation_for_source actions (Path.v "tests/helper.ml") then
            Error "did not expect runtime scope to compile tests/helper.ml"
          else if has_compile_implementation_for_source actions (Path.v "tests/foo_tests.ml") then
            Error "did not expect runtime scope to compile tests/foo_tests.ml"
          else if
            has_compile_implementation_for_source actions (Path.v "examples/example_helper.ml")
          then
            Error "did not expect runtime scope to compile examples/example_helper.ml"
          else if has_compile_implementation_for_source actions (Path.v "examples/demo.ml") then
            Error "did not expect runtime scope to compile examples/demo.ml"
          else if
            has_compile_implementation_for_source actions (Path.v "bench/bench_helper.ml")
          then
            Error "did not expect runtime scope to compile bench/bench_helper.ml"
          else if has_compile_implementation_for_source actions (Path.v "bench/foo_bench.ml") then
            Error "did not expect runtime scope to compile bench/foo_bench.ml"
          else if Option.is_some (find_create_executable_named action_graph "foo_tests") then
            Error "did not expect runtime scope to plan test executables"
          else if Option.is_some (find_create_executable_named action_graph "demo") then
            Error "did not expect runtime scope to plan example executables"
          else if Option.is_some (find_create_executable_named action_graph "foo_bench") then
            Error "did not expect runtime scope to plan bench executables"
          else
            Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_build_scope_excludes_runtime_and_dev_roots = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_build_scope_excludes_sources"
    (fun tmpdir ->
      let build_helper =
        make_package_with_files
          ~library:None
          ~tmpdir
          ~package_name:"build-helper"
          ~binaries:[]
          ~files:[]
      in
      let package_dir = Path.(tmpdir / Path.v "build-scope-demo") in
      let _ =
        Fs.create_dir_all package_dir
        |> Result.expect ~msg:"create package dir failed"
      in
      let files = [
        ("src/build_scope_demo.ml", "module Public = struct end\n");
        ("tests/helper.ml", "let value = 1\n");
        ("tests/foo_tests.ml", "let () = ignore Helper.value\n");
        ("examples/example_helper.ml", "let value = 2\n");
        ("examples/demo.ml", "let () = ignore Example_helper.value\n");
        ("bench/bench_helper.ml", "let value = 3\n");
        ("bench/foo_bench.ml", "let () = ignore Bench_helper.value\n");
      ]
      in
      let () = write_package_files ~package_dir files in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "build-scope-demo"
            |> Result.expect ~msg:"expected valid package name: build-scope-demo"
          )
          ~path:package_dir
          ~relative_path:(Path.v "build-scope-demo")
          ~build_dependencies:[ workspace_dependency "build-helper" ]
          ~binaries:[
            Riot_model.Package.{ name = "foo_tests"; path = Path.v "tests/foo_tests.ml" };
          ]
          ~sources:(source_buckets_of_files files)
          ()
      in
      let workspace = make_test_workspace tmpdir [ build_helper; package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let helper_hash = Crypto.hash_string "build-helper" in
      let helper_dependency =
        Riot_planner.Dependency.{
          package = Riot_model.Package.for_scope Riot_model.Package.Normal build_helper;
          artifact_dir = Path.(tmpdir / Path.v "cache-build-helper");
          depset = [];
          input_hash = helper_hash;
          output_hash = helper_hash;
        }
      in
      let build_package = Riot_model.Package.for_scope Riot_model.Package.Build package in
      let unit_key =
        build_unit_key
          build_package
          (Riot_planner.Build_unit.SyntheticTool { name = "build-scope-demo" })
      in
      let unit =
        Riot_planner.Build_unit.from_artifact
          ~package:build_package
          ~artifact:unit_key.artifact
          ~target:unit_key.target
          ~profile:unit_key.profile
      in
      match plan_build_unit ~workspace ~store ~unit ~depset:[ helper_dependency ] ~build_ctx with
      | Error _ as err -> err
      | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) ->
          let actions = Riot_planner.Action_graph.to_action_list action_graph in
          if List.any
            actions
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Riot_planner.Action.CompileInterface _
              | Riot_planner.Action.CompileImplementation _ -> true
              | _ -> false) then
            Error "did not expect build scope to compile runtime or dev source roots"
          else if List.any
            actions
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Riot_planner.Action.CreateLibrary _
              | Riot_planner.Action.CreateExecutable _ -> true
              | _ -> false) then
            Error "did not expect build scope to archive or link projected package sources"
          else
            Ok ()
      | Ok result ->
          Error ("expected build package plan to return Planned, got " ^ describe_plan_result result)) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_entrypoint_accepts_single_labeled_args_main = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_main_valid"
    (fun tmpdir ->
      match plan_single_binary_source ~tmpdir "let main ~args:_ = Ok ()\n" with
      | Ok (Riot_planner.Package_planner.Planned _) -> Ok ()
      | Ok result -> Error ("expected package to be planned, got " ^ describe_plan_result result)
      | Error err ->
          Error ("expected valid executable main, got " ^ Riot_planner.Planning_error.to_string err)) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_entrypoint_rejects_fun_labeled_args_main = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_main_fun_invalid"
    (fun tmpdir ->
      match plan_single_binary_source ~tmpdir "let main = fun ~args -> Ok ()\n" with
      | Error (
        Riot_planner.Planning_error.InvalidExecutableMain {
          error = Riot_planner.Planning_error.InvalidMainParameters { parameters };
          _;
        }
      ) ->
          if parameters = [] then
            Ok ()
          else
            Error ("expected no direct main parameters, got " ^ String.concat ", " parameters)
      | Error err ->
          Error ("expected invalid-main-parameters executable error, got "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok _ -> Error "expected planner to reject fun-expression executable main") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_entrypoint_requires_main_binding = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_main_missing"
    (fun tmpdir ->
      match plan_single_binary_source ~tmpdir "let () = print_endline \"hello\"\n" with
      | Error (
        Riot_planner.Planning_error.InvalidExecutableMain {
          target_name;
          source;
          error = Riot_planner.Planning_error.MissingMain;
        }
      ) ->
          if not (String.equal target_name "entry-demo") then
            Error ("expected target entry-demo, got " ^ target_name)
          else if not (Path.equal source (Path.v "src/main.ml")) then
            Error ("expected source src/main.ml, got " ^ Path.to_string source)
          else
            Ok ()
      | Error err ->
          Error ("expected missing-main executable error, got "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok _ -> Error "expected planner to reject missing executable main") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_entrypoint_rejects_multiple_main_bindings = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_main_duplicate"
    (fun tmpdir ->
      let source = "let main ~args:_ = Ok ()\nlet main ~args:_ = Ok ()\n" in
      match plan_single_binary_source ~tmpdir source with
      | Error (
        Riot_planner.Planning_error.InvalidExecutableMain {
          error = Riot_planner.Planning_error.MultipleMainDefinitions { count };
          _;
        }
      ) ->
          if count = 2 then
            Ok ()
          else
            Error ("expected duplicate count 2, got " ^ Int.to_string count)
      | Error err ->
          Error ("expected duplicate-main executable error, got "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok _ -> Error "expected planner to reject duplicate executable main bindings") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_entrypoint_rejects_positional_args_parameter = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_main_positional"
    (fun tmpdir ->
      match plan_single_binary_source ~tmpdir "let main args = Ok ()\n" with
      | Error (
        Riot_planner.Planning_error.InvalidExecutableMain {
          error = Riot_planner.Planning_error.InvalidMainParameters { parameters };
          _;
        }
      ) ->
          if parameters = [ "args" ] then
            Ok ()
          else
            Error ("expected positional args parameter, got " ^ String.concat ", " parameters)
      | Error err ->
          Error ("expected invalid-main-parameters executable error, got "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok _ -> Error "expected planner to reject positional executable main args") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_entrypoint_rejects_extra_parameters = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_main_extra_parameter"
    (fun tmpdir ->
      match plan_single_binary_source ~tmpdir "let main ~args () = Ok ()\n" with
      | Error (
        Riot_planner.Planning_error.InvalidExecutableMain {
          error = Riot_planner.Planning_error.InvalidMainParameters { parameters };
          _;
        }
      ) ->
          if parameters = [ "~args"; "<positional>" ] then
            Ok ()
          else
            Error ("expected ~args and positional parameter, got " ^ String.concat ", " parameters)
      | Error err ->
          Error ("expected invalid-main-parameters executable error, got "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok _ -> Error "expected planner to reject executable main with extra parameters") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let plan_kernel_package_with_fresh_store = fun () ->
  match Fs.with_tempdir
    ~prefix:"planner_kernel_order"
    (fun tempdir ->
      match load_repo_workspace () with
      | Error _ as err -> err
      | Ok repo_workspace ->
          match find_package_by_name repo_workspace "kernel" with
          | None -> Error "kernel package not found in workspace"
          | Some package ->
              let workspace =
                clone_workspace_with_target
                  repo_workspace
                  ~target_dir:Path.(tempdir / Path.v "target")
              in
              let store = Riot_store.Store.create ~workspace in
              let unit_graph = runtime_unit_graph workspace in
              let unit_key = Riot_planner.Build_unit.key (library_unit package) in
              let session_id = Riot_model.Session_id.make () in
              let profile = Riot_model.Profile.debug in
              let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
              let runtime_result =
                plan_build_unit_from_graph ~workspace ~store ~unit_graph ~unit_key ~build_ctx
              in
              match runtime_result with
              | Error err -> Error ("kernel live plan failed: " ^ err)
              | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; _ }) -> (
                  match find_create_library_objects action_graph with
                  | Error _ as err -> err
                  | Ok live_objects -> (
                      match plan_build_unit_from_graph
                        ~workspace
                        ~store
                        ~unit_graph
                        ~unit_key
                        ~build_ctx with
                      | Error err -> Error ("kernel cached plan failed: " ^ err)
                      | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) -> (
                          match find_create_library_objects action_graph with
                          | Error _ as err -> err
                          | Ok cached_objects -> Ok (module_graph, live_objects, cached_objects)
                        )
                      | Ok _ -> Error "expected cached kernel plan to return Planned"
                    )
                )
              | Ok _ -> Error "expected live kernel plan to return Planned") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let plan_kernel_runtime_graphs = fun ~workspace ~store ~build_ctx ->
  match find_package_by_name workspace "kernel" with
  | None -> Error "kernel package not found in workspace"
  | Some package ->
      let unit_graph = runtime_unit_graph workspace in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      match plan_build_unit_from_graph ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error err -> Error ("kernel runtime plan failed: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned {
              module_graph;
              action_graph;
              hash;
              depset;
              _;
            }) ->
          Ok (package, module_graph, action_graph, hash, depset)
      | Ok _ -> Error "expected kernel runtime plan to return Planned"

let test_plan_bundle_cache_hit_restores_module_and_action_graphs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_bundle_hit_test"
    (fun tmpdir ->
      let package = make_package tmpdir "pkg" in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let unit = library_unit package in
      let package = Riot_planner.Build_unit.package unit in
      let input_hash =
        Riot_planner.Package_planner.compute_input_hash
          ~package
          ~depset:[]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      let action_graph_json =
        let ag = Riot_planner.Action_graph.create () in
        let action = Riot_planner.Action.WriteFile {
          destination = Path.v "out.txt";
          content = "cached";
        }
        in
        let spec =
          Riot_planner.Action_node.make
            ~actions:[ action ]
            ~outs:[ Path.v "out.txt" ]
            ~srcs:[]
            ~package
            ~toolchain:test_toolchain
            ~dependency_hashes:(fun _ -> Crypto.hash_string "")
            ~deps:[]
        in
        let _ = Riot_planner.Action_graph.add_node ag spec in
        Riot_planner.Action_graph.to_json ag
      in
      let module_graph_json = Std.Data.Json.Object [
        (
          "nodes",
          Std.Data.Json.Array [
            Std.Data.Json.Object [
              ("id", Std.Data.Json.Int 1);
              (
                "file",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "concrete");
                  ("path", Std.Data.Json.String "");
                ]
              );
              ("kind", Std.Data.Json.Object [ ("kind", Std.Data.Json.String "root"); ]);
              ("deps", Std.Data.Json.Array []);
              ("opens", Std.Data.Json.Array []);
            ];
          ]
        );
      ]
      in
      let bundle = Std.Data.Json.Object [
        ("version", Std.Data.Json.Int 1);
        ("package", Std.Data.Json.String (Package_name.to_string package.name));
        ("module_graph", module_graph_json);
        ("action_graph", action_graph_json);
      ]
      in
      let _ =
        Riot_store.Store.save_plan_bundle store ~hash:input_hash ~plan:bundle
        |> Result.expect ~msg:"save_plan_bundle should succeed"
      in
      match plan_build_unit ~workspace ~store ~unit ~depset:[] ~build_ctx with
      | Error err -> Error ("expected cache-hit plan result, got planner error: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; _ }) ->
          let module_nodes =
            match G.topo_sort module_graph with
            | Ok nodes -> nodes
            | Error _ -> []
          in
          let action_nodes = Riot_planner.Action_graph.nodes action_graph in
          if List.length module_nodes = 1 && List.length action_nodes = 1 then
            Ok ()
          else
            Error ("expected restored module/action graphs with one node each, got "
            ^ Int.to_string (List.length module_nodes)
            ^ " module nodes and "
            ^ Int.to_string (List.length action_nodes)
            ^ " action nodes")
      | Ok _ -> Error "expected Planned result") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_cached_artifact_and_exports_short_circuit_without_plan_bundle = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_cached_artifact_hit_test"
    (fun tmpdir ->
      let package = make_package tmpdir "pkg" in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let unit = library_unit package in
      let package = Riot_planner.Build_unit.package unit in
      let input_hash =
        Riot_planner.Package_planner.compute_input_hash
          ~package
          ~depset:[]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
      let output = Path.(sandbox_dir / Path.v "pkg.cma") in
      let _ =
        Fs.create_dir_all sandbox_dir
        |> Result.expect ~msg:"sandbox dir creation should succeed"
      in
      let _ =
        Fs.write "cached" output
        |> Result.expect ~msg:"artifact output write should succeed"
      in
      let exports = [
        Riot_store.Store.{
          name = "pkg.cma";
          path = Path.v "pkg.cma";
          action_hash = Std.Crypto.Digest.hex input_hash;
        };
      ]
      in
      let _action_artifact =
        Riot_store.Store.save_action
          store
          ~package:(Package_name.to_string package.name)
          ~input_hash
          ~sandbox_dir
          ~outs:[ output ]
        |> Result.expect ~msg:"matching action artifact save should succeed"
      in
      let _artifact =
        Riot_store.Store.save
          store
          ~package:(Package_name.to_string package.name)
          ~exports
          ~input_hash:input_hash
          ~sandbox_dir
          ~outs:[ output ]
        |> Result.expect ~msg:"artifact save should succeed"
      in
      if Option.is_some (Riot_store.Store.load_plan_bundle store ~hash:input_hash) then
        Error "expected no plan bundle before cached planner lookup"
      else
        match plan_build_unit ~workspace ~store ~unit ~depset:[] ~build_ctx with
        | Error err -> Error ("expected cached plan result, got planner error: " ^ err)
        | Ok (
          Riot_planner.Package_planner.Cached {
            hash;
            artifact = cached_artifact;
            exports = cached_exports;
            _;
          }
        ) ->
            if Std.Crypto.Hash.compare hash input_hash != Std.Order.EQ then
              Error "expected cached plan hash to match input hash"
            else if not (List.length cached_artifact.Riot_store.Artifact.files = 1) then
              Error "expected cached artifact to expose one file"
            else if not (List.length cached_exports = 1) then
              Error "expected cached export manifest to expose one export"
            else
              Ok ()
        | Ok _ -> Error "expected Cached result") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_cached_artifact_missing_native_objects_rebuilds_plan_graphs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_cached_artifact_missing_native_objects_test"
    (fun tmpdir ->
      let package_dir = Path.(tmpdir / Path.v "pkg") in
      let src_dir = Path.(package_dir / Path.v "src") in
      let native_dir = Path.(package_dir / Path.v "native") in
      let src_path = Path.(src_dir / Path.v "pkg.ml") in
      let native_path = Path.(native_dir / Path.v "pkg_stubs.c") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"expected src dir creation to succeed"
      in
      let _ =
        Fs.create_dir_all native_dir
        |> Result.expect ~msg:"expected native dir creation to succeed"
      in
      let _ =
        Fs.write "let value = 1\n" src_path
        |> Result.expect ~msg:"expected source write to succeed"
      in
      let _ =
        Fs.write "int riot_pkg_stubs_marker(void) { return 0; }\n" native_path
        |> Result.expect ~msg:"expected native source write to succeed"
      in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "pkg"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:package_dir
          ~relative_path:(Path.v "pkg")
          ~library:{ path = Path.v "src/pkg.ml" }
          ~sources:{
            src = [ Path.v "src/pkg.ml" ];
            native = [ Path.v "native/pkg_stubs.c" ];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let unit = library_unit package in
      let package = Riot_planner.Build_unit.package unit in
      let input_hash =
        Riot_planner.Package_planner.compute_input_hash
          ~package
          ~depset:[]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
      let output = Path.(sandbox_dir / Path.v "pkg.cmxa") in
      let _ =
        Fs.create_dir_all sandbox_dir
        |> Result.expect ~msg:"expected sandbox dir creation to succeed"
      in
      let _ =
        Fs.write "cached without native object" output
        |> Result.expect ~msg:"expected cached output write to succeed"
      in
      let _artifact =
        Riot_store.Store.save
          store
          ~package:(Package_name.to_string package.name)
          ~input_hash
          ~sandbox_dir
          ~outs:[ output ]
        |> Result.expect ~msg:"expected incomplete artifact save to succeed"
      in
      let input_hash_cache = Riot_planner.Package_planner.create_input_hash_cache () in
      match Riot_planner.Package_planner.plan_build_unit_with_cache
        ~on_source_analyzed:(fun _ -> ())
        ~input_hash_cache
        ~workspace
        ~toolchain:test_toolchain
        ~store
        ~unit
        ~depset:[]
        ~build_ctx with
      | Error err ->
          Error ("expected incomplete cached artifact miss to replan package, got planner error: "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok (Riot_planner.Package_planner.Planned _) -> Ok ()
      | Ok (Riot_planner.Package_planner.Cached _) ->
          Error "expected cached artifact missing native objects to be ignored") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let stale_cached_artifact_version_rebuilds_plan_graphs = fun planner_version _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_cached_artifact_stale_version_test"
    (fun tmpdir ->
      let package = make_package tmpdir "pkg" in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let unit = library_unit package in
      let package = Riot_planner.Build_unit.package unit in
      let stale_input_hash =
        Riot_planner.Package_planner.compute_input_hash
          ~planner_version
          ~package
          ~depset:[]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
      let output = Path.(sandbox_dir / Path.v "pkg.cma") in
      let _ =
        Fs.create_dir_all sandbox_dir
        |> Result.expect ~msg:"sandbox dir creation should succeed"
      in
      let _ =
        Fs.write "stale artifact" output
        |> Result.expect ~msg:"artifact output write should succeed"
      in
      let exports = [
        Riot_store.Store.{
          name = "pkg.cma";
          path = Path.v "pkg.cma";
          action_hash = Std.Crypto.Digest.hex stale_input_hash;
        };
      ]
      in
      let _artifact =
        Riot_store.Store.save
          store
          ~package:(Package_name.to_string package.name)
          ~exports
          ~input_hash:stale_input_hash
          ~sandbox_dir
          ~outs:[ output ]
        |> Result.expect ~msg:"stale artifact save should succeed"
      in
      match plan_build_unit ~workspace ~store ~unit ~depset:[] ~build_ctx with
      | Error err ->
          Error ("expected stale artifact miss to replan package, got planner error: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned _) -> Ok ()
      | Ok (Riot_planner.Package_planner.Cached _) ->
          Error "expected stale cached artifact to be ignored after planner version bump") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_stale_cached_artifact_version_rebuilds_plan_graphs =
  stale_cached_artifact_version_rebuilds_plan_graphs legacy_planner_artifacts_version

let test_foreign_hash_version_rebuilds_plan_graphs =
  stale_cached_artifact_version_rebuilds_plan_graphs "planner-artifacts:v31"

let test_stale_plan_bundle_version_rebuilds_plan_graphs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_bundle_stale_version_test"
    (fun tmpdir ->
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "pkg"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:Path.(tmpdir / Path.v "pkg")
          ~relative_path:(Path.v "pkg")
          ~library:{ path = Path.v "src/pkg.ml" }
          ~sources:{
            src = [ Path.v "src/pkg.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let src_dir = Path.(package.path / Path.v "src") in
      let source = Path.(src_dir / Path.v "pkg.ml") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"expected src dir creation to succeed"
      in
      let _ =
        Fs.write "let value = 1\n" source
        |> Result.expect ~msg:"expected source write to succeed"
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.release in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let unit = library_unit ~profile package in
      let package = Riot_planner.Build_unit.package unit in
      let stale_input_hash =
        Riot_planner.Package_planner.compute_input_hash
          ~planner_version:"planner-artifacts:v2"
          ~package
          ~depset:[]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      let stale_action_graph_json =
        let ag = Riot_planner.Action_graph.create () in
        let action = Riot_planner.Action.WriteFile {
          destination = Path.v "out.txt";
          content = "stale";
        }
        in
        let spec =
          Riot_planner.Action_node.make
            ~actions:[ action ]
            ~outs:[ Path.v "out.txt" ]
            ~srcs:[]
            ~package
            ~toolchain:test_toolchain
            ~dependency_hashes:(fun _ -> Crypto.hash_string "")
            ~deps:[]
        in
        let _ = Riot_planner.Action_graph.add_node ag spec in
        Riot_planner.Action_graph.to_json ag
      in
      let stale_module_graph_json = Std.Data.Json.Object [
        (
          "nodes",
          Std.Data.Json.Array [
            Std.Data.Json.Object [
              ("id", Std.Data.Json.Int 1);
              (
                "file",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "concrete");
                  ("path", Std.Data.Json.String "");
                ]
              );
              ("kind", Std.Data.Json.Object [ ("kind", Std.Data.Json.String "root"); ]);
              ("deps", Std.Data.Json.Array []);
              ("opens", Std.Data.Json.Array []);
            ];
          ]
        );
      ]
      in
      let stale_bundle = Std.Data.Json.Object [
        ("version", Std.Data.Json.Int 1);
        ("package", Std.Data.Json.String (Package_name.to_string package.name));
        ("module_graph", stale_module_graph_json);
        ("action_graph", stale_action_graph_json);
      ]
      in
      let _ =
        Riot_store.Store.save_plan_bundle store ~hash:stale_input_hash ~plan:stale_bundle
        |> Result.expect ~msg:"expected stale plan bundle save to succeed"
      in
      match plan_build_unit ~workspace ~store ~unit ~depset:[] ~build_ctx with
      | Error err ->
          Error ("expected stale bundle miss to replan package, got planner error: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) ->
          let actions = Riot_planner.Action_graph.to_action_list action_graph in
          if List.any
            actions
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Riot_planner.Action.CreateLibrary _ -> true
              | _ -> false) then
            Ok ()
          else
            Error "expected stale plan bundle to be ignored and rebuilt"
      | Ok _ -> Error "expected Planned result") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_runtime_plan_keeps_direct_src_files_as_library_objects = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_runtime_src_root_test"
    (fun tmpdir ->
      let package_dir = Path.(tmpdir / Path.v "pkg") in
      let source_dir = Path.(package_dir / Path.v "src") in
      let _ =
        Fs.create_dir_all source_dir
        |> Result.expect ~msg:"expected source dir creation to succeed"
      in
      let _ =
        Fs.write "let value = 1\n" Path.(source_dir / Path.v "pkg.ml")
        |> Result.expect ~msg:"expected source write to succeed"
      in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "pkg"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:package_dir
          ~relative_path:(Path.v "pkg")
          ~library:{ path = Path.v "src/pkg.ml" }
          ()
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let unit_graph = runtime_unit_graph workspace in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      match plan_build_unit_from_graph ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error err -> Error err
      | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) -> (
          match find_create_library_objects action_graph with
          | Error err -> Error err
          | Ok objects ->
              if List.contains objects ~value:"Pkg.cmx" then
                Ok ()
              else
                Error ("expected runtime library to link Pkg.cmx, got ["
                ^ String.concat ", " objects
                ^ "]")
        )
      | Ok other -> Error ("expected Planned result, got " ^ describe_plan_result other)) with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_plan_bundle_with_empty_library_objects_rebuilds_plan_graphs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_empty_library_bundle_test"
    (fun tmpdir ->
      let package =
        make_package_with_files
          ~library:(Some { path = Path.v "src/pkg.ml" })
          ~tmpdir
          ~package_name:"pkg"
          ~files:[ ("src/pkg.ml", "let value = 1\n") ]
          ~binaries:[]
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let unit_graph = runtime_unit_graph workspace in
      let unit = library_unit package in
      let unit_key = Riot_planner.Build_unit.key unit in
      let package = Riot_planner.Build_unit.package unit in
      let input_hash =
        Riot_planner.Package_planner.compute_input_hash
          ~package
          ~depset:[]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      match plan_build_unit_from_graph ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error err -> Error err
      | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) -> (
          match find_create_library_objects action_graph with
          | Error err -> Error err
          | Ok live_objects ->
              if not (List.contains live_objects ~value:"Pkg.cmx") then
                Error ("expected live plan to link Pkg.cmx, got ["
                ^ String.concat ", " live_objects
                ^ "]")
              else
                match Riot_store.Store.load_plan_bundle store ~hash:input_hash with
                | None -> Error "expected live plan to save a plan bundle"
                | Some bundle ->
                    let bad_bundle =
                      rewrite_plan_bundle_action_graph bundle ~rewrite:(fun _objects -> [])
                    in
                    let _ =
                      Riot_store.Store.save_plan_bundle store ~hash:input_hash ~plan:bad_bundle
                      |> Result.expect ~msg:"expected bad plan bundle save to succeed"
                    in
                    match plan_build_unit_from_graph
                      ~workspace
                      ~store
                      ~unit_graph
                      ~unit_key
                      ~build_ctx with
                    | Error err -> Error err
                    | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) -> (
                        match find_create_library_objects action_graph with
                        | Error err -> Error err
                        | Ok replanned_objects ->
                            if List.contains replanned_objects ~value:"Pkg.cmx" then
                              Ok ()
                            else
                              Error ("expected bad plan bundle to be ignored, got ["
                              ^ String.concat ", " replanned_objects
                              ^ "]")
                      )
                    | Ok other ->
                        Error ("expected bad plan bundle to replan, got "
                        ^ describe_plan_result other)
        )
      | Ok other -> Error ("expected Planned result, got " ^ describe_plan_result other)) with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_plan_bundle_cache_hit_preserves_module_dependency_order = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_bundle_order_test"
    (fun tmpdir ->
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "pkg"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:Path.(tmpdir / Path.v "pkg")
          ~relative_path:(Path.v "pkg")
          ~library:{ path = Path.v "src/pkg.ml" }
          ~sources:{
            src = [ Path.v "src/a.ml"; Path.v "src/b.ml"; Path.v "src/c.ml"; Path.v "src/pkg.ml"; ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let input_hash =
        Riot_planner.Package_planner.compute_input_hash
          ~package
          ~depset:[]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      let module_graph_json = Std.Data.Json.Object [
        (
          "nodes",
          Std.Data.Json.Array [
            Std.Data.Json.Object [
              ("id", Std.Data.Json.Int 1);
              (
                "file",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "concrete");
                  ("path", Std.Data.Json.String "src/a.ml");
                ]
              );
              (
                "kind",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "ml");
                  ("filename", Std.Data.Json.String "src/a.ml");
                  ("namespace", Std.Data.Json.Array []);
                ]
              );
              ("deps", Std.Data.Json.Array []);
              ("opens", Std.Data.Json.Array []);
            ];
            Std.Data.Json.Object [
              ("id", Std.Data.Json.Int 2);
              (
                "file",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "concrete");
                  ("path", Std.Data.Json.String "src/b.ml");
                ]
              );
              (
                "kind",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "ml");
                  ("filename", Std.Data.Json.String "src/b.ml");
                  ("namespace", Std.Data.Json.Array []);
                ]
              );
              ("deps", Std.Data.Json.Array []);
              ("opens", Std.Data.Json.Array []);
            ];
            Std.Data.Json.Object [
              ("id", Std.Data.Json.Int 3);
              (
                "file",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "concrete");
                  ("path", Std.Data.Json.String "src/c.ml");
                ]
              );
              (
                "kind",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "ml");
                  ("filename", Std.Data.Json.String "src/c.ml");
                  ("namespace", Std.Data.Json.Array []);
                ]
              );
              ("deps", Std.Data.Json.Array []);
              ("opens", Std.Data.Json.Array []);
            ];
            Std.Data.Json.Object [
              ("id", Std.Data.Json.Int 4);
              (
                "file",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "concrete");
                  ("path", Std.Data.Json.String "");
                ]
              );
              (
                "kind",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "library");
                  ("name", Std.Data.Json.String (Package_name.to_string package.name));
                  ("includes", Std.Data.Json.Array []);
                ]
              );
              (
                "deps",
                Std.Data.Json.Array [
                  Std.Data.Json.Int 1;
                  Std.Data.Json.Int 2;
                  Std.Data.Json.Int 3;
                ]
              );
              ("opens", Std.Data.Json.Array []);
            ];
          ]
        );
      ]
      in
      let action_graph_json =
        let graph = Riot_planner.Action_graph.create () in
        let spec =
          Riot_planner.Action_node.make
            ~actions:[
              Riot_planner.Action.WriteFile { destination = Path.v "out.txt"; content = "cached" };
            ]
            ~outs:[ Path.v "out.txt" ]
            ~srcs:[]
            ~package
            ~toolchain:test_toolchain
            ~dependency_hashes:(fun _ -> Crypto.hash_string "")
            ~deps:[]
        in
        let _ = Riot_planner.Action_graph.add_node graph spec in
        Riot_planner.Action_graph.to_json graph
      in
      let bundle = Std.Data.Json.Object [
        ("version", Std.Data.Json.Int 1);
        ("package", Std.Data.Json.String (Package_name.to_string package.name));
        ("module_graph", module_graph_json);
        ("action_graph", action_graph_json);
      ]
      in
      let _ =
        Riot_store.Store.save_plan_bundle store ~hash:input_hash ~plan:bundle
        |> Result.expect ~msg:"save_plan_bundle should succeed"
      in
      let unit = library_unit package in
      match plan_build_unit ~workspace ~store ~unit ~depset:[] ~build_ctx with
      | Error err -> Error ("expected cache-hit plan result, got planner error: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned { module_graph; _ }) -> (
          match find_library_node module_graph with
          | None -> Error "expected restored library node"
          | Some library_node ->
              let actual = module_dependency_labels module_graph library_node in
              let expected = [ "ML(A)"; "ML(B)"; "ML(C)" ] in
              if actual = expected then
                Ok ()
              else
                Error ("expected library dependency order ["
                ^ String.concat ", " expected
                ^ "] but got ["
                ^ String.concat ", " actual
                ^ "]")
        )
      | Ok _ -> Error "expected Planned result") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_underscore_sibling_module_dependency_is_planned = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_underscore_sibling_dep"
    (fun tmpdir ->
      let package_root = Path.(tmpdir / Path.v "pkg") in
      let src_dir = Path.(package_root / Path.v "src") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"expected src dir creation to succeed"
      in
      let _ =
        Fs.write
          "module Udp_socket = Udp_socket\nmodule Udp_server = Udp_server\n"
          Path.(src_dir / Path.v "pkg.ml")
        |> Result.expect ~msg:"expected pkg.ml write to succeed"
      in
      let _ =
        Fs.write "type t\nval create : unit -> t\n" Path.(src_dir / Path.v "udp_socket.mli")
        |> Result.expect ~msg:"expected udp_socket.mli write to succeed"
      in
      let _ =
        Fs.write "type t = unit\nlet create () = ()\n" Path.(src_dir / Path.v "udp_socket.ml")
        |> Result.expect ~msg:"expected udp_socket.ml write to succeed"
      in
      let _ =
        Fs.write
          "type handler = socket:Udp_socket.t -> bytes -> unit\nval run : handler -> unit\n"
          Path.(src_dir / Path.v "udp_server.mli")
        |> Result.expect ~msg:"expected udp_server.mli write to succeed"
      in
      let _ =
        Fs.write
          "type handler = socket:Udp_socket.t -> bytes -> unit\nlet run _ = ()\n"
          Path.(src_dir / Path.v "udp_server.ml")
        |> Result.expect ~msg:"expected udp_server.ml write to succeed"
      in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "pkg"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:package_root
          ~relative_path:(Path.v "pkg")
          ~library:{ path = Path.v "src/pkg.ml" }
          ~sources:{
            src = [
              Path.v "src/pkg.ml";
              Path.v "src/udp_server.mli";
              Path.v "src/udp_socket.mli";
              Path.v "src/udp_socket.ml";
              Path.v "src/udp_server.ml";
            ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let unit_graph = runtime_unit_graph workspace in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      match plan_build_unit_from_graph ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error err -> Error ("expected package plan to succeed, got planner error: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned { module_graph; _ }) -> (
          match find_module_node_by_label module_graph "MLI(Pkg__Udp_server)" with
          | None -> Error "missing MLI(Pkg__Udp_server) in module graph"
          | Some node ->
              let deps = module_dependency_labels module_graph node in
              if
                List.any
                  deps
                  ~fn:(fun label ->
                    String.equal label "ML(Pkg__Udp_socket)"
                    || String.equal label "MLI(Pkg__Udp_socket)")
              then
                Ok ()
              else
                Error ("expected MLI(Pkg__Udp_server) to depend on Udp_socket, got ["
                ^ String.concat ", " deps
                ^ "]")
        )
      | Ok _ -> Error "expected Planned result") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_planner_rejects_direct_internal_library_access = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_rejects_direct_internal_access"
    (fun tmpdir ->
      let package =
        make_package_with_files
          ~tmpdir
          ~package_name:"berrybot"
          ~library:(Some { path = Path.v "src/berrybot.ml" })
          ~files:[
            ("src/berrybot.ml", "module A = A\n");
            ("src/a.ml", "let value = 42\n");
            ("src/main.ml", binary_main "ignore A.value");
          ]
          ~binaries:[ ("berrybot", "src/main.ml"); ]
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let unit_graph = runtime_unit_graph workspace in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      match plan_build_unit_from_graph_raw ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error (
        Riot_planner.Planning_error.TargetDependsOnInternalLibraryModule {
          target_name;
          source;
          requested_module;
          internal_module;
          public_module;
        }
      ) ->
          if not (String.equal target_name "berrybot") then
            Error ("expected target name berrybot, got " ^ target_name)
          else if not (Path.equal source (Path.v "src/main.ml")) then
            Error ("expected source src/main.ml, got " ^ Path.to_string source)
          else if not (String.equal requested_module "A") then
            Error ("expected requested module A, got " ^ requested_module)
          else if not (String.equal internal_module "Berrybot__A") then
            Error ("expected internal module Berrybot__A, got " ^ internal_module)
          else if not (String.equal public_module "Berrybot") then
            Error ("expected public module Berrybot, got " ^ public_module)
          else
            Ok ()
      | Error err ->
          Error ("expected direct internal access planner error, got "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok _ -> Error "expected planner to reject direct internal library access") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_planner_rejects_namespaced_internal_library_access = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_rejects_namespaced_internal_access"
    (fun tmpdir ->
      let package =
        make_package_with_files
          ~tmpdir
          ~package_name:"berrybot"
          ~library:(Some { path = Path.v "src/berrybot.ml" })
          ~files:[
            ("src/berrybot.ml", "module A = A\n");
            ("src/a.ml", "let value = 42\n");
            ("src/main.ml", binary_main "ignore Berrybot__A.value");
          ]
          ~binaries:[ ("berrybot", "src/main.ml"); ]
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let unit_graph = runtime_unit_graph workspace in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      match plan_build_unit_from_graph_raw ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error (
        Riot_planner.Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
          target_name;
          source;
          requested_module;
          internal_module;
          public_module;
        }
      ) ->
          if not (String.equal target_name "berrybot") then
            Error ("expected target name berrybot, got " ^ target_name)
          else if not (Path.equal source (Path.v "src/main.ml")) then
            Error ("expected source src/main.ml, got " ^ Path.to_string source)
          else if not (String.equal requested_module "Berrybot__A") then
            Error ("expected requested module Berrybot__A, got " ^ requested_module)
          else if not (String.equal internal_module "Berrybot__A") then
            Error ("expected internal module Berrybot__A, got " ^ internal_module)
          else if not (String.equal public_module "Berrybot") then
            Error ("expected public module Berrybot, got " ^ public_module)
          else
            Ok ()
      | Error err ->
          Error ("expected namespaced internal access planner error, got "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok _ -> Error "expected planner to reject namespaced internal library access") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_planner_rejects_direct_other_binary_root_access = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_rejects_direct_other_binary_root"
    (fun tmpdir ->
      let package =
        make_package_with_files
          ~tmpdir
          ~package_name:"berrybot"
          ~library:(Some { path = Path.v "src/berrybot.ml" })
          ~files:[
            ("src/berrybot.ml", "module A = A\n");
            ("src/a.ml", "let value = 42\n");
            ("src/main.ml", binary_main "Admin.run ()");
            (
              "src/admin.ml",
              "let run () = ignore Berrybot.A.value\n\nlet main ~args:_ =\n  run ();\n  Ok ()\n"
            );
          ]
          ~binaries:[ ("berrybot", "src/main.ml"); ("admin", "src/admin.ml"); ]
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let unit_graph = runtime_unit_graph workspace in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      match plan_build_unit_from_graph_raw ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error (
        Riot_planner.Planning_error.TargetDependsOnOtherTargetRoot {
          target_name;
          source;
          requested_module;
          other_target_name;
          other_target_module;
          public_module;
        }
      ) ->
          if not (String.equal target_name "berrybot") then
            Error ("expected target name berrybot, got " ^ target_name)
          else if not (Path.equal source (Path.v "src/main.ml")) then
            Error ("expected source src/main.ml, got " ^ Path.to_string source)
          else if not (String.equal requested_module "Admin") then
            Error ("expected requested module Admin, got " ^ requested_module)
          else if not (String.equal other_target_name "admin") then
            Error ("expected other target name admin, got " ^ other_target_name)
          else if not (String.equal other_target_module "Berrybot__Admin") then
            Error ("expected other target module Berrybot__Admin, got " ^ other_target_module)
          else if not (String.equal public_module "Berrybot") then
            Error ("expected public module Berrybot, got " ^ public_module)
          else
            Ok ()
      | Error err ->
          Error ("expected other target root planner error, got "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok _ -> Error "expected planner to reject direct other-binary-root access") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_planner_rejects_namespaced_other_binary_root_access = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_rejects_namespaced_other_binary_root"
    (fun tmpdir ->
      let package =
        make_package_with_files
          ~tmpdir
          ~package_name:"berrybot"
          ~library:(Some { path = Path.v "src/berrybot.ml" })
          ~files:[
            ("src/berrybot.ml", "module A = A\n");
            ("src/a.ml", "let value = 42\n");
            ("src/main.ml", binary_main "Berrybot__Admin.run ()");
            (
              "src/admin.ml",
              "let run () = ignore Berrybot.A.value\n\nlet main ~args:_ =\n  run ();\n  Ok ()\n"
            );
          ]
          ~binaries:[ ("berrybot", "src/main.ml"); ("admin", "src/admin.ml"); ]
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let unit_graph = runtime_unit_graph workspace in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      match plan_build_unit_from_graph_raw ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error (
        Riot_planner.Planning_error.TargetDependsOnOtherTargetRoot {
          target_name;
          source;
          requested_module;
          other_target_name;
          other_target_module;
          public_module;
        }
      ) ->
          if not (String.equal target_name "berrybot") then
            Error ("expected target name berrybot, got " ^ target_name)
          else if not (Path.equal source (Path.v "src/main.ml")) then
            Error ("expected source src/main.ml, got " ^ Path.to_string source)
          else if not (String.equal requested_module "Berrybot__Admin") then
            Error ("expected requested module Berrybot__Admin, got " ^ requested_module)
          else if not (String.equal other_target_name "admin") then
            Error ("expected other target name admin, got " ^ other_target_name)
          else if not (String.equal other_target_module "Berrybot__Admin") then
            Error ("expected other target module Berrybot__Admin, got " ^ other_target_module)
          else if not (String.equal public_module "Berrybot") then
            Error ("expected public module Berrybot, got " ^ public_module)
          else
            Ok ()
      | Error err ->
          Error ("expected namespaced other target root planner error, got "
          ^ Riot_planner.Planning_error.to_string err)
      | Ok _ -> Error "expected planner to reject namespaced other-binary-root access") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_nested_library_interfaces_depend_on_inherited_aliases = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_nested_library_aliases"
    (fun tmpdir ->
      let package_root = Path.(tmpdir / Path.v "pkg") in
      let src_dir = Path.(package_root / Path.v "src") in
      let archive_dir = Path.(src_dir / Path.v "archive") in
      let _ =
        Fs.create_dir_all archive_dir
        |> Result.expect ~msg:"expected archive dir creation to succeed"
      in
      let _ =
        Fs.write "module Archive = Archive\n" Path.(src_dir / Path.v "pkg.ml")
        |> Result.expect ~msg:"expected pkg.ml write to succeed"
      in
      let _ =
        Fs.write "type t\n" Path.(archive_dir / Path.v "archive.mli")
        |> Result.expect ~msg:"expected archive.mli write to succeed"
      in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "pkg"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:package_root
          ~relative_path:(Path.v "pkg")
          ~library:{ path = Path.v "src/pkg.ml" }
          ~sources:{
            src = [ Path.v "src/pkg.ml"; Path.v "src/archive/archive.mli" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let unit_graph = runtime_unit_graph workspace in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      match plan_build_unit_from_graph ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error err -> Error ("expected package plan to succeed, got planner error: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned { module_graph; _ }) -> (
          match find_module_node_by_label module_graph "MLI(Pkg__Archive)" with
          | None -> Error "missing MLI(Pkg__Archive) in module graph"
          | Some node ->
              let deps = module_dependency_labels module_graph node in
              if
                List.any deps ~fn:(String.equal "ML(Pkg__Aliases)")
                && List.any deps ~fn:(String.equal "ML(Pkg__Archive__Aliases)")
              then
                Ok ()
              else
                Error ("expected MLI(Pkg__Archive) to depend on inherited aliases, got ["
                ^ String.concat ", " deps
                ^ "]")
        )
      | Ok _ -> Error "expected Planned result") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_legacy_nested_sibling_plan_bundle_is_ignored_after_version_bump = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_nested_sibling_legacy_bundle"
    (fun tmpdir ->
      let package_root = Path.(tmpdir / Path.v "demo") in
      let src_dir = Path.(package_root / Path.v "src") in
      let net_dir = Path.(src_dir / Path.v "net") in
      let _ =
        Fs.create_dir_all net_dir
        |> Result.expect ~msg:"expected nested src dir creation to succeed"
      in
      let _ =
        Fs.write "module Net = Net\n" Path.(src_dir / Path.v "demo.ml")
        |> Result.expect ~msg:"expected demo.ml write to succeed"
      in
      let _ =
        Fs.write
          "module Udp_socket = Udp_socket\nmodule Udp_server = Udp_server\n"
          Path.(net_dir / Path.v "net.ml")
        |> Result.expect ~msg:"expected net.ml write to succeed"
      in
      let _ =
        Fs.write "type t\n" Path.(net_dir / Path.v "udp_socket.mli")
        |> Result.expect ~msg:"expected udp_socket.mli write to succeed"
      in
      let _ =
        Fs.write "type t = unit\n" Path.(net_dir / Path.v "udp_socket.ml")
        |> Result.expect ~msg:"expected udp_socket.ml write to succeed"
      in
      let _ =
        Fs.write
          "type handler = socket:Udp_socket.t -> bytes -> unit\nval run : handler -> unit\n"
          Path.(net_dir / Path.v "udp_server.mli")
        |> Result.expect ~msg:"expected udp_server.mli write to succeed"
      in
      let _ =
        Fs.write
          "type handler = socket:Udp_socket.t -> bytes -> unit\nlet run _ = ()\n"
          Path.(net_dir / Path.v "udp_server.ml")
        |> Result.expect ~msg:"expected udp_server.ml write to succeed"
      in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "demo"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:package_root
          ~relative_path:(Path.v "demo")
          ~library:{ path = Path.v "src/demo.ml" }
          ~sources:{
            src = [
              Path.v "src/demo.ml";
              Path.v "src/net/net.ml";
              Path.v "src/net/udp_socket.mli";
              Path.v "src/net/udp_socket.ml";
              Path.v "src/net/udp_server.mli";
              Path.v "src/net/udp_server.ml";
            ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let workspace = make_test_workspace tmpdir [ package ] in
      let store = Riot_store.Store.create ~workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let stale_input_hash =
        Riot_planner.Package_planner.compute_input_hash
          ~planner_version:nested_sibling_dependency_fix_planner_artifacts_version
          ~package
          ~depset:[]
          ~workspace
          ~profile
          ~build_ctx
          ~toolchain:test_toolchain
          ()
      in
      let stale_action_graph_json =
        let ag = Riot_planner.Action_graph.create () in
        let action = Riot_planner.Action.WriteFile {
          destination = Path.v "out.txt";
          content = "stale";
        }
        in
        let spec =
          Riot_planner.Action_node.make
            ~actions:[ action ]
            ~outs:[ Path.v "out.txt" ]
            ~srcs:[]
            ~package
            ~toolchain:test_toolchain
            ~dependency_hashes:(fun _ -> Crypto.hash_string "")
            ~deps:[]
        in
        let _ = Riot_planner.Action_graph.add_node ag spec in
        Riot_planner.Action_graph.to_json ag
      in
      let stale_module_graph_json = Std.Data.Json.Object [
        (
          "nodes",
          Std.Data.Json.Array [
            Std.Data.Json.Object [
              ("id", Std.Data.Json.Int 1);
              (
                "file",
                Std.Data.Json.Object [
                  ("kind", Std.Data.Json.String "concrete");
                  ("path", Std.Data.Json.String "");
                ]
              );
              ("kind", Std.Data.Json.Object [ ("kind", Std.Data.Json.String "root"); ]);
              ("deps", Std.Data.Json.Array []);
              ("opens", Std.Data.Json.Array []);
            ];
          ]
        );
      ]
      in
      let stale_bundle = Std.Data.Json.Object [
        ("version", Std.Data.Json.Int 1);
        ("package", Std.Data.Json.String (Package_name.to_string package.name));
        ("module_graph", stale_module_graph_json);
        ("action_graph", stale_action_graph_json);
      ]
      in
      let _ =
        Riot_store.Store.save_plan_bundle store ~hash:stale_input_hash ~plan:stale_bundle
        |> Result.expect ~msg:"expected stale nested plan bundle save to succeed"
      in
      let unit_graph = runtime_unit_graph workspace in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      match plan_build_unit_from_graph ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
      | Error err ->
          Error ("expected nested sibling plan bundle to be ignored, got planner error: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned { module_graph; _ }) -> (
          match find_module_node_by_label module_graph "MLI(Demo__Net__Udp_server)" with
          | None ->
              Error "expected stale nested plan bundle to be ignored and rebuilt, but udp_server node was missing"
          | Some node ->
              let deps = module_dependency_labels module_graph node in
              if
                List.any
                  deps
                  ~fn:(fun label ->
                    String.equal label "ML(Demo__Net__Udp_socket)"
                    || String.equal label "MLI(Demo__Net__Udp_socket)")
              then
                Ok ()
              else
                Error ("expected rebuilt nested plan graph to restore Udp_socket dependency, got ["
                ^ String.concat ", " deps
                ^ "]")
        )
      | Ok _ -> Error "expected Planned result") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_kernel_live_create_library_orders_dependencies_before_error = fun _ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error _ as err -> err
  | Ok (_module_graph, live_objects, _cached_objects) ->
      match require_order
        live_objects
        ~before:"Kernel__Net__Tcp_listener.cmx"
        ~after:"Kernel__Error.cmx" with
      | Error _ as err -> err
      | Ok () ->
          match require_order
            live_objects
            ~before:"Kernel__Net__Udp_socket.cmx"
            ~after:"Kernel__Error.cmx" with
          | Error _ as err -> err
          | Ok () ->
              require_order live_objects ~before:"Kernel__Process.cmx" ~after:"Kernel__Error.cmx"

let test_kernel_unix_addr_interface_keeps_module_graph_dependencies = fun _ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error _ as err -> err
  | Ok (module_graph, _live_objects, _cached_objects) ->
      match find_module_node_by_label module_graph "MLI(Kernel__Net__Addr__Unix)" with
      | None -> Error "missing MLI(Kernel__Net__Addr__Unix) in module graph"
      | Some node ->
          let deps = module_dependency_labels module_graph node in
          let has_any labels = List.any labels ~fn:(fun label -> List.contains deps ~value:label) in
          if not (has_any [ "ML(Kernel__Result)"; "MLI(Kernel__Result)" ]) then
            Error ("expected MLI(Kernel__Net__Addr__Unix) to keep Result dependency in module graph, got ["
            ^ String.concat ", " deps
            ^ "]")
          else if not (has_any [ "ML(Kernel__System_error)"; "MLI(Kernel__System_error)" ]) then
            Error ("expected MLI(Kernel__Net__Addr__Unix) to keep System_error dependency in module graph, got ["
            ^ String.concat ", " deps
            ^ "]")
          else if
            not (has_any [ "ML(Kernel__Net__Socket_addr)"; "MLI(Kernel__Net__Socket_addr)" ])
          then
            Error ("expected MLI(Kernel__Net__Addr__Unix) to keep Socket_addr dependency in module graph, got ["
            ^ String.concat ", " deps
            ^ "]")
          else
            Ok ()

let test_kernel_process_interface_keeps_public_child_root_dependency = fun _ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error _ as err -> err
  | Ok (module_graph, _live_objects, _cached_objects) ->
      match find_module_node_by_label module_graph "MLI(Kernel__Process)" with
      | None -> Error "missing MLI(Kernel__Process) in module graph"
      | Some node ->
          let deps = module_dependency_labels module_graph node in
          if
            not (List.contains deps ~value:"ML(Kernel__Fs)")
            && not (List.contains deps ~value:"MLI(Kernel__Fs)")
          then
            Error ("expected MLI(Kernel__Process) to keep public Fs dependency in module graph, got ["
            ^ String.concat ", " deps
            ^ "]")
          else if
            List.contains deps ~value:"ML(Kernel__Fs__File)"
            || List.contains deps ~value:"MLI(Kernel__Fs__File)"
          then
            Error ("did not expect MLI(Kernel__Process) to depend directly on Kernel__Fs__File, got ["
            ^ String.concat ", " deps
            ^ "]")
          else if
            not (List.contains deps ~value:"ML(Kernel__System_error)")
            && not (List.contains deps ~value:"MLI(Kernel__System_error)")
          then
            Error ("expected MLI(Kernel__Process) to keep System_error dependency in module graph, got ["
            ^ String.concat ", " deps
            ^ "]")
          else
            Ok ()

let test_kernel_plan_bundle_cache_hit_preserves_live_create_library_order = fun _ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error _ as err -> err
  | Ok (_module_graph, live_objects, cached_objects) ->
      if live_objects = cached_objects then
        Ok ()
      else
        Error ("expected cached CreateLibrary object order to match live plan\nlive: ["
        ^ String.concat ", " live_objects
        ^ "]\ncached: ["
        ^ String.concat ", " cached_objects
        ^ "]")

let test_kernel_create_library_is_topological = fun _ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error err -> Error err
  | Ok (module_graph, live_objects, _cached_objects) ->
      validate_create_library_topological_order module_graph live_objects

let test_kernel_create_library_dependencies_are_unique = fun _ctx ->
  let check tempdir =
    match load_repo_workspace () with
    | Error _ as err -> err
    | Ok repo_workspace ->
        let workspace =
          clone_workspace_with_target repo_workspace ~target_dir:Path.(tempdir / Path.v "target")
        in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        match plan_kernel_runtime_graphs ~workspace ~store ~build_ctx with
        | Error _ as err -> err
        | Ok (_, _, action_graph, _, _) ->
            match find_create_library_node action_graph with
            | None -> Error "missing kernel CreateLibrary action node"
            | Some node ->
                let seen = Collections.HashSet.create () in
                let duplicates =
                  List.filter_map
                    (G.deps node)
                    ~fn:(fun dep_id ->
                      if Collections.HashSet.insert seen ~value:dep_id then
                        None
                      else
                        Some (G.Node_id.to_string dep_id))
                in
                if List.is_empty duplicates then
                  Ok ()
                else
                  Error ("expected unique CreateLibrary deps, found duplicates: "
                  ^ String.concat ", " duplicates)
  in
  match Fs.with_tempdir ~prefix:"planner_kernel_unique_deps" check with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_kernel_create_library_depends_on_object_producers = fun _ctx ->
  let check tempdir =
    match load_repo_workspace () with
    | Error _ as err -> err
    | Ok repo_workspace ->
        let workspace =
          clone_workspace_with_target repo_workspace ~target_dir:Path.(tempdir / Path.v "target")
        in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        match plan_kernel_runtime_graphs ~workspace ~store ~build_ctx with
        | Error _ as err -> err
        | Ok (_, _, action_graph, _, _) ->
            match find_create_library_node action_graph with
            | None -> Error "missing kernel CreateLibrary action node"
            | Some create_node ->
                let producer_by_output = Collections.HashMap.create () in
                let () =
                  Riot_planner.Action_graph.nodes action_graph
                  |> List.for_each
                    ~fn:(fun (node: Riot_planner.Action_node.t) ->
                      List.for_each
                        (G.value node).outs
                        ~fn:(fun output ->
                          let _ =
                            Collections.HashMap.insert
                              producer_by_output
                              ~key:(Path.to_string output)
                              ~value:(G.id node)
                          in
                          ()))
                in
                let create_library_objects =
                  (G.value create_node).actions
                  |> List.filter_map
                    ~fn:(fun __tmp1 ->
                      match __tmp1 with
                      | Riot_planner.Action.CreateLibrary { objects; _ } -> Some objects
                      | _ -> None)
                  |> List.concat
                in
                let missing_edges =
                  List.filter_map
                    create_library_objects
                    ~fn:(fun object_path ->
                      let object_name = Path.to_string object_path in
                      match Collections.HashMap.get producer_by_output ~key:object_name with
                      | None -> Some (object_name ^ " has no producer")
                      | Some producer_id ->
                          if List.any (G.deps create_node) ~fn:(G.Node_id.eq producer_id) then
                            None
                          else
                            Some (object_name
                            ^ " produced by "
                            ^ G.Node_id.to_string producer_id
                            ^ " is not a CreateLibrary dependency"))
                in
                match missing_edges with
                | [] -> Ok ()
                | _ ->
                    Error ("expected CreateLibrary to depend on every object producer; missing: "
                    ^ String.concat ", " missing_edges)
  in
  match Fs.with_tempdir ~prefix:"planner_kernel_object_producers" check with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_kernel_unix_addr_interface_depends_on_sibling_modules = fun _ctx ->
  let check tempdir =
    match load_repo_workspace () with
    | Error _ as err -> err
    | Ok repo_workspace ->
        let workspace =
          clone_workspace_with_target repo_workspace ~target_dir:Path.(tempdir / Path.v "target")
        in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        match plan_kernel_runtime_graphs ~workspace ~store ~build_ctx with
        | Error _ as err -> err
        | Ok (_, _, action_graph, _, _) ->
            match find_action_node_by_source action_graph (Path.v "src/net/addr/unix.mli") with
            | None -> Error "expected compile action for src/net/addr/unix.mli"
            | Some unix_addr_node ->
                let dep_outputs = dependency_output_names_flat action_graph unix_addr_node in
                let has output = List.any dep_outputs ~fn:(String.equal output) in
                if not (has "Kernel__System_error.cmi") then
                  Error ("expected src/net/addr/unix.mli to depend on Kernel__System_error.cmi; deps: ["
                  ^ String.concat ", " dep_outputs
                  ^ "]")
                else if not (has "Kernel__Result.cmi") then
                  Error ("expected src/net/addr/unix.mli to depend on Kernel__Result.cmi; deps: ["
                  ^ String.concat ", " dep_outputs
                  ^ "]")
                else if not (has "Kernel__Net__Socket_addr.cmi") then
                  Error ("expected src/net/addr/unix.mli to depend on Kernel__Net__Socket_addr.cmi; deps: ["
                  ^ String.concat ", " dep_outputs
                  ^ "]")
                else
                  Ok ()
  in
  match Fs.with_tempdir ~prefix:"planner_kernel_unix_addr_interface_deps" check with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_suri_socket_pool_implementation_depends_on_acceptor = fun _ctx ->
  let check tempdir =
    match load_repo_workspace () with
    | Error _ as err -> err
    | Ok repo_workspace ->
        let workspace =
          clone_workspace_with_target repo_workspace ~target_dir:Path.(tempdir / Path.v "target")
        in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        match find_package_by_name workspace "suri" with
        | None -> Error "suri package not found in workspace"
        | Some package ->
            let unit_graph = runtime_unit_graph workspace in
            let unit_key = Riot_planner.Build_unit.key (library_unit package) in
            match plan_build_unit_from_graph ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
            | Error err -> Error ("suri runtime plan failed: " ^ err)
            | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; _ }) -> (
                match find_action_node_by_source
                  action_graph
                  (Path.v "src/socket_pool/socket_pool.ml") with
                | None -> Error "expected compile action for src/socket_pool/socket_pool.ml"
                | Some socket_pool_node ->
                    let dep_outputs = dependency_output_names_flat action_graph socket_pool_node in
                    let module_deps =
                      match G.topo_sort module_graph with
                      | Error _ -> [ "module graph cycle" ]
                      | Ok nodes ->
                          match List.find
                            nodes
                            ~fn:(fun (node: Riot_planner.Module_node.t G.node) ->
                              match (G.value node).kind with
                              | Riot_planner.Module_node.ML mod_ ->
                                  String.equal
                                    (Riot_model.Module.namespaced_name mod_)
                                    "Suri__Socket_pool"
                              | _ -> false) with
                          | None -> [ "missing Suri__Socket_pool module node" ]
                          | Some node -> module_dependency_labels module_graph node
                    in
                    let has output = List.any dep_outputs ~fn:(String.equal output) in
                    if not (has "Suri__Socket_pool__Acceptor.cmi") then
                      Error ("expected socket_pool.ml to depend on Suri__Socket_pool__Acceptor.cmi; deps: ["
                      ^ String.concat ", " dep_outputs
                      ^ "]; module deps: ["
                      ^ String.concat ", " module_deps
                      ^ "]")
                    else if not (has "Suri__Socket_pool__Acceptor.cmt") then
                      Error ("expected socket_pool.ml to compile after Suri__Socket_pool__Acceptor.cmt; deps: ["
                      ^ String.concat ", " dep_outputs
                      ^ "]; module deps: ["
                      ^ String.concat ", " module_deps
                      ^ "]")
                    else
                      Ok ()
              )
            | Ok _ -> Error "expected suri runtime plan to return Planned"
  in
  match Fs.with_tempdir ~prefix:"planner_suri_socket_pool_acceptor_deps" check with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_riot_cli_create_library_orders_run_before_install = fun _ctx ->
  let check tempdir =
    match load_repo_workspace () with
    | Error _ as err -> err
    | Ok repo_workspace ->
        let workspace =
          clone_workspace_with_target repo_workspace ~target_dir:Path.(tempdir / Path.v "target")
        in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        match find_package_by_name workspace "riot-cli" with
        | None -> Error "riot-cli package not found in workspace"
        | Some package ->
            let unit_graph = runtime_unit_graph workspace in
            let unit_key = Riot_planner.Build_unit.key (library_unit package) in
            match plan_build_unit_from_graph ~workspace ~store ~unit_graph ~unit_key ~build_ctx with
            | Error err -> Error ("riot-cli runtime plan failed: " ^ err)
            | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) -> (
                match find_create_library_objects action_graph with
                | Error _ as err -> err
                | Ok objects ->
                    require_order objects ~before:"Riot_cli__Run.cmx" ~after:"Riot_cli__Install.cmx"
              )
            | Ok _ -> Error "expected riot-cli runtime plan to return Planned"
  in
  match Fs.with_tempdir ~prefix:"planner_riot_cli_run_install_order" check with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_kernel_dependency_walk_snapshot = fun ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error err -> Error err
  | Ok (module_graph, live_objects, _cached_objects) ->
      let actual =
        String.concat
          "\n\n"
          [
            "MODULE GRAPH";
            render_module_graph_dependency_walk module_graph;
            "CREATE LIBRARY OBJECTS";
            String.concat "\n" (List.map live_objects ~fn:(fun object_ -> "- " ^ object_));
          ]
        ^ "\n"
      in
      Test.Snapshot.assert_text ~ctx ~actual

let test_legacy_kernel_plan_bundle_is_ignored_after_version_bump = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_kernel_legacy_bundle"
    (fun tempdir ->
      match load_repo_workspace () with
      | Error _ as err -> err
      | Ok repo_workspace ->
          let analysis_workspace =
            clone_workspace_with_target
              repo_workspace
              ~target_dir:Path.(tempdir / Path.v "analysis-target")
          in
          let test_workspace =
            clone_workspace_with_target
              repo_workspace
              ~target_dir:Path.(tempdir / Path.v "test-target")
          in
          let analysis_store = Riot_store.Store.create ~workspace:analysis_workspace in
          let test_store = Riot_store.Store.create ~workspace:test_workspace in
          let session_id = Riot_model.Session_id.make () in
          let profile = Riot_model.Profile.debug in
          let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
          match plan_kernel_runtime_graphs
            ~workspace:analysis_workspace
            ~store:analysis_store
            ~build_ctx with
          | Error _ as err -> err
          | Ok (package, _module_graph, action_graph, current_input_hash, depset) ->
              match Riot_store.Store.load_plan_bundle analysis_store ~hash:current_input_hash with
              | None -> Error "expected current kernel plan bundle to be persisted"
              | Some bundle ->
                  let stale_bundle =
                    rewrite_plan_bundle_action_graph
                      bundle
                      ~rewrite:(move_item_to_front "Kernel__Error.cmx")
                  in
                  let stale_input_hash =
                    Riot_planner.Package_planner.compute_input_hash
                      ~planner_version:legacy_planner_artifacts_version
                      ~depset
                      ~package
                      ~workspace:test_workspace
                      ~profile
                      ~build_ctx
                      ~toolchain:test_toolchain
                      ()
                  in
                  let _ =
                    Riot_store.Store.save_plan_bundle
                      test_store
                      ~hash:stale_input_hash
                      ~plan:stale_bundle
                    |> Result.expect ~msg:"expected legacy plan bundle save to succeed"
                  in
                  match plan_kernel_runtime_graphs
                    ~workspace:test_workspace
                    ~store:test_store
                    ~build_ctx with
                  | Error _ as err -> err
                  | Ok (_, _, replanned_action_graph, _, _) ->
                      match find_create_library_objects replanned_action_graph with
                      | Error _ as err -> err
                      | Ok objects ->
                          match require_order
                            objects
                            ~before:"Kernel__Net__Tcp_listener.cmx"
                            ~after:"Kernel__Error.cmx" with
                          | Error _ as err -> err
                          | Ok () ->
                              match require_order
                                objects
                                ~before:"Kernel__Net__Udp_socket.cmx"
                                ~after:"Kernel__Error.cmx" with
                              | Error _ as err -> err
                              | Ok () ->
                                  require_order
                                    objects
                                    ~before:"Kernel__Process.cmx"
                                    ~after:"Kernel__Error.cmx") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_legacy_krasny_plan_bundle_with_bad_root_module_is_ignored_after_version_bump = fun _ctx ->
  let rewrite_bad_krasny_root objects =
    List.map
      objects
      ~fn:(fun object_ ->
        if String.equal object_ "Krasny.cmx" then
          "Krasny__Krasny.cmx"
        else
          object_)
  in
  match Fs.with_tempdir
    ~prefix:"planner_krasny_legacy_bundle"
    (fun tempdir ->
      let package_root = Path.(tempdir / Path.v "krasny") in
      let src_dir = Path.(package_root / Path.v "src") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"expected krasny src dir creation to succeed"
      in
      let _ =
        Fs.write "let format value = value\n" Path.(src_dir / Path.v "Krasny.ml")
        |> Result.expect ~msg:"expected Krasny.ml write to succeed"
      in
      let _ =
        Fs.write (binary_main "ignore (Krasny.format \"ok\")") Path.(src_dir / Path.v "main.ml")
        |> Result.expect ~msg:"expected main.ml write to succeed"
      in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "krasny"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:package_root
          ~relative_path:(Path.v "krasny")
          ~library:{ path = Path.v "src/krasny.ml" }
          ~binaries:[ Riot_model.Package.{ name = "krasny"; path = Path.v "src/main.ml" } ]
          ~sources:{
            src = [ Path.v "src/Krasny.ml"; Path.v "src/main.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let analysis_workspace = make_test_workspace Path.(tempdir / Path.v "analysis") [ package ] in
      let test_workspace = make_test_workspace Path.(tempdir / Path.v "test") [ package ] in
      let analysis_store = Riot_store.Store.create ~workspace:analysis_workspace in
      let test_store = Riot_store.Store.create ~workspace:test_workspace in
      let session_id = Riot_model.Session_id.make () in
      let profile = Riot_model.Profile.debug in
      let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
      let unit_key = Riot_planner.Build_unit.key (library_unit package) in
      let analysis_unit_graph = runtime_unit_graph analysis_workspace in
      match plan_build_unit_from_graph
        ~workspace:analysis_workspace
        ~store:analysis_store
        ~unit_graph:analysis_unit_graph
        ~unit_key
        ~build_ctx with
      | Error _ as err -> err
      | Ok (Riot_planner.Package_planner.Planned { hash = current_input_hash; depset; _ }) -> (
          match Riot_store.Store.load_plan_bundle analysis_store ~hash:current_input_hash with
          | None -> Error "expected current krasny plan bundle to be persisted"
          | Some bundle ->
              let stale_bundle =
                rewrite_plan_bundle_action_graph bundle ~rewrite:rewrite_bad_krasny_root
              in
              let stale_input_hash =
                Riot_planner.Package_planner.compute_input_hash
                  ~planner_version:explicit_root_library_path_fix_planner_artifacts_version
                  ~depset
                  ~package
                  ~workspace:test_workspace
                  ~profile
                  ~build_ctx
                  ~toolchain:test_toolchain
                  ()
              in
              let _ =
                Riot_store.Store.save_plan_bundle
                  test_store
                  ~hash:stale_input_hash
                  ~plan:stale_bundle
                |> Result.expect ~msg:"expected legacy krasny plan bundle save to succeed"
              in
              let test_unit_graph = runtime_unit_graph test_workspace in
              match plan_build_unit_from_graph
                ~workspace:test_workspace
                ~store:test_store
                ~unit_graph:test_unit_graph
                ~unit_key
                ~build_ctx with
              | Error _ as err -> err
              | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) -> (
                  match find_create_library_objects action_graph with
                  | Error _ as err -> err
                  | Ok objects ->
                      if List.contains objects ~value:"Krasny__Krasny.cmx" then
                        Error ("expected stale krasny plan bundle to be ignored, got ["
                        ^ String.concat ", " objects
                        ^ "]")
                      else if List.contains objects ~value:"Krasny.cmx" then
                        Ok ()
                      else
                        Error ("expected replanned krasny bundle to include Krasny.cmx, got ["
                        ^ String.concat ", " objects
                        ^ "]")
                )
              | Ok _ -> Error "expected cached krasny plan to return Planned"
        )
      | Ok _ -> Error "expected analysis krasny plan to return Planned") with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.[
    case "runtime scope excludes dev-only roots" test_runtime_scope_excludes_dev_only_roots;
    case
      "build scope excludes runtime and dev roots"
      test_build_scope_excludes_runtime_and_dev_roots;
    case
      "dev scope test binaries include private helpers"
      test_dev_scope_test_binaries_include_private_helpers;
    case
      "dev scope no-library tests include package-named helpers"
      test_dev_scope_no_library_tests_include_package_named_helpers;
    case
      "dev scope tests can import own runtime library root"
      test_dev_scope_tests_can_import_own_runtime_library_root;
    case
      "dev scope example can use opened own runtime library exports"
      test_dev_scope_example_can_use_opened_own_runtime_library_exports;
    case
      "dev scope multiple binaries keep opened own runtime library exports"
      test_dev_scope_multiple_binaries_keep_opened_own_runtime_library_exports;
    case
      "dev scope record local open keeps opened runtime exports"
      test_dev_scope_record_local_open_keeps_opened_runtime_exports;
    case
      "direct dependency exports prefer direct depset package"
      test_direct_dependency_exports_prefer_direct_depset_package;
    case
      "dev scope example binaries include private helpers"
      test_dev_scope_example_binaries_include_private_helpers;
    case
      "dev scope bench binaries include private helpers"
      test_dev_scope_bench_binaries_include_private_helpers;
    case
      "dev scope keeps private helpers separated by root"
      test_dev_scope_keeps_private_helpers_separated_by_root;
    case
      "binary entrypoint accepts single labeled args main"
      test_binary_entrypoint_accepts_single_labeled_args_main;
    case
      "binary entrypoint rejects fun labeled args main"
      test_binary_entrypoint_rejects_fun_labeled_args_main;
    case "binary entrypoint requires main binding" test_binary_entrypoint_requires_main_binding;
    case
      "binary entrypoint rejects multiple main bindings"
      test_binary_entrypoint_rejects_multiple_main_bindings;
    case
      "binary entrypoint rejects positional args parameter"
      test_binary_entrypoint_rejects_positional_args_parameter;
    case
      "binary entrypoint rejects extra parameters"
      test_binary_entrypoint_rejects_extra_parameters;
    case
      ~size:Large
      "kernel input hash is not empty digest"
      test_kernel_input_hash_is_not_empty_digest;
    case
      "plan bundle cache hit restores module and action graphs"
      test_plan_bundle_cache_hit_restores_module_and_action_graphs;
    case
      "cached artifact and exports short-circuit without plan bundle"
      test_cached_artifact_and_exports_short_circuit_without_plan_bundle;
    case
      "cached artifact missing native objects rebuilds plan graphs"
      test_cached_artifact_missing_native_objects_rebuilds_plan_graphs;
    case
      "package input hash tracks dependency output hash"
      test_package_input_hash_tracks_dependency_output_hash;
    case
      "planned sandbox files link dependency native objects"
      test_planned_sandbox_files_link_dependency_native_objects;
    case
      "stale cached artifact version rebuilds plan graphs"
      test_stale_cached_artifact_version_rebuilds_plan_graphs;
    case
      "foreign hash version invalidates previously cached package"
      test_foreign_hash_version_rebuilds_plan_graphs;
    case
      "stale plan bundle version rebuilds plan graphs"
      test_stale_plan_bundle_version_rebuilds_plan_graphs;
    case
      "runtime plan keeps direct src files as library objects"
      test_runtime_plan_keeps_direct_src_files_as_library_objects;
    case
      "plan bundle with empty library objects rebuilds plan graphs"
      test_plan_bundle_with_empty_library_objects_rebuilds_plan_graphs;
    case
      "plan bundle cache hit preserves module dependency order"
      test_plan_bundle_cache_hit_preserves_module_dependency_order;
    case
      "underscore sibling module dependency is planned"
      test_underscore_sibling_module_dependency_is_planned;
    case
      "planner rejects direct internal library access"
      test_planner_rejects_direct_internal_library_access;
    case
      "planner rejects namespaced internal library access"
      test_planner_rejects_namespaced_internal_library_access;
    case
      "planner rejects direct other binary root access"
      test_planner_rejects_direct_other_binary_root_access;
    case
      "planner rejects namespaced other binary root access"
      test_planner_rejects_namespaced_other_binary_root_access;
    case
      "nested library interfaces depend on inherited aliases"
      test_nested_library_interfaces_depend_on_inherited_aliases;
    case
      "legacy nested sibling plan bundle is ignored after version bump"
      test_legacy_nested_sibling_plan_bundle_is_ignored_after_version_bump;
    case
      ~size:Large
      "kernel live CreateLibrary orders dependencies before Error"
      test_kernel_live_create_library_orders_dependencies_before_error;
    case
      ~size:Large
      "kernel unix addr interface keeps module graph dependencies"
      test_kernel_unix_addr_interface_keeps_module_graph_dependencies;
    case
      ~size:Large
      "kernel process interface keeps public child root dependency"
      test_kernel_process_interface_keeps_public_child_root_dependency;
    case
      ~size:Large
      "kernel plan bundle cache hit preserves live CreateLibrary order"
      test_kernel_plan_bundle_cache_hit_preserves_live_create_library_order;
    case
      ~size:Large
      "kernel CreateLibrary objects are topological"
      test_kernel_create_library_is_topological;
    case
      ~size:Large
      "kernel CreateLibrary dependencies are unique"
      test_kernel_create_library_dependencies_are_unique;
    case
      ~size:Large
      "kernel CreateLibrary depends on object producers"
      test_kernel_create_library_depends_on_object_producers;
    case
      ~size:Large
      "kernel unix addr interface depends on sibling modules"
      test_kernel_unix_addr_interface_depends_on_sibling_modules;
    case
      ~size:Large
      "suri socket_pool implementation depends on acceptor"
      test_suri_socket_pool_implementation_depends_on_acceptor;
    case
      ~size:Large
      "riot-cli CreateLibrary orders Run before Install"
      test_riot_cli_create_library_orders_run_before_install;
    case ~size:Large "kernel dependency walk snapshot" test_kernel_dependency_walk_snapshot;
    case
      ~size:Large
      "legacy kernel plan bundle is ignored after version bump"
      test_legacy_kernel_plan_bundle_is_ignored_after_version_bump;
    case
      "legacy krasny plan bundle with bad root module is ignored after version bump"
      test_legacy_krasny_plan_bundle_with_bad_root_module_is_ignored_after_version_bump;
  ]

let name = "Planner Package Planning Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
