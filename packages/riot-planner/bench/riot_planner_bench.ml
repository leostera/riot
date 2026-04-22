open Std
open Std.Bench
open Std.Collections
open Riot_model
module Package = Package
module Workspace = Workspace
module Package_graph = Riot_planner.Package_graph
module Workspace_planner = Riot_planner.Workspace_planner

let test_toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
|> Result.expect ~msg:"riot-planner bench toolchain init should succeed"

let workspace_dependency = fun name ->
  Package.{
    name = Package_name.from_string name
    |> Result.expect ~msg:("expected valid package name: " ^ name);
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

let write_file = fun path contents ->
  let parent =
    match Path.parent path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  let _ = Fs.create_dir_all parent |> Result.expect ~msg:"create bench parent should succeed" in
  Fs.write contents path |> Result.expect ~msg:"write bench file should succeed"

let make_workspace = fun ~root ~packages -> Workspace.make_realized ~root ~packages ()

let make_workspace_package = fun ~root ~name ~dependencies ~dev_dependencies ~build_dependencies ->
  Package.make
    ~name:(Package_name.from_string name
    |> Result.expect ~msg:("expected valid package name: " ^ name))
    ~path:Path.(root / Path.v "packages" / Path.v name)
    ~relative_path:(Path.v ("packages/" ^ name))
    ~dependencies:(List.map dependencies ~fn:workspace_dependency)
    ~dev_dependencies:(List.map dev_dependencies ~fn:workspace_dependency)
    ~build_dependencies:(List.map build_dependencies ~fn:workspace_dependency)
    ~library:{ path = Path.v "src/lib.ml" }
    ()

let make_workspace_fixture = fun root ~count ->
  let rec loop index acc =
    if index = count then
      List.reverse acc
    else
      let name = "pkg" ^ Int.to_string index in
      let dependencies =
        if index = 0 then
          []
        else
          [ "pkg" ^ Int.to_string (index - 1) ]
      in
      let dev_dependencies =
        if index > 1 && index mod 5 = 0 then
          [ "pkg" ^ Int.to_string (index - 2) ]
        else
          []
      in
      let build_dependencies =
        if index > 2 && index mod 7 = 0 then
          [ "pkg" ^ Int.to_string (index - 3) ]
        else
          []
      in
      loop
        (index + 1)
        (make_workspace_package ~root ~name ~dependencies ~dev_dependencies ~build_dependencies :: acc)
  in
  make_workspace ~root ~packages:(loop 0 [])

let make_package_graph_runtime_bench = fun root ~count ->
  let workspace = make_workspace_fixture
    Path.(root / Path.v ("runtime-graph-" ^ Int.to_string count))
    ~count in
  fun () ->
    let _ = Package_graph.create ~scope:Package_graph.Runtime workspace |> Result.expect ~msg:"runtime package graph bench should succeed" in
    ()

let make_package_graph_dev_bench = fun root ~count ->
  let workspace = make_workspace_fixture Path.(root / Path.v ("dev-graph-" ^ Int.to_string count)) ~count in
  fun () ->
    let _ = Package_graph.create ~scope:Package_graph.Dev workspace |> Result.expect ~msg:"dev package graph bench should succeed" in
    ()

let make_plan_workspace_all_bench = fun root ~count ->
  let workspace = make_workspace_fixture Path.(root / Path.v ("plan-all-" ^ Int.to_string count)) ~count in
  fun () ->
    let _ = Riot_planner.plan_workspace
      ~workspace
      ~target:Workspace_planner.All
      ~scope:Package_graph.Runtime
      ~load_errors:[]
      ~dev_artifacts:{ tests = true; examples = true; benches = true }
    |> Result.expect ~msg:"plan workspace all bench should succeed" in
    ()

let make_plan_workspace_target_bench = fun root ~count ->
  let workspace = make_workspace_fixture
    Path.(root / Path.v ("plan-target-" ^ Int.to_string count))
    ~count in
  let target_package = "pkg" ^ Int.to_string (count - 1) in
  fun () ->
    let _ = Riot_planner.plan_workspace
      ~workspace
      ~target:(Workspace_planner.Package (Package_name.from_string target_package
      |> Result.expect ~msg:("expected valid package name: " ^ target_package)))
      ~scope:Package_graph.Runtime
      ~load_errors:[]
      ~dev_artifacts:{ tests = true; examples = true; benches = true }
    |> Result.expect ~msg:"plan workspace target bench should succeed" in
    ()

type package_fixture = {
  workspace: Workspace.t;
  package: Package.t;
  store: Riot_store.Store.t;
  build_ctx: Riot_model.Build_ctx.t;
  package_key: Package.key;
}

let make_package_sources = fun package_name ->
  let root_source = "src/" ^ package_name ^ ".ml" in
  {
    Package.src = [
      Path.v root_source;
      Path.v "src/lexer.ml";
      Path.v "src/parser.ml";
      Path.v "src/types.ml";
      Path.v "src/types.mli";
      Path.v "src/utils.ml";
      Path.v "src/utils.mli";
    ];
    native = [];
    tests = [];
    examples = [];
    bench = [];
  }

let write_package_fixture_files = fun package_root package_name ->
  write_file Path.(package_root / Path.v "src" / Path.v (package_name ^ ".ml")) "let parse input = Parser.parse (Lexer.tokenize input)\n";
  write_file Path.(package_root / Path.v "src" / Path.v "lexer.ml") "let tokenize input = [ Utils.normalize input ]\n";
  write_file Path.(package_root / Path.v "src" / Path.v "parser.ml") "let parse tokens = Types.Token_stream tokens\n";
  write_file Path.(package_root / Path.v "src" / Path.v "types.ml") "type t = Token_stream of string list\n";
  write_file Path.(package_root / Path.v "src" / Path.v "types.mli") "type t = Token_stream of string list\n";
  write_file Path.(package_root / Path.v "src" / Path.v "utils.ml") "let normalize input = input\n";
  write_file Path.(package_root / Path.v "src" / Path.v "utils.mli") "val normalize : string -> string\n"

let make_package_fixture = fun root label ->
  let workspace_root = Path.(root / Path.v label) in
  let package_name = "planner_pkg" in
  let package_root = Path.(workspace_root / Path.v "packages" / Path.v package_name) in
  let _ = Fs.create_dir_all Path.(package_root / Path.v "src") |> Result.expect ~msg:"create planner bench src dir should succeed" in
  let _ = write_package_fixture_files package_root package_name in
  let package = Package.make
    ~name:(Package_name.from_string package_name
    |> Result.expect ~msg:("expected valid package name: " ^ package_name))
    ~path:package_root
    ~relative_path:(Path.v ("packages/" ^ package_name))
    ~library:{ path = Path.v ("src/" ^ package_name ^ ".ml") }
    ~sources:(make_package_sources package_name)
    () in
  let workspace = make_workspace ~root:workspace_root ~packages:[ package ] in
  let store = Riot_store.Store.create ~workspace in
  let build_ctx = Riot_model.Build_ctx.make
    ~session_id:(Riot_model.Session_id.make ())
    ~profile:Riot_model.Profile.debug
    () in
  let package_key = Package_graph.package_key ~package_name Package_graph.Runtime in
  {
    workspace;
    package;
    store;
    build_ctx;
    package_key;
  }

let run_package_plan = fun fixture ->
  let package_graph = Package_graph.create ~scope:Package_graph.Runtime fixture.workspace
  |> Result.expect ~msg:"package planning bench graph creation should succeed" in
  let _ = Riot_planner.Package_planner.plan_package
    ~workspace:fixture.workspace
    ~toolchain:test_toolchain
    ~store:fixture.store
    ~package_graph
    ~package_key:fixture.package_key
    ~package:fixture.package
    ~build_ctx:fixture.build_ctx
  |> Result.expect ~msg:"package planning bench should succeed" in
  ()

let make_plan_package_cold_bench = fun root ->
  let fixture_count = 48 in
  let fixtures =
    Array.init
      ~count:fixture_count
      ~fn:(fun index -> make_package_fixture root ("cold-package-" ^ Int.to_string index))
  in
  let cursor = ref 0 in
  fun () ->
    let fixture = Array.get_unchecked fixtures ~at:!cursor in
    cursor := !cursor + 1;
    run_package_plan fixture

let make_plan_package_warm_bench = fun root ->
  let fixture = make_package_fixture root "warm-package" in
  let _ = run_package_plan fixture in
  fun () -> run_package_plan fixture

let graph_config: Bench.bench_config = { iterations = 120; warmup = 12 }

let workspace_plan_config: Bench.bench_config = { iterations = 80; warmup = 10 }

let package_plan_cold_config: Bench.bench_config = { iterations = 24; warmup = 8 }

let package_plan_warm_config: Bench.bench_config = { iterations = 60; warmup = 10 }

let benchmark_suite = fun root ->
  Bench.[
    with_config
      ~config:graph_config
      "riot-planner package graph runtime 64 packages"
      (make_package_graph_runtime_bench root ~count:64);
    with_config
      ~config:graph_config
      "riot-planner package graph dev 64 packages"
      (make_package_graph_dev_bench root ~count:64);
    with_config
      ~config:workspace_plan_config
      "riot-planner plan workspace all 128 packages"
      (make_plan_workspace_all_bench root ~count:128);
    with_config
      ~config:workspace_plan_config
      "riot-planner plan workspace target package 128 packages"
      (make_plan_workspace_target_bench root ~count:128);
    with_config
      ~config:package_plan_cold_config
      "riot-planner plan package cold explicit root library"
      (make_plan_package_cold_bench root);
    with_config
      ~config:package_plan_warm_config
      "riot-planner plan package warm explicit root library"
      (make_plan_package_warm_bench root);
  ]

let () =
  Runtime.run
    ~main:(fun ~args ->
      match Fs.with_tempdir
        ~prefix:"riot_planner_bench"
        (fun root ->
          Bench.Cli.main ~name:"riot-planner benchmarks" ~benchmarks:(benchmark_suite root) ~args) with
      | Ok result -> result
      | Error err -> panic ("failed to prepare riot-planner bench fixture: " ^ IO.error_message err))
    ~args:Env.args
    ()
