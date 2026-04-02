open Std
module Test = Std.Test
module Workspace_planner = Tusk_planner.Workspace_planner
module Package = Tusk_model.Package
module Workspace = Tusk_model.Workspace

let dependency = fun name ->
  Package.{ name; source = { workspace = true; builtin = false; path = None; version = None } }

let make_package = fun ?(dependencies = []) ?(dev_dependencies = []) ?(build_dependencies = []) name ->
  Package.{
    name;
    path = Path.v ("packages/" ^ name);
    relative_path = Path.v ("packages/" ^ name);
    dependencies = List.map dependency dependencies;
    dev_dependencies = List.map dependency dev_dependencies;
    build_dependencies = List.map dependency build_dependencies;
    foreign_dependencies = [];
    binaries = [];
    library = Some { path = Path.v "src/lib.ml" };
    sources =
      {
        src = [];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      };
    compiler = { profile_overrides = []; target_overrides = [] };
    commands = [];
    fix_providers = [];
    publish = { version = None; description = None; license = None; is_public = None };
  }

let make_workspace = fun packages ->
  Workspace.{
    root = Path.v "/tmp/workspace_planner_target_tests";
    target_dir_root = Path.v "/tmp/workspace_planner_target_tests/_build";
    packages;
    profile_overrides = []
  }

let plan_workspace = fun workspace target scope ->
  Workspace_planner.plan_workspace ~workspace ~target ~scope ~load_errors:[]

let package_names = fun plan ->
  Workspace_planner.packages_in_plan plan |> List.map (fun (pkg: Package.t) -> pkg.name)

let plan_all_runtime_returns_workspace_like_order = fun _ctx ->
  let workspace = make_workspace
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "std"; "kernel" ] "miniriot";
      make_package ~dependencies:[ "std"; "miniriot" ] "tusk-model";
      make_package ~dependencies:[ "std"; "tusk-model" ] "tusk-planner";
      make_package ~dependencies:[ "std"; "tusk-model"; "tusk-planner" ] "tusk-store";
      make_package ~dependencies:[ "std"; "tusk-model"; "tusk-planner"; "tusk-store" ] "tusk-build";
    ] in
  match plan_workspace workspace All Runtime with
  | Error _ -> Error "expected successful workspace plan"
  | Ok plan ->
      let names = package_names plan in
      let position name = List.find_index (String.equal name) names
      |> Option.expect ~msg:(("missing package in plan: " ^ name)) in
      Test.assert_true (position "std" < position "kernel");
      Test.assert_true (position "kernel" < position "miniriot");
      Test.assert_true (position "miniriot" < position "tusk-model");
      Test.assert_true (position "tusk-model" < position "tusk-planner");
      Test.assert_true (position "tusk-planner" < position "tusk-store");
      Test.assert_true (position "tusk-store" < position "tusk-build");
      Ok ()

let plan_single_package_includes_only_transitive_closure = fun _ctx ->
  let workspace = make_workspace
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "kernel" ] "a";
      make_package ~dependencies:[ "a" ] "app";
      make_package ~dependencies:[ "std" ] "unrelated";
    ] in
  match plan_workspace workspace (Package "app") Runtime with
  | Error _ -> Error "expected successful package-target plan"
  | Ok plan ->
      let names = package_names plan |> List.sort_uniq String.compare in
      Test.assert_equal ~expected:[ "a"; "app"; "kernel"; "std" ] ~actual:names;
      Ok ()

let plan_multiple_packages_includes_union_of_dependencies = fun _ctx ->
  let workspace = make_workspace
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "kernel" ] "a";
      make_package ~dependencies:[ "std" ] "b";
      make_package ~dependencies:[ "a" ] "app";
      make_package ~dependencies:[ "b" ] "tool";
      make_package ~dependencies:[ "std" ] "unrelated";
    ] in
  match plan_workspace workspace (Packages [ "app"; "tool" ]) Runtime with
  | Error _ -> Error "expected successful multi-package plan"
  | Ok plan ->
      let names = package_names plan |> List.sort_uniq String.compare in
      Test.assert_equal ~expected:[ "a"; "app"; "b"; "kernel"; "std"; "tool" ] ~actual:names;
      Ok ()

let plan_unknown_package_reports_available_packages = fun _ctx ->
  let workspace = make_workspace [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ] in
  match plan_workspace workspace (Package "missing") Runtime with
  | Ok _ ->
      Error "expected PackageNotFound"
  | Error (PackageNotFound { name; available }) ->
      Test.assert_equal ~expected:"missing" ~actual:name;
      Test.assert_equal ~expected:[ "app"; "std" ] ~actual:(List.sort String.compare available);
      Ok ()
  | Error _ ->
      Error "expected PackageNotFound"

let plan_multiple_unknown_packages_reports_all_missing_names = fun _ctx ->
  let workspace = make_workspace [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ] in
  match plan_workspace workspace (Packages [ "missing-a"; "app"; "missing-b" ]) Runtime with
  | Ok _ ->
      Error "expected PackagesNotFound"
  | Error (PackagesNotFound { names; available }) ->
      Test.assert_equal ~expected:[ "missing-a"; "missing-b" ] ~actual:names;
      Test.assert_equal ~expected:[ "app"; "std" ] ~actual:(List.sort String.compare available);
      Ok ()
  | Error _ ->
      Error "expected PackagesNotFound"

let plan_reports_missing_dependencies_before_sorting = fun _ctx ->
  let workspace = make_workspace [ make_package ~dependencies:[ "missing-lib" ] "app" ] in
  match plan_workspace workspace All Runtime with
  | Ok _ ->
      Error "expected MissingDependencies"
  | Error (MissingDependencies { missing }) ->
      let entries =
        List.map
          (fun (item: Tusk_planner.Package_graph.missing_dependency) -> item.package ^ "->" ^ item.dependency)
          missing
      in
      Test.assert_equal ~expected:[ "app->missing-lib" ] ~actual:entries;
      Ok ()
  | Error _ ->
      Error "expected MissingDependencies"

let tests =
  Test.[
    case "plan all runtime returns workspace-like order" plan_all_runtime_returns_workspace_like_order;
    case "plan single package includes only transitive closure" plan_single_package_includes_only_transitive_closure;
    case "plan multiple packages includes union of dependencies" plan_multiple_packages_includes_union_of_dependencies;
    case "plan unknown package reports available packages" plan_unknown_package_reports_available_packages;
    case "plan multiple unknown packages report all missing names" plan_multiple_unknown_packages_reports_all_missing_names;
    case "plan reports missing dependencies before sorting" plan_reports_missing_dependencies_before_sorting;
  ]

let name = "tusk-planner:workspace-planner-targets"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
