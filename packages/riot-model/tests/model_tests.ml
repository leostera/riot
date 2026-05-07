open Std
open Std.Result.Syntax
open Riot_model

module Test = Std.Test

let package_name = fun name ->
  Package_name.from_string name
  |> Result.expect ~msg:("Expected valid package name: " ^ name)

let source = fun ?(workspace = false) ?(builtin = false) ?path ?source_locator ?ref_ ?version () ->
  Riot_model.Package.{
    workspace;
    builtin;
    path;
    source_locator;
    ref_;
    version;
  }

let make_command = fun () ->
  Package_command.{
    name = "demo";
    description = "Run the demo";
    package_name = package_name "minttea";
    package_path = Path.v "packages/minttea";
    command_module = "Demo_cmd";
    command_source = Path.v "src/demo_cmd.ml";
    command_binary = Path.v "_build/debug/out/minttea/demo";
  }

let make_package = fun () ->
  let command = make_command () in
  let publish =
    Package.{
      version = Some (Std.Version.make ~major:0 ~minor:1 ~patch:0 ());
      description = Some "minttea";
      license = Some "Apache-2.0";
      is_public = Some true;
    }
  in
  Package.make
    ~name:(package_name "minttea")
    ~path:(Path.v "packages/minttea")
    ~relative_path:(Path.v "packages/minttea")
    ~dependencies:[ { name = package_name "std"; source = source ~workspace:true () } ]
    ~dev_dependencies:[ { name = package_name "propane"; source = source ~workspace:true () } ]
    ~build_dependencies:[ { name = package_name "std"; source = source ~workspace:true () } ]
    ~binaries:[ { name = "demo-bin"; path = Path.v "src/demo_bin.ml" } ]
    ~library:{ path = Path.v "src/minttea.ml" }
    ~sources:{
      src = [ Path.v "src/minttea.ml"; Path.v "src/demo_cmd.ml" ];
      native = [];
      tests = [ Path.v "tests/model_tests.ml" ];
      examples = [];
      bench = [];
    }
    ~commands:[ command ]
    ~publish
    ()

let hash_of_package = fun pkg ->
  let state = Crypto.Sha256.create () in
  Package.hash state pkg;
  Crypto.Sha256.finish state

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error _ -> Error "Tempdir creation failed"

let path_error_message = fun __tmp1 ->
  match __tmp1 with
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } ->
      "system call '" ^ syscall ^ "' returned invalid UTF-8 path: " ^ path
  | Path.SystemError error -> error

let test_build_scope_drops_commands_and_runtime_outputs = fun _ctx ->
  let pkg = make_package () in
  let projected = Package.for_scope Package.Build pkg in
  let no_commands = projected.commands = [] in
  let no_binaries = projected.binaries = [] in
  let no_library = projected.library = None in
  let no_runtime_deps = projected.dependencies = [] in
  let no_dev_deps = projected.dev_dependencies = [] in
  if no_commands && no_binaries && no_library && no_runtime_deps && no_dev_deps then
    Ok ()
  else
    Error "build scope should drop commands, binaries, library, and non-build deps"

let test_runtime_scope_keeps_commands = fun _ctx ->
  let pkg = make_package () in
  let projected = Package.for_scope Package.Normal pkg in
  if List.length projected.commands = 1 && List.length projected.binaries = 1 then
    Ok ()
  else
    Error "runtime scope should preserve package commands and normal binaries"

let test_dev_scope_keeps_only_dev_outputs = fun _ctx ->
  let pkg = make_package () in
  let projected = Package.for_scope Package.Dev pkg in
  let no_library = projected.library = None in
  let no_commands = projected.commands = [] in
  let no_runtime_sources = projected.sources.src = [] && projected.sources.native = [] in
  let kept_dev_deps =
    (
      projected.dev_dependencies
      |> List.map ~fn:(fun (dep: Package.dependency) -> dep.name)
    )
    = [ package_name "propane" ]
  in
  let kept_runtime_deps =
    (
      projected.dependencies
      |> List.map ~fn:(fun (dep: Package.dependency) -> dep.name)
    )
    = [ package_name "std" ]
  in
  let kept_build_deps =
    (
      projected.build_dependencies
      |> List.map ~fn:(fun (dep: Package.dependency) -> dep.name)
    )
    = [ package_name "std" ]
  in
  let no_normal_binaries =
    projected.binaries
    |> List.all
      ~fn:(fun (bin: Riot_model.Package.binary) ->
        String.starts_with ~prefix:"tests/" (Path.to_string bin.path)
        || String.starts_with ~prefix:"examples/" (Path.to_string bin.path)
        || String.starts_with ~prefix:"bench/" (Path.to_string bin.path))
  in
  if
    no_library
    && no_commands
    && no_runtime_sources
    && kept_dev_deps
    && kept_runtime_deps
    && kept_build_deps
    && no_normal_binaries
  then
    Ok ()
  else
    Error "dev scope should reuse runtime deps while keeping only dev outputs"

let test_for_binary_keeps_only_selected_dev_source = fun _ctx ->
  let pkg =
    Package.make
      ~name:(package_name "minttea")
      ~path:(Path.v "packages/minttea")
      ~relative_path:(Path.v "packages/minttea")
      ~dependencies:[ { name = package_name "std"; source = source ~workspace:true () } ]
      ~dev_dependencies:[ { name = package_name "propane"; source = source ~workspace:true () } ]
      ~build_dependencies:[]
      ~binaries:[
        { name = "one_tests"; path = Path.v "tests/one_tests.ml" };
        { name = "two_tests"; path = Path.v "tests/two_tests.ml" };
        { name = "demo"; path = Path.v "examples/demo.ml" };
        { name = "other"; path = Path.v "examples/other.ml" };
        { name = "throughput"; path = Path.v "bench/throughput.ml" };
        { name = "latency"; path = Path.v "bench/latency.ml" };
      ]
      ~library:{ path = Path.v "src/minttea.ml" }
      ~sources:{
        src = [ Path.v "src/minttea.ml" ];
        native = [];
        tests = [
          Path.v "tests/one_tests.ml";
          Path.v "tests/two_tests.ml";
          Path.v "tests/support.ml";
        ];
        examples = [
          Path.v "examples/demo.ml";
          Path.v "examples/other.ml";
          Path.v "examples/support.ml";
        ];
        bench = [
          Path.v "bench/throughput.ml";
          Path.v "bench/latency.ml";
          Path.v "bench/support.ml";
        ];
      }
      ()
  in
  let expect_sources binary_name ~tests ~examples ~bench =
    match Package.for_binary ~binary_name pkg with
    | None -> Error ("expected binary projection for " ^ binary_name)
    | Some projected ->
        if
          projected.sources.tests = tests
          && projected.sources.examples = examples
          && projected.sources.bench = bench
        then
          Ok ()
        else
          let render_paths = fun paths ->
            paths
            |> List.map ~fn:Path.to_string
            |> String.concat ", "
          in
          Error
            ("unexpected projected sources for "
            ^ binary_name
            ^ ": tests=["
            ^ render_paths projected.sources.tests
            ^ "] examples=["
            ^ render_paths projected.sources.examples
            ^ "] bench=["
            ^ render_paths projected.sources.bench
            ^ "]")
  in
  let* () =
    expect_sources
      "one_tests"
      ~tests:[ Path.v "tests/one_tests.ml"; Path.v "tests/support.ml" ]
      ~examples:[]
      ~bench:[]
  in
  let* () =
    expect_sources
      "demo"
      ~tests:[]
      ~examples:[ Path.v "examples/demo.ml"; Path.v "examples/support.ml" ]
      ~bench:[]
  in
  expect_sources
    "throughput"
    ~tests:[]
    ~examples:[]
    ~bench:[ Path.v "bench/support.ml"; Path.v "bench/throughput.ml" ]

let test_runtime_scope_keeps_build_dependencies_for_hashing = fun _ctx ->
  let pkg = make_package () in
  let projected = Package.for_scope Package.Normal pkg in
  let kept_build_deps =
    (
      projected.build_dependencies
      |> List.map ~fn:(fun (dep: Package.dependency) -> dep.name)
    )
    = [ package_name "std" ]
  in
  if kept_build_deps then
    Ok ()
  else
    Error "runtime scope should preserve build dependencies as cache inputs"

let test_package_hash_changes_when_build_dependency_path_changes = fun _ctx ->
  let with_build_dependency path =
    let command = make_command () in
    let publish =
      Package.{
        version = Some (Std.Version.make ~major:0 ~minor:1 ~patch:0 ());
        description = Some "minttea";
        license = Some "Apache-2.0";
        is_public = Some true;
      }
    in
    Package.make
      ~name:(package_name "minttea")
      ~path:(Path.v "packages/minttea")
      ~relative_path:(Path.v "packages/minttea")
      ~dependencies:[ { name = package_name "std"; source = source ~workspace:true () } ]
      ~dev_dependencies:[
        { name = package_name "propane"; source = source ~workspace:true () };
      ]
      ~build_dependencies:[
        { name = package_name "fixme"; source = source ~path:(Path.v path) () };
      ]
      ~binaries:[ { name = "demo-bin"; path = Path.v "src/demo_bin.ml" } ]
      ~library:{ path = Path.v "src/minttea.ml" }
      ~sources:{
        src = [ Path.v "src/minttea.ml"; Path.v "src/demo_cmd.ml" ];
        native = [];
        tests = [ Path.v "tests/model_tests.ml" ];
        examples = [];
        bench = [];
      }
      ~commands:[ command ]
      ~publish
      ()
  in
  let first = with_build_dependency "../tools/fixme-one" in
  let second = with_build_dependency "../tools/fixme-two" in
  if Crypto.Hash.equal (hash_of_package first) (hash_of_package second) then
    Error "expected Package.hash to change when build dependency path changes"
  else
    Ok ()

let test_declared_example_binaries_suppress_example_autodiscovery = fun _ctx ->
  with_tempdir
    "riot_model_package"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let examples_dir = Path.(tmpdir / Path.v "examples") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all examples_dir) ~msg:"Failed to create examples directory";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write library source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "test_https_httpbin.ml"))
        ~msg:"Failed to write explicit example";
      Result.expect
        (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "simple_https.ml"))
        ~msg:"Failed to write autodiscovered example";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"

[[bin]]
name = "test_https_httpbin"
path = "examples/test_https_httpbin.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      let binary_names =
        pkg.binaries
        |> List.map ~fn:(fun (bin: Riot_model.Package.binary) -> bin.name)
        |> List.sort ~compare:String.compare
      in
      match binary_names with
      | [ "test_https_httpbin" ] -> Ok ()
      | _ ->
          Error ("expected declared example binaries to suppress same-bucket autodiscovery, got: ["
          ^ String.concat ", " binary_names
          ^ "]"))

let test_declared_runtime_binaries_suppress_main_autodiscovery = fun _ctx ->
  with_tempdir
    "riot_model_declared_runtime_bin"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect
        (Fs.write "let () = ()\n" Path.(src_dir / Path.v "main.ml"))
        ~msg:"Failed to write main source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(src_dir / Path.v "custom.ml"))
        ~msg:"Failed to write custom source";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "hello-world"
version = "0.1.0"

[[bin]]
name = "custom"
path = "src/custom.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v ".")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      let binary_names =
        pkg.binaries
        |> List.map ~fn:(fun (bin: Riot_model.Package.binary) -> bin.name)
        |> List.sort ~compare:String.compare
      in
      match binary_names with
      | [ "custom" ] -> Ok ()
      | _ ->
          Error ("expected declared runtime binaries to suppress src/main.ml autodiscovery, got: ["
          ^ String.concat ", " binary_names
          ^ "]"))

let test_src_main_autodiscovers_runtime_binary = fun _ctx ->
  with_tempdir
    "riot_model_main_binary"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect
        (Fs.write "let () = ()\n" Path.(src_dir / Path.v "main.ml"))
        ~msg:"Failed to write main source";
      let manifest =
        Std.Data.Toml.parse {|
[package]
name = "hello-world"
version = "0.1.0"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v ".")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      match pkg.binaries with
      | [ Riot_model.Package.{ name; path } ] ->
          if String.equal name "hello-world" && Path.equal path (Path.v "src/main.ml") then
            Ok ()
          else
            Error ("expected src/main.ml to autodiscover hello-world runtime binary, got "
            ^ name
            ^ " at "
            ^ Path.to_string path)
      | binaries ->
          Error ("expected exactly one autodiscovered runtime binary, got "
          ^ Int.to_string (List.length binaries)))

let test_scan_sources_ignores_hidden_entries = fun _ctx ->
  with_tempdir
    "riot_model_hidden_sources"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let native_dir = Path.(tmpdir / Path.v "native") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all native_dir) ~msg:"Failed to create native directory";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write visible source";
      Result.expect
        (Fs.write "junk\n" Path.(src_dir / Path.v "._demo.ml"))
        ~msg:"Failed to write hidden source";
      Result.expect
        (Fs.write "junk\n" Path.(native_dir / Path.v "._demo.c"))
        ~msg:"Failed to write hidden native source";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      if pkg.sources.src = [ Path.v "src/demo.ml" ] && pkg.sources.native = [] then
        Ok ()
      else
        Error "expected hidden source entries to be ignored")

let test_scan_sources_ignores_test_support_entries = fun _ctx ->
  with_tempdir
    "riot_model_test_support_sources"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let fixtures_dir = Path.(tests_dir / Path.v "fixtures") in
      let diagnostics_dir = Path.(tests_dir / Path.v "diagnostics") in
      let generated_dir = Path.(tests_dir / Path.v "generated") in
      let autofix_fixtures_dir = Path.(tests_dir / Path.v "autofix_fixtures") in
      let workspace_fixtures_dir = Path.(tests_dir / Path.v "workspace_fixtures") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all fixtures_dir) ~msg:"Failed to create fixtures directory";
      Result.expect
        (Fs.create_dir_all diagnostics_dir)
        ~msg:"Failed to create diagnostics directory";
      Result.expect (Fs.create_dir_all generated_dir) ~msg:"Failed to create generated directory";
      Result.expect
        (Fs.create_dir_all autofix_fixtures_dir)
        ~msg:"Failed to create autofix fixtures directory";
      Result.expect
        (Fs.create_dir_all workspace_fixtures_dir)
        ~msg:"Failed to create workspace fixtures directory";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write visible source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write real test source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(fixtures_dir / Path.v "0056_bad-module-name.ml"))
        ~msg:"Failed to write fixture source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(diagnostics_dir / Path.v "broken.ml"))
        ~msg:"Failed to write diagnostics source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(generated_dir / Path.v "generated.ml"))
        ~msg:"Failed to write generated source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(autofix_fixtures_dir / Path.v "rewrite.ml"))
        ~msg:"Failed to write autofix fixture source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(workspace_fixtures_dir / Path.v "nested.ml"))
        ~msg:"Failed to write workspace fixture source";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          ~source_ignore_patterns:[
            "tests/fixtures";
            "tests/generated";
            "tests/diagnostics";
            "tests/autofix_fixtures";
            "tests/workspace_fixtures";
          ]
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      if
        pkg.sources.tests = [ Path.v "tests/demo_tests.ml" ]
        && pkg.binaries
        = [ Riot_model.Package.{ name = "demo_tests"; path = Path.v "tests/demo_tests.ml" } ]
      then
        Ok ()
      else
        Error "expected test support entries under tests/fixtures|generated|diagnostics|autofix_fixtures|workspace_fixtures to be ignored")

let test_scan_sources_keeps_similarly_named_test_directories = fun _ctx ->
  with_tempdir
    "riot_model_test_support_boundary_sources"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let fixtures_dir = Path.(tests_dir / Path.v "fixtures") in
      let fixtures_generated_dir = Path.(tests_dir / Path.v "fixtures-generated") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all fixtures_dir) ~msg:"Failed to create fixtures directory";
      Result.expect
        (Fs.create_dir_all fixtures_generated_dir)
        ~msg:"Failed to create similarly named fixtures directory";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write visible source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write real test source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(fixtures_dir / Path.v "ignored.ml"))
        ~msg:"Failed to write ignored fixture source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(fixtures_generated_dir / Path.v "keep.ml"))
        ~msg:"Failed to write similarly named test source";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          ~source_ignore_patterns:[ "tests/fixtures" ]
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      let expected_tests = [
        Path.v "tests/demo_tests.ml";
        Path.v "tests/fixtures-generated/keep.ml";
      ]
      in
      if pkg.sources.tests = expected_tests then
        Ok ()
      else
        Error "expected only exact test support directories to be ignored")

let test_scan_sources_keeps_source_directories_matching_test_support_names = fun _ctx ->
  with_tempdir
    "riot_model_source_diagnostics_sources"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let src_diagnostics_dir = Path.(src_dir / Path.v "diagnostics") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let tests_diagnostics_dir = Path.(tests_dir / Path.v "diagnostics") in
      Result.expect
        (Fs.create_dir_all src_diagnostics_dir)
        ~msg:"Failed to create source diagnostics directory";
      Result.expect
        (Fs.create_dir_all tests_diagnostics_dir)
        ~msg:"Failed to create test diagnostics directory";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write visible source";
      Result.expect
        (Fs.write "let check = 1\n" Path.(src_diagnostics_dir / Path.v "check.ml"))
        ~msg:"Failed to write source diagnostics module";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write test source";
      Result.expect
        (Fs.write "let broken = true\n" Path.(tests_diagnostics_dir / Path.v "broken.ml"))
        ~msg:"Failed to write test diagnostics fixture";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          ~source_ignore_patterns:[ "tests/diagnostics" ]
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      if
        pkg.sources.src = [ Path.v "src/demo.ml"; Path.v "src/diagnostics/check.ml" ]
        && pkg.sources.tests = [ Path.v "tests/demo_tests.ml" ]
      then
        Ok ()
      else
        Error "expected scoped test diagnostics ignore to preserve src/diagnostics modules")

let test_scan_sources_respects_package_root_gitignore = fun _ctx ->
  with_tempdir
    "riot_model_gitignore_sources"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let generated_dir = Path.(src_dir / Path.v "generated") in
      let gitignore = Path.(tmpdir / Path.v ".gitignore") in
      Result.expect (Fs.create_dir_all generated_dir) ~msg:"Failed to create generated directory";
      Result.expect (Fs.write "generated/\n" gitignore) ~msg:"Failed to write gitignore";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write visible source";
      Result.expect
        (Fs.write "let generated = 1\n" Path.(generated_dir / Path.v "skip.ml"))
        ~msg:"Failed to write ignored generated source";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      if pkg.sources.src = [ Path.v "src/demo.ml" ] then
        Ok ()
      else
        Error "expected package-root gitignore entries to prune source scanning")

let test_scan_sources_ignores_non_ocaml_files = fun _ctx ->
  with_tempdir
    "riot_model_non_ocaml_sources"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let examples_dir = Path.(tmpdir / Path.v "examples") in
      let bench_dir = Path.(tmpdir / Path.v "bench") in
      let native_dir = Path.(tmpdir / Path.v "native") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all tests_dir) ~msg:"Failed to create tests directory";
      Result.expect (Fs.create_dir_all examples_dir) ~msg:"Failed to create examples directory";
      Result.expect (Fs.create_dir_all bench_dir) ~msg:"Failed to create bench directory";
      Result.expect (Fs.create_dir_all native_dir) ~msg:"Failed to create native directory";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write src source";
      Result.expect
        (Fs.write "not ocaml\n" Path.(src_dir / Path.v "README.txt"))
        ~msg:"Failed to write src non-ocaml file";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write test source";
      Result.expect
        (Fs.write "print('hi')\n" Path.(tests_dir / Path.v "fixture_audit.py"))
        ~msg:"Failed to write test non-ocaml file";
      Result.expect
        (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "demo.ml"))
        ~msg:"Failed to write example source";
      Result.expect
        (Fs.write "#!/bin/sh\n" Path.(examples_dir / Path.v "demo.sh"))
        ~msg:"Failed to write example non-ocaml file";
      Result.expect
        (Fs.write "let () = ()\n" Path.(bench_dir / Path.v "demo_bench.ml"))
        ~msg:"Failed to write bench source";
      Result.expect
        (Fs.write "#!/bin/sh\n" Path.(bench_dir / Path.v "bench.sh"))
        ~msg:"Failed to write bench non-ocaml file";
      Result.expect
        (Fs.write "int demo(void) { return 1; }\n" Path.(native_dir / Path.v "demo.c"))
        ~msg:"Failed to write native source";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      if
        pkg.sources.src = [ Path.v "src/demo.ml" ]
        && pkg.sources.tests = [ Path.v "tests/demo_tests.ml" ]
        && pkg.sources.examples = [ Path.v "examples/demo.ml" ]
        && pkg.sources.bench = [ Path.v "bench/demo_bench.ml" ]
        && pkg.sources.native = [ Path.v "native/demo.c" ]
      then
        Ok ()
      else
        Error "expected non-OCaml files outside native/ to be ignored during source scanning")

let test_scan_sources_ignores_deps_fixture_support_entries = fun _ctx ->
  with_tempdir
    "riot_model_deps_fixture_sources"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let tests_dir = Path.(tmpdir / Path.v "tests") in
      let deps_fixtures_dir = Path.(tests_dir / Path.v "deps_fixtures") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect
        (Fs.create_dir_all deps_fixtures_dir)
        ~msg:"Failed to create deps_fixtures directory";
      Result.expect
        (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml"))
        ~msg:"Failed to write src source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(tests_dir / Path.v "demo_tests.ml"))
        ~msg:"Failed to write test source";
      Result.expect
        (Fs.write "let fixture = 1\n" Path.(deps_fixtures_dir / Path.v "sample.ml"))
        ~msg:"Failed to write deps fixture source";
      Result.expect
        (Fs.write "module Sample : sig end\n" Path.(deps_fixtures_dir / Path.v "sample.mli"))
        ~msg:"Failed to write deps fixture interface";
      Result.expect
        (Fs.write "fixture\n" Path.(deps_fixtures_dir / Path.v "sample.expected"))
        ~msg:"Failed to write deps fixture support file";
      let manifest =
        Std.Data.Toml.parse
          {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"
|}
        |> Result.expect ~msg:"Expected package TOML to parse"
      in
      let pkg =
        Riot_model.Package.from_toml
          ~source_ignore_patterns:[ "tests/deps_fixtures" ]
          manifest
          ~workspace_deps:[]
          ~workspace_dev_deps:[]
          ~workspace_build_deps:[]
          ~path:tmpdir
          ~relative_path:(Path.v "packages/demo")
        |> Result.expect ~msg:"Expected package manifest to parse"
      in
      if
        pkg.sources.tests = [ Path.v "tests/demo_tests.ml" ]
        && pkg.binaries
        = [ Riot_model.Package.{ name = "demo_tests"; path = Path.v "tests/demo_tests.ml" } ]
      then
        Ok ()
      else
        Error "expected tests/deps_fixtures to be treated as non-compilable support input")

let test_workspace_fmt_ignore_parses = fun _ctx ->
  let toml =
    Std.Data.Toml.parse
      {|
[workspace]
members = ["packages/demo"]

[riot.fmt]
ignore = ["fixtures", "generated"]
|}
    |> Result.expect ~msg:"expected workspace TOML to parse"
  in
  let config = Riot_model.Fmt_config.from_toml toml in
  Test.assert_equal ~expected:[ "fixtures"; "generated" ] ~actual:config.ignore_patterns;
  Ok ()

let test_workspace_source_ignore_parses = fun _ctx ->
  let toml =
    Std.Data.Toml.parse
      {|
[workspace]
members = ["packages/demo"]
ignore = ["fixtures", "diagnostics"]
|}
    |> Result.expect ~msg:"expected workspace TOML to parse"
  in
  let manifest =
    Riot_model.Workspace_manifest.from_toml toml
    |> Result.expect ~msg:"expected workspace manifest to parse"
  in
  Test.assert_equal
    ~expected:[ "fixtures"; "diagnostics" ]
    ~actual:manifest.source_ignore_patterns;
  Ok ()

let test_package_fmt_ignore_loads = fun _ctx ->
  with_tempdir
    "riot_model_fmt_config"
    (fun tmpdir ->
      let manifest_path = Path.(tmpdir / Path.v "riot.toml") in
      Fs.write
        {|
[package]
name = "demo"
version = "0.1.0"

[riot.fmt]
ignore = ["tests/fixtures", "vendor"]
|}
        manifest_path
      |> Result.expect ~msg:"expected package manifest to write";
      let config = Riot_model.Fmt_config.load manifest_path in
      Test.assert_equal ~expected:[ "tests/fixtures"; "vendor" ] ~actual:config.ignore_patterns;
      Ok ())

let test_legacy_fmt_ignore_still_loads = fun _ctx ->
  let toml =
    Std.Data.Toml.parse {|
[fmt]
ignore = ["fixtures"]
|}
    |> Result.expect ~msg:"expected legacy fmt TOML to parse"
  in
  let config = Riot_model.Fmt_config.from_toml toml in
  Test.assert_equal ~expected:[ "fixtures" ] ~actual:config.ignore_patterns;
  Ok ()

let test_package_dependency_requirement_parses_structurally = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse
      {|
[package]
name = "demo"
version = "0.1.0"

[dependencies]
std = ">= 1.2.3"
|}
    |> Result.expect ~msg:"expected package TOML to parse"
  in
  let pkg =
    Riot_model.Package.from_toml
      manifest
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/demo")
      ~relative_path:(Path.v "packages/demo")
    |> Result.expect ~msg:"expected package manifest to parse"
  in
  match pkg.dependencies with
  | [
      {
        Riot_model.Package.source = {
          workspace = false;
          builtin = false;
          path = None;
          version = Some requirement;
        };
        _;
      };
    ] ->
      Test.assert_equal ~expected:">= 1.2.3" ~actual:(Std.Version.requirement_to_string requirement);
      Ok ()
  | _ -> Error "expected a parsed registry dependency requirement"

let test_package_dependency_invalid_requirement_fails = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse
      {|
[package]
name = "demo"
version = "0.1.0"

[dependencies]
std = "not-a-semver-range"
|}
    |> Result.expect ~msg:"expected package TOML to parse"
  in
  match Package.from_toml
    manifest
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:(Path.v "/tmp/demo")
    ~relative_path:(Path.v "packages/demo") with
  | Ok _ -> Error "expected invalid package semver requirement to fail"
  | Error _ -> Ok ()

let test_package_star_requirement_becomes_unconstrained_registry_dep = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse {|
[package]
name = "demo"
version = "0.1.0"

[dependencies]
std = "*"
|}
    |> Result.expect ~msg:"expected package TOML to parse"
  in
  let pkg =
    Riot_model.Package.from_toml
      manifest
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/demo")
      ~relative_path:(Path.v "packages/demo")
    |> Result.expect ~msg:"expected package manifest to parse"
  in
  match pkg.dependencies with
  | [
      {
        Riot_model.Package.source = {
          workspace = false;
          builtin = false;
          path = None;
          version = Some requirement;
        };
        _;
      };
    ] ->
      Test.assert_equal ~expected:"*" ~actual:(Std.Version.requirement_to_string requirement);
      Ok ()
  | _ -> Error "expected '*' package dependency to become an unconstrained registry dependency"

let test_package_builtin_dependency_parses_structurally = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse {|
[package]
name = "demo"
version = "0.1.0"

[dependencies]
stdlib = "*"
|}
    |> Result.expect ~msg:"expected package TOML to parse"
  in
  let pkg =
    Riot_model.Package.from_toml
      manifest
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/demo")
      ~relative_path:(Path.v "packages/demo")
    |> Result.expect ~msg:"expected package manifest to parse"
  in
  match pkg.dependencies with
  | [ { Riot_model.Package.name; source = { builtin = true; version = Some requirement; _ } } ] when Package_name.equal
    name
    (package_name "stdlib")
  && String.equal (Std.Version.requirement_to_string requirement) "*" -> Ok ()
  | _ -> Error "expected stdlib '*' to parse as a builtin dependency"

let test_package_builtin_dependency_rejects_version_constraints = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse
      {|
[package]
name = "demo"
version = "0.1.0"

[dependencies]
stdlib = ">= 1.0.0"
|}
    |> Result.expect ~msg:"expected package TOML to parse"
  in
  match Riot_model.Package.from_toml
    manifest
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:(Path.v "/tmp/demo")
    ~relative_path:(Path.v "packages/demo") with
  | Ok _ -> Error "expected builtin dependency version constraints to fail"
  | Error _ -> Ok ()

let test_package_json_roundtrips_registry_requirement = fun _ctx ->
  let requirement =
    Std.Version.parse_requirement ">= 1.2.3"
    |> Result.expect ~msg:"expected requirement to parse"
  in
  let package =
    Package.make
      ~name:(package_name "demo")
      ~path:(Path.v "/tmp/demo")
      ~relative_path:(Path.v "packages/demo")
      ~dependencies:[ { name = package_name "std"; source = source ~version:requirement () } ]
      ()
  in
  let decoded =
    Package.to_json package
    |> Package.from_json
    |> Result.expect ~msg:"expected package JSON to roundtrip"
  in
  match decoded.dependencies with
  | [
      {
        Riot_model.Package.source = {
          workspace = false;
          builtin = false;
          path = None;
          version = Some requirement;
        };
        _;
      };
    ] ->
      Test.assert_equal ~expected:">= 1.2.3" ~actual:(Std.Version.requirement_to_string requirement);
      Ok ()
  | _ -> Error "expected registry dependency after JSON roundtrip"

let test_workspace_dependency_requirement_parses_structurally = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse {|
[workspace]
members = []

[dependencies]
std = ">= 1.2.3"
|}
    |> Result.expect ~msg:"expected workspace TOML to parse"
  in
  let workspace_manifest =
    Riot_model.Workspace_manifest.from_toml manifest
    |> Result.expect ~msg:"expected workspace manifest to parse"
  in
  match workspace_manifest.dependencies with
  | [
      {
        Riot_model.Package.source = {
          workspace = false;
          builtin = false;
          path = None;
          version = Some requirement;
        };
        _;
      };
    ] ->
      Test.assert_equal ~expected:">= 1.2.3" ~actual:(Std.Version.requirement_to_string requirement);
      Ok ()
  | _ -> Error "expected a parsed workspace registry dependency requirement"

let test_workspace_star_requirement_becomes_unconstrained_registry_dep = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse {|
[workspace]
members = []

[dependencies]
std = "*"
|}
    |> Result.expect ~msg:"expected workspace TOML to parse"
  in
  let workspace_manifest =
    Riot_model.Workspace_manifest.from_toml manifest
    |> Result.expect ~msg:"expected workspace manifest to parse"
  in
  match workspace_manifest.dependencies with
  | [
      {
        Riot_model.Package.source = {
          workspace = false;
          builtin = false;
          path = None;
          version = Some requirement;
        };
        _;
      };
    ] ->
      Test.assert_equal ~expected:"*" ~actual:(Std.Version.requirement_to_string requirement);
      Ok ()
  | _ -> Error "expected '*' workspace dependency to become an unconstrained registry dependency"

let test_workspace_dependency_non_string_version_returns_typed_error = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse {|
[workspace]
members = []

[dependencies]
std = { version = 123 }
|}
    |> Result.expect ~msg:"expected workspace TOML to parse"
  in
  match Riot_model.Workspace_manifest.from_toml manifest with
  | Error (
    Riot_model.Workspace_manifest.DependencyError (
      Riot_model.Workspace_manifest.DependencyFieldMustBeString {
        dependency_name = "std";
        field = Riot_model.Workspace_manifest.Version;
      }
    )
  ) ->
      Ok ()
  | Error err ->
      Error ("expected typed dependency version error, got "
      ^ Riot_model.Workspace_manifest.error_message err)
  | Ok _ -> Error "expected workspace manifest parse to fail for non-string dependency version"

let test_workspace_manager_resolves_member_path_dependencies_relative_to_package = fun _ctx ->
  with_tempdir
    "riot_model_workspace_paths"
    (fun root ->
      let write path content =
        Fs.write content path
        |> Result.expect ~msg:("expected write to succeed: " ^ Path.to_string path)
      in
      let mkdir path =
        Fs.create_dir_all path
        |> Result.expect ~msg:("expected mkdir to succeed: " ^ Path.to_string path)
      in
      mkdir Path.(root / Path.v "packages/app/src");
      mkdir Path.(root / Path.v "packages/vendor/src");
      mkdir Path.(root / Path.v "packages/kernel/src");
      write Path.(root / Path.v "riot.toml") {|
[workspace]
members = ["packages/app"]
|};
      write
        Path.(root / Path.v "packages/app/riot.toml")
        {|
[package]
name = "app"
version = "0.1.0"

[dependencies]
vendor = { path = "../vendor" }
|};
      write
        Path.(root / Path.v "packages/vendor/riot.toml")
        {|
[package]
name = "vendor"
version = "0.1.0"

[dependencies]
kernel = { path = "../kernel" }
|};
      write
        Path.(root / Path.v "packages/kernel/riot.toml")
        {|
[package]
name = "kernel"
version = "0.1.0"
|};
      let workspace_manager = Riot_model.Workspace_manager.create () in
      match Riot_model.Workspace_manager.scan workspace_manager root with
      | Error err -> Error (Riot_model.Workspace_manager.scan_error_message err)
      | Ok (workspace, errors) ->
          if errors != [] then
            Error ("expected no workspace loading errors, got: "
            ^ String.concat
              "; "
              (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string))
          else
            let names =
              workspace.Riot_model.Workspace_manifest.packages
              |> List.map ~fn:(fun (p: Riot_model.Package_manifest.t) -> p.name)
              |> List.sort ~compare:Riot_model.Package_name.compare
              |> List.map ~fn:Riot_model.Package_name.to_string
            in
            Test.assert_equal ~expected:[ "app"; "kernel"; "vendor" ] ~actual:names;
          Ok ())

let test_workspace_manager_reports_member_manifest_decode_errors = fun _ctx ->
  with_tempdir
    "riot_model_workspace_member_decode_error"
    (fun root ->
      let write path content =
        Fs.write content path
        |> Result.expect ~msg:("expected write to succeed: " ^ Path.to_string path)
      in
      let mkdir path =
        Fs.create_dir_all path
        |> Result.expect ~msg:("expected mkdir to succeed: " ^ Path.to_string path)
      in
      mkdir Path.(root / Path.v "packages/app/src");
      write Path.(root / Path.v "riot.toml") {|
[workspace]
members = ["packages/app"]
|};
      write
        Path.(root / Path.v "packages/app/riot.toml")
        {|
[package]
name = "app"
version = "0.1.0"

[dependencies]
minttea = "not-a-version"
|};
      let workspace_manager = Riot_model.Workspace_manager.create () in
      match Riot_model.Workspace_manager.scan workspace_manager root with
      | Error err -> Error (Riot_model.Workspace_manager.scan_error_message err)
      | Ok (_workspace, errors) -> (
          match errors with
          | [
              Riot_model.Workspace_manager.PackageFromTomlFailed {
                package;
                error =
                  Riot_model.Package.InvalidDependency (
                    Riot_model.Package.InvalidDependencyRequirement {
                      dependency_name;
                      requirement;
                      _;
                    }
                  );
                _;
              };
            ] when String.equal package "app"
          && String.equal dependency_name "minttea"
          && String.equal requirement "not-a-version" -> Ok ()
          | [ Riot_model.Workspace_manager.PackageFromTomlFailed { error; _ } ] ->
              Error ("unexpected member decode error: "
              ^ Riot_model.Package_manifest.error_message error)
          | _ -> Error "expected invalid member manifest to surface as a workspace load error"
        ))

let test_workspace_manager_load_riot_toml_returns_typed_parse_errors = fun _ctx ->
  with_tempdir
    "riot_model_workspace_toml_parse_error"
    (fun root ->
      let manifest_path = Path.(root / Path.v "riot.toml") in
      let* () =
        Fs.write {|
[workspace]
members = [
|} manifest_path
        |> Result.map_err ~fn:IO.error_message
      in
      let workspace_manager = Riot_model.Workspace_manager.create () in
      match Riot_model.Workspace_manager.load_riot_toml workspace_manager manifest_path with
      | Error (Riot_model.Workspace_manager.ManifestParseFailed { path; _ }) when Path.equal
        path
        manifest_path -> Ok ()
      | Error err ->
          Error ("expected typed manifest parse error, got "
          ^ Riot_model.Workspace_manager.manifest_load_error_message err)
      | Ok _ -> Error "expected invalid riot.toml to fail parsing")

let test_workspace_manager_scan_reports_no_workspace_root = fun _ctx ->
  with_tempdir
    "riot_model_workspace_scan_missing_root"
    (fun root ->
      let original_dir = Env.current_dir () in
      let result =
        let* () =
          Env.set_current_dir root
          |> Result.map_err ~fn:path_error_message
        in
        let workspace_manager = Riot_model.Workspace_manager.create () in
        match Riot_model.Workspace_manager.scan workspace_manager (Path.v ".") with
        | Error Riot_model.Workspace_manager.NoWorkspaceRootFound -> Ok ()
        | Error err ->
            Error ("expected NoWorkspaceRootFound, got "
            ^ Riot_model.Workspace_manager.scan_error_message err)
        | Ok _ -> Error "expected workspace scan without manifests to fail"
      in
      let _ =
        match original_dir with
        | Ok dir -> Env.set_current_dir dir
        | Error _ -> Ok ()
      in
      result)

let test_workspace_manager_scan_reports_typed_workspace_manifest_errors = fun _ctx ->
  with_tempdir
    "riot_model_workspace_scan_manifest_error"
    (fun root ->
      let manifest_path = Path.(root / Path.v "riot.toml") in
      let* () =
        Fs.write
          {|
[workspace]
members = ["packages/app"]

[dependencies]
std = "not-a-version"
|}
          manifest_path
        |> Result.map_err ~fn:IO.error_message
      in
      let workspace_manager = Riot_model.Workspace_manager.create () in
      match Riot_model.Workspace_manager.scan workspace_manager root with
      | Error (Riot_model.Workspace_manager.WorkspaceManifestDecodeFailed { path; _ }) when Path.equal
        path
        manifest_path -> Ok ()
      | Error err ->
          Error ("expected typed workspace manifest decode error, got "
          ^ Riot_model.Workspace_manager.scan_error_message err)
      | Ok _ -> Error "expected invalid workspace manifest to fail scan")

let test_workspace_manager_skips_missing_path_dependencies_with_registry_fallback = fun _ctx ->
  with_tempdir
    "riot_model_workspace_missing_path_fallback"
    (fun root ->
      let write path content =
        Fs.write content path
        |> Result.expect ~msg:("expected write to succeed: " ^ Path.to_string path)
      in
      let mkdir path =
        Fs.create_dir_all path
        |> Result.expect ~msg:("expected mkdir to succeed: " ^ Path.to_string path)
      in
      mkdir Path.(root / Path.v "packages/app/src");
      write Path.(root / Path.v "riot.toml") {|
[workspace]
members = ["packages/app"]
|};
      write
        Path.(root / Path.v "packages/app/riot.toml")
        {|
[package]
name = "app"
version = "0.1.0"

[dependencies]
std = { path = "../std", version = "*" }
|};
      let workspace_manager = Riot_model.Workspace_manager.create () in
      match Riot_model.Workspace_manager.scan workspace_manager root with
      | Error err -> Error (Riot_model.Workspace_manager.scan_error_message err)
      | Ok (workspace, errors) ->
          if not (List.is_empty errors) then
            Error ("expected missing path+version dependency to defer to later resolution, got: "
            ^ String.concat
              "; "
              (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string))
          else
            let names =
              workspace.Riot_model.Workspace_manifest.packages
              |> List.map ~fn:(fun (p: Riot_model.Package_manifest.t) -> p.name)
              |> List.sort ~compare:Riot_model.Package_name.compare
              |> List.map ~fn:Riot_model.Package_name.to_string
            in
            Test.assert_equal ~expected:[ "app" ] ~actual:names;
          Ok ())

let test_workspace_manager_synthesizes_single_package_workspace = fun _ctx ->
  with_tempdir
    "riot_model_single_package_workspace"
    (fun root ->
      let original_dir = Env.current_dir () in
      let result =
        let src_dir = Path.(root / Path.v "src") in
        let* () =
          Fs.create_dir_all src_dir
          |> Result.map_err ~fn:IO.error_message
        in
        let* () =
          Fs.write
            {|
[package]
name = "demo"
version = "0.1.0"

[lib]
path = "src/demo.ml"

[[bin]]
name = "main"
path = "src/demo.ml"
|}
            Path.(root / Path.v "riot.toml")
          |> Result.map_err ~fn:IO.error_message
        in
        let* () =
          Fs.write "let () = print_endline \"demo\"\n" Path.(src_dir / Path.v "demo.ml")
          |> Result.map_err ~fn:IO.error_message
        in
        let* () =
          Env.set_current_dir root
          |> Result.map_err ~fn:path_error_message
        in
        let workspace_manager = Riot_model.Workspace_manager.create () in
        match Riot_model.Workspace_manager.scan workspace_manager (Path.v ".") with
        | Error err -> Error (Riot_model.Workspace_manager.scan_error_message err)
        | Ok (workspace, errors) ->
            if not (List.is_empty errors) then
              Error ("expected no standalone package load errors, got: "
              ^ String.concat
                "; "
                (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string))
            else
              match workspace.Riot_model.Workspace_manifest.packages with
              | [ package ] ->
                  if
                    Riot_model.Package_name.equal package.name (package_name "demo")
                    && Path.equal package.relative_path (Path.v ".")
                  then
                    Ok ()
                  else
                    Error ("expected detached package scan to synthesize a one-package workspace, got root="
                    ^ Path.to_string workspace.root
                    ^ " package="
                    ^ Riot_model.Package_name.to_string package.name
                    ^ " relative="
                    ^ Path.to_string package.relative_path)
              | packages ->
                  Error ("expected one package, got "
                  ^ Int.to_string (List.length packages)
                  ^ " root="
                  ^ Path.to_string workspace.root
                  ^ " names="
                  ^ String.concat
                    ", "
                    (List.map
                      packages
                      ~fn:(fun (pkg: Riot_model.Package_manifest.t) ->
                        Riot_model.Package_name.to_string
                          pkg.name)))
      in
      let _ =
        match original_dir with
        | Ok dir -> Env.set_current_dir dir
        | Error _ -> Ok ()
      in
      result)

let test_user_config_parses_registry_api_token = fun _ctx ->
  let toml =
    Std.Data.Toml.parse {|
[registry."pkgs.ml"]
api_token = "root-secret"
|}
    |> Result.expect ~msg:"expected user config TOML to parse"
  in
  match Riot_model.User_config.from_toml toml with
  | Error err -> Error (Riot_model.User_config.message err)
  | Ok config -> (
      match Riot_model.User_config.api_token config ~registry_name:"pkgs.ml" with
      | Some token when String.equal token "root-secret" -> Ok ()
      | _ -> Error "expected pkgs.ml API token to be parsed from config"
    )

let test_user_config_load_reads_config_file = fun _ctx ->
  with_tempdir
    "riot_model_user_config"
    (fun tmpdir ->
      let config_path = Path.(tmpdir / Path.v "config.toml") in
      Fs.write {|
[registry."pkgs.ml"]
api_token = "publish-token"
|} config_path
      |> Result.expect ~msg:"expected config to write";
      match Riot_model.User_config.load config_path with
      | Error err -> Error (Riot_model.User_config.message err)
      | Ok config -> (
          match Riot_model.User_config.api_token config ~registry_name:"pkgs.ml" with
          | Some token when String.equal token "publish-token" -> Ok ()
          | _ -> Error "expected config loader to expose registry token"
        ))

let test_user_config_parses_empty_registry_entry = fun _ctx ->
  let toml =
    Std.Data.Toml.parse {|
[registry."pkgs.ml"]
|}
    |> Result.expect ~msg:"expected user config TOML to parse"
  in
  match Riot_model.User_config.from_toml toml with
  | Error err -> Error (Riot_model.User_config.message err)
  | Ok config -> (
      match Riot_model.User_config.api_token config ~registry_name:"pkgs.ml" with
      | None -> Ok ()
      | Some _ -> Error "expected empty registry config to keep missing api_token"
    )

let test_user_config_parses_registry_urls = fun _ctx ->
  let toml =
    Std.Data.Toml.parse
      {|
[registry."pkgs.ml"]
api_url = "https://api.pkgs.ml"
cdn_url = "https://cdn.pkgs.ml"
api_token = "publish-token"
|}
    |> Result.expect ~msg:"expected user config TOML to parse"
  in
  match Riot_model.User_config.from_toml toml with
  | Error err -> Error (Riot_model.User_config.message err)
  | Ok config -> (
      match List.find
        config.Riot_model.User_config.registries
        ~fn:(fun (name, _registry) -> String.equal name "pkgs.ml") with
      | None -> Error "expected pkgs.ml registry entry to be present"
      | Some (_name, registry) ->
          if not (String.equal (Net.Uri.to_string registry.api_url) "https://api.pkgs.ml/") then
            Error "expected api_url to parse"
          else if
            not (String.equal (Net.Uri.to_string registry.cdn_url) "https://cdn.pkgs.ml/")
          then
            Error "expected cdn_url to parse"
          else if not (registry.api_token = Some "publish-token") then
            Error "expected api_token to parse"
          else
            Ok ()
    )

let test_user_config_save_roundtrips_default_registry_config = fun _ctx ->
  with_tempdir
    "riot_model_user_config_default"
    (fun tmpdir ->
      let config_path = Path.(tmpdir / Path.v "config.toml") in
      Riot_model.User_config.save Riot_model.User_config.default config_path
      |> Result.expect ~msg:"expected default config to write";
      match Riot_model.User_config.load config_path with
      | Error err -> Error (Riot_model.User_config.message err)
      | Ok config -> (
          match Riot_model.User_config.api_token config ~registry_name:"pkgs.ml" with
          | None -> Ok ()
          | Some _ -> Error "expected saved default config to keep missing api_token"
        ))

let test_user_config_rejects_non_string_registry_api_url = fun _ctx ->
  let toml =
    Std.Data.Toml.parse {|
[registry."pkgs.ml"]
api_url = 42
|}
    |> Result.expect ~msg:"expected user config TOML to parse"
  in
  match Riot_model.User_config.from_toml toml with
  | Error (
    Riot_model.User_config.InvalidRegistryConfig {
      registry_name;
      error = Riot_model.User_config.FieldMustBeString Riot_model.User_config.Api_url;
    }
  ) when String.equal registry_name "pkgs.ml" ->
      Ok ()
  | Error err ->
      Error ("expected typed api_url field error, got " ^ Riot_model.User_config.message err)
  | Ok _ -> Error "expected non-string registry api_url to fail"

let test_workspace_operational_config_defaults_when_missing = fun _ctx ->
  with_tempdir
    "riot_model_workspace_operational_config_missing"
    (fun tmpdir ->
      match Riot_model.Workspace_operational_config.load ~workspace_root:tmpdir with
      | Error err -> Error (Riot_model.Workspace_operational_config.message err)
      | Ok config ->
          if
            config.cache.keep_generations = 10
            && Int64.equal config.cache.max_size_bytes (Int64.mul 50L 1_073_741_824L)
            && Option.is_none config.test.small_test_timeout
            && config.test.flaky_max_retries = 0
            && Option.is_none config.trace.profiler
            && Option.is_none config.trace.perf.sample_rate_hz
            && Option.is_none config.trace.xctrace.template_
          then
            Ok ()
          else
            Error "expected missing .riot/config.toml to use built-in operational defaults")

let test_workspace_operational_config_parses_riot_cache = fun _ctx ->
  with_tempdir
    "riot_model_workspace_operational_config_parse"
    (fun tmpdir ->
      let riot_dir = Path.(tmpdir / Path.v ".riot") in
      Result.expect (Fs.create_dir_all riot_dir) ~msg:"Failed to create .riot directory";
      Result.expect
        (Fs.write
          {|
[riot.cache]
keep_generations = 5
max_size = "2 GiB"
|}
          Path.(riot_dir / Path.v "config.toml"))
        ~msg:"Failed to write .riot/config.toml";
      match Riot_model.Workspace_operational_config.load ~workspace_root:tmpdir with
      | Error err -> Error (Riot_model.Workspace_operational_config.message err)
      | Ok config ->
          if
            config.cache.keep_generations = 5
            && Int64.equal config.cache.max_size_bytes (Int64.mul 2L 1_073_741_824L)
          then
            Ok ()
          else
            Error "expected .riot/config.toml to override cache policy")

let test_workspace_operational_config_parses_riot_test = fun _ctx ->
  with_tempdir
    "riot_model_workspace_operational_config_test"
    (fun tmpdir ->
      let riot_dir = Path.(tmpdir / Path.v ".riot") in
      Result.expect (Fs.create_dir_all riot_dir) ~msg:"Failed to create .riot directory";
      Result.expect
        (Fs.write
          {|
[riot.test]
small_test_timeout = "500ms"
flakey_max_retry = 3
|}
          Path.(riot_dir / Path.v "config.toml"))
        ~msg:"Failed to write .riot/config.toml";
      match Riot_model.Workspace_operational_config.load ~workspace_root:tmpdir with
      | Error err -> Error (Riot_model.Workspace_operational_config.message err)
      | Ok config ->
          if
            config.test.small_test_timeout = Some (Time.Duration.from_millis 500)
            && config.test.flaky_max_retries = 3
          then
            Ok ()
          else
            Error "expected .riot/config.toml to parse riot.test policy")

let test_workspace_operational_config_parses_riot_trace = fun _ctx ->
  with_tempdir
    "riot_model_workspace_operational_config_trace"
    (fun tmpdir ->
      let riot_dir = Path.(tmpdir / Path.v ".riot") in
      Result.expect (Fs.create_dir_all riot_dir) ~msg:"Failed to create .riot directory";
      Result.expect
        (Fs.write
          {|
[riot.trace]
profiler = "xctrace"

[riot.trace.xctrace]
template = "Time Profiler"
time_limit = "5s"
window = "750 ms"

[riot.trace.perf]
sample_rate = 997
call_graph = "dwarf"
call_graph_stack_size = 8192
|}
          Path.(riot_dir / Path.v "config.toml"))
        ~msg:"Failed to write .riot/config.toml";
      match Riot_model.Workspace_operational_config.load ~workspace_root:tmpdir with
      | Error err -> Error (Riot_model.Workspace_operational_config.message err)
      | Ok config ->
          if
            config.trace.profiler = Some "xctrace"
            && config.trace.xctrace.template_ = Some "Time Profiler"
            && config.trace.xctrace.time_limit = Some "5s"
            && config.trace.xctrace.window = Some "750ms"
            && config.trace.perf.sample_rate_hz = Some 997
            && config.trace.perf.call_graph = Some "dwarf"
            && config.trace.perf.call_graph_stack_size = Some 8192
          then
            Ok ()
          else
            Error "expected .riot/config.toml to parse riot.trace policy")

let test_workspace_operational_config_rejects_invalid_max_size_unit = fun _ctx ->
  with_tempdir
    "riot_model_workspace_operational_config_invalid_size"
    (fun tmpdir ->
      let riot_dir = Path.(tmpdir / Path.v ".riot") in
      Result.expect (Fs.create_dir_all riot_dir) ~msg:"Failed to create .riot directory";
      Result.expect
        (Fs.write {|
[riot.cache]
max_size = "3 frogs"
|} Path.(riot_dir / Path.v "config.toml"))
        ~msg:"Failed to write .riot/config.toml";
      match Riot_model.Workspace_operational_config.load ~workspace_root:tmpdir with
      | Error (
        Riot_model.Workspace_operational_config.InvalidConfig {
          error = CacheConfig (InvalidMaxSize (UnsupportedUnit unit_name));
          _;
        }
      ) when String.equal unit_name "frogs" ->
          Ok ()
      | Error err ->
          Error ("expected typed max_size unit error, got "
          ^ Riot_model.Workspace_operational_config.message err)
      | Ok _ -> Error "expected invalid max_size unit to fail")

let test_debug_profile_defaults_to_native_with_debug_symbols = fun _ctx ->
  let profile = Riot_model.Profile.debug in
  let flags = Riot_model.Profile.to_compiler_flags profile in
  let rendered_flags = String.concat " " flags in
  if
    profile.kind = Riot_model.Ocaml_compiler.Native
    && List.contains flags ~value:"-inline"
    && List.contains flags ~value:"0"
    && List.contains flags ~value:"-g"
    && String.contains rendered_flags "-warn-error"
    && String.contains rendered_flags "+6"
  then
    Ok ()
  else
    Error ("expected debug profile to default to native with -inline 0 -g and warning 6 as error, got kind="
    ^ Riot_model.Ocaml_compiler.compilation_kind_to_string profile.kind
    ^ " flags=["
    ^ String.concat ", " flags
    ^ "]")

let test_release_profile_defaults_to_strict_native_optimization = fun _ctx ->
  let profile = Riot_model.Profile.release in
  let flags = Riot_model.Profile.to_compiler_flags profile in
  if not (profile.kind = Riot_model.Ocaml_compiler.Native) then
    Error "expected release profile to stay native"
  else if not (List.contains flags ~value:"-noassert") then
    Error "expected release profile to include -noassert"
  else if not (List.contains flags ~value:"-compact") then
    Error "expected release profile to include -compact"
  else if not (List.contains flags ~value:"-inline" && List.contains flags ~value:"100") then
    Error "expected release profile to include -inline 100"
  else if not (List.contains flags ~value:"-warn-error" && List.contains flags ~value:"+a") then
    Error "expected release profile to treat all warnings as errors"
  else
    Ok ()

let test_fuzz_profile_enables_afl_instrumentation = fun _ctx ->
  let profile = Riot_model.Profile.fuzz in
  let flags = Riot_model.Profile.to_compiler_flags profile in
  if not (profile.kind = Riot_model.Ocaml_compiler.Native) then
    Error "expected fuzz profile to stay native"
  else if not (String.equal profile.name "fuzz") then
    Error "expected fuzz profile name to be fuzz"
  else if not (List.contains flags ~value:"-afl-instrument") then
    Error "expected fuzz profile to include -afl-instrument"
  else if not (List.contains flags ~value:"-g") then
    Error "expected fuzz profile to include debug symbols"
  else
    Ok ()

let tests =
  Test.[
    case
      "for_scope: build drops commands and runtime outputs"
      test_build_scope_drops_commands_and_runtime_outputs;
    case "for_scope: runtime keeps commands" test_runtime_scope_keeps_commands;
    case "for_scope: dev keeps only dev outputs" test_dev_scope_keeps_only_dev_outputs;
    case
      "for_binary: keeps only selected dev source"
      test_for_binary_keeps_only_selected_dev_source;
    case
      "for_scope: runtime keeps build dependencies for hashing"
      test_runtime_scope_keeps_build_dependencies_for_hashing;
    case
      "package: hash changes when build dependency path changes"
      test_package_hash_changes_when_build_dependency_path_changes;
    case
      "package: declared example binaries suppress example autodiscovery"
      test_declared_example_binaries_suppress_example_autodiscovery;
    case
      "package: declared runtime binaries suppress src/main autodiscovery"
      test_declared_runtime_binaries_suppress_main_autodiscovery;
    case
      "package: src/main.ml autodiscovers runtime binary"
      test_src_main_autodiscovers_runtime_binary;
    case "package: source scan ignores hidden entries" test_scan_sources_ignores_hidden_entries;
    case
      "package: source scan ignores test support entries"
      test_scan_sources_ignores_test_support_entries;
    case
      "package: source scan keeps similarly named test directories"
      test_scan_sources_keeps_similarly_named_test_directories;
    case
      "package: source scan keeps source directories matching support names"
      test_scan_sources_keeps_source_directories_matching_test_support_names;
    case
      "package: source scan respects package-root gitignore"
      test_scan_sources_respects_package_root_gitignore;
    case "package: source scan ignores non-ocaml files" test_scan_sources_ignores_non_ocaml_files;
    case
      "package: source scan ignores deps fixture support entries"
      test_scan_sources_ignores_deps_fixture_support_entries;
    case "fmt config: workspace ignore parses" test_workspace_fmt_ignore_parses;
    case "workspace: source ignore parses" test_workspace_source_ignore_parses;
    case "fmt config: package ignore loads" test_package_fmt_ignore_loads;
    case "fmt config: legacy top-level fmt still loads" test_legacy_fmt_ignore_still_loads;
    case
      "package: registry dependency requirement parses structurally"
      test_package_dependency_requirement_parses_structurally;
    case
      "package: invalid dependency requirement fails"
      test_package_dependency_invalid_requirement_fails;
    case
      "package: star dependency becomes unconstrained registry dependency"
      test_package_star_requirement_becomes_unconstrained_registry_dep;
    case
      "package: builtin dependency parses structurally"
      test_package_builtin_dependency_parses_structurally;
    case
      "package: builtin dependency rejects version constraints"
      test_package_builtin_dependency_rejects_version_constraints;
    case
      "package: registry dependency JSON roundtrips"
      test_package_json_roundtrips_registry_requirement;
    case
      "workspace: registry dependency requirement parses structurally"
      test_workspace_dependency_requirement_parses_structurally;
    case
      "workspace: star dependency becomes unconstrained registry dependency"
      test_workspace_star_requirement_becomes_unconstrained_registry_dep;
    case
      "workspace: non-string dependency version returns typed error"
      test_workspace_dependency_non_string_version_returns_typed_error;
    case
      "workspace manager: package path deps resolve relative to declaring package"
      test_workspace_manager_resolves_member_path_dependencies_relative_to_package;
    case
      "workspace manager: member manifest decode failures surface as load errors"
      test_workspace_manager_reports_member_manifest_decode_errors;
    case
      "workspace manager: load_riot_toml returns typed parse errors"
      test_workspace_manager_load_riot_toml_returns_typed_parse_errors;
    case
      "workspace manager: scan reports missing workspace roots"
      test_workspace_manager_scan_reports_no_workspace_root;
    case
      "workspace manager: scan reports typed workspace manifest errors"
      test_workspace_manager_scan_reports_typed_workspace_manifest_errors;
    case
      "workspace manager: missing path+version deps defer to external resolution"
      test_workspace_manager_skips_missing_path_dependencies_with_registry_fallback;
    case
      "workspace manager: standalone package scan synthesizes workspace"
      test_workspace_manager_synthesizes_single_package_workspace;
    case "user config: parses empty registry entry" test_user_config_parses_empty_registry_entry;
    case "user config: parses registry urls" test_user_config_parses_registry_urls;
    case "user config: parses registry API token" test_user_config_parses_registry_api_token;
    case "user config: loads config file" test_user_config_load_reads_config_file;
    case
      "user config: saves default registry config"
      test_user_config_save_roundtrips_default_registry_config;
    case
      "user config: rejects non-string registry api_url"
      test_user_config_rejects_non_string_registry_api_url;
    case
      "workspace operational config: defaults when missing"
      test_workspace_operational_config_defaults_when_missing;
    case
      "workspace operational config: parses riot.cache"
      test_workspace_operational_config_parses_riot_cache;
    case
      "workspace operational config: parses riot.test"
      test_workspace_operational_config_parses_riot_test;
    case
      "workspace operational config: parses riot.trace"
      test_workspace_operational_config_parses_riot_trace;
    case
      "workspace operational config: rejects invalid max_size unit"
      test_workspace_operational_config_rejects_invalid_max_size_unit;
    case
      "profile: debug defaults to native with debug symbols"
      test_debug_profile_defaults_to_native_with_debug_symbols;
    case
      "profile: release defaults to strict native optimization"
      test_release_profile_defaults_to_strict_native_optimization;
    case "profile: fuzz enables AFL instrumentation" test_fuzz_profile_enables_afl_instrumentation;
  ]

let name = "Riot Model Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
