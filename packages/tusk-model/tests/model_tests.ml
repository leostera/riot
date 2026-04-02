open Std
module Test = Std.Test

let source = fun ?(workspace = false) ?(builtin = false) ?path ?version () ->
  Tusk_model.Package.{ workspace; builtin; path; version }

let make_command = fun () ->
  Tusk_model.Package_command.{
    name = "demo";
    description = "Run the demo";
    package_name = "minttea";
    package_path = Path.v "packages/minttea";
    command_module = "Demo_cmd";
    command_source = Path.v "src/demo_cmd.ml";
    command_binary = Path.v "_build/debug/out/minttea/demo";
  }

let make_package = fun () ->
  let command = make_command () in
  let publish =
    Tusk_model.Package.{
      version = Some (Std.Version.make ~major:0 ~minor:1 ~patch:0 ());
      description = Some "minttea";
      license = Some "Apache-2.0";
      is_public = Some true
    } in
  Tusk_model.Package.{
    name = "minttea";
    path = Path.v "packages/minttea";
    relative_path = Path.v "packages/minttea";
    dependencies = [ { name = "std"; source = source ~workspace:true () } ];
    dev_dependencies = [ { name = "propane"; source = source ~workspace:true () } ];
    build_dependencies = [ { name = "std"; source = source ~workspace:true () } ];
    foreign_dependencies = [];
    binaries = [ { name = "demo-bin"; path = Path.v "src/demo_bin.ml" } ];
    library = Some { path = Path.v "src/minttea.ml" };
    sources =
      {
        src = [ Path.v "src/minttea.ml"; Path.v "src/demo_cmd.ml" ];
        native = [];
        tests = [ Path.v "tests/model_tests.ml" ];
        examples = [];
        bench = [];
      };
    compiler = { profile_overrides = []; target_overrides = [] };
    commands = [ command ];
    fix_providers = [];
    publish;
  }

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error _ -> Error "Tempdir creation failed"

let test_build_scope_drops_commands_and_runtime_outputs = fun _ctx ->
  let pkg = make_package () in
  let projected = Tusk_model.Package.for_scope Tusk_model.Package.Build pkg in
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
  let projected = Tusk_model.Package.for_scope Tusk_model.Package.Normal pkg in
  if List.length projected.commands = 1 && List.length projected.binaries = 1 then
    Ok ()
  else
    Error "runtime scope should preserve package commands and normal binaries"

let test_dev_scope_keeps_only_dev_outputs = fun _ctx ->
  let pkg = make_package () in
  let projected = Tusk_model.Package.for_scope Tusk_model.Package.Dev pkg in
  let no_library = projected.library = None in
  let no_commands = projected.commands = [] in
  let no_runtime_sources = projected.sources.src = [] && projected.sources.native = [] in
  let kept_dev_deps = List.map (fun (dep: Tusk_model.Package.dependency) -> dep.name) projected.dev_dependencies
  = [ "propane" ] in
  let kept_runtime_deps = List.map (fun (dep: Tusk_model.Package.dependency) -> dep.name) projected.dependencies
  = [ "std" ] in
  let no_normal_binaries =
    List.for_all
      (fun (bin: Tusk_model.Package.binary) ->
        String.starts_with ~prefix:"tests/" (Path.to_string bin.path)
        || String.starts_with ~prefix:"examples/" (Path.to_string bin.path)
        || String.starts_with ~prefix:"bench/" (Path.to_string bin.path))
      projected.binaries
  in
  if
    no_library && no_commands && no_runtime_sources && kept_dev_deps && kept_runtime_deps && no_normal_binaries
  then
    Ok ()
  else
    Error "dev scope should reuse runtime deps while keeping only dev outputs"

let test_explicit_binaries_override_autodiscovery = fun _ctx ->
  with_tempdir "tusk_model_package"
    (fun tmpdir ->
      let src_dir = Path.(tmpdir / Path.v "src") in
      let examples_dir = Path.(tmpdir / Path.v "examples") in
      Result.expect (Fs.create_dir_all src_dir) ~msg:"Failed to create src directory";
      Result.expect (Fs.create_dir_all examples_dir) ~msg:"Failed to create examples directory";
      Result.expect (Fs.write "let version = 1\n" Path.(src_dir / Path.v "demo.ml")) ~msg:"Failed to write library source";
      Result.expect
        (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "test_https_httpbin.ml"))
        ~msg:"Failed to write explicit example";
      Result.expect (Fs.write "let () = ()\n" Path.(examples_dir / Path.v "simple_https.ml")) ~msg:"Failed to write autodiscovered example";
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
      let pkg = Tusk_model.Package.from_toml
        manifest
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:tmpdir
        ~relative_path:(Path.v "packages/demo")
      |> Result.expect ~msg:"Expected package manifest to parse" in
      let binary_names = pkg.binaries |> List.map (fun (bin: Tusk_model.Package.binary) -> bin.name) in
      match binary_names with
      | ["test_https_httpbin";"simple_https"] -> Ok ()
      | _ ->
          Error (
            "expected explicit example binary to suppress autodiscovery \
              duplicate, got: [" ^ String.concat ", " binary_names ^ "]"
          ))

let test_workspace_fmt_ignore_parses = fun _ctx ->
  let toml =
    Std.Data.Toml.parse
      {|
[workspace]
members = ["packages/demo"]

[tusk.fmt]
ignore = ["fixtures", "generated"]
|}
    |> Result.expect ~msg:"expected workspace TOML to parse"
  in
  let config = Tusk_model.Fmt_config.of_toml toml in
  Test.assert_equal ~expected:[ "fixtures"; "generated" ] ~actual:config.ignore_patterns;
  Ok ()

let test_package_fmt_ignore_loads = fun _ctx ->
  with_tempdir "tusk_model_fmt_config"
    (fun tmpdir ->
      let manifest_path = Path.(tmpdir / Path.v "tusk.toml") in
      Fs.write
        {|
[package]
name = "demo"
version = "0.1.0"

[tusk.fmt]
ignore = ["tests/fixtures", "vendor"]
|}
        manifest_path |> Result.expect ~msg:"expected package manifest to write";
      let config = Tusk_model.Fmt_config.load manifest_path in
      Test.assert_equal ~expected:[ "tests/fixtures"; "vendor" ] ~actual:config.ignore_patterns;
      Ok ())

let test_legacy_fmt_ignore_still_loads = fun _ctx ->
  let toml =
    Std.Data.Toml.parse
      {|
[fmt]
ignore = ["fixtures"]
|}
    |> Result.expect ~msg:"expected legacy fmt TOML to parse"
  in
  let config = Tusk_model.Fmt_config.of_toml toml in
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
  let pkg = Tusk_model.Package.from_toml
    manifest
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:(Path.v "/tmp/demo")
    ~relative_path:(Path.v "packages/demo")
  |> Result.expect ~msg:"expected package manifest to parse" in
  match pkg.dependencies with
  | [
    {
      Tusk_model.Package.source={
        workspace=false;
        builtin=false;
        path=None;
        version=Some requirement
      };
      _
    }
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
  match Tusk_model.Package.from_toml
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
    Std.Data.Toml.parse
      {|
[package]
name = "demo"
version = "0.1.0"

[dependencies]
std = "*"
|}
    |> Result.expect ~msg:"expected package TOML to parse"
  in
  let pkg = Tusk_model.Package.from_toml
    manifest
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:(Path.v "/tmp/demo")
    ~relative_path:(Path.v "packages/demo")
  |> Result.expect ~msg:"expected package manifest to parse" in
  match pkg.dependencies with
  | [
    {
      Tusk_model.Package.source={
        workspace=false;
        builtin=false;
        path=None;
        version=Some requirement
      };
      _
    }
  ] ->
      Test.assert_equal ~expected:"*" ~actual:(Std.Version.requirement_to_string requirement);
      Ok ()
  | _ -> Error "expected '*' package dependency to become an unconstrained registry dependency"

let test_package_builtin_dependency_parses_structurally = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse
      {|
[package]
name = "demo"
version = "0.1.0"

[dependencies]
stdlib = "*"
|}
    |> Result.expect ~msg:"expected package TOML to parse"
  in
  let pkg = Tusk_model.Package.from_toml
    manifest
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:(Path.v "/tmp/demo")
    ~relative_path:(Path.v "packages/demo")
  |> Result.expect ~msg:"expected package manifest to parse" in
  match pkg.dependencies with
  | [ { Tusk_model.Package.name="stdlib"; source={ builtin=true; version=Some requirement; _ } } ] when String.equal
    (Std.Version.requirement_to_string requirement)
    "*" -> Ok ()
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
  match Tusk_model.Package.from_toml
    manifest
    ~workspace_deps:[]
    ~workspace_dev_deps:[]
    ~workspace_build_deps:[]
    ~path:(Path.v "/tmp/demo")
    ~relative_path:(Path.v "packages/demo") with
  | Ok _ -> Error "expected builtin dependency version constraints to fail"
  | Error _ -> Ok ()

let test_package_json_roundtrips_registry_requirement = fun _ctx ->
  let requirement = Std.Version.parse_requirement ">= 1.2.3" |> Result.expect ~msg:"expected requirement to parse" in
  let package =
    Tusk_model.Package.{
      name = "demo";
      path = Path.v "/tmp/demo";
      relative_path = Path.v "packages/demo";
      dependencies = [ { name = "std"; source = source ~version:requirement () } ];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
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
  in
  let decoded = Tusk_model.Package.to_json package
  |> Tusk_model.Package.from_json
  |> Result.expect ~msg:"expected package JSON to roundtrip" in
  match decoded.dependencies with
  | [
    {
      Tusk_model.Package.source={
        workspace=false;
        builtin=false;
        path=None;
        version=Some requirement
      };
      _
    }
  ] ->
      Test.assert_equal ~expected:">= 1.2.3" ~actual:(Std.Version.requirement_to_string requirement);
      Ok ()
  | _ -> Error "expected registry dependency after JSON roundtrip"

let test_workspace_dependency_requirement_parses_structurally = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse
      {|
[workspace]
members = []

[dependencies]
std = ">= 1.2.3"
|}
    |> Result.expect ~msg:"expected workspace TOML to parse"
  in
  let workspace_manifest = Tusk_model.Workspace.of_toml manifest |> Result.expect ~msg:"expected workspace manifest to parse" in
  match workspace_manifest.dependencies with
  | [
    {
      Tusk_model.Package.source={
        workspace=false;
        builtin=false;
        path=None;
        version=Some requirement
      };
      _
    }
  ] ->
      Test.assert_equal ~expected:">= 1.2.3" ~actual:(Std.Version.requirement_to_string requirement);
      Ok ()
  | _ -> Error "expected a parsed workspace registry dependency requirement"

let test_workspace_star_requirement_becomes_unconstrained_registry_dep = fun _ctx ->
  let manifest =
    Std.Data.Toml.parse
      {|
[workspace]
members = []

[dependencies]
std = "*"
|}
    |> Result.expect ~msg:"expected workspace TOML to parse"
  in
  let workspace_manifest = Tusk_model.Workspace.of_toml manifest |> Result.expect ~msg:"expected workspace manifest to parse" in
  match workspace_manifest.dependencies with
  | [
    {
      Tusk_model.Package.source={
        workspace=false;
        builtin=false;
        path=None;
        version=Some requirement
      };
      _
    }
  ] ->
      Test.assert_equal ~expected:"*" ~actual:(Std.Version.requirement_to_string requirement);
      Ok ()
  | _ -> Error "expected '*' workspace dependency to become an unconstrained registry dependency"

let test_workspace_manager_resolves_member_path_dependencies_relative_to_package = fun _ctx ->
  with_tempdir "tusk_model_workspace_paths"
    (fun root ->
      let write path content = Fs.write content path
      |> Result.expect ~msg:(("expected write to succeed: " ^ Path.to_string path)) in
      let mkdir path = Fs.create_dir_all path
      |> Result.expect ~msg:(("expected mkdir to succeed: " ^ Path.to_string path)) in
      mkdir Path.(root / Path.v "packages/app/src");
      mkdir Path.(root / Path.v "packages/vendor/src");
      mkdir Path.(root / Path.v "packages/kernel/src");
      write Path.(root / Path.v "tusk.toml")
        {|
[workspace]
members = ["packages/app"]
|};
      write Path.(root / Path.v "packages/app/tusk.toml")
        {|
[package]
name = "app"
version = "0.1.0"

[dependencies]
vendor = { path = "../vendor" }
|};
      write Path.(root / Path.v "packages/vendor/tusk.toml")
        {|
[package]
name = "vendor"
version = "0.1.0"

[dependencies]
kernel = { path = "../kernel" }
|};
      write Path.(root / Path.v "packages/kernel/tusk.toml")
        {|
[package]
name = "kernel"
version = "0.1.0"
|};
      match Tusk_model.Workspace_manager.scan root with
      | Error err -> Error err
      | Ok (workspace, errors) ->
          if errors != [] then
            Error ("expected no workspace loading errors, got: "
            ^ String.concat "; " (List.map Tusk_model.Workspace_manager.load_error_to_string errors))
          else
            let names = workspace.Tusk_model.Workspace.packages
            |> List.map (fun p -> p.Tusk_model.Package.name)
            |> List.sort String.compare in
            Test.assert_equal ~expected:[ "app"; "kernel"; "vendor" ] ~actual:names;
            Ok ())

let test_user_config_parses_registry_api_token = fun _ctx ->
  let toml =
    Std.Data.Toml.parse
      {|
[registry."pkgs.ml"]
api_token = "root-secret"
|}
    |> Result.expect ~msg:"expected user config TOML to parse"
  in
  match Tusk_model.User_config.of_toml toml with
  | Error err -> Error (Tusk_model.User_config.message err)
  | Ok config -> (
      match Tusk_model.User_config.api_token config ~registry_name:"pkgs.ml" with
      | Some token when String.equal token "root-secret" -> Ok ()
      | _ -> Error "expected pkgs.ml API token to be parsed from config"
    )

let test_user_config_load_reads_config_file = fun _ctx ->
  with_tempdir "tusk_model_user_config"
    (fun tmpdir ->
      let config_path = Path.(tmpdir / Path.v "config.toml") in
      Fs.write
        {|
[registry."pkgs.ml"]
api_token = "publish-token"
|}
        config_path |> Result.expect ~msg:"expected config to write";
      match Tusk_model.User_config.load config_path with
      | Error err -> Error (Tusk_model.User_config.message err)
      | Ok config -> (
          match Tusk_model.User_config.api_token config ~registry_name:"pkgs.ml" with
          | Some token when String.equal token "publish-token" -> Ok ()
          | _ -> Error "expected config loader to expose registry token"
        ))

let test_user_config_parses_empty_registry_entry = fun _ctx ->
  let toml =
    Std.Data.Toml.parse
      {|
[registry."pkgs.ml"]
|}
    |> Result.expect ~msg:"expected user config TOML to parse"
  in
  match Tusk_model.User_config.of_toml toml with
  | Error err -> Error (Tusk_model.User_config.message err)
  | Ok config -> (
      match Tusk_model.User_config.api_token config ~registry_name:"pkgs.ml" with
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
  match Tusk_model.User_config.of_toml toml with
  | Error err -> Error (Tusk_model.User_config.message err)
  | Ok config -> (
      match
        List.find_opt
          (fun (name, _registry) ->
            String.equal name "pkgs.ml")
          config.Tusk_model.User_config.registries
      with
      | None -> Error "expected pkgs.ml registry entry to be present"
      | Some (_name, registry) ->
          if not (String.equal (Net.Uri.to_string registry.api_url) "https://api.pkgs.ml/") then
            Error "expected api_url to parse"
          else if not (String.equal (Net.Uri.to_string registry.cdn_url) "https://cdn.pkgs.ml/") then
            Error "expected cdn_url to parse"
          else if not (registry.api_token = Some "publish-token") then
            Error "expected api_token to parse"
          else
            Ok ()
    )

let test_user_config_save_roundtrips_default_registry_config = fun _ctx ->
  with_tempdir "tusk_model_user_config_default"
    (fun tmpdir ->
      let config_path = Path.(tmpdir / Path.v "config.toml") in
      Tusk_model.User_config.save Tusk_model.User_config.default config_path |> Result.expect ~msg:"expected default config to write";
      match Tusk_model.User_config.load config_path with
      | Error err -> Error (Tusk_model.User_config.message err)
      | Ok config -> (
          match Tusk_model.User_config.api_token config ~registry_name:"pkgs.ml" with
          | None -> Ok ()
          | Some _ -> Error "expected saved default config to keep missing api_token"
        ))

let test_debug_profile_defaults_to_native_with_debug_symbols = fun _ctx ->
  let profile = Tusk_model.Profile.debug in
  let flags = Tusk_model.Profile.to_compiler_flags profile in
  if
    profile.kind = Tusk_model.Ocaml_compiler.Native
    && List.mem "-inline" flags
    && List.mem "0" flags
    && List.mem "-g" flags
  then
    Ok ()
  else
    Error ("expected debug profile to default to native with -inline 0 -g, got kind="
    ^ Tusk_model.Ocaml_compiler.compilation_kind_to_string profile.kind
    ^ " flags=["
    ^ String.concat ", " flags
    ^ "]")

let test_release_profile_defaults_to_strict_native_optimization = fun _ctx ->
  let profile = Tusk_model.Profile.release in
  let flags = Tusk_model.Profile.to_compiler_flags profile in
  if not (profile.kind = Tusk_model.Ocaml_compiler.Native) then
    Error "expected release profile to stay native"
  else if not (List.mem "-noassert" flags) then
    Error "expected release profile to include -noassert"
  else if not (List.mem "-compact" flags) then
    Error "expected release profile to include -compact"
  else if not (List.mem "-inline" flags && List.mem "100" flags) then
    Error "expected release profile to include -inline 100"
  else if not (List.mem "-warn-error" flags && List.mem "+a" flags) then
    Error "expected release profile to treat all warnings as errors"
  else
    Ok ()

let tests =
  Test.[
    case "for_scope: build drops commands and runtime outputs" test_build_scope_drops_commands_and_runtime_outputs;
    case "for_scope: runtime keeps commands" test_runtime_scope_keeps_commands;
    case "for_scope: dev keeps only dev outputs" test_dev_scope_keeps_only_dev_outputs;
    case "package: explicit binaries suppress autodiscovery duplicates" test_explicit_binaries_override_autodiscovery;
    case "fmt config: workspace ignore parses" test_workspace_fmt_ignore_parses;
    case "fmt config: package ignore loads" test_package_fmt_ignore_loads;
    case "fmt config: legacy top-level fmt still loads" test_legacy_fmt_ignore_still_loads;
    case "package: registry dependency requirement parses structurally" test_package_dependency_requirement_parses_structurally;
    case "package: invalid dependency requirement fails" test_package_dependency_invalid_requirement_fails;
    case "package: star dependency becomes unconstrained registry dependency" test_package_star_requirement_becomes_unconstrained_registry_dep;
    case "package: builtin dependency parses structurally" test_package_builtin_dependency_parses_structurally;
    case "package: builtin dependency rejects version constraints" test_package_builtin_dependency_rejects_version_constraints;
    case "package: registry dependency JSON roundtrips" test_package_json_roundtrips_registry_requirement;
    case "workspace: registry dependency requirement parses structurally" test_workspace_dependency_requirement_parses_structurally;
    case "workspace: star dependency becomes unconstrained registry dependency" test_workspace_star_requirement_becomes_unconstrained_registry_dep;
    case "workspace manager: package path deps resolve relative to declaring package" test_workspace_manager_resolves_member_path_dependencies_relative_to_package;
    case "user config: parses empty registry entry" test_user_config_parses_empty_registry_entry;
    case "user config: parses registry urls" test_user_config_parses_registry_urls;
    case "user config: parses registry API token" test_user_config_parses_registry_api_token;
    case "user config: loads config file" test_user_config_load_reads_config_file;
    case "user config: saves default registry config" test_user_config_save_roundtrips_default_registry_config;
    case "profile: debug defaults to native with debug symbols" test_debug_profile_defaults_to_native_with_debug_symbols;
    case "profile: release defaults to strict native optimization" test_release_profile_defaults_to_strict_native_optimization;
  ]

let name = "Tusk Model Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
