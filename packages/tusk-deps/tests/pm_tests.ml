open Std
module Test = Std.Test

let source = fun ?(workspace = false) ?(builtin = false) ?path ?version () ->
  Tusk_model.Package.{ workspace; builtin; path; version }

let make_sources = fun () ->
  Tusk_model.Package.{
    src = [];
    native = [];
    tests = [];
    examples = [];
    bench = [];
  }

let make_package = fun ?(dependencies = []) ?(build_dependencies = []) ?(dev_dependencies = []) ~name ~path () ->
  let publish =
    Tusk_model.Package.{
      version = Some (Std.Version.make ~major:0 ~minor:1 ~patch:0 ());
      description = Some ("Package " ^ name);
      license = Some "Apache-2.0";
      is_public = Some true
    } in
  Tusk_model.Package.{
    name;
    path;
    relative_path = path;
    dependencies;
    dev_dependencies;
    build_dependencies;
    foreign_dependencies = [];
    binaries = [];
    library = None;
    sources = make_sources ();
    compiler = { profile_overrides = []; target_overrides = [] };
    commands = [];
    fix_providers = [];
    publish;
  }

let make_registry_cache = fun () ->
  Pkgs_ml.Registry_cache.create ~tusk_home:(Path.v "/Users/example/.tusk") ~registry_name:"pkgs.ml" ()
  |> Result.expect ~msg:"expected registry cache to initialize"

let make_release = fun ?(dependencies = []) ~version () ->
  Pkgs_ml.Sparse_index.{
    version;
    published_at = "2026-04-01T00:00:00Z";
    canonical_locator = "github.com/example/" ^ version;
    repo_url = "https://github.com/example/repo";
    subdir = ".";
    sha = "deadbeef";
    description = None;
    license = Some "Apache-2.0";
    homepage = None;
    repository = Some "https://github.com/example/repo";
    root_module = None;
    categories = [];
    keywords = [];
    manifest_key = "manifests/" ^ version ^ ".json";
    source_key = "sources/" ^ version ^ ".tar.gz";
    dependencies;
  }

let make_registry_dependency = fun name ->
  Pkgs_ml.Sparse_index.{ name; raw = Data.Json.Object [ ("name", Data.Json.String name) ] }

let make_registry_document = fun ?(releases = []) ~name ~latest () ->
  Pkgs_ml.Sparse_index.{
    schema_version = 1;
    name;
    latest;
    updated_at = "2026-04-01T00:00:00Z";
    releases;
  }

let make_registry = fun packages ->
  Pkgs_ml.Registry.in_memory ~cache:(make_registry_cache ()) ~packages ()

let make_registry_with_releases = fun ~packages ~releases ->
  Pkgs_ml.Registry.in_memory ~cache:(make_registry_cache ()) ~packages ~releases ()

let write_package_manifest = fun ~root contents ->
  Fs.create_dir_all root |> Result.expect ~msg:"expected package root to be created";
  Fs.write contents Path.(root / Path.v "tusk.toml") |> Result.expect ~msg:"expected package manifest to be written"

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let run_lock_deps = fun ?emit ?(registry = make_registry []) ?(workspace_root = Path.v "/workspace") ~mode ~existing_lock packages ->
  Tusk_deps.Dep_solver.lock_deps ?emit ~mode ~registry ~existing_lock ~workspace_root packages

let collect_event_names = fun fn ->
  let names = ref [] in
  let emit event =
    names := Tusk_model.Event.name event :: !names
  in
  match fn emit with
  | Ok value -> Ok (value, List.rev !names)
  | Error err -> Error err

let pm_error_message = Tusk_model.Pm_error.message

let write_file = fun path contents ->
  let parent =
    match Path.parent path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  Fs.create_dir_all parent |> Result.expect ~msg:"expected parent directory to be created";
  Fs.write contents path |> Result.expect ~msg:"expected file to be written"

let list_tar_entries = fun artifact_path ->
  match Command.make "tar" ~args:[ "-tzf"; Path.to_string artifact_path ] |> Command.output with
  | Error (Command.SystemError err) -> Error ("failed to spawn tar: " ^ err)
  | Ok output when not (Int.equal output.status 0) -> Error ("failed to list artifact entries: "
  ^ output.stderr)
  | Ok output -> Ok (String.split_on_char '\n' output.stdout
  |> List.filter (fun line -> not (String.equal line "")))

let run_git = fun ~cwd args ->
  let command = Command.make
    "env"
    ~args:(([
      "-u";
      "GIT_DIR";
      "-u";
      "GIT_WORK_TREE";
      "-u";
      "GIT_INDEX_FILE";
      "git";
      "-C";
      Path.to_string cwd;
    ]
    @ args)) in
  match Command.output command with
  | Error (Command.SystemError err) ->
      Error ("failed to spawn git: " ^ err)
  | Ok output when not (Int.equal output.status 0) ->
      let detail =
        if String.equal output.stderr "" then
          output.stdout
        else
          output.stderr
      in
      Error ("git command failed: " ^ detail)
  | Ok output ->
      Ok (String.trim output.stdout)

let run_git_steps = fun ~cwd commands ->
  let rec loop outputs = function
    | [] -> Ok (List.rev outputs)
    | args :: rest -> (
        match run_git ~cwd args with
        | Ok output -> loop (output :: outputs) rest
        | Error _ as err -> err
      )
  in
  loop [] commands

type recorded_request = {
  method_: string;
  url: string;
  headers: (string * string) list;
  body: string option;
}

let make_fetch_recorder = fun ?(post_handler = fun _uri ~headers:_ ~body:_ -> Error "unexpected POST") get_handler ->
  let requests = ref [] in
  let record ~method_ uri ~headers ~body =
    requests := { method_; url = Net.Uri.to_string uri; headers; body } :: !requests
  in
  let fetch =
    Pkgs_ml.Registry.make_fetch
      ~get:(fun uri ->
        record ~method_:"GET" uri ~headers:[] ~body:None;
        get_handler uri)
      ~post:(fun uri ~headers ~body ->
        record ~method_:"POST" uri ~headers ~body:(Some body);
        post_handler uri ~headers ~body)
      ()
  in
  (fetch, requests)

let test_publisher_rejects_path_only_runtime_dependencies = fun () ->
  let package = make_package
    ~name:"demo"
    ~path:(Path.v "/workspace/packages/demo")
    ~dependencies:[ { name = "std"; source = source ~path:(Path.v "../std") () } ]
    () in
  match Tusk_deps.Publisher.validate_runtime_dependencies ~package with
  | Ok () -> Error "expected path-only runtime dependency to be rejected for publish"
  | Error (Tusk_deps.Publisher.RuntimeDependencyNotPublishable {
    dependency;
    reason=`PathOnly path;
    _
  }) ->
      if String.equal dependency "std" && Path.equal path (Path.v "../std") then
        Ok ()
      else
        Error "unexpected path-only runtime dependency payload"
  | Error err -> Error ("unexpected publish validation error: " ^ Tusk_deps.Publisher.message err)

let test_publisher_allows_path_with_version_runtime_dependencies = fun () ->
  let package = make_package
    ~name:"demo"
    ~path:(Path.v "/workspace/packages/demo")
    ~dependencies:[
      { name = "std"; source = source ~path:(Path.v "../std") ~version:Std.Version.any () }
    ]
    () in
  match Tusk_deps.Publisher.validate_runtime_dependencies ~package with
  | Ok () -> Ok ()
  | Error err -> Error ("expected path+version runtime dependency to be publishable: "
  ^ Tusk_deps.Publisher.message err)

let test_publisher_creates_package_root_tarball = fun () ->
  with_tempdir "tusk_deps_publish_tarball"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file Path.(package_root / Path.v "tusk.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      write_file Path.(package_root / Path.v "README.md") "# Demo\n";
      write_file Path.(package_root / Path.v "_build/ignore.txt") "ignore\n";
      write_file Path.(package_root / Path.v ".git/config") "ignore\n";
      write_file Path.(package_root / Path.v "node_modules/left-pad.js") "ignore\n";
      write_file Path.(package_root / Path.v ".DS_Store") "ignore\n";
      let package = make_package ~name:"demo" ~path:package_root () in
      match Tusk_deps.Publisher.create_artifact
        ~target_dir_root:root
        ~package
        ~version:(Std.Version.make ~major:0 ~minor:1 ~patch:0 ()) with
      | Error err -> Error ("expected artifact creation to succeed: " ^ Tusk_deps.Publisher.message err)
      | Ok artifact -> (
          match list_tar_entries artifact with
          | Error _ as err -> err
          | Ok entries ->
              let entries = List.sort String.compare entries in
              let expected = List.sort String.compare [ "README.md"; "src/demo.ml"; "tusk.toml" ] in
              if entries = expected then
                Ok ()
              else
                Error ("unexpected publish artifact entries: " ^ String.concat "," entries)
        ))

let test_publisher_rejects_symlink_entries = fun () ->
  with_tempdir "tusk_deps_publish_symlink"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file Path.(package_root / Path.v "tusk.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      let link = Path.(package_root / Path.v "README.md") in
      Fs.symlink ~src:(Path.v "src/demo.ml") ~dst:link |> Result.expect ~msg:"expected symlink to be created";
      let package = make_package ~name:"demo" ~path:package_root () in
      match Tusk_deps.Publisher.create_artifact
        ~target_dir_root:root
        ~package
        ~version:(Std.Version.make ~major:0 ~minor:1 ~patch:0 ()) with
      | Ok _ -> Error "expected publisher to reject symlink entries"
      | Error (Tusk_deps.Publisher.SymlinkNotAllowed { path }) ->
          if Path.equal path link then
            Ok ()
          else
            Error "unexpected symlink rejection path"
      | Error err -> Error ("unexpected publish artifact error: " ^ Tusk_deps.Publisher.message err))

let test_publisher_publishes_from_locator = fun () ->
  with_tempdir "tusk_deps_publish_from_locator"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file Path.(package_root / Path.v "tusk.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      let package = make_package ~name:"demo" ~path:package_root () in
      let fetch, requests =
        make_fetch_recorder
          ~post_handler:(fun _uri ~headers:_ ~body:_ ->
            Ok {
              Pkgs_ml.Registry.status_code = 200;
              body =
                {|{
  "package": "github.com/example/demo",
  "source_url": "https://github.com/example/demo",
  "package_subdir": ".",
  "selector": "main",
  "resolved_sha": "0123456789abcdef0123456789abcdef01234567",
  "package_name": "demo",
  "package_version": "0.1.0",
  "manifest": {
    "key": "packages/github.com/example/demo/manifest.json",
    "url": "https://api.pkgs.ml/v1/packages/github.com/example/demo/manifest/main.json",
    "cdn_url": "https://cdn.pkgs.ml/packages/github.com/example/demo/manifest.json"
  },
  "source_archive": {
    "key": "sources/github.com/example/demo/source.tar.gz",
    "url": "https://api.pkgs.ml/v1/packages/github.com/example/demo/source/main.tar.gz",
    "cdn_url": "https://cdn.pkgs.ml/sources/github.com/example/demo/source.tar.gz"
  },
  "claim": {
    "key": "claims/demo.json",
    "created": true
  },
  "release": {
    "key": "releases/demo/0.1.0.json",
    "created": true
  },
  "materialization": {
    "manifest": false,
    "source": false
  }
}|};
            })
          (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
      in
      let registry = Pkgs_ml.Registry.filesystem ~fetch (make_registry_cache ()) in
      match Tusk_deps.Publisher.publish_from_locator
        ~registry
        ~target_dir_root:root
        ~package
        ~locator:"github.com/example/demo"
        ~selector:"main"
        ~api_token:"root-secret" with
      | Error err -> Error ("expected publish to succeed: " ^ Tusk_deps.Publisher.message err)
      | Ok published -> (
          match List.rev !requests with
          | [ request ] ->
              let has_header name value =
                List.exists
                  (fun (header_name, header_value) ->
                    String.equal header_name name && String.equal header_value value)
                  request.headers
              in
              if
                String.equal request.method_ "POST"
                && String.equal request.url "https://api.pkgs.ml/v1/publish"
                && has_header "authorization" "Bearer root-secret"
                && has_header "content-type" "application/gzip"
                && String.equal published.package_name "demo"
                && String.equal published.package_version "0.1.0"
              then
                Ok ()
              else
                Error "unexpected publish request or response"
          | _ -> Error "expected exactly one publish request"
        ))

let test_publisher_bubbles_registry_publish_errors = fun () ->
  with_tempdir "tusk_deps_publish_registry_error"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file Path.(package_root / Path.v "tusk.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      let package = make_package ~name:"demo" ~path:package_root () in
      let fetch, _requests =
        make_fetch_recorder
          ~post_handler:(fun _uri ~headers:_ ~body:_ ->
            Ok {
              Pkgs_ml.Registry.status_code = 404;
              body =
                {|{
  "error": "package_not_found",
  "message": "package `demo` was not found in registry `pkgs.ml`"
}|};
            })
          (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
      in
      let registry = Pkgs_ml.Registry.filesystem ~fetch (make_registry_cache ()) in
      match Tusk_deps.Publisher.publish_from_locator
        ~registry
        ~target_dir_root:root
        ~package
        ~locator:"github.com/example/demo"
        ~selector:"main"
        ~api_token:"root-secret" with
      | Ok _ -> Error "expected publish to bubble registry error"
      | Error (Tusk_deps.Publisher.RegistryPublishFailed { locator; error }) ->
          if
            String.equal locator "github.com/example/demo" && String.equal error "package `demo` was not found in registry `pkgs.ml`"
          then
            Ok ()
          else
            Error "unexpected registry publish error payload"
      | Error err -> Error ("unexpected publish error: " ^ Tusk_deps.Publisher.message err))

let test_publisher_workspace_publish_order_uses_runtime_local_dependencies = fun () ->
  let core = make_package ~name:"core" ~path:(Path.v "packages/core") () in
  let util = make_package
    ~name:"util"
    ~path:(Path.v "packages/util")
    ~dependencies:[ { name = "core"; source = source ~workspace:true () } ]
    () in
  let app = make_package
    ~name:"app"
    ~path:(Path.v "packages/app")
    ~dependencies:[
      { name = "util"; source = source ~path:(Path.v "../util") ~version:Std.Version.any () }
    ]
    () in
  match Tusk_deps.Publisher.workspace_publish_order ~packages:[ app; util; core ] with
  | Error err -> Error ("expected publish order to succeed: " ^ Tusk_deps.Publisher.message err)
  | Ok ordered ->
      if List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) ordered = [ "core"; "util"; "app" ] then
        Ok ()
      else
        Error "unexpected workspace publish order"

let test_publisher_workspace_publish_order_ignores_dev_and_build_dependencies = fun () ->
  let core = make_package ~name:"core" ~path:(Path.v "packages/core") () in
  let app = make_package
    ~name:"app"
    ~path:(Path.v "packages/app")
    ~build_dependencies:[ { name = "core"; source = source ~workspace:true () } ]
    ~dev_dependencies:[ { name = "core"; source = source ~workspace:true () } ]
    () in
  match Tusk_deps.Publisher.workspace_publish_order ~packages:[ app; core ] with
  | Error err -> Error ("expected publish order to succeed: " ^ Tusk_deps.Publisher.message err)
  | Ok ordered ->
      if List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) ordered = [ "app"; "core" ] then
        Ok ()
      else
        Error "expected workspace publish order to ignore dev/build edges"

let test_publisher_workspace_publish_order_reports_cycles = fun () ->
  let a = make_package
    ~name:"a"
    ~path:(Path.v "packages/a")
    ~dependencies:[ { name = "b"; source = source ~workspace:true () } ]
    () in
  let b = make_package
    ~name:"b"
    ~path:(Path.v "packages/b")
    ~dependencies:[ { name = "a"; source = source ~workspace:true () } ]
    () in
  match Tusk_deps.Publisher.workspace_publish_order ~packages:[ a; b ] with
  | Ok _ -> Error "expected cyclic workspace publish order to fail"
  | Error (Tusk_deps.Publisher.CyclicWorkspacePublishOrder _) -> Ok ()
  | Error err -> Error ("unexpected publish order error: " ^ Tusk_deps.Publisher.message err)

let test_publisher_validate_registry_dependencies_skips_workspace_publish_set = fun () ->
  let core = make_package ~name:"core" ~path:(Path.v "packages/core") () in
  let app = make_package
    ~name:"app"
    ~path:(Path.v "packages/app")
    ~dependencies:[
      { name = "core"; source = source ~path:(Path.v "../core") ~version:Std.Version.any () }
    ]
    () in
  let registry = make_registry [] in
  match Tusk_deps.Publisher.validate_registry_dependencies
    ~registry
    ~publishing_workspace_packages:[ core.name; app.name ]
    ~package:app with
  | Ok () -> Ok ()
  | Error err -> Error ("expected workspace publish set to skip registry lookup: "
  ^ Tusk_deps.Publisher.message err)

let test_git_provenance_discovers_nested_package_locator = fun () ->
  with_tempdir "tusk_deps_git_provenance_nested"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file Path.(package_root / Path.v "tusk.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      match run_git_steps
        ~cwd:root
        [
          [ "init"; "-q" ];
          [ "config"; "user.email"; "demo@example.com" ];
          [ "config"; "user.name"; "Demo" ];
          [ "remote"; "add"; "origin"; "https://github.com/example/riot.git" ];
          [ "add"; "." ];
          [ "-c"; "commit.gpgsign=false"; "commit"; "-qm"; "init" ];
        ] with
      | Ok _ -> (
          let canonical_root = Fs.canonicalize root |> Result.expect ~msg:"expected temp repo root to canonicalize" in
          match Tusk_deps.Git_provenance.discover ~package_root with
          | Error err -> Error ("expected git provenance discovery to succeed: "
          ^ Tusk_deps.Git_provenance.message err)
          | Ok provenance ->
              if
                String.equal provenance.locator "github.com/example/riot/packages/demo"
                && Path.equal provenance.repository_root canonical_root
                && provenance.package_subdir = Some (Path.v "packages/demo")
                && String.equal provenance.origin_url "https://github.com/example/riot.git"
                && String.length provenance.selector = 40
              then
                Ok ()
              else
                Error "unexpected nested git provenance"
        )
      | Error err -> Error err)

let test_git_provenance_discovers_repo_root_locator = fun () ->
  with_tempdir "tusk_deps_git_provenance_root"
    (fun root ->
      write_file Path.(root / Path.v "tusk.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(root / Path.v "src/demo.ml") "let answer = 42\n";
      match run_git_steps
        ~cwd:root
        [
          [ "init"; "-q" ];
          [ "config"; "user.email"; "demo@example.com" ];
          [ "config"; "user.name"; "Demo" ];
          [ "remote"; "add"; "origin"; "git@github.com:example/demo.git" ];
          [ "add"; "." ];
          [ "-c"; "commit.gpgsign=false"; "commit"; "-qm"; "init" ];
        ] with
      | Ok _ -> (
          let canonical_root = Fs.canonicalize root |> Result.expect ~msg:"expected temp repo root to canonicalize" in
          match Tusk_deps.Git_provenance.discover ~package_root:root with
          | Error err -> Error ("expected git provenance discovery to succeed: "
          ^ Tusk_deps.Git_provenance.message err)
          | Ok provenance ->
              if
                String.equal provenance.locator "github.com/example/demo"
                && Path.equal provenance.repository_root canonical_root
                && provenance.package_subdir = None
                && String.equal provenance.origin_url "git@github.com:example/demo.git"
                && String.length provenance.selector = 40
              then
                Ok ()
              else
                Error "unexpected root git provenance"
        )
      | Error err -> Error err)

let test_publisher_publish_discovers_git_provenance = fun () ->
  with_tempdir "tusk_deps_publish_with_git_provenance"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file Path.(package_root / Path.v "tusk.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      match run_git_steps
        ~cwd:root
        [
          [ "init"; "-q" ];
          [ "config"; "user.email"; "demo@example.com" ];
          [ "config"; "user.name"; "Demo" ];
          [ "remote"; "add"; "origin"; "https://github.com/example/riot.git" ];
          [ "add"; "." ];
          [ "-c"; "commit.gpgsign=false"; "commit"; "-qm"; "init" ];
        ] with
      | Ok _ -> (
          match run_git ~cwd:package_root [ "rev-parse"; "HEAD" ] with
          | Error err -> Error err
          | Ok selector -> (
              let package = make_package ~name:"demo" ~path:package_root () in
              let fetch, requests =
                make_fetch_recorder
                  ~post_handler:(fun _uri ~headers:_ ~body:_ ->
                    Ok {
                      Pkgs_ml.Registry.status_code = 200;
                      body =
                        {|{
  "package": "github.com/example/riot/packages/demo",
  "source_url": "https://github.com/example/riot",
  "package_subdir": "packages/demo",
  "selector": "ignored-by-test",
  "resolved_sha": "0123456789abcdef0123456789abcdef01234567",
  "package_name": "demo",
  "package_version": "0.1.0",
  "manifest": {
    "key": "packages/github.com/example/riot/packages/demo/manifest.json",
    "url": "https://api.pkgs.ml/v1/packages/github.com/example/riot/packages/demo/manifest/main.json",
    "cdn_url": "https://cdn.pkgs.ml/packages/github.com/example/riot/packages/demo/manifest.json"
  },
  "source_archive": {
    "key": "sources/github.com/example/riot/packages/demo/source.tar.gz",
    "url": "https://api.pkgs.ml/v1/packages/github.com/example/riot/packages/demo/source/main.tar.gz",
    "cdn_url": "https://cdn.pkgs.ml/sources/github.com/example/riot/packages/demo/source.tar.gz"
  },
  "claim": {
    "key": "claims/demo.json",
    "created": true
  },
  "release": {
    "key": "releases/demo/0.1.0.json",
    "created": true
  },
  "materialization": {
    "manifest": false,
    "source": false
  }
}|};
                    })
                  (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
              in
              let registry = Pkgs_ml.Registry.filesystem ~fetch (make_registry_cache ()) in
              match Tusk_deps.Publisher.publish
                ~registry
                ~target_dir_root:root
                ~publishing_workspace_packages:[]
                ~package
                ~api_token:"root-secret" with
              | Error err -> Error ("expected publish to succeed: " ^ Tusk_deps.Publisher.message err)
              | Ok published -> (
                  match List.rev !requests with
                  | [ request ] ->
                      if
                        String.equal request.method_ "POST"
                        && String.equal
                          request.url
                          ("https://api.pkgs.ml/v1/packages/github.com/example/riot/packages/demo/publish?ref="
                          ^ selector)
                        && String.equal published.package_name "demo"
                      then
                        Ok ()
                      else
                        Error "unexpected publish request discovered from git provenance"
                  | _ -> Error "expected exactly one publish request"
                )
            )
        )
      | Error err -> Error err)

let test_publisher_prepare_publish_discovers_git_provenance_without_registry = fun () ->
  with_tempdir "tusk_deps_prepare_publish"
    (fun root ->
      let package_root = Path.(root / Path.v "packages/demo") in
      write_file Path.(package_root / Path.v "tusk.toml")
        {|
[package]
name = "demo"
version = "0.1.0"
description = "demo"
license = "Apache-2.0"
public = true
|};
      write_file Path.(package_root / Path.v "src/demo.ml") "let answer = 42\n";
      match run_git_steps
        ~cwd:root
        [
          [ "init"; "-q" ];
          [ "config"; "user.email"; "demo@example.com" ];
          [ "config"; "user.name"; "Demo" ];
          [ "remote"; "add"; "origin"; "https://github.com/example/riot.git" ];
          [ "add"; "." ];
          [ "-c"; "commit.gpgsign=false"; "commit"; "-qm"; "init" ];
        ] with
      | Ok _ -> (
          match run_git ~cwd:package_root [ "rev-parse"; "HEAD" ] with
          | Error err -> Error err
          | Ok selector -> (
              let package = make_package ~name:"demo" ~path:package_root () in
              let registry = Pkgs_ml.Registry.filesystem (make_registry_cache ()) in
              match Tusk_deps.Publisher.prepare_publish
                ~registry
                ~target_dir_root:root
                ~publishing_workspace_packages:[]
                ~package with
              | Error err -> Error ("expected prepare_publish to succeed: "
              ^ Tusk_deps.Publisher.message err)
              | Ok prepared ->
                  if
                    String.equal prepared.package.name "demo"
                    && String.equal prepared.locator "github.com/example/riot/packages/demo"
                    && String.equal prepared.selector selector
                    && String.length (Path.to_string prepared.artifact_path) > 0
                  then
                    Ok ()
                  else
                    Error "unexpected prepared publish payload"
            )
        )
      | Error err -> Error err)

let test_lock_deps_projects_workspace_packages = fun () ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = source ~workspace:true () } ]
    ~build_dependencies:[ { name = "std"; source = source ~workspace:true () } ]
    () in
  match run_lock_deps ~mode:Refresh ~existing_lock:None [ app_pkg; std_pkg ] with
  | Error err -> Error ("expected workspace lock projection to succeed: " ^ pm_error_message err)
  | Ok lockfile ->
      let app_lock = List.hd lockfile.packages in
      let std_lock = List.nth lockfile.packages 1 in
      if
        lockfile.format_version = 1
        && app_lock.id.name = "app"
        && std_lock.id.name = "std"
        && app_lock.provenance = Tusk_model.Lockfile.Workspace
        && std_lock.provenance = Tusk_model.Lockfile.Workspace
        && app_lock.root = Some (Path.v "packages/app")
        && std_lock.root = Some (Path.v "packages/std")
        && List.length app_lock.dependencies = 1
        && List.length app_lock.build_dependencies = 1
        && (List.hd app_lock.dependencies).package.name = "std"
      then
        Ok ()
      else
        Error "expected workspace packages to be projected into the lockfile"

let test_lock_deps_resolves_path_dependencies = fun () ->
  with_tempdir "tusk_deps_path_dep"
    (fun workspace_root ->
      let foo_root = Path.(workspace_root / Path.v "vendor/foo") in
      write_package_manifest ~root:foo_root
        {|
[package]
name = "foo"
version = "1.2.3"
|};
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "foo"; source = source ~path:(Path.v "../../vendor/foo") () } ]
        () in
      match run_lock_deps ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err -> Error ("expected path dependency locking to succeed: " ^ pm_error_message err)
      | Ok lockfile -> (
          let app_lock =
            List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "app") lockfile.packages
          in
          let foo_lock =
            List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "foo") lockfile.packages
          in
          match app_lock, foo_lock with
          | Some app_lock, Some foo_lock ->
              if
                List.length lockfile.packages = 2
                && (List.hd app_lock.dependencies).package.name = "foo"
                && foo_lock.root = Some (Path.v "vendor/foo")
                && foo_lock.provenance = Tusk_model.Lockfile.Path (Path.v "../../vendor/foo")
              then
                Ok ()
              else
                Error "expected path dependency to resolve to an exact local lock package"
          | _ -> Error "expected app and foo to appear in the lockfile"
        ))

let test_lock_deps_resolves_transitive_path_dependencies = fun () ->
  with_tempdir "tusk_deps_transitive_path_dep"
    (fun workspace_root ->
      let foo_root = Path.(workspace_root / Path.v "vendor/foo") in
      let bar_root = Path.(workspace_root / Path.v "vendor/bar") in
      write_package_manifest ~root:foo_root
        {|
[package]
name = "foo"
version = "1.2.3"

[dependencies]
bar = { path = "../bar" }
|};
      write_package_manifest ~root:bar_root
        {|
[package]
name = "bar"
version = "2.0.0"
|};
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "foo"; source = source ~path:(Path.v "../../vendor/foo") () } ]
        () in
      match run_lock_deps ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err -> Error ("expected transitive path dependencies to resolve: " ^ pm_error_message err)
      | Ok lockfile -> (
          let foo_lock =
            List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "foo") lockfile.packages
          in
          let bar_lock =
            List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "bar") lockfile.packages
          in
          match foo_lock, bar_lock with
          | Some foo_lock, Some bar_lock ->
              if
                List.length lockfile.packages = 3
                && (List.hd foo_lock.dependencies).package.name = "bar"
                && bar_lock.root = Some (Path.v "vendor/bar")
                && bar_lock.provenance = Tusk_model.Lockfile.Path (Path.v "../bar")
              then
                Ok ()
              else
                Error "expected nested path dependency roots to resolve from the declaring package"
          | _ -> Error "expected both foo and bar lock packages"
        ))

let test_lock_deps_collapses_workspace_path_dependencies = fun () ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = source ~path:(Path.v "../std") () } ]
    () in
  match run_lock_deps ~mode:Refresh ~existing_lock:None [ app_pkg; std_pkg ] with
  | Error err -> Error ("expected workspace path dependency to collapse to workspace package: "
  ^ pm_error_message err)
  | Ok lockfile -> (
      let app_lock =
        List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "app") lockfile.packages
      in
      let std_lock =
        List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "std") lockfile.packages
      in
      match app_lock, std_lock with
      | Some app_lock, Some std_lock ->
          if
            List.length lockfile.packages = 2
            && app_lock.dependencies
            = [
              Tusk_model.Lockfile.{
                name = "std";
                package = { registry = None; name = "std"; version = None }
              }
            ]
            && std_lock.provenance = Tusk_model.Lockfile.Workspace
          then
            Ok ()
          else
            Error "expected workspace path dependency to reuse the workspace lock package"
      | _ -> Error "expected app and std workspace packages to appear in the lockfile"
    )

let test_lock_deps_resolves_registry_dependencies_to_exact_versions = fun () ->
  let requirement = Std.Version.parse_requirement ">= 1.2.3" |> Result.expect ~msg:"expected requirement to parse" in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = source ~version:requirement () } ]
    () in
  let registry = make_registry
    [
      make_registry_document
        ~name:"std"
        ~latest:"0.2.0"
        ~releases:[
          make_release ~version:"0.2.0" ~dependencies:[ make_registry_dependency "kernel" ] ();
        ]
        ();
      make_registry_document
        ~name:"kernel"
        ~latest:"1.0.0"
        ~releases:[ make_release ~version:"1.0.0" () ]
        ();
    ] in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected registry dependency locking to succeed: " ^ pm_error_message err)
  | Ok lockfile -> (
      let app_lock =
        List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "app") lockfile.packages
      in
      let std_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "std" && pkg.id.version = Some "0.2.0")
          lockfile.packages
      in
      let kernel_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) ->
            pkg.id.name = "kernel" && pkg.id.version = Some "1.0.0")
          lockfile.packages
      in
      match app_lock, std_lock, kernel_lock with
      | Some app_lock, Some std_lock, Some kernel_lock ->
          let app_dependency_name, app_dependency_version =
            match app_lock.dependencies with
            | [ dep ] -> (dep.package.name, dep.package.version)
            | _ -> ("", None)
          in
          let std_dependency_name =
            match std_lock.dependencies with
            | [ dep ] -> dep.package.name
            | _ -> ""
          in
          if
            List.length lockfile.packages = 3
            && app_dependency_name = "std"
            && app_dependency_version = Some "0.2.0"
            && std_lock.id.version = Some "0.2.0"
            && std_lock.root = None
            && std_dependency_name = "kernel"
            && kernel_lock.id.version = Some "1.0.0"
          then
            Ok ()
          else
            Error "expected registry dependency to resolve to exact external lock packages"
      | _ -> Error "expected workspace and transitive registry lock packages"
    )

let test_lock_deps_reports_missing_registry_package_with_required_by = fun () ->
  let app_root = Path.v "/workspace/packages/app" in
  let app_pkg = make_package
    ~name:"app"
    ~path:app_root
    ~dependencies:[ { name = "std"; source = source ~version:Std.Version.any () } ]
    () in
  match run_lock_deps ~registry:(make_registry []) ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Ok _ -> Error "expected missing registry package to fail"
  | Error (Tusk_deps.Error.PackageNotFound { package; registry; required_by=Some required_by }) ->
      if
        String.equal package "std"
        && String.equal registry "pkgs.ml"
        && String.equal required_by.package "app"
        && required_by.path = Some app_root
      then
        Ok ()
      else
        Error "expected missing registry package error to include the requiring workspace package"
  | Error err -> Error ("expected missing registry package error, got: " ^ pm_error_message err)

let test_lock_deps_prefers_workspace_packages_over_registry_for_matching_names = fun () ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = source ~version:Std.Version.any () } ]
    () in
  match run_lock_deps
    ~registry:(make_registry [])
    ~mode:Refresh
    ~existing_lock:None [ app_pkg; std_pkg ] with
  | Error err -> Error ("expected workspace package to satisfy matching registry requirement locally: "
  ^ pm_error_message err)
  | Ok lockfile -> (
      let app_lock =
        List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "app") lockfile.packages
      in
      let std_lock =
        List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "std") lockfile.packages
      in
      match app_lock, std_lock with
      | Some app_lock, Some std_lock ->
          if
            List.length lockfile.packages = 2
            && app_lock.dependencies
            = [
              Tusk_model.Lockfile.{
                name = "std";
                package = { registry = None; name = "std"; version = None }
              }
            ]
            && std_lock.id.registry = None
            && std_lock.id.version = None
          then
            Ok ()
          else
            Error "expected matching workspace packages to win over registry resolution"
      | _ -> Error "expected app and std workspace packages to appear in the lockfile"
    )

let test_lock_deps_prefers_available_local_packages_over_registry_dependencies = fun () ->
  with_tempdir "tusk_deps_local_beats_registry"
    (fun workspace_root ->
      let std_root = Path.(workspace_root / Path.v "vendor/std") in
      let fixme_root = Path.(workspace_root / Path.v "vendor/fixme") in
      let model_root = Path.(workspace_root / Path.v "vendor/model") in
      write_package_manifest ~root:std_root
        {|
[package]
name = "std"
version = "0.1.0"

[build-dependencies]
fixme = { path = "../fixme" }
|};
      write_package_manifest ~root:fixme_root
        {|
[package]
name = "fixme"
version = "0.1.0"
|};
      write_package_manifest ~root:model_root
        {|
[package]
name = "model"
version = "0.1.0"

[dependencies]
std = "*"
|};
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[
          { name = "std"; source = source ~path:(Path.v "../../vendor/std") () };
          { name = "model"; source = source ~path:(Path.v "../../vendor/model") () };
        ]
        () in
      let registry = make_registry
        [
          make_registry_document
            ~name:"std"
            ~latest:"9.9.9"
            ~releases:[ make_release ~version:"9.9.9" () ]
            ();
        ] in
      match run_lock_deps ~registry ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err -> Error ("expected local path package to beat registry dependency: "
      ^ pm_error_message err)
      | Ok lockfile -> (
          let local_std =
            List.find_opt
              (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "std" && pkg.id.registry = None)
              lockfile.packages
          in
          let registry_std =
            List.find_opt
              (fun (pkg: Tusk_model.Lockfile.package) ->
                pkg.id.name = "std" && pkg.id.registry = Some "pkgs.ml")
              lockfile.packages
          in
          let model_lock =
            List.find_opt
              (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "model")
              lockfile.packages
          in
          match local_std, registry_std, model_lock with
          | Some local_std, None, Some model_lock ->
              if
                List.length model_lock.dependencies = 1
                && (List.hd model_lock.dependencies).package = local_std.id
              then
                Ok ()
              else
                Error "expected version-only dependency to reuse the available local path package"
          | Some _, Some _, _ -> Error "expected registry std to stay out of the lock graph"
          | _ -> Error "expected local std and model lock packages"
        ))

let test_lock_deps_ignores_builtin_dependencies = fun () ->
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "stdlib"; source = source ~builtin:true ~version:Std.Version.any () } ]
    () in
  match run_lock_deps ~registry:(make_registry []) ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected builtin dependency locking to succeed: " ^ pm_error_message err)
  | Ok lockfile -> (
      match lockfile.packages with
      | [ app_lock ] when app_lock.id.name = "app" && app_lock.dependencies = [] -> Ok ()
      | _ -> Error "expected builtin dependencies to stay out of the lock graph"
    )

let test_lock_deps_ignores_builtin_registry_release_dependencies = fun () ->
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = source ~version:Std.Version.any () } ]
    () in
  let registry = make_registry
    [
      make_registry_document
        ~name:"std"
        ~latest:"0.1.0"
        ~releases:[
          make_release
            ~version:"0.1.0"
            ~dependencies:[ make_registry_dependency "stdlib"; make_registry_dependency "unix" ]
            ();
        ]
        ();
    ] in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected builtin registry dependencies to be ignored: " ^ pm_error_message err)
  | Ok lockfile -> (
      let std_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "std" && pkg.id.version = Some "0.1.0")
          lockfile.packages
      in
      match std_lock with
      | Some pkg when pkg.dependencies = [] && List.length lockfile.packages = 2 -> Ok ()
      | Some _ -> Error "expected builtin registry release dependencies to stay out of the lock graph"
      | None -> Error "expected std registry package to be locked"
    )

let test_lock_deps_handles_cyclic_registry_dependencies = fun () ->
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "foo"; source = source ~version:Std.Version.any () } ]
    () in
  let registry = make_registry
    [
      make_registry_document
        ~name:"foo"
        ~latest:"1.0.0"
        ~releases:[
          make_release ~version:"1.0.0" ~dependencies:[ make_registry_dependency "bar" ] ();
        ]
        ();
      make_registry_document
        ~name:"bar"
        ~latest:"2.0.0"
        ~releases:[
          make_release ~version:"2.0.0" ~dependencies:[ make_registry_dependency "foo" ] ();
        ]
        ();
    ] in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:None [ app_pkg ] with
  | Error err -> Error ("expected cyclic registry dependencies to resolve: " ^ pm_error_message err)
  | Ok lockfile -> (
      let foo_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "foo" && pkg.id.version = Some "1.0.0")
          lockfile.packages
      in
      let bar_lock =
        List.find_opt
          (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "bar" && pkg.id.version = Some "2.0.0")
          lockfile.packages
      in
      match foo_lock, bar_lock with
      | Some foo_lock, Some bar_lock ->
          let foo_dep_name, foo_dep_version =
            match foo_lock.dependencies with
            | [ dep ] -> (dep.package.name, dep.package.version)
            | _ -> ("", None)
          in
          let bar_dep_name, bar_dep_version =
            match bar_lock.dependencies with
            | [ dep ] -> (dep.package.name, dep.package.version)
            | _ -> ("", None)
          in
          if
            List.length lockfile.packages = 3
            && foo_dep_name = "bar"
            && foo_dep_version = Some "2.0.0"
            && bar_dep_name = "foo"
            && bar_dep_version = Some "1.0.0"
          then
            Ok ()
          else
            Error "expected cyclic registry dependencies to terminate with exact cross-links"
      | _ -> Error "expected foo and bar to appear in the cyclic lockfile"
    )

let test_lock_deps_handles_cyclic_local_path_dependencies = fun () ->
  with_tempdir "tusk_deps_cyclic_local_path_dep"
    (fun workspace_root ->
      let std_root = Path.(workspace_root / Path.v "vendor/std") in
      let fixme_root = Path.(workspace_root / Path.v "vendor/fixme") in
      write_package_manifest ~root:std_root
        {|
[package]
name = "std"
version = "0.1.0"

[build-dependencies]
fixme = { path = "../fixme" }
|};
      write_package_manifest ~root:fixme_root
        {|
[package]
name = "fixme"
version = "0.1.0"

[dependencies]
std = { path = "../std" }
|};
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = source ~path:(Path.v "../../vendor/std") () } ]
        () in
      match run_lock_deps ~workspace_root ~mode:Refresh ~existing_lock:None [ app_pkg ] with
      | Error err -> Error ("expected cyclic local path dependencies to resolve: "
      ^ pm_error_message err)
      | Ok lockfile -> (
          let std_lock =
            List.find_opt (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "std") lockfile.packages
          in
          let fixme_lock =
            List.find_opt
              (fun (pkg: Tusk_model.Lockfile.package) -> pkg.id.name = "fixme")
              lockfile.packages
          in
          match std_lock, fixme_lock with
          | Some std_lock, Some fixme_lock ->
              if
                List.length lockfile.packages = 3
                && List.length std_lock.build_dependencies = 1
                && (List.hd std_lock.build_dependencies).package.name = "fixme"
                && List.length fixme_lock.dependencies = 1
                && (List.hd fixme_lock.dependencies).package.name = "std"
              then
                Ok ()
              else
                Error "expected local path dependency cycle to reuse in-flight lock nodes"
          | _ -> Error "expected std and fixme to appear in the local cyclic lockfile"
        ))

let test_lock_refresh_preserves_existing_registry_version = fun () ->
  let requirement = Std.Version.parse_requirement "*" |> Result.expect ~msg:"expected requirement to parse" in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = source ~version:requirement () } ]
    () in
  let existing_lock =
    Tusk_model.Lockfile.{
      format_version = 1;
      packages =
        [ {
            id = { registry = None; name = "app"; version = None };
            root = Some (Path.v "packages/app");
            provenance = Workspace;
            dependencies = [
              {
                name = "std";
                package = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" }
              }
            ];
            build_dependencies = [];
            dev_dependencies = [];
          }; {
            id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" };
            root = None;
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          } ];
    }
  in
  let registry = make_registry
    [
      make_registry_document
        ~name:"std"
        ~latest:"0.2.0"
        ~releases:[ make_release ~version:"0.2.0" () ]
        ();
    ] in
  match run_lock_deps ~registry ~mode:Refresh ~existing_lock:(Some existing_lock) [ app_pkg ] with
  | Error err -> Error ("expected refresh lock to preserve registry version: " ^ pm_error_message err)
  | Ok lockfile ->
      let app_lock = List.hd lockfile.packages in
      if
        List.length lockfile.packages = 2
        && (List.hd app_lock.dependencies).package.version = Some "0.1.0"
        && (List.nth lockfile.packages 1).id.version = Some "0.1.0"
      then
        Ok ()
      else
        Error "expected refresh to preserve existing locked registry selections"

let test_lock_refresh_preserves_existing_external_nodes = fun () ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let existing_lock =
    Tusk_model.Lockfile.{
      format_version = 1;
      packages =
        [ {
            id = { registry = None; name = "app"; version = None };
            root = Some (Path.v "packages/app");
            provenance = Workspace;
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          }; {
            id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" };
            root = None;
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          }; ];
    }
  in
  match run_lock_deps ~mode:Refresh ~existing_lock:(Some existing_lock) [ app_pkg ] with
  | Error err -> Error ("expected refresh lock to preserve existing nodes: " ^ pm_error_message err)
  | Ok lockfile ->
      if
        List.length lockfile.packages = 2
        && (List.nth lockfile.packages 1).id.name = "std"
        && (List.nth lockfile.packages 1).id.version = Some "0.1.0"
      then
        Ok ()
      else
        Error "expected refresh to preserve existing external lock nodes"

let test_unlock_discards_existing_external_nodes = fun () ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let existing_lock =
    Tusk_model.Lockfile.{
      format_version = 1;
      packages =
        [ {
            id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" };
            root = None;
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          }; ];
    }
  in
  match run_lock_deps ~mode:Unlock ~existing_lock:(Some existing_lock) [ app_pkg ] with
  | Error err -> Error ("expected unlock to rebuild workspace nodes: " ^ pm_error_message err)
  | Ok lockfile ->
      if List.length lockfile.packages = 1 && (List.hd lockfile.packages).id.name = "app" then
        Ok ()
      else
        Error "expected unlock to discard preserved external lock nodes"

let test_lock_refresh_requires_lock_when_missing = fun () ->
  with_tempdir "tusk_deps_missing_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected manifest write to succeed";
      match Tusk_deps.Lock_refresh.needs_refresh ~workspace_root ~manifest_paths:[ manifest_path ] with
      | Ok true -> Ok ()
      | Ok false -> Error "expected missing lockfile to require refresh"
      | Error err -> Error err)

let test_lock_refresh_false_when_lock_is_newer = fun () ->
  with_tempdir "tusk_deps_fresh_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected manifest write to succeed";
      sleep (Time.Duration.from_millis 20);
      Fs.write "format_version = 1\npackages = []\n" lock_path |> Result.expect ~msg:"expected lockfile write to succeed";
      match Tusk_deps.Lock_refresh.needs_refresh ~workspace_root ~manifest_paths:[ manifest_path ] with
      | Ok false -> Ok ()
      | Ok true -> Error "expected newer lockfile to avoid refresh"
      | Error err -> Error err)

let test_lock_refresh_true_when_manifest_is_newer = fun () ->
  with_tempdir "tusk_deps_stale_lock"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected manifest write to succeed";
      Fs.write "format_version = 1\npackages = []\n" lock_path |> Result.expect ~msg:"expected lockfile write to succeed";
      sleep (Time.Duration.from_millis 20);
      Fs.write "[workspace]\nmembers = [\"packages/demo\"]\n" manifest_path |> Result.expect ~msg:"expected manifest rewrite to succeed";
      match Tusk_deps.Lock_refresh.needs_refresh ~workspace_root ~manifest_paths:[ manifest_path ] with
      | Ok true -> Ok ()
      | Ok false -> Error "expected newer manifest to require refresh"
      | Error err -> Error err)

let test_lockfile_store_roundtrips = fun () ->
  with_tempdir "tusk_deps_lockfile_store"
    (fun workspace_root ->
      let lockfile =
        Tusk_model.Lockfile.{
          format_version = 1;
          packages =
            [ {
                id = { registry = None; name = "app"; version = None };
                root = Some (Path.v "packages/app");
                provenance = Workspace;
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              }; ];
        }
      in
      match Tusk_deps.Lockfile_store.write ~workspace_root lockfile with
      | Error err -> Error ("expected lockfile write to succeed: " ^ err)
      | Ok () -> (
          match Tusk_deps.Lockfile_store.read ~workspace_root with
          | Error err -> Error ("expected lockfile read to succeed: " ^ err)
          | Ok None -> Error "expected written lockfile to exist"
          | Ok (Some reloaded) ->
              if
                reloaded.format_version = 1
                && List.length reloaded.packages = 1
                && (List.hd reloaded.packages).id.name = "app"
              then
                Ok ()
              else
                Error "expected lockfile store roundtrip to preserve package data"
        ))

let test_lockfile_store_returns_none_when_missing = fun () ->
  with_tempdir "tusk_deps_missing_store"
    (fun workspace_root ->
      match Tusk_deps.Lockfile_store.read ~workspace_root with
      | Ok None -> Ok ()
      | Ok (Some _) -> Error "expected missing lockfile to return none"
      | Error err -> Error err)

let test_lockfile_store_bubbles_parse_errors = fun () ->
  with_tempdir "tusk_deps_invalid_lockfile"
    (fun workspace_root ->
      let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
      Fs.write "not = [valid\n" lock_path |> Result.expect ~msg:"expected invalid lockfile write to succeed";
      match Tusk_deps.Lockfile_store.read ~workspace_root with
      | Ok _ -> Error "expected invalid lockfile to fail"
      | Error err ->
          if
            String.contains err "failed to parse lockfile TOML" || String.contains err "failed to decode lockfile"
          then
            Ok ()
          else
            Error ("unexpected error: " ^ err))

let test_ensure_lock_refreshes_missing_lock_and_resolves_workspace = fun () ->
  with_tempdir "tusk_deps_ensure_lock_missing"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected workspace manifest to be written";
      let std_pkg = make_package ~name:"std" ~path:Path.(workspace_root / Path.v "packages/std") () in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = source ~workspace:true () } ]
        () in
      match collect_event_names
        (fun emit ->
          Tusk_deps.ensure_lock
            ~emit
            ~mode:Tusk_deps.Dep_solver.Refresh
            ~registry:(make_registry [])
            ~workspace_root
            ~manifest_paths:[ manifest_path ]
            ~packages:[ app_pkg; std_pkg ]
            ()) with
      | Error err -> Error ("expected ensure_lock to refresh missing lock: " ^ pm_error_message err)
      | Ok ((lockfile, resolved), event_names) ->
          let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
          if
            List.length lockfile.packages = 2
            && List.length resolved = 2
            && List.mem "tusk.pm.resolution.started" event_names
            && List.mem "tusk.pm.resolution.refreshing_lock" event_names
            && List.mem "tusk.pm.lockfile.write.started" event_names
            && List.mem "tusk.pm.lockfile.write.finished" event_names
            && List.mem "tusk.pm.resolution.finished" event_names
            && Result.unwrap_or ~default:false (Fs.exists lock_path)
          then
            Ok ()
          else
            Error "expected ensure_lock to write a fresh lockfile and emit PM lifecycle events")

let test_ensure_lock_uses_existing_fresh_lock = fun () ->
  with_tempdir "tusk_deps_ensure_lock_existing"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected workspace manifest to be written";
      let std_pkg = make_package ~name:"std" ~path:Path.(workspace_root / Path.v "packages/std") () in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = source ~workspace:true () } ]
        () in
      sleep (Time.Duration.from_millis 20);
      let existing_lock = run_lock_deps
        ~workspace_root
        ~mode:Refresh
        ~existing_lock:None [ app_pkg; std_pkg ]
      |> Result.expect ~msg:"expected workspace lock projection to succeed" in
      Tusk_deps.Lockfile_store.write ~workspace_root existing_lock |> Result.expect ~msg:"expected initial lockfile to be written";
      match collect_event_names
        (fun emit ->
          Tusk_deps.ensure_lock
            ~emit
            ~mode:Tusk_deps.Dep_solver.Refresh
            ~registry:(make_registry [])
            ~workspace_root
            ~manifest_paths:[ manifest_path ]
            ~packages:[ app_pkg; std_pkg ]
            ()) with
      | Error err -> Error ("expected ensure_lock to use existing lock: " ^ pm_error_message err)
      | Ok ((lockfile, resolved), event_names) ->
          if
            List.length lockfile.packages = 2
            && List.length resolved = 2
            && not (List.mem "tusk.pm.resolution.using_existing_lock" event_names)
            && not (List.mem "tusk.pm.lockfile.write.started" event_names)
            && not (List.mem "tusk.pm.resolution.finished" event_names)
          then
            Ok ()
          else
            Error "expected ensure_lock to reuse a fresh existing lock without rewriting it")

let test_ensure_lock_materializes_registry_packages_before_projection = fun () ->
  with_tempdir "tusk_deps_ensure_lock_materializes"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected workspace manifest to be written";
      let requirement = Std.Version.parse_requirement "*" |> Result.expect ~msg:"expected requirement to parse" in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = source ~version:requirement () } ]
        () in
      let registry_cache = Pkgs_ml.Registry_cache.create
        ~tusk_home:Path.(workspace_root / Path.v ".tusk")
        ~registry_name:"pkgs.ml"
        ()
      |> Result.expect ~msg:"expected registry cache to initialize" in
      let registry = Pkgs_ml.Registry.in_memory
        ~cache:registry_cache
        ~packages:[
          make_registry_document
            ~name:"std"
            ~latest:"0.2.0"
            ~releases:[ make_release ~version:"0.2.0" () ]
            ();
        ]
        ~releases:[
          {
            Pkgs_ml.Registry.package_name = "std";
            version = "0.2.0";
            manifest_toml = "[package]\nname = \"std\"\n";
            files = [ { path = Path.v "src/std.ml"; contents = "let answer = 42\n" } ]
          };
        ]
        () in
      match collect_event_names
        (fun emit ->
          Tusk_deps.ensure_lock
            ~emit
            ~mode:Tusk_deps.Dep_solver.Refresh
            ~registry
            ~workspace_root
            ~manifest_paths:[ manifest_path ]
            ~packages:[ app_pkg ]
            ()) with
      | Error err -> Error ("expected ensure_lock to materialize registry packages: "
      ^ pm_error_message err)
      | Ok ((_, resolved), event_names) ->
          let manifest_path = Pkgs_ml.Registry_cache.package_src_dir
            registry_cache
            ~package_name:"std"
            ~version:"0.2.0"
          |> fun root -> Path.(root / Path.v "tusk.toml") in
          if
            List.length resolved = 2
            && Result.unwrap_or ~default:false (Fs.exists manifest_path)
            && List.mem "tusk.pm.universe.building" event_names
            && List.mem "tusk.pm.universe.built" event_names
            && List.mem "tusk.pm.package_metadata.fetch.started" event_names
            && List.mem "tusk.pm.package_metadata.fetch.finished" event_names
            && List.mem "tusk.pm.package_materialization.started" event_names
            && List.mem "tusk.pm.package_materialization.finished" event_names
            && List.mem "tusk.pm.package_manifest.fetch.started" event_names
            && List.mem "tusk.pm.package_manifest.fetch.finished" event_names
            && List.mem "tusk.pm.package_resolved_for_build" event_names
          then
            Ok ()
          else
            Error "expected ensure_lock to materialize external package manifests before projection")

let test_ensure_lock_reuses_existing_lock_and_materializes_missing_registry_packages = fun () ->
  with_tempdir "tusk_deps_ensure_lock_materializes_existing"
    (fun workspace_root ->
      let manifest_path = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = []\n" manifest_path |> Result.expect ~msg:"expected workspace manifest to be written";
      let requirement = Std.Version.parse_requirement "*" |> Result.expect ~msg:"expected requirement to parse" in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = source ~version:requirement () } ]
        () in
      let registry_cache = Pkgs_ml.Registry_cache.create
        ~tusk_home:Path.(workspace_root / Path.v ".tusk")
        ~registry_name:"pkgs.ml"
        ()
      |> Result.expect ~msg:"expected registry cache to initialize" in
      let registry = Pkgs_ml.Registry.in_memory
        ~cache:registry_cache
        ~packages:[
          make_registry_document
            ~name:"std"
            ~latest:"0.2.0"
            ~releases:[ make_release ~version:"0.2.0" () ]
            ();
        ]
        ~releases:[
          {
            Pkgs_ml.Registry.package_name = "std";
            version = "0.2.0";
            manifest_toml = "[package]\nname = \"std\"\n";
            files = []
          };
        ]
        () in
      let existing_lock = Tusk_deps.Dep_solver.lock_deps
        ~mode:Tusk_deps.Dep_solver.Refresh
        ~registry
        ~existing_lock:None
        ~workspace_root
        [ app_pkg ]
      |> Result.expect ~msg:"expected initial lock solve to succeed" in
      Tusk_deps.Lockfile_store.write ~workspace_root existing_lock |> Result.expect ~msg:"expected initial lockfile write to succeed";
      match collect_event_names
        (fun emit ->
          Tusk_deps.ensure_lock
            ~emit
            ~mode:Tusk_deps.Dep_solver.Refresh
            ~registry
            ~workspace_root
            ~manifest_paths:[ manifest_path ]
            ~packages:[ app_pkg ]
            ()) with
      | Error err -> Error ("expected ensure_lock to reuse lock and materialize missing packages: "
      ^ pm_error_message err)
      | Ok ((_, resolved), event_names) ->
          if
            List.length resolved = 2
            && not (List.mem "tusk.pm.resolution.using_existing_lock" event_names)
            && List.mem "tusk.pm.package_materialization.finished" event_names
          then
            Ok ()
          else
            Error "expected ensure_lock to reuse the lock while still materializing missing registry packages")

let test_ensure_workspace_projects_materialized_registry_packages = fun () ->
  with_tempdir "tusk_deps_ensure_workspace"
    (fun workspace_root ->
      let workspace_manifest = Path.(workspace_root / Path.v "tusk.toml") in
      Fs.write "[workspace]\nmembers = [\"packages/app\"]\n" workspace_manifest
      |> Result.expect ~msg:"expected workspace manifest to be written";
      let registry_cache = Pkgs_ml.Registry_cache.create
        ~tusk_home:Path.(workspace_root / Path.v ".tusk")
        ~registry_name:"pkgs.ml"
        ()
      |> Result.expect ~msg:"expected registry cache to initialize" in
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = source ~version:Std.Version.any () } ]
        () in
      let app_pkg = { app_pkg with relative_path = Path.v "packages/app" } in
      let workspace = Tusk_model.Workspace.make ~root:workspace_root ~packages:[ app_pkg ] () in
      let registry = Pkgs_ml.Registry.in_memory ~cache:registry_cache ~packages:[
        make_registry_document
          ~name:"std"
          ~latest:"0.2.0"
          ~releases:[ make_release ~version:"0.2.0" () ]
          ();
      ]
        ~releases:[ {
            Pkgs_ml.Registry.package_name = "std";
            version = "0.2.0";
            manifest_toml =
              {|
[package]
name = "std"
version = "0.2.0"
|};
            files = [];
          } ]
        ()
      in
      match Tusk_deps.ensure_workspace ~mode:Tusk_deps.Dep_solver.Refresh ~registry ~workspace () with
      | Error err -> Error ("expected ensure_workspace to succeed: " ^ pm_error_message err)
      | Ok resolved_workspace ->
          let std_pkg =
            List.find_opt
              (fun (pkg: Tusk_model.Package.t) ->
                String.equal pkg.name "std")
              resolved_workspace.packages
          in
          let expected_std_root = Pkgs_ml.Registry_cache.package_src_dir
            registry_cache
            ~package_name:"std"
            ~version:"0.2.0" in
          match std_pkg with
          | Some std_pkg ->
              if
                List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) resolved_workspace.packages
                = [ "app"; "std" ]
                && Path.equal std_pkg.path expected_std_root
              then
                Ok ()
              else
                Error "expected ensure_workspace to return a build-ready workspace with registry packages"
          | None -> Error "expected ensure_workspace to project std into the workspace")

let test_projection_resolves_workspace_packages = fun () ->
  let std_pkg = make_package ~name:"std" ~path:(Path.v "/workspace/packages/std") () in
  let app_pkg = make_package
    ~name:"app"
    ~path:(Path.v "/workspace/packages/app")
    ~dependencies:[ { name = "std"; source = source ~workspace:true () } ]
    () in
  let lockfile = run_lock_deps
    ~mode:Tusk_deps.Dep_solver.Refresh
    ~existing_lock:None [ app_pkg; std_pkg ]
  |> Result.expect ~msg:"expected lock projection to succeed" in
  match Tusk_deps.Projection.resolve_packages
    ~registry:(make_registry [])
    ~workspace_root:(Path.v "/workspace")
    ~packages:[ app_pkg; std_pkg ]
    ~lockfile
    () with
  | Error err -> Error ("expected projection to resolve workspace packages: " ^ pm_error_message err)
  | Ok resolved ->
      let app = List.hd resolved in
      if
        List.length resolved = 2
        && app.id.name = "app"
        && List.length app.runtime_resolved = 1
        && (List.hd app.runtime_resolved).resolved_id.name = "std"
      then
        Ok ()
      else
        Error "expected projection to preserve resolved runtime dependency ids"

let test_projection_loads_external_manifests_from_lockfile = fun () ->
  with_tempdir "tusk_deps_projection_external"
    (fun workspace_root ->
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = source ~version:Std.Version.any () } ]
        () in
      let std_root = Path.(workspace_root / Path.v ".tusk/registry/pkgs.ml/src/std/0.2.0") in
      let kernel_root = Path.(workspace_root / Path.v ".tusk/registry/pkgs.ml/src/kernel/1.0.0") in
      let std_manifest_path = Path.(std_root / Path.v "tusk.toml") in
      let kernel_manifest_path = Path.(kernel_root / Path.v "tusk.toml") in
      Fs.create_dir_all std_root |> Result.expect ~msg:"expected std root to be created";
      Fs.create_dir_all kernel_root |> Result.expect ~msg:"expected kernel root to be created";
      Fs.write
        {|
[package]
name = "std"
version = "0.2.0"

[dependencies]
kernel = "*"
|}
        std_manifest_path |> Result.expect ~msg:"expected std manifest to be written";
      Fs.write
        {|
[package]
name = "kernel"
version = "1.0.0"
|}
        kernel_manifest_path |> Result.expect ~msg:"expected kernel manifest to be written";
      let lockfile =
        Tusk_model.Lockfile.{
          format_version = 1;
          packages =
            [ {
                id = { registry = None; name = "app"; version = None };
                root = Some (Path.v "packages/app");
                provenance = Workspace;
                dependencies = [
                  {
                    name = "std";
                    package = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.2.0" }
                  }
                ];
                build_dependencies = [];
                dev_dependencies = [];
              }; {
                id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.2.0" };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [
                  {
                    name = "kernel";
                    package = { registry = Some "pkgs.ml"; name = "kernel"; version = Some "1.0.0" }
                  }
                ];
                build_dependencies = [];
                dev_dependencies = [];
              }; {
                id = { registry = Some "pkgs.ml"; name = "kernel"; version = Some "1.0.0" };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              } ];
        }
      in
      let registry_cache = Pkgs_ml.Registry_cache.create
        ~tusk_home:Path.(workspace_root / Path.v ".tusk")
        ~registry_name:"pkgs.ml"
        ()
      |> Result.expect ~msg:"expected registry cache to initialize" in
      let registry = Pkgs_ml.Registry.in_memory ~cache:registry_cache ~packages:[] () in
      match collect_event_names
        (fun emit ->
          Tusk_deps.Projection.resolve_packages
            ~emit
            ~registry
            ~workspace_root
            ~packages:[ app_pkg ]
            ~lockfile
            ()) with
      | Error err -> Error ("expected projection to load external manifests: " ^ pm_error_message err)
      | Ok (resolved, event_names) ->
          let std_resolved =
            List.find_opt
              (fun (pkg: Tusk_model.Package.resolved) ->
                pkg.id.name = "std" && pkg.id.version = Some "0.2.0")
              resolved
          in
          let kernel_resolved =
            List.find_opt
              (fun (pkg: Tusk_model.Package.resolved) ->
                pkg.id.name = "kernel" && pkg.id.version = Some "1.0.0")
              resolved
          in
          match std_resolved, kernel_resolved with
          | Some std_resolved, Some kernel_resolved ->
              if
                List.length resolved = 3
                && List.mem "tusk.pm.package_manifest.fetch.started" event_names
                && List.mem "tusk.pm.package_manifest.fetch.finished" event_names
                && List.mem "tusk.pm.package_resolved_for_build" event_names
                && Path.to_string std_resolved.materialized_root = Path.to_string std_root
                && List.length std_resolved.runtime_resolved = 1
                && (List.hd std_resolved.runtime_resolved).resolved_id.name = "kernel"
                && Path.to_string kernel_resolved.materialized_root = Path.to_string kernel_root
              then
                Ok ()
              else
                Error "expected projection to include external lockfile packages"
          | _ -> Error "expected projection to resolve both std and kernel from external manifests")

let test_projection_bubbles_external_manifest_errors = fun () ->
  with_tempdir "tusk_deps_projection_manifest_error"
    (fun workspace_root ->
      let app_pkg = make_package
        ~name:"app"
        ~path:Path.(workspace_root / Path.v "packages/app")
        ~dependencies:[ { name = "std"; source = source ~version:Std.Version.any () } ]
        () in
      let std_root = Path.(workspace_root / Path.v ".tusk/registry/pkgs.ml/src/std/0.2.0") in
      let std_manifest_path = Path.(std_root / Path.v "tusk.toml") in
      Fs.create_dir_all std_root |> Result.expect ~msg:"expected std root to be created";
      Fs.write
        {|
[package]
name = "std"
version = "0.2.0"

[dependencies]
kernel = 123
|}
        std_manifest_path |> Result.expect ~msg:"expected invalid std manifest to be written";
      let lockfile =
        Tusk_model.Lockfile.{
          format_version = 1;
          packages =
            [ {
                id = { registry = None; name = "app"; version = None };
                root = Some (Path.v "packages/app");
                provenance = Workspace;
                dependencies = [
                  {
                    name = "std";
                    package = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.2.0" }
                  }
                ];
                build_dependencies = [];
                dev_dependencies = [];
              }; {
                id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.2.0" };
                root = None;
                provenance = Registry { registry = "pkgs.ml" };
                dependencies = [];
                build_dependencies = [];
                dev_dependencies = [];
              } ];
        }
      in
      let registry_cache = Pkgs_ml.Registry_cache.create
        ~tusk_home:Path.(workspace_root / Path.v ".tusk")
        ~registry_name:"pkgs.ml"
        ()
      |> Result.expect ~msg:"expected registry cache to initialize" in
      let registry = Pkgs_ml.Registry.in_memory ~cache:registry_cache ~packages:[] () in
      match Tusk_deps.Projection.resolve_packages
        ~registry
        ~workspace_root
        ~packages:[ app_pkg ]
        ~lockfile
        () with
      | Ok _ -> Error "expected invalid external manifest to fail projection"
      | Error err ->
          let message = pm_error_message err in
          if
            String.contains message "must be a string or table" || String.contains message "failed to decode package manifest"
          then
            Ok ()
          else
            Error ("unexpected projection error: " ^ pm_error_message err))

let test_projection_fails_when_lockfile_is_missing_package = fun () ->
  let app_pkg = make_package ~name:"app" ~path:(Path.v "/workspace/packages/app") () in
  let lockfile = Tusk_model.Lockfile.{ format_version = 1; packages = [] } in
  match Tusk_deps.Projection.resolve_packages
    ~registry:(make_registry [])
    ~workspace_root:(Path.v "/workspace")
    ~packages:[ app_pkg ]
    ~lockfile
    () with
  | Ok _ -> Error "expected projection to fail when lockfile is missing package"
  | Error err ->
      if String.contains (pm_error_message err) "lockfile is missing package 'app'" then
        Ok ()
      else
        Error ("unexpected error: " ^ pm_error_message err)

let tests =
  Test.[
    case "dep solver: projects workspace packages into lockfile" test_lock_deps_projects_workspace_packages;
    case "dep solver: resolves path dependencies" test_lock_deps_resolves_path_dependencies;
    case "dep solver: resolves transitive path dependencies" test_lock_deps_resolves_transitive_path_dependencies;
    case "dep solver: collapses workspace path dependencies" test_lock_deps_collapses_workspace_path_dependencies;
    case "dep solver: resolves registry dependencies to exact versions" test_lock_deps_resolves_registry_dependencies_to_exact_versions;
    case "dep solver: reports missing registry packages with required-by context" test_lock_deps_reports_missing_registry_package_with_required_by;
    case "dep solver: prefers workspace packages over registry for matching names" test_lock_deps_prefers_workspace_packages_over_registry_for_matching_names;
    case "dep solver: prefers available local packages over registry dependencies" test_lock_deps_prefers_available_local_packages_over_registry_dependencies;
    case "dep solver: ignores builtin dependencies" test_lock_deps_ignores_builtin_dependencies;
    case "dep solver: ignores builtin registry release dependencies" test_lock_deps_ignores_builtin_registry_release_dependencies;
    case "dep solver: handles cyclic registry dependencies" test_lock_deps_handles_cyclic_registry_dependencies;
    case "dep solver: handles cyclic local path dependencies" test_lock_deps_handles_cyclic_local_path_dependencies;
    case "dep solver: refresh preserves existing registry versions" test_lock_refresh_preserves_existing_registry_version;
    case "dep solver: refresh preserves existing external nodes" test_lock_refresh_preserves_existing_external_nodes;
    case "dep solver: unlock discards existing external nodes" test_unlock_discards_existing_external_nodes;
    case "lock refresh: missing lock requires refresh" test_lock_refresh_requires_lock_when_missing;
    case "lock refresh: newer lock avoids refresh" test_lock_refresh_false_when_lock_is_newer;
    case "lock refresh: newer manifest requires refresh" test_lock_refresh_true_when_manifest_is_newer;
    case "lockfile store: roundtrips root lockfile" test_lockfile_store_roundtrips;
    case "lockfile store: missing lockfile returns none" test_lockfile_store_returns_none_when_missing;
    case "lockfile store: bubbles parse errors" test_lockfile_store_bubbles_parse_errors;
    case "ensure lock: refreshes missing lock and resolves workspace graph" test_ensure_lock_refreshes_missing_lock_and_resolves_workspace;
    case "ensure lock: uses existing fresh lock" test_ensure_lock_uses_existing_fresh_lock;
    case "ensure lock: materializes registry packages before projection" test_ensure_lock_materializes_registry_packages_before_projection;
    case "ensure lock: reuses existing lock and materializes missing registry packages" test_ensure_lock_reuses_existing_lock_and_materializes_missing_registry_packages;
    case "ensure workspace: projects materialized registry packages" test_ensure_workspace_projects_materialized_registry_packages;
    case "projection: resolves workspace packages from lockfile" test_projection_resolves_workspace_packages;
    case "projection: loads external manifests from lockfile" test_projection_loads_external_manifests_from_lockfile;
    case "projection: bubbles external manifest errors" test_projection_bubbles_external_manifest_errors;
    case "projection: fails when lockfile is missing package" test_projection_fails_when_lockfile_is_missing_package;
    case "publisher: rejects path-only runtime dependencies" test_publisher_rejects_path_only_runtime_dependencies;
    case "publisher: allows path+version runtime dependencies" test_publisher_allows_path_with_version_runtime_dependencies;
    case "publisher: creates package-root tarball" test_publisher_creates_package_root_tarball;
    case "publisher: rejects symlink entries" test_publisher_rejects_symlink_entries;
    case "publisher: publishes package artifact from locator" test_publisher_publishes_from_locator;
    case "publisher: bubbles registry publish errors" test_publisher_bubbles_registry_publish_errors;
    case "publisher: workspace publish order uses runtime local dependencies" test_publisher_workspace_publish_order_uses_runtime_local_dependencies;
    case "publisher: workspace publish order ignores dev and build dependencies" test_publisher_workspace_publish_order_ignores_dev_and_build_dependencies;
    case "publisher: workspace publish order reports cycles" test_publisher_workspace_publish_order_reports_cycles;
    case "publisher: validate registry deps skips workspace publish set" test_publisher_validate_registry_dependencies_skips_workspace_publish_set;
    case "git provenance: discovers nested package locator" test_git_provenance_discovers_nested_package_locator;
    case "git provenance: discovers repo root locator" test_git_provenance_discovers_repo_root_locator;
    case "publisher: prepare_publish discovers git provenance automatically" test_publisher_prepare_publish_discovers_git_provenance_without_registry;
    case "publisher: publish discovers git provenance automatically" test_publisher_publish_discovers_git_provenance;
  ]

let name = "Tusk PM Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
