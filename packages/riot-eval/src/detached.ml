open Std
open Std.Result.Syntax

let bootstrap_package_name =
  Riot_model.Package_name.from_string "eval-bootstrap"
  |> Std.Result.expect ~msg:"expected eval-bootstrap package name to be valid"

let std_package_name =
  Riot_model.Package_name.from_string "std"
  |> Std.Result.expect ~msg:"expected std package name to be valid"

let registry_name = "pkgs.ml"

type t = {
  root: Std.Path.t;
  target_dir_root: Std.Path.t;
  registry: Pkgs_ml.Registry.t;
  pm_session_id: Riot_model.Session_id.t;
}

let fs_error_message = fun prefix error -> prefix ^ ": " ^ Std.IO.error_message error

let registry_error_message = fun error ->
  "failed to initialize registry '"
  ^ registry_name
  ^ "': "
  ^ Pkgs_ml.Registry_cache.create_error_message error

let pm_error_message = fun prefix error ->
  prefix ^ ": " ^ Riot_deps.Error.message error

let request_counter = Std.Sync.Atomic.make 0

let short_hash = fun hash ->
  if Std.String.length hash <= 16 then
    hash
  else
    Std.String.sub hash ~offset:0 ~len:16

let sorted_unique_package_names = fun package_names ->
  package_names
  |> Std.List.unique ~compare:Riot_model.Package_name.compare
  |> Std.List.sort ~compare:Riot_model.Package_name.compare

let dependency = fun package_name ->
  Riot_model.Package.{
    name = package_name;
    source = {
      workspace = false;
      builtin = false;
      path = None;
      source_locator = None;
      ref_ = None;
      version = Some Std.Version.any;
    };
  }

let bool_key = fun value ->
  if value then
    "true"
  else
    "false"

let dependency_key = fun (dependency: Riot_model.Package.dependency) ->
  let source = dependency.source in
  Std.String.concat
    ":"
    [
      Riot_model.Package_name.to_string dependency.name;
      bool_key source.workspace;
      bool_key source.builtin;
      (
        match source.path with
        | Some path -> Std.Path.to_string path
        | None -> ""
      );
      Std.Option.unwrap_or ~default:"" source.source_locator;
      Std.Option.unwrap_or ~default:"" source.ref_;
      (
        match source.version with
        | Some requirement -> Std.Version.requirement_to_string requirement
        | None -> ""
      );
    ]

let upsert_dependency = fun
  (dependencies: Riot_model.Package.dependency list)
  ((dependency: Riot_model.Package.dependency) as replacement) ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Std.List.reverse (replacement :: acc)
    | (current: Riot_model.Package.dependency) :: rest
      when Riot_model.Package_name.equal current.name dependency.name ->
        Std.List.append (Std.List.reverse acc) (replacement :: rest)
    | current :: rest -> loop (current :: acc) rest
  in
  loop [] dependencies

let unique_dependencies = fun dependencies ->
  Std.List.fold_left dependencies ~init:[] ~fn:upsert_dependency

let package_dependencies = fun package_names ->
  package_names
  |> Std.List.map ~fn:dependency

let loader_package_name_for_dependencies = fun dependencies ->
  let input =
    dependencies
    |> unique_dependencies
    |> Std.List.map ~fn:dependency_key
    |> Std.List.sort ~compare:Std.String.compare
    |> Std.String.concat "\n"
  in
  let hash =
    Std.Crypto.hash_string input
    |> Std.Crypto.Digest.hex
    |> short_hash
  in
  Riot_model.Package_name.from_string ("eval-loader-" ^ hash)
  |> Std.Result.expect ~msg:"expected eval loader package name to be valid"

let loader_package_name = fun package_names ->
  package_names
  |> sorted_unique_package_names
  |> package_dependencies
  |> loader_package_name_for_dependencies

let main_source =
  Std.String.concat
    "\n"
    [
      "open Std;;";
      "";
      "let main ~args =";
      "  let _ = args in";
      "  Ok ();;";
      "";
      "let () =";
      "  Std.Runtime.run ~main ~args:Std.Env.args ();;";
      "";
    ]

let empty_sources =
  Riot_model.Package.{
    src = [ Std.Path.v "src/main.ml" ];
    native = [];
    tests = [];
    examples = [];
    bench = [];
  }

let materialize_member = fun t package_name ->
  let package_name_string = Riot_model.Package_name.to_string package_name in
  let relative_path = Std.Path.v ("packages/" ^ package_name_string) in
  let package_dir = Std.Path.(t.root / relative_path) in
  let src_dir = Std.Path.(package_dir / Std.Path.v "src") in
  let main_path = Std.Path.(src_dir / Std.Path.v "main.ml") in
  let* () =
    Std.Fs.create_dir_all src_dir
    |> Std.Result.map_err ~fn:(fs_error_message "failed to create detached eval package source")
  in
  let* () =
    Std.Fs.write main_source main_path
    |> Std.Result.map_err ~fn:(fs_error_message "failed to write detached eval package source")
  in
  Ok (package_dir, relative_path)

let member_package = fun t ~package_name ~dependencies ->
  let* (package_dir, relative_path) = materialize_member t package_name in
  let binary_name = Riot_model.Package_name.to_string package_name in
  Ok (
    Riot_model.Package.make
      ~name:package_name
      ~path:package_dir
      ~relative_path
      ~dependencies
      ~binaries:[ Riot_model.Package.{ name = binary_name; path = Std.Path.v "src/main.ml" } ]
      ~sources:empty_sources
      ()
    |> Riot_model.Package_manifest.from_package
  )

let temp_base = fun () ->
  let nonempty var =
    match Std.Env.get Std.Env.String ~var with
    | Some value when not (Std.String.equal (Std.String.trim value) "") -> Some (Std.Path.v value)
    | Some _
    | None -> None
  in
  match nonempty "TMPDIR" with
  | Some path -> path
  | None -> (
      match nonempty "TEMP" with
      | Some path -> path
      | None -> (
          match nonempty "TMP" with
          | Some path -> path
          | None -> Std.Path.v "/tmp"
        )
    )

let request_id = fun () ->
  let pid = Std.Process.id () |> Std.Int32.to_string in
  let nanos = Std.Time.SystemTime.(now () |> nanos) |> Std.Int64.to_string in
  let count = Std.Sync.Atomic.fetch_and_add request_counter 1 |> Std.Int.to_string in
  pid ^ "-" ^ nanos ^ "-" ^ count

let target_dir_root = fun () ->
  Std.Path.(Riot_model.Riot_dirs.dot_riot / Std.Path.v "eval" / Std.Path.v "_build")

let create = fun () ->
  let* registry =
    Pkgs_ml.Registry.create_filesystem ~registry_name ()
    |> Std.Result.map_err ~fn:registry_error_message
  in
  let id = request_id () in
  let root = Std.Path.(temp_base () / Std.Path.v "riot-eval" / Std.Path.v id) in
  Ok {
    root;
    target_dir_root = target_dir_root ();
    registry;
    pm_session_id = Riot_model.Session_id.make ();
  }

let emit_pm_event = fun t on_event kind ->
  on_event
    (Riot_build.Event.Pm
      (Riot_model.Event.create
        ~session_id:t.pm_session_id
        ~level:Riot_model.Event.Info
        kind))

let resolve_workspace = fun
  ?(on_event = fun (_: Riot_build.Event.t) -> ())
  t
  (workspace_manifest: Riot_model.Workspace_manifest.t) ->
  let emit = emit_pm_event t on_event in
  let* lockfile =
    Riot_deps.Dep_solver.lock_deps
      ~emit
      ~mode:Riot_deps.Dep_solver.Unlock
      ~registry:t.registry
      ~existing_lock:None
      ~workspace:workspace_manifest
      ()
    |> Std.Result.map_err ~fn:(pm_error_message "detached eval dependency resolution failed")
  in
  let* resolved_packages =
    Riot_deps.Projection.resolve_packages
      ~emit
      ~materialize_emit:emit
      ~registry:t.registry
      ~workspace_root:t.root
      ~packages:workspace_manifest.packages
      ~lockfile
      ()
    |> Std.Result.map_err ~fn:(pm_error_message "detached eval package projection failed")
  in
  Ok (
    Riot_model.Workspace.make
      ~root:t.root
      ~target_dir:t.target_dir_root
      ~packages:(Std.List.map
        resolved_packages
        ~fn:(fun (pkg: Riot_model.Package.resolved) ->
          Riot_model.Package_manifest.from_package pkg.package))
      ()
  )

let dependency_of_manifest = fun (manifest: Riot_model.Package_manifest.t) ->
  Riot_model.Package.{
    name = manifest.name;
    source = {
      workspace = false;
      builtin = false;
      path = Some manifest.path;
      source_locator = None;
      ref_ = None;
      version = Some Std.Version.any;
    };
  }

let expand_loader_dependencies = fun workspace loader_name ->
  let dependencies =
    workspace.Riot_model.Workspace.packages
    |> Std.List.filter ~fn:(fun (manifest: Riot_model.Package_manifest.t) ->
      not (Riot_model.Package_manifest.is_workspace_member manifest))
    |> Std.List.map ~fn:dependency_of_manifest
  in
  {
    workspace with
    packages =
      Std.List.map
        workspace.packages
        ~fn:(fun (manifest: Riot_model.Package_manifest.t) ->
          if Riot_model.Package_name.equal manifest.name loader_name then
            { manifest with dependencies }
          else
            manifest);
  }

let workspace_with_dependencies = fun ?on_event t ~dependencies ->
  let dependencies = unique_dependencies dependencies in
  let bootstrap_dependencies = package_dependencies [ std_package_name ] in
  let* bootstrap =
    member_package t ~package_name:bootstrap_package_name ~dependencies:bootstrap_dependencies
  in
  let* member_packages =
    if Std.List.is_empty dependencies then
      Ok [ bootstrap ]
    else
      let loader_name = loader_package_name_for_dependencies dependencies in
      let* loader =
        member_package t ~package_name:loader_name ~dependencies
      in
      Ok [ bootstrap; loader ]
  in
  let* workspace =
    Riot_model.Workspace_manifest.make
    ~root:t.root
    ~packages:member_packages
    ()
    |> resolve_workspace ?on_event t
  in
  match dependencies with
  | [] -> Ok workspace
  | _ -> Ok (expand_loader_dependencies workspace (loader_package_name_for_dependencies dependencies))

let workspace = fun ?on_event t ~packages ->
  let dependencies =
    packages
    |> sorted_unique_package_names
    |> package_dependencies
  in
  workspace_with_dependencies ?on_event t ~dependencies
