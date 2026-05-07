open Std
open Std.Result.Syntax

type run_request = {
  workspace: Riot_model.Workspace.t;
  package_name: Riot_model.Package_name.t option;
  binary_name: string;
  profile: string;
  args: string list;
}

type source_run_request = {
  source_spec: string;
  binary_name: string;
  profile: string;
  update: bool;
  args: string list;
}

type runnable_binary = {
  package_name: Riot_model.Package_name.t;
  binary_name: string;
  source_path: Path.t;
}

type built_binary = {
  package_name: Riot_model.Package_name.t;
  binary_name: string;
  path: Path.t;
  args: string list;
}

type run_event =
  | Build of Riot_build.Event.t
  | RunningBinary of {
      package: Riot_model.Package_name.t;
      binary: string;
      args: string list;
    }

type run_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of {
      package_name: Riot_model.Package_name.t;
      binary_name: string;
    }
  | BuildFailed of Riot_build.error
  | ArtifactNotFound of {
      package_name: Riot_model.Package_name.t;
      binary_name: string;
      reason: string;
    }
  | ProcessExited of int
  | SystemError of string
  | ExternalTargetLoadFailed of {
      target: string;
      error: Riot_deps.package_error;
    }

let no_event: run_event -> unit = fun _ -> ()

let realized_runnable_packages = fun ?package_filter (workspace: Riot_model.Workspace.t) ->
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Run workspace
  |> List.filter ~fn:Riot_model.Package.is_workspace_member
  |> List.filter
    ~fn:(fun (pkg: Riot_model.Package.t) ->
      match package_filter with
      | None -> true
      | Some package_name -> Riot_model.Package_name.equal package_name pkg.name)

let build_scope_for_binary = fun (workspace: Riot_model.Workspace.t) ~package_name ~binary_name ->
  match List.find
    (Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Dev workspace)
    ~fn:(fun (pkg: Riot_model.Package.t) -> Riot_model.Package_name.equal pkg.name package_name) with
  | None -> Riot_build.Request.Runtime
  | Some pkg -> (
      match Riot_model.Package.scope_of_binary_name pkg ~binary_name with
      | Some Riot_model.Package.Dev -> Riot_build.Request.Dev
      | Some Riot_model.Package.Normal
      | Some Riot_model.Package.Build
      | None -> Riot_build.Request.Runtime
    )

let is_listed_runnable = fun (bin: Riot_model.Package.binary) ->
  let path = Path.to_string bin.path in
  (not (String.starts_with ~prefix:"tests/" path)
  && not (String.starts_with ~prefix:"examples/" path)
  && not (String.starts_with ~prefix:"bench/" path))
  || String.starts_with ~prefix:"examples/" path

let list_binaries = fun (workspace: Riot_model.Workspace.t) ?package_filter () ->
  realized_runnable_packages ?package_filter workspace
  |> List.flat_map
    ~fn:(fun (pkg: Riot_model.Package.t) ->
      pkg.binaries
      |> List.filter ~fn:is_listed_runnable
      |> List.map
        ~fn:(fun (bin: Riot_model.Package.binary) -> {
          package_name = pkg.name;
          binary_name = bin.name;
          source_path = Path.(pkg.path / bin.path);
        }))
  |> List.sort
    ~compare:(fun (left:runnable_binary) (right:runnable_binary) ->
      match Riot_model.Package_name.compare left.package_name right.package_name with
      | Order.EQ -> String.compare left.binary_name right.binary_name
      | diff -> diff)

let resolve_binary = fun ~(workspace:Riot_model.Workspace.t) ~package_name ~binary_name ->
  let packages = realized_runnable_packages ?package_filter:package_name workspace in
  match package_name with
  | Some expected_package -> (
      match List.find
        packages
        ~fn:(fun (pkg: Riot_model.Package.t) ->
          Riot_model.Package_name.equal pkg.name expected_package
          && List.any
            pkg.binaries
            ~fn:(fun (bin: Riot_model.Package.binary) -> String.equal bin.name binary_name)) with
      | Some pkg -> Ok pkg.name
      | None -> Error (BinaryNotFoundInPackage { package_name = expected_package; binary_name })
    )
  | None -> (
      match List.find
        packages
        ~fn:(fun (pkg: Riot_model.Package.t) ->
          List.any
            pkg.binaries
            ~fn:(fun (bin: Riot_model.Package.binary) -> String.equal bin.name binary_name)) with
      | Some pkg -> Ok pkg.name
      | None -> Error (BinaryNotFound { binary_name })
    )

let run_error_message = fun __tmp1 ->
  match __tmp1 with
  | BinaryNotFound { binary_name } -> "binary '" ^ binary_name ^ "' not found"
  | BinaryNotFoundInPackage { package_name; binary_name } ->
      "binary '"
      ^ binary_name
      ^ "' not found in package '"
      ^ Riot_model.Package_name.to_string package_name
      ^ "'"
  | BuildFailed err -> Riot_build.error_message err
  | ArtifactNotFound { reason; _ } -> reason
  | ProcessExited code -> "process exited with " ^ Int.to_string code
  | SystemError msg -> msg
  | ExternalTargetLoadFailed { target; error } ->
      "failed to load external target '" ^ target ^ "': " ^ Riot_deps.package_error_message error

let run_event_to_json = fun __tmp1 ->
  match __tmp1 with
  | Build event -> Riot_build.Event.to_json event
  | RunningBinary { package; binary; args } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "RunningBinary");
        ("package", Data.Json.String (Riot_model.Package_name.to_string package));
        ("binary", Data.Json.String binary);
        ("args", Data.Json.Array (List.map args ~fn:Data.Json.string));
      ])

let make_pm_event = fun ~session_id kind ->
  Riot_model.Event.create
    ~session_id
    ~level:Riot_model.Event.Info
    kind

let emit_pm_build_event = fun ~session_id ~on_event kind ->
  on_event
    (Build (Riot_build.Event.Pm (make_pm_event ~session_id kind)))

let load_source_workspace = fun ~on_event ~source_spec ~update ->
  let session_id = Riot_model.Session_id.make () in
  let workspace_manager = Riot_model.Workspace_manager.create () in
  Riot_deps.load_source_workspace
    ~workspace_manager
    ~emit:(emit_pm_build_event ~session_id ~on_event)
    ~update
    ~spec:source_spec
    ()
  |> Result.map_err ~fn:(fun error -> ExternalTargetLoadFailed { target = source_spec; error })

let find_built_binary_path = fun
  ~(store:Riot_store.Store.t) ~(output:Riot_build.Build_result.t) ~package_name ~binary_name ->
  let ensure_executable_binary_path path =
    match Fs.metadata path with
    | Error err -> Error ("failed to read binary metadata: " ^ IO.error_message err)
    | Ok metadata ->
        let mode = Fs.Metadata.mode metadata in
        if mode land 0o111 != 0 then
          Ok path
        else
          Fs.set_permissions path (Fs.Permissions.from_mode (mode lor 0o111))
          |> Result.map ~fn:(fun () -> path)
          |> Result.map_err
            ~fn:(fun err -> "failed to mark binary executable: " ^ IO.error_message err)
  in
  match Riot_build.Build_result.find_package output package_name
  |> Option.and_then
    ~fn:(fun package_output -> Riot_build.Build_result.find_export package_output binary_name) with
  | None ->
      Error (ArtifactNotFound {
        package_name;
        binary_name;
        reason = "binary '" ^ binary_name ^ "' was not produced by build output";
      })
  | Some export_entry -> (
      match Riot_store.Store.export_source_path store export_entry with
      | Some path ->
          ensure_executable_binary_path path
          |> Result.map_err
            ~fn:(fun reason -> ArtifactNotFound { package_name; binary_name; reason })
      | None ->
          Error (ArtifactNotFound {
            package_name;
            binary_name;
            reason = "binary '" ^ binary_name ^ "' resolved to an invalid absolute export path";
          })
    )

let build_profile = fun name ->
  match name with
  | "release" -> Riot_model.Profile.release
  | _ -> Riot_model.Profile.debug

let build_binary = fun ?(on_event = no_event) (request: run_request) ->
  let* package_name =
    resolve_binary
      ~workspace:request.workspace
      ~package_name:request.package_name
      ~binary_name:request.binary_name
  in
  let scope =
    build_scope_for_binary request.workspace ~package_name ~binary_name:request.binary_name
  in
  let build_request =
    Riot_build.Request.make
      ~workspace:request.workspace
      ~packages:[ package_name ]
      ~targets:Riot_model.Target.Host
      ~scope
      ~profile:(build_profile request.profile)
      ()
  in
  let* output =
    Riot_build.build ~on_event:(fun event -> on_event (Build event)) build_request
    |> Result.map_err ~fn:(fun err -> BuildFailed err)
  in
  let store =
    Riot_store.Store.create_for_lane
      ~workspace:request.workspace
      ~profile:request.profile
      ~target:(Riot_model.Riot_dirs.host_target ())
  in
  let* path = find_built_binary_path ~store ~output ~package_name ~binary_name:request.binary_name in
  Ok {
    package_name;
    binary_name = request.binary_name;
    path;
    args = request.args;
  }

let build_source_binary = fun ?(on_event = no_event) (request: source_run_request) ->
  let* loaded =
    load_source_workspace ~on_event ~source_spec:request.source_spec ~update:request.update
  in
  build_binary
    ~on_event
    {
      workspace = loaded.workspace;
      package_name = Some loaded.package_name;
      binary_name = request.binary_name;
      profile = request.profile;
      args = request.args;
    }

let run = fun ?(on_event = no_event) (request: run_request) ->
  let* built = build_binary ~on_event request in
  on_event
    (RunningBinary {
      package = built.package_name;
      binary = built.binary_name;
      args = built.args;
    });
  let cmd = Command.make (Path.to_string built.path) ~args:built.args in
  match Command.status cmd with
  | Ok 0 -> Ok ()
  | Ok code -> Error (ProcessExited code)
  | Error (Command.SystemError msg) -> Error (SystemError msg)

let run_source = fun ?(on_event = no_event) (request: source_run_request) ->
  let* built = build_source_binary ~on_event request in
  on_event
    (RunningBinary {
      package = built.package_name;
      binary = built.binary_name;
      args = built.args;
    });
  let cmd = Command.make (Path.to_string built.path) ~args:built.args in
  match Command.status cmd with
  | Ok 0 -> Ok ()
  | Ok code -> Error (ProcessExited code)
  | Error (Command.SystemError msg) -> Error (SystemError msg)
