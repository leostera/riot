open Std
open Std.Result.Syntax

type request = {
  workspace: Riot_model.Workspace.t;
  detached: Detached.t option;
  on_event: Riot_build.Event.t -> unit;
}

type session = {
  mutable workspace: Riot_model.Workspace.t;
  detached: Detached.t option;
  toolchain: Riot_toolchain.t;
  session_dir: Std.Path.t;
  module_prefix: string;
  host_includes: Std.Path.t list;
  mutable includes: Std.Path.t list;
  mutable requested_packages: Riot_model.Package_name.t list;
  mutable loaded_libraries: string list;
  mutable package_aliases: package_alias list;
  mutable next_phrase_id: int;
  mutable opened_modules: string list;
  mutable loaded_modules: string list;
  args: string list option;
  on_event: Riot_build.Event.t -> unit;
}

and package_alias = {
  public_root: string;
  compiled_root: string;
}

type library_archive = {
  package_name: Riot_model.Package_name.t;
  public_root: string;
  compiled_root: string;
  archive: Std.Path.t;
}

let profile = Riot_model.Profile.debug

let no_event: Riot_build.Event.t -> unit = fun _ -> ()

let host_target = Riot_model.Riot_dirs.host_target ()

let host_includes_env = "RIOT_EVAL_HOST_INCLUDES"

let std_package_name =
  Riot_model.Package_name.from_string "std"
  |> Std.Result.expect ~msg:"std package name should be valid"

let kernel_package_name =
  Riot_model.Package_name.from_string "kernel"
  |> Std.Result.expect ~msg:"kernel package name should be valid"

let runtime_package_names = [ kernel_package_name; std_package_name ]

let is_runtime_package_name = fun package_name ->
  Std.List.any runtime_package_names ~fn:(Riot_model.Package_name.equal package_name)

let fs_error_message = fun prefix error -> prefix ^ ": " ^ Std.IO.error_message error

let unique_paths = fun paths ->
  let rec loop seen acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Std.List.reverse acc
    | path :: rest ->
        if Std.List.any seen ~fn:(Std.String.equal path) then
          loop seen acc rest
        else
          loop (path :: seen) (path :: acc) rest
  in
  loop [] [] paths

let unique_package_names = fun package_names ->
  Std.List.unique package_names ~compare:Riot_model.Package_name.compare

let base_includes = fun session_dir build_includes ->
  unique_paths ([ session_dir; Std.Path.v "+unix"; Std.Path.v "+dynlink" ] @ build_includes)

let absolute_path = fun path ->
  if Std.Path.is_absolute path then
    path
  else
    match Std.Env.current_dir () with
    | Ok cwd -> Std.Path.(cwd / path)
    | Error _ -> path

let path_is_dir = fun path ->
  match Std.Fs.is_dir path with
  | Ok true -> true
  | Ok false
  | Error _ -> false

let child_dirs = fun dir ->
  if not (path_is_dir dir) then
    []
  else
    match Std.Fs.read_dir dir with
    | Error _ -> []
    | Ok entries ->
        entries
        |> Std.Iter.MutIterator.to_list
        |> Std.List.map ~fn:(Std.Path.join dir)
        |> Std.List.filter ~fn:path_is_dir

let child_files = fun dir ->
  if not (path_is_dir dir) then
    []
  else
    match Std.Fs.read_dir dir with
    | Error _ -> []
    | Ok entries ->
        entries
        |> Std.Iter.MutIterator.to_list
        |> Std.List.map ~fn:(Std.Path.join dir)
        |> Std.List.filter
          ~fn:(fun path ->
            match Std.Fs.is_dir path with
            | Ok false -> true
            | Ok true
            | Error _ -> false)

let file_hash = fun path ->
  match Std.Fs.read path with
  | Ok content -> Some (Std.Crypto.hash_string content |> Std.Crypto.Digest.hex)
  | Error _ -> None

let archive_key = fun path ->
  match file_hash path with
  | None -> None
  | Some hash -> Some (Std.Path.basename path, hash)

let archive_key_matches = fun keys path ->
  match archive_key path with
  | None -> false
  | Some key -> Std.List.any keys ~fn:(fun candidate -> candidate = key)

let archive_paths_in_out_dir = fun out_dir ->
  out_dir
  |> child_dirs
  |> Std.List.flat_map ~fn:child_files
  |> Std.List.filter ~fn:(fun path -> Std.String.ends_with ~suffix:".cmxa" (Std.Path.basename path))

let package_artifact_dirs = fun target_root ->
  Std.Path.(target_root / Std.Path.v "cache" / Std.Path.v "package-artifacts" / Std.Path.v "trees")
  |> child_dirs
  |> Std.List.flat_map ~fn:child_dirs

let package_artifact_matches = fun keys dir ->
  child_files dir
  |> Std.List.any ~fn:(fun path ->
    Std.String.ends_with ~suffix:".cmxa" (Std.Path.basename path)
    && archive_key_matches keys path)

let package_artifact_dirs_for_out_dir = fun out_dir ->
  let keys =
    out_dir
    |> archive_paths_in_out_dir
    |> Std.List.filter_map ~fn:archive_key
  in
  if Std.List.is_empty keys then
    []
  else
    out_dir
    |> Std.Path.dirname
    |> package_artifact_dirs
    |> Std.List.filter ~fn:(package_artifact_matches keys)

let out_dir_from_root = fun root ~profile_name ->
  Std.Path.(
    root
    / Std.Path.v "_build"
    / Std.Path.v profile_name
    / Std.Path.v (Riot_model.Target.to_string host_target)
    / Std.Path.v "out"
  )

let inferred_host_include_roots = fun executable ->
  let executable = absolute_path (Std.Path.v executable) in
  let parent = Std.Path.dirname executable in
  let root_copy_release_out = out_dir_from_root parent ~profile_name:Riot_model.Profile.release.name in
  let root_copy_debug_out = out_dir_from_root parent ~profile_name:Riot_model.Profile.debug.name in
  let direct_out =
    if Std.String.equal (Std.Path.basename parent) "riot-cli" then
      Some (Std.Path.dirname parent)
    else
      None
  in
  let root_copy_out =
    if path_is_dir root_copy_release_out then
      root_copy_release_out
    else
      root_copy_debug_out
  in
  unique_paths (Std.Option.to_list direct_out @ [ root_copy_out ])

let inferred_host_includes = fun () ->
  match Std.Env.args with
  | executable :: _ ->
      executable
      |> inferred_host_include_roots
      |> Std.List.flat_map ~fn:package_artifact_dirs_for_out_dir
  | [] -> []

let host_includes = fun () ->
  let includes =
    match Std.Env.get Std.Env.String ~var:host_includes_env with
    | None -> []
    | Some raw ->
        raw
        |> Std.String.split ~by:"\n"
        |> Std.List.filter ~fn:(fun item -> not (Std.String.equal (Std.String.trim item) ""))
        |> Std.List.map ~fn:Std.Path.v
  in
  if Std.List.is_empty includes then
    inferred_host_includes ()
  else
    includes

let session_id = fun () ->
  let pid = Std.Process.id () |> Std.Int32.to_string in
  let nanos = Std.Time.SystemTime.(now () |> nanos) |> Std.Int64.to_string in
  pid ^ "-" ^ nanos

let module_prefix = fun id ->
  "eval_"
  ^ Std.String.map
    id
    ~fn:(fun ch ->
      match ch with
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9' -> ch
      | _ -> '_')

let session_dir = fun target_dir_root id ->
  Std.Path.(target_dir_root / Std.Path.v "eval" / Std.Path.v "sessions" / Std.Path.v id)

let init_toolchain = fun workspace_root ->
  let config = Riot_model.Toolchain_config.from_root ~root:workspace_root in
  Riot_toolchain.init ~config

let of_workspace = fun ?(on_event = no_event) workspace -> { workspace; detached = None; on_event }

let detached = fun ?(on_event = no_event) ?(packages = []) () ->
  let* detached = Detached.create () in
  let* workspace = Detached.workspace ~on_event detached ~packages in
  Ok { workspace; detached = Some detached; on_event }

let with_packages = fun (request: request) ~packages ->
  match request.detached with
  | None -> Ok request
  | Some detached ->
      let* workspace = Detached.workspace ~on_event:request.on_event detached ~packages in
      Ok { request with workspace }

let with_dependencies = fun (request: request) ~dependencies ->
  match request.detached with
  | None -> Ok request
  | Some detached ->
      let* workspace =
        Detached.workspace_with_dependencies
          ~on_event:request.on_event
          detached
          ~dependencies
      in
      Ok { request with workspace }

let ensure_session_packages = fun (session: session) ~packages ->
  match session.detached with
  | None -> Ok ()
  | Some detached ->
      let* workspace = Detached.workspace ~on_event:session.on_event detached ~packages in
      session.workspace <- workspace;
      Ok ()

let package_request_for_session = fun (session: session) ~package_names ->
  match session.detached with
  | Some _ -> (
      match package_names with
      | [] -> [ Detached.bootstrap_package_name ]
      | package_names -> [ Detached.loader_package_name package_names ]
    )
  | None -> package_names

let create_session = fun ?args (request: request) ->
  let workspace = request.workspace in
  let* toolchain = init_toolchain workspace.Riot_model.Workspace.root in
  let session_id = session_id () in
  let session_dir = session_dir workspace.target_dir_root session_id in
  let* () =
    Std.Fs.create_dir_all session_dir
    |> Std.Result.map_err ~fn:(fs_error_message "failed to create eval session directory")
  in
  let host_includes = host_includes () in
  Ok {
    workspace;
    detached = request.detached;
    toolchain;
    session_dir;
    module_prefix = module_prefix session_id;
    host_includes;
    includes = base_includes session_dir host_includes;
    requested_packages = [];
    loaded_libraries = [];
    package_aliases = [];
    next_phrase_id = 1;
    opened_modules = [];
    loaded_modules = [];
    args;
    on_event = request.on_event;
  }
