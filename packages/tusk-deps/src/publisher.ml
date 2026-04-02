open Std

type error =
  | MissingPublishVersion of { package: string }
  | MissingPublishDescription of { package: string }
  | MissingPublishLicense of { package: string }
  | PackageNotPublic of { package: string }
  | MissingManifest of { package_root: Path.t }
  | RuntimeDependencyNotPublishable of { package: string; dependency: string; reason:
        [
          | `PathOnly of Path.t
          | `WorkspaceOnly
          | `MissingVersionOrPath
        ] }
  | RuntimeDependencyRegistryLookupFailed of {
      package: string;
      dependency: string;
      registry: string;
      error: string
    }
  | RuntimeDependencyNotFoundInRegistry of { package: string; dependency: string; registry: string }
  | SymlinkNotAllowed of { path: Path.t }
  | UnsupportedEntry of { path: Path.t; kind: string }
  | DirectoryReadFailed of { path: Path.t; error: string }
  | MetadataReadFailed of { path: Path.t; error: string }
  | ArtifactReadFailed of { path: Path.t; error: string }
  | TarCommandFailed of { command: string; status: int; stdout: string; stderr: string }
  | TarCommandSpawnFailed of { command: string; error: string }
  | GitProvenanceFailed of Git_provenance.error
  | RegistryPublishFailed of { locator: string; error: string }
  | CyclicWorkspacePublishOrder of { cycle: string list }

type prepared_publish = {
  package: Tusk_model.Package.t;
  version: Std.Version.t;
  locator: string;
  selector: string;
  artifact_path: Path.t;
}

type publish_plan = {
  package: Tusk_model.Package.t;
  version: Std.Version.t;
  locator: string;
  selector: string;
}

let excluded_entry_names = [
  "_build";
  ".git";
  ".hg";
  "node_modules";
  ".direnv";
  ".DS_Store";
  "dist";
  "coverage";
  ".turbo";
  ".cache";
  ".tmp";
  "tmp";
]

let message = function
  | MissingPublishVersion { package } ->
      "package '" ^ package ^ "' is missing [package].version"
  | MissingPublishDescription { package } ->
      "package '" ^ package ^ "' is missing [package].description"
  | MissingPublishLicense { package } ->
      "package '" ^ package ^ "' is missing [package].license"
  | PackageNotPublic { package } ->
      "package '" ^ package ^ "' must set [package].public = true to be published"
  | MissingManifest { package_root } ->
      "package root '" ^ Path.to_string package_root ^ "' is missing tusk.toml at archive root"
  | RuntimeDependencyNotPublishable { package; dependency; reason=`PathOnly path } ->
      "runtime dependency '"
      ^ dependency
      ^ "' in package '"
      ^ package
      ^ "' is path-only and cannot be published (path = "
      ^ Path.to_string path
      ^ ")"
  | RuntimeDependencyNotPublishable { package; dependency; reason=`WorkspaceOnly } ->
      "runtime dependency '" ^ dependency ^ "' in package '" ^ package ^ "' is workspace-only and cannot be published"
  | RuntimeDependencyNotPublishable { package; dependency; reason=`MissingVersionOrPath } ->
      "runtime dependency '" ^ dependency ^ "' in package '" ^ package ^ "' must declare a version or publishable source"
  | RuntimeDependencyRegistryLookupFailed { package; dependency; registry; error } ->
      "failed to verify runtime dependency '"
      ^ dependency
      ^ "' for package '"
      ^ package
      ^ "' in registry '"
      ^ registry
      ^ "': "
      ^ error
  | RuntimeDependencyNotFoundInRegistry { package; dependency; registry } ->
      "runtime dependency '"
      ^ dependency
      ^ "' for package '"
      ^ package
      ^ "' was not found in registry '"
      ^ registry
      ^ "'"
  | SymlinkNotAllowed { path } ->
      "publish artifacts do not support symlinks: " ^ Path.to_string path
  | UnsupportedEntry { path; kind } ->
      "publish artifacts only support regular files and directories; found "
      ^ kind
      ^ " at "
      ^ Path.to_string path
  | DirectoryReadFailed { path; error } ->
      "failed to read directory '" ^ Path.to_string path ^ "': " ^ error
  | MetadataReadFailed { path; error } ->
      "failed to read metadata for '" ^ Path.to_string path ^ "': " ^ error
  | ArtifactReadFailed { path; error } ->
      "failed to read publish artifact '" ^ Path.to_string path ^ "': " ^ error
  | TarCommandFailed { command; status; stdout; stderr } ->
      let detail =
        if String.equal stderr "" then
          stdout
        else
          stderr
      in
      "failed to create publish artifact with '"
      ^ command
      ^ "' (exit "
      ^ Int.to_string status
      ^ "): "
      ^ detail
  | TarCommandSpawnFailed { command; error } ->
      "failed to spawn publish artifact command '" ^ command ^ "': " ^ error
  | GitProvenanceFailed error ->
      Git_provenance.message error
  | RegistryPublishFailed { locator; error } ->
      "failed to publish '" ^ locator ^ "': " ^ error
  | CyclicWorkspacePublishOrder { cycle } ->
      "workspace publish order contains a cycle: " ^ String.concat " -> " cycle

let should_skip_entry = fun path ->
  let name = Path.basename path in
  List.exists (String.equal name) excluded_entry_names

let file_kind_to_string = function
  | `Regular -> "regular file"
  | `Directory -> "directory"
  | `Symlink -> "symlink"
  | `Block -> "block device"
  | `Character -> "character device"
  | `Fifo -> "fifo"
  | `Socket -> "socket"

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid utf8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "invalid utf8 from " ^ syscall ^ ": " ^ path
  | Path.SystemError msg -> msg

let validate_publish_metadata = fun ~(package:Tusk_model.Package.t) ->
  match package.publish.version with
  | None -> Error (MissingPublishVersion { package = package.name })
  | Some version -> (
      match package.publish.description with
      | None -> Error (MissingPublishDescription { package = package.name })
      | Some _ -> (
          match package.publish.license with
          | None -> Error (MissingPublishLicense { package = package.name })
          | Some _ -> (
              match package.publish.is_public with
              | Some true -> Ok version
              | Some false
              | None -> Error (PackageNotPublic { package = package.name })
            )
        )
    )

let validate_runtime_dependency = fun ~(package:Tusk_model.Package.t) (
  dep: Tusk_model.Package.dependency
) ->
  match dep.source with
  | { builtin=true; _ } -> Ok ()
  | { workspace=true; _ } -> Error (RuntimeDependencyNotPublishable {
    package = package.name;
    dependency = dep.name;
    reason = `WorkspaceOnly
  })
  | { path=Some path; version=None; _ } -> Error (RuntimeDependencyNotPublishable {
    package = package.name;
    dependency = dep.name;
    reason = `PathOnly path
  })
  | { path=None; version=None; _ } -> Error (RuntimeDependencyNotPublishable {
    package = package.name;
    dependency = dep.name;
    reason = `MissingVersionOrPath
  })
  | _ -> Ok ()

let validate_runtime_dependencies = fun ~(package:Tusk_model.Package.t) ->
  let rec loop = function
    | [] -> Ok ()
    | dep :: rest -> (
        match validate_runtime_dependency ~package dep with
        | Ok () -> loop rest
        | Error _ as err -> err
      )
  in
  loop package.dependencies

let validate_registry_dependencies = fun ~registry ~publishing_workspace_packages ~(package:Tusk_model.Package.t) ->
  let rec loop = function
    | [] -> Ok ()
    | dep :: rest -> (
        if Tusk_model.Package.is_builtin_dependency dep then
          loop rest
        else if List.exists (String.equal dep.name) publishing_workspace_packages then
          loop rest
        else
          match Pkgs_ml.Registry.read_package_document registry ~package_name:dep.name with
          | Error error -> Error (RuntimeDependencyRegistryLookupFailed {
            package = package.name;
            dependency = dep.name;
            registry = Pkgs_ml.Registry.name registry;
            error
          })
          | Ok None -> Error (RuntimeDependencyNotFoundInRegistry {
            package = package.name;
            dependency = dep.name;
            registry = Pkgs_ml.Registry.name registry
          })
          | Ok (Some _) -> loop rest
      )
  in
  loop package.dependencies

let published_version_exists = fun ~registry ~package_name ~version ->
  match Pkgs_ml.Registry.read_package_document registry ~package_name with
  | Error error ->
      Error (RuntimeDependencyRegistryLookupFailed {
        package = package_name;
        dependency = package_name;
        registry = Pkgs_ml.Registry.name registry;
        error
      })
  | Ok None ->
      Ok false
  | Ok (Some document) ->
      let version = Std.Version.to_string version in
      Ok (
        List.exists
          (fun (release: Pkgs_ml.Sparse_index.release) ->
            String.equal release.version version)
          document.releases
      )

let collect_relative_files = fun ~package_root ->
  let rec walk_dir acc dir =
    match Fs.read_dir dir with
    | Error err -> Error (DirectoryReadFailed { path = dir; error = IO.error_message err })
    | Ok iter ->
        let entries = Std.Iter.MutIterator.to_list iter in
        let rec walk_entries acc = function
          | [] -> Ok acc
          | entry :: rest ->
              if should_skip_entry entry then
                walk_entries acc rest
              else
                let full_path = Path.join dir entry in
                match Fs.symlink_metadata full_path with
                | Error err ->
                    Error (MetadataReadFailed { path = full_path; error = IO.error_message err })
                | Ok meta when Fs.Metadata.is_symlink meta ->
                    Error (SymlinkNotAllowed { path = full_path })
                | Ok meta when Fs.Metadata.is_dir meta -> (
                    match walk_dir acc full_path with
                    | Ok acc -> walk_entries acc rest
                    | Error _ as err -> err
                  )
                | Ok meta when Fs.Metadata.is_file meta -> (
                    match Path.strip_prefix full_path ~prefix:package_root with
                    | Ok relative -> walk_entries (relative :: acc) rest
                    | Error err -> Error (MetadataReadFailed {
                      path = full_path;
                      error = path_error_message err
                    })
                  )
                | Ok meta ->
                    Error (UnsupportedEntry {
                      path = full_path;
                      kind = file_kind_to_string (Fs.Metadata.file_type meta)
                    })
        in
        walk_entries acc entries
  in
  walk_dir [] package_root

let publish_artifact_path = fun ~target_dir_root ~(package:Tusk_model.Package.t) ~version ->
  Path.(target_dir_root
  / Path.v "release"
  / Path.v "publish"
  / Path.v package.name
  / Path.v (Std.Version.to_string version)
  / Path.v "package.tar.gz")

let create_archive = fun ~package_root ~artifact_path ~relative_files ->
  let parent =
    match Path.parent artifact_path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  match Fs.create_dir_all parent with
  | Error err -> Error (ArtifactReadFailed { path = artifact_path; error = IO.error_message err })
  | Ok () ->
      let args = [ "-czf"; Path.to_string artifact_path; "-C"; Path.to_string package_root ]
      @ List.map Path.to_string relative_files in
      let command = Command.make "tar" ~args in
      match Command.output command with
      | Error (Command.SystemError error) -> Error (TarCommandSpawnFailed {
        command = Command.to_string command;
        error
      })
      | Ok output when not (Int.equal output.status 0) -> Error (TarCommandFailed {
        command = Command.to_string command;
        status = output.status;
        stdout = output.stdout;
        stderr = output.stderr
      })
      | Ok _ -> Ok artifact_path

let create_artifact = fun ~target_dir_root ~(package:Tusk_model.Package.t) ~version ->
  match collect_relative_files ~package_root:package.path with
  | Error _ as err -> err
  | Ok relative_files ->
      let relative_files =
        List.sort
          (fun left right ->
            String.compare (Path.to_string left) (Path.to_string right))
          relative_files
      in
      if not (List.exists (Path.equal (Path.v "tusk.toml")) relative_files) then
        Error (MissingManifest { package_root = package.path })
      else
        let artifact_path = publish_artifact_path ~target_dir_root ~package ~version in
        create_archive ~package_root:package.path ~artifact_path ~relative_files

let plan_publish = fun ~registry ~publishing_workspace_packages ~(package:Tusk_model.Package.t) ->
  match validate_publish_metadata ~package with
  | Error _ as err -> err
  | Ok version -> (
      match validate_runtime_dependencies ~package with
      | Error _ as err -> err
      | Ok () -> (
          match validate_registry_dependencies ~registry ~publishing_workspace_packages ~package with
          | Error _ as err -> err
          | Ok () -> (
              match Git_provenance.discover ~package_root:package.path with
              | Error error -> Error (GitProvenanceFailed error)
              | Ok provenance -> Ok {
                package;
                version;
                locator = provenance.locator;
                selector = provenance.selector
              }
            )
        )
    )

let prepare_publish_artifact = fun ~target_dir_root (plan: publish_plan) ->
  match create_artifact ~target_dir_root ~package:plan.package ~version:plan.version with
  | Error _ as err -> err
  | Ok artifact_path ->
      Ok {
        package = plan.package;
        version = plan.version;
        locator = plan.locator;
        selector = plan.selector;
        artifact_path;
      }

let prepare_publish = fun ~registry ~target_dir_root ~publishing_workspace_packages ~(package:Tusk_model.Package.t) ->
  match plan_publish ~registry ~publishing_workspace_packages ~package with
  | Error _ as err -> err
  | Ok plan -> prepare_publish_artifact ~target_dir_root plan

let publish_prepared = fun ~registry ~api_token (prepared: prepared_publish) ->
  match Fs.read prepared.artifact_path with
  | Error err -> Error (ArtifactReadFailed {
    path = prepared.artifact_path;
    error = IO.error_message err
  })
  | Ok artifact -> (
      match Pkgs_ml.Registry.publish_artifact registry ~api_token ~artifact with
      | Ok published -> Ok published
      | Error error -> Error (RegistryPublishFailed { locator = prepared.locator; error })
    )

let publish_from_locator = fun ~registry ~target_dir_root ~(package:Tusk_model.Package.t) ~locator ~selector:_ ~api_token ->
  match validate_publish_metadata ~package with
  | Error _ as err -> err
  | Ok version -> (
      match validate_runtime_dependencies ~package with
      | Error _ as err -> err
      | Ok () -> (
          match validate_registry_dependencies ~registry ~publishing_workspace_packages:[] ~package with
          | Error _ as err -> err
          | Ok () -> (
              match create_artifact ~target_dir_root ~package ~version with
              | Error _ as err -> err
              | Ok artifact_path -> (
                  match Fs.read artifact_path with
                  | Error err -> Error (ArtifactReadFailed {
                    path = artifact_path;
                    error = IO.error_message err
                  })
                  | Ok artifact -> (
                      match Pkgs_ml.Registry.publish_artifact registry ~api_token ~artifact with
                      | Ok published -> Ok published
                      | Error error -> Error (RegistryPublishFailed { locator; error })
                    )
                )
            )
        )
    )

let publish = fun ~registry ~target_dir_root ~publishing_workspace_packages ~(package:Tusk_model.Package.t) ~api_token ->
  match prepare_publish ~registry ~target_dir_root ~publishing_workspace_packages ~package with
  | Error _ as err -> err
  | Ok prepared -> publish_prepared ~registry ~api_token prepared

let assoc_package = fun packages name ->
  List.find_opt
    (fun (pkg_name, _pkg) ->
      String.equal pkg_name name)
    packages |> Option.map snd

let workspace_runtime_dependency_names = fun ~workspace_packages (pkg: Tusk_model.Package.t) ->
  let is_workspace_dependency (dep: Tusk_model.Package.dependency) =
    if dep.source.workspace then
      Option.is_some (assoc_package workspace_packages dep.name)
    else
      match dep.source.path with
      | Some _ -> Option.is_some (assoc_package workspace_packages dep.name)
      | None -> false
  in
  pkg.dependencies
  |> List.filter is_workspace_dependency
  |> List.map (fun (dep: Tusk_model.Package.dependency) -> dep.name)

let workspace_publish_order = fun ~packages ->
  let workspace_packages = packages
  |> List.filter Tusk_model.Package.is_workspace_member
  |> List.map (fun (pkg: Tusk_model.Package.t) -> (pkg.name, pkg)) in
  let rec visit ~visiting ~visited ordered name =
    if List.exists (String.equal name) visited then
      Ok (visited, ordered)
    else if List.exists (String.equal name) visiting then
      Error (CyclicWorkspacePublishOrder { cycle = List.rev (name :: visiting) })
    else
      match assoc_package workspace_packages name with
      | None -> Ok (visited, ordered)
      | Some pkg ->
          let visiting = name :: visiting in
          let dependency_names = workspace_runtime_dependency_names ~workspace_packages pkg in
          let rec visit_dependencies visited ordered = function
            | [] ->
                let visited = name :: visited in
                Ok (visited, pkg :: ordered)
            | dep_name :: rest -> (
                match visit ~visiting ~visited ordered dep_name with
                | Error _ as err -> err
                | Ok (visited, ordered) -> visit_dependencies visited ordered rest
              )
          in
          visit_dependencies visited ordered dependency_names
  in
  let rec walk_names visited ordered = function
    | [] -> Ok (List.rev ordered)
    | name :: rest -> (
        match visit ~visiting:[] ~visited ordered name with
        | Error _ as err -> err
        | Ok (visited, ordered) -> walk_names visited ordered rest
      )
  in
  walk_names [] [] (List.map fst workspace_packages)
