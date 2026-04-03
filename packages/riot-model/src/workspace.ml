(** Workspace - TOML parsing for workspace manifests *)
open Std
open Std.Collections
open Std.Data
open Std.IO

(** Types *)
type t = {
  root: Path.t;
  target_dir_root: Path.t;
  packages: Package.t list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
}

(** Workspace TOML parsing *)
type manifest = {
  members: Path.t list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
  target_dir: string option;
}

let version_parse_error_to_string = function
  | Version.Invalid_format msg -> msg
  | Version.Invalid_version_segment segment -> "invalid version segment: " ^ segment
  | Version.Invalid_pre_release_segment segment -> "invalid pre-release segment: " ^ segment

let validate_requirement = fun ~dependency_name requirement ->
  let trimmed = String.trim requirement in
  match Version.parse_requirement trimmed with
  | Ok requirement -> Ok requirement
  | Error err -> Error ("dependency '"
  ^ dependency_name
  ^ "' has invalid version requirement '"
  ^ requirement
  ^ "': "
  ^ version_parse_error_to_string err)

let requirement_is_any = fun requirement ->
  String.equal (Version.requirement_to_string requirement) "*"

let validate_dependency_source = fun ~dependency_name (source: Package.dependency_source) ->
  if source.workspace then
    Error ("workspace dependency '" ^ dependency_name ^ "' cannot use workspace = true")
  else if Option.is_some source.ref_ && Option.is_none source.source_locator then
    Error ("dependency '" ^ dependency_name ^ "' cannot specify ref without source")
  else if
    source.builtin
    && (Option.is_some source.path || Option.is_some source.source_locator || Option.is_some source.ref_)
  then
    Error ("builtin dependency '" ^ dependency_name ^ "' does not support path or source overrides")
  else if source.builtin then
    match source.version with
    | None -> Ok { source with version = Some Version.any }
    | Some version when requirement_is_any version -> Ok source
    | Some version -> Error ("builtin dependency '"
    ^ dependency_name
    ^ "' does not support version requirement '"
    ^ Version.requirement_to_string version
    ^ "'")
  else if
    Option.is_some source.path || Option.is_some source.source_locator || Option.is_some source.version
  then
    Ok source
  else
    Ok { source with version = Some Version.any }

let normalize_source_locator = fun raw ->
  let raw = String.trim raw in
  let raw =
    if String.starts_with ~prefix:"https://" raw then
      String.sub raw 8 (String.length raw - 8)
    else if String.starts_with ~prefix:"http://" raw then
      String.sub raw 7 (String.length raw - 7)
    else
      raw
  in
  if String.ends_with ~suffix:".git" raw then
    String.sub raw 0 (String.length raw - 4)
  else
    raw

let github_locator_of_value = fun value -> "github.com/" ^ String.trim value

let parse_dependency: string -> Toml.value -> (Package.dependency, string) result = fun name value ->
  let make_dependency source: Package.dependency = { name; source } in
  match value with
  | Toml.Table attrs -> (
      let path =
        match List.assoc_opt "path" attrs with
        | Some (Toml.String path_str) -> Ok (Some (Path.v path_str))
        | Some _ -> Error ("dependency '" ^ name ^ "' has non-string path")
        | None -> Ok None
      in
      let source_locator =
        match List.assoc_opt "source" attrs, List.assoc_opt "github" attrs with
        | Some _, Some _ -> Error ("dependency '" ^ name ^ "' cannot specify both source and github")
        | Some (Toml.String locator), None -> Ok (Some (normalize_source_locator locator))
        | Some _, None -> Error ("dependency '" ^ name ^ "' has non-string source locator")
        | None, Some (Toml.String github) -> Ok (Some (github_locator_of_value github))
        | None, Some _ -> Error ("dependency '" ^ name ^ "' has non-string github shorthand")
        | None, None -> Ok None
      in
      let ref_ =
        match List.assoc_opt "ref" attrs with
        | Some (Toml.String ref_) -> Ok (Some (String.trim ref_))
        | Some _ -> Error ("dependency '" ^ name ^ "' has non-string ref")
        | None -> Ok None
      in
      let version =
        match List.assoc_opt "version" attrs with
        | Some (Toml.String requirement) -> validate_requirement ~dependency_name:name requirement
        |> Result.map (fun version -> Some version)
        | Some _ -> Error ("dependency '" ^ name ^ "' has non-string version requirement")
        | None -> Ok None
      in
      match path, source_locator, ref_, version with
      | (Error _ as err), _, _, _ -> err
      | _, (Error _ as err), _, _ -> err
      | _, _, (Error _ as err), _ -> err
      | _, _, _, (Error _ as err) -> err
      | Ok path, Ok source_locator, Ok ref_, Ok version ->
          validate_dependency_source ~dependency_name:name
            {
              workspace = false;
              builtin = Package.is_builtin_dependency_name name;
              path;
              source_locator;
              ref_;
              version;
            } |> Result.map make_dependency
    )
  | Toml.String requirement -> (
      match validate_requirement ~dependency_name:name requirement with
      | Error _ as err -> err
      | Ok version ->
          validate_dependency_source ~dependency_name:name
            {
              workspace = false;
              builtin = Package.is_builtin_dependency_name name;
              path = None;
              source_locator = None;
              ref_ = None;
              version = Some version;
            } |> Result.map make_dependency
    )
  | _ ->
      Error ("dependency '" ^ name ^ "' must be a string or table")

let parse_dependencies: (string * Toml.value) list -> (Package.dependency list, string) result = fun items ->
  let rec loop acc entries =
    match entries with
    | [] -> Ok (List.rev acc)
    | (name, value) :: rest -> (
        match parse_dependency name value with
        | Ok dep -> loop (dep :: acc) rest
        | Error _ as err -> err
      )
  in
  loop [] items

let parse_dependency_section section_name (toml: Toml.value): (Package.dependency list, string) result =
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt section_name items with
      | Some (Toml.Table dep_items) -> parse_dependencies dep_items
      | Some _ -> Error ("[" ^ section_name ^ "] must be a table")
      | None -> Ok []
    )
  | _ -> Ok []

let parse_members: Toml.value -> Path.t list = fun toml ->
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt "workspace" items with
      | Some (Toml.Table workspace_items) -> (
          match List.assoc_opt "members" workspace_items with
          | Some (Toml.Array members) ->
              List.filter_map
                (fun m ->
                  Option.map Path.v (Toml.get_string m))
                members
          | _ -> []
        )
      | _ -> []
    )
  | _ -> []

let parse_workspace_dependencies: Toml.value -> Package.dependency list = fun toml ->
  Log.debug ("[WORKSPACE] parse_workspacE_dependencies has items: " ^ Toml.to_string toml);
  parse_dependency_section "dependencies" toml |> Result.expect ~msg:"workspace dependencies should be parsed through of_toml"

let parse_workspace_dev_dependencies: Toml.value -> Package.dependency list = fun toml ->
  parse_dependency_section "dev-dependencies" toml |> Result.expect ~msg:"workspace dev dependencies should be parsed through of_toml"

let parse_workspace_build_dependencies: Toml.value -> Package.dependency list = fun toml ->
  parse_dependency_section "build-dependencies" toml |> Result.expect ~msg:"workspace build dependencies should be parsed through of_toml"

let parse_profile_overrides: Toml.value -> (string * Profile.profile_override) list = fun toml ->
  Log.debug "[WORKSPACE] parse_profile_overrides called";
  match toml with
  | Toml.Table items -> (
      Log.debug
        ("[WORKSPACE] Looking for [profile] in TOML with " ^ Int.to_string (List.length items) ^ " top-level keys");
      Log.debug ("[WORKSPACE] Top-level keys: " ^ String.concat ", " (List.map fst items));
      match List.assoc_opt "profile" items with
      | Some (Toml.Table profile_items) ->
          Log.debug
            ("[WORKSPACE] Found [profile] section with "
            ^ Int.to_string (List.length profile_items)
            ^ " profiles");
          let result =
            List.filter_map
              (fun ((profile_name, value)) ->
                Log.debug ("[WORKSPACE] Parsing profile: " ^ profile_name);
                match value with
                | Toml.Table profile_table ->
                    Log.debug
                      ("[WORKSPACE] Profile "
                      ^ profile_name
                      ^ " has "
                      ^ Int.to_string (List.length profile_table)
                      ^ " fields");
                    Some (profile_name, Profile.override_from_toml profile_table)
                | _ ->
                    Log.debug ("[WORKSPACE] Profile " ^ profile_name ^ " is not a table, skipping");
                    None)
              profile_items
          in
          Log.debug
            ("[WORKSPACE] Parsed " ^ Int.to_string (List.length result) ^ " profile overrides");
          result
      | _ ->
          Log.debug "[WORKSPACE] No [profile] section found in TOML";
          []
    )
  | _ ->
      Log.debug "[WORKSPACE] TOML root is not a table";
      []

let parse_target_dir: Toml.value -> string option = fun toml ->
  match toml with
  | Toml.Table items -> (
      match List.assoc_opt "riot" items with
      | Some (Toml.Table riot_items) -> (
          match List.assoc_opt "target_dir" riot_items with
          | Some (Toml.String target_dir) -> Some target_dir
          | _ -> None
        )
      | _ -> None
    )
  | _ -> None

let of_toml: Toml.value -> (manifest, string) result = fun toml ->
  let members = parse_members toml in
  match parse_dependency_section "dependencies" toml with
  | Error _ as err -> err
  | Ok dependencies -> (
      match parse_dependency_section "dev-dependencies" toml with
      | Error _ as err -> err
      | Ok dev_dependencies -> (
          match parse_dependency_section "build-dependencies" toml with
          | Error _ as err -> err
          | Ok build_dependencies ->
              let profile_overrides = parse_profile_overrides toml in
              let target_dir = parse_target_dir toml in
              Ok {
                members;
                dependencies;
                dev_dependencies;
                build_dependencies;
                profile_overrides;
                target_dir;
              }
        )
    )

let resolve_target_dir_root = fun ~root ?target_dir () ->
  let target_dir =
    match target_dir with
    | Some target_dir -> target_dir
    | None -> "_build"
  in
  let target_dir_path = Path.v target_dir in
  if Path.is_absolute target_dir_path then
    target_dir_path
  else
    Path.(root / target_dir_path)

let make ~root ~packages ?(dependencies = []) ?(dev_dependencies = []) ?(build_dependencies = []) ?(profile_overrides = []) ?target_dir ():
  t = {
  root;
  target_dir_root = resolve_target_dir_root ~root ?target_dir ();
  packages;
  dependencies;
  dev_dependencies;
  build_dependencies;
  profile_overrides;
}

let dependencies_for_scope = fun scope (workspace: t) ->
  match scope with
  | Package.Normal -> workspace.dependencies
  | Package.Dev -> workspace.dev_dependencies
  | Package.Build -> workspace.build_dependencies

let find_package_for_path = fun (workspace: t) ~path ->
  let path = Path.normalize path in
  let contains_path (pkg: Package.t) =
    let package_root = Path.normalize pkg.path in
    Path.equal path package_root || match Path.strip_prefix path ~prefix:package_root with
    | Ok _ -> true
    | Error _ -> false
  in
  workspace.packages |> List.filter contains_path |> List.sort
    (fun (left: Package.t) (right: Package.t) ->
      Int.compare
        (String.length (Path.to_string right.path))
        (String.length (Path.to_string left.path))) |> function
  | pkg :: _ -> Some pkg
  | [] -> None

(** Utility functions *)
let project_id = fun workspace ->
  let root_str = Path.to_string workspace.root in
  String.map
    (fun c ->
      if c = '/' then
        '-'
      else
        c)
    root_str

let server_port = fun workspace ->
  let root_str = Path.to_string workspace.root in
  let hash = Std.Crypto.hash_string root_str in
  let hash_int = Std.Crypto.Digest.to_int hash in
  let port_range = 65_535 - 49_152 in
  50_152 + (abs hash_int mod port_range)

(** Command discovery functions - moved here to avoid circular dependency *)
let discover_commands: t -> Package_command.t list = fun workspace ->
  List.concat_map (fun (pkg: Package.t) -> pkg.commands) workspace.packages

let find_command: t -> string -> Package_command.t option = fun workspace name ->
  discover_commands workspace |> List.find_opt (fun (cmd: Package_command.t) -> cmd.name = name)

let discover_fix_providers: t -> Fix_provider.t list = fun workspace ->
  List.concat_map (fun (pkg: Package.t) -> pkg.fix_providers) workspace.packages

let discover_macro_providers: t -> Macro_provider.t list = fun workspace ->
  List.concat_map (fun (pkg: Package.t) -> pkg.macro_providers) workspace.packages

module Tests = struct
  let test_parse_workspace_toml (): (unit, string) result = Ok () [@test]

  let test_parse_target_dir (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[workspace]
members = ["packages/foo"]

[riot]
target_dir = "build-out"
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let manifest = of_toml toml |> Result.expect ~msg:"expected workspace manifest" in
    if manifest.target_dir = Some "build-out" then
      Ok ()
    else
      Error "expected [riot].target_dir to be parsed" [@test]

  let test_make_uses_custom_target_dir (): (unit, string) result =
    let workspace = make ~root:(Path.v "/tmp/example") ~packages:[] ~target_dir:"build-out" () in
    if Path.to_string workspace.target_dir_root = "/tmp/example/build-out" then
      Ok ()
    else
      Error "expected custom target_dir_root" [@test]

  let test_workspace_dependencies_parse_registry_requirements (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[workspace]
members = []

[dependencies]
std = ">= 1.2.3"
|}
      |> Result.expect ~msg:"expected workspace toml to parse"
    in
    match of_toml toml with
    | Error err -> Error err
    | Ok manifest -> (
        match manifest.dependencies with
        | [ { Package.source={
              workspace=false;
              builtin=false;
              path=None;
              source_locator=None;
              ref_=None;
              version=Some requirement;

            }; _ } ] ->
            if String.equal (Version.requirement_to_string requirement) ">= 1.2.3" then
              Ok ()
            else
              Error "expected workspace registry requirement to be parsed structurally"
        | _ -> Error "expected workspace dependency to parse as a registry requirement"
      ) [@test]

  let test_discover_fix_providers (): (unit, string) result =
    let package_toml =
      Std.Data.Toml.parse
        {|
[package]
name = "std"
version = "0.1.0"

[riot.fix.provider]
path = "fix/no_stdlib_provider.ml"
rules = ["no-stdlib"]
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let package = Package.from_toml
      package_toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example/packages/std")
      ~relative_path:(Path.v "packages/std")
    |> Result.expect ~msg:"expected package manifest" in
    let workspace = make ~root:(Path.v "/tmp/example") ~packages:[ package ] () in
    match discover_fix_providers workspace with
    | [ provider ] ->
        if
          String.equal provider.package_name "std"
          && String.equal (Path.to_string provider.source_path) "/tmp/example/packages/std/fix/no_stdlib_provider.ml"
          && String.equal provider.name "std"
          && provider.rules = [ "std:no-stdlib" ]
        then
          Ok ()
        else
          Error "expected provider metadata to round-trip"
    | _ -> Error "expected one fix provider" [@test]

  let test_discover_macro_providers (): (unit, string) result =
    let package_toml =
      Std.Data.Toml.parse
        {|
[package]
name = "sqlx"
version = "0.1.0"

[lib]
path = "src/sqlx.ml"

[riot.macro.provider]
path = "src/sqlx_macro.ml"
module_path = "Sqlx"
macros = ["query", "query_as"]
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let package = Package.from_toml
      package_toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example/packages/sqlx")
      ~relative_path:(Path.v "packages/sqlx")
    |> Result.expect ~msg:"expected package manifest" in
    let workspace = make ~root:(Path.v "/tmp/example") ~packages:[ package ] () in
    match discover_macro_providers workspace with
    | [ provider ] ->
        if
          String.equal provider.package_name "sqlx"
          && String.equal provider.module_name "Sqlx"
          && provider.module_path = [ "Sqlx" ]
          && provider.macros = [ "query"; "query_as" ]
          && String.equal (Path.to_string provider.source_path) "/tmp/example/packages/sqlx/src/sqlx_macro.ml"
        then
          Ok ()
        else
          Error "expected macro provider metadata to round-trip"
    | _ -> Error "expected one macro provider" [@test]

  let test_parse_workspace_dependency_classes (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[workspace]
members = ["packages/foo"]

[dependencies]
std = { path = "packages/std" }

[dev-dependencies]
propane = { path = "packages/propane" }

[build-dependencies]
fixme = { path = "packages/fixme" }
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let manifest = of_toml toml |> Result.expect ~msg:"expected workspace manifest" in
    if
      List.map (fun (dep: Package.dependency) -> dep.Package.name) manifest.dependencies = [ "std" ]
      && List.map (fun (dep: Package.dependency) -> dep.Package.name) manifest.dev_dependencies
      = [ "propane" ]
      && List.map (fun (dep: Package.dependency) -> dep.Package.name) manifest.build_dependencies
      = [ "fixme" ]
    then
      Ok ()
    else
      Error "expected workspace dependency classes to parse" [@test]
end [@test]
