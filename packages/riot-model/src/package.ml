(** Package - TOML parsing for package manifests *)
open Std
open Std.Data
open Std.Collections

(** Types *)
type dependency_source = {
  workspace: bool;
  builtin: bool;
  path: Path.t option;
  source_locator: string option;
  ref_: string option;
  version: Std.Version.requirement option;
}

type dependency_scope =
  Normal
  | Dev
  | Build

type key =
  Key of string

type dependency = {
  name: string;
  source: dependency_source;
}

type resolved_dependency = {
  requirement: dependency;
  resolved_id: Lockfile.package_id;
}

type publish_metadata = {
  version: Std.Version.t option;
  description: string option;
  license: string option;
  is_public: bool option;
}

type binary = {
  name: string;
  path: Path.t;
}

type library = {
  path: Path.t;
}

type sources = {
  src: Path.t list;
  native: Path.t list;
  tests: Path.t list;
  examples: Path.t list;
  bench: Path.t list;
}

type target_platform = string

(* "macos", "linux", "windows", etc. *)

(** Re-export types from Profile *)
type 'value override = 'value Profile.override

type profile_override = Profile.profile_override

(** Target-specific override - can override any profile field for a specific platform *)
type target_override = {
  profile_override: Profile.profile_override option;  (* Profile fields that can be overridden *)
}

type compiler_config = {
  profile_overrides: (string * profile_override) list;  (* "debug" -> override, "release" -> override *)
  target_overrides: (target_platform * target_override) list;  (* "macos" -> override, etc. *)
}

type foreign_dependency = {
  name: string;
  path: Path.t;
  inputs: Path.t list;
  build_cmd: string list;
  clean_cmd: string list option;
  test_cmd: string list option;
  outputs: Path.t list;
  env: (string * string) list;
}

type t = {
  name: string;
  path: Path.t;
  relative_path: Path.t;
  dependencies: dependency list;
  dev_dependencies: dependency list;
  build_dependencies: dependency list;
  foreign_dependencies: foreign_dependency list;
  binaries: binary list;
  library: library option;
  sources: sources;
  compiler: compiler_config;
  commands: Package_command.t list;
  fix_providers: Fix_provider.t list;
  macro_providers: Macro_provider.t list;
  publish: publish_metadata;
}

type resolved = {
  package: t;
  id: Lockfile.package_id;
  manifest_path: Path.t;
  materialized_root: Path.t;
  provenance: Lockfile.provenance;
  runtime_resolved: resolved_dependency list;
  build_resolved: resolved_dependency list;
  dev_resolved: resolved_dependency list;
}

let default_publish_metadata = {
  version = None;
  description = None;
  license = None;
  is_public = None
}

let empty_sources = {
  src = [];
  native = [];
  tests = [];
  examples = [];
  bench = [];
}

let compare_path = fun left right ->
  String.compare (Path.to_string left) (Path.to_string right)

let compare_option compare_value left right =
  match left, right with
  | None, None -> 0
  | None, Some _ -> (-1)
  | Some _, None -> 1
  | Some left, Some right -> compare_value left right

let compare_dependency_source = fun left right ->
  let by_workspace = Bool.compare left.workspace right.workspace in
  if not (Int.equal by_workspace 0) then
    by_workspace
  else
    let by_builtin = Bool.compare left.builtin right.builtin in
    if not (Int.equal by_builtin 0) then
      by_builtin
    else
      let by_path = compare_option compare_path left.path right.path in
      if not (Int.equal by_path 0) then
        by_path
      else
        let by_source_locator = compare_option String.compare left.source_locator right.source_locator in
        if not (Int.equal by_source_locator 0) then
          by_source_locator
        else
          let by_ref = compare_option String.compare left.ref_ right.ref_ in
          if not (Int.equal by_ref 0) then
            by_ref
          else
            compare_option
              (fun left right ->
                String.compare
                  (Version.requirement_to_string left)
                  (Version.requirement_to_string right))
              left.version
              right.version

let compare_dependency = fun (left: dependency) (right: dependency) ->
  let by_name = String.compare left.name right.name in
  if not (Int.equal by_name 0) then
    by_name
  else
    compare_dependency_source left.source right.source

let compare_binary = fun (left: binary) (right: binary) ->
  let by_name = String.compare left.name right.name in
  if not (Int.equal by_name 0) then
    by_name
  else
    compare_path left.path right.path

let compare_fix_provider = fun (left: Fix_provider.t) (right: Fix_provider.t) ->
  let by_name = String.compare left.name right.name in
  if not (Int.equal by_name 0) then
    by_name
  else
    compare_path left.source_path right.source_path

let compare_profile_override = fun (left_name, _) (right_name, _) ->
  String.compare left_name right_name

let compare_target_override = fun (left_name, _) (right_name, _) ->
  String.compare left_name right_name

let compare_foreign_dependency = fun (left: foreign_dependency) (right: foreign_dependency) ->
  let by_name = String.compare left.name right.name in
  if not (Int.equal by_name 0) then
    by_name
  else
    compare_path left.path right.path

let canonicalize_path_list = fun paths ->
  List.sort_uniq compare_path paths

let canonicalize_sources = fun sources ->
  {
    src = canonicalize_path_list sources.src;
    native = canonicalize_path_list sources.native;
    tests = canonicalize_path_list sources.tests;
    examples = canonicalize_path_list sources.examples;
    bench = canonicalize_path_list sources.bench;
  }

let canonicalize_foreign_dependency = fun (foreign: foreign_dependency) ->
  {
    foreign
    with inputs = canonicalize_path_list foreign.inputs;
    outputs = canonicalize_path_list foreign.outputs
  }

let canonicalize = fun (pkg: t) ->
  {
    pkg
    with dependencies = List.sort compare_dependency pkg.dependencies;
    dev_dependencies = List.sort compare_dependency pkg.dev_dependencies;
    build_dependencies = List.sort compare_dependency pkg.build_dependencies;
    foreign_dependencies = pkg.foreign_dependencies
    |> List.map canonicalize_foreign_dependency
    |> List.sort compare_foreign_dependency;
    binaries = List.sort compare_binary pkg.binaries;
    sources = canonicalize_sources pkg.sources;
    compiler = {
      profile_overrides = List.sort compare_profile_override pkg.compiler.profile_overrides;
      target_overrides = List.sort compare_target_override pkg.compiler.target_overrides
    };
    fix_providers = List.sort compare_fix_provider pkg.fix_providers;
  }

let make = fun ~name ~path ~relative_path ?(dependencies = []) ?(dev_dependencies = []) ?(build_dependencies = []) ?(foreign_dependencies = []) ?(binaries = []) ?library ?(sources = empty_sources) ?(compiler = {
  profile_overrides = [];
  target_overrides = []
}) ?(commands = []) ?(fix_providers = []) ?(publish = default_publish_metadata) () ->
  canonicalize
    {
      name;
      path;
      relative_path;
      dependencies;
      dev_dependencies;
      build_dependencies;
      foreign_dependencies;
      binaries;
      library;
      sources;
      compiler;
      commands;
      fix_providers;
      publish;
    }

let synthetic = fun ~name ~path ~relative_path -> make ~name ~path ~relative_path ()

let equal = fun a b -> a.name = b.name && a.path = b.path

let key_of_string = fun value -> Key value

let key_to_string = fun (Key value) -> value

let key_equal = fun left right ->
  String.equal (key_to_string left) (key_to_string right)

let key_compare = fun left right ->
  String.compare (key_to_string left) (key_to_string right)

let dependencies_for_scope = fun scope (pkg: t) ->
  match scope with
  | Normal -> pkg.dependencies
  | Dev -> pkg.dev_dependencies
  | Build -> pkg.build_dependencies

let is_builtin_dependency_name = fun name ->
  match name with
  | "unix"
  | "stdlib"
  | "threads"
  | "str"
  | "bigarray"
  | "dynlink"
  | "compiler-libs"
  | "graphics" -> true
  | _ -> false

let is_builtin_dependency = fun (dep: dependency) -> dep.source.builtin

let is_macro_package = fun (pkg: t) -> pkg.macro_providers != []

let macro_provider = fun (pkg: t) ->
  match pkg.macro_providers with
  | provider :: _ -> Some provider
  | [] -> None
let binary_scope = fun (bin: binary) ->
  let path_str = Path.to_string bin.path in
  if
    String.starts_with ~prefix:"tests/" path_str
    || String.starts_with ~prefix:"examples/" path_str
    || String.starts_with ~prefix:"bench/" path_str
  then
    Dev
  else
    Normal

let scope_of_binary_name = fun (pkg: t) ~binary_name ->
  List.find_opt
    (fun (bin: binary) ->
      String.equal bin.name binary_name)
    pkg.binaries |> Option.map binary_scope

let binaries_for_scope = fun scope (pkg: t) ->
  match scope with
  | Normal -> List.filter (fun bin -> binary_scope bin = Normal) pkg.binaries
  | Dev -> List.filter (fun bin -> binary_scope bin = Dev) pkg.binaries
  | Build -> []

let commands_for_scope = fun scope (pkg: t) ->
  match scope with
  | Normal -> pkg.commands
  | Dev
  | Build -> []

let sources_for_scope = fun scope (pkg: t) ->
  match scope with
  | Normal -> { pkg.sources with tests = []; examples = []; bench = [] }
  | Dev -> { pkg.sources with src = []; native = [] }
  | Build ->
      {
        src = [];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      }

let for_scope = fun scope (pkg: t) ->
  match scope with
  | Normal ->
      canonicalize
        {
          pkg
          with dev_dependencies = [];
          build_dependencies = [];
          binaries = binaries_for_scope Normal pkg;
          commands = commands_for_scope Normal pkg;
          sources = sources_for_scope Normal pkg;
        }
  | Dev ->
      canonicalize
        {
          pkg
          with build_dependencies = [];
          library = None;
          binaries = binaries_for_scope Dev pkg;
          commands = commands_for_scope Dev pkg;
          sources = sources_for_scope Dev pkg;
        }
  | Build ->
      canonicalize
        {
          pkg
          with dependencies = [];
          dev_dependencies = [];
          library = None;
          binaries = [];
          commands = commands_for_scope Build pkg;
          sources = sources_for_scope Build pkg;
        }

let build_graph_dependencies = fun (pkg: t) -> pkg.dependencies @ pkg.dev_dependencies

let all_dependencies = fun (pkg: t) -> pkg.dependencies @ pkg.dev_dependencies @ pkg.build_dependencies

let resolve_scope = fun ~scope_name ~manifest_dependencies ~lock_dependencies ->
  let rec loop (acc: resolved_dependency list) (requirements: dependency list) =
    match requirements with
    | [] -> Ok (List.rev acc)
    | (requirement: dependency) :: rest ->
        if is_builtin_dependency requirement then
          loop acc rest
        else
          (
            match List.find_opt (fun (dep: Lockfile.dependency) -> dep.name = requirement.name) lock_dependencies with
            | Some resolved -> loop ({ requirement; resolved_id = resolved.package } :: acc) rest
            | None -> Error ("lockfile is missing resolved "
            ^ scope_name
            ^ " dependency '"
            ^ requirement.name
            ^ "'")
          )
  in
  loop [] manifest_dependencies

let resolve = fun ~(package:t) ~(lock_package:Lockfile.package) ~manifest_path ~materialized_root ->
  match resolve_scope
    ~scope_name:"runtime"
    ~manifest_dependencies:package.dependencies
    ~lock_dependencies:lock_package.dependencies with
  | Error _ as err -> err
  | Ok dependencies -> (
      match resolve_scope
        ~scope_name:"build"
        ~manifest_dependencies:package.build_dependencies
        ~lock_dependencies:lock_package.build_dependencies with
      | Error _ as err -> err
      | Ok build_dependencies -> (
          match resolve_scope
            ~scope_name:"dev"
            ~manifest_dependencies:package.dev_dependencies
            ~lock_dependencies:lock_package.dev_dependencies with
          | Error _ as err -> err
          | Ok dev_dependencies ->
              Ok {
                package;
                id = lock_package.id;
                manifest_path;
                materialized_root;
                provenance = lock_package.provenance;
                runtime_resolved = dependencies;
                build_resolved = build_dependencies;
                dev_resolved = dev_dependencies;
              }
        )
    )

(** Check if this package is a workspace member (not an external dependency).
    External dependencies have relative_path that escapes the workspace (starts with "../")
    or uses absolute paths. *)
let is_workspace_member: t -> bool = fun pkg ->
  let rel_str = Path.to_string pkg.relative_path in
  not (String.starts_with ~prefix:"../" rel_str || Path.is_absolute pkg.relative_path)

(** Validate package name according to Riot naming conventions *)
let validate_name = fun name ->
  let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let is_lowercase c = c >= 'a' && c <= 'z' in
  let is_digit c = c >= '0' && c <= '9' in
  let is_alphanum c = is_alpha c || is_digit c in
  let is_valid_char c = is_alphanum c || c = '-' || c = '_' in
  if String.length name = 0 then
    Error "Package name cannot be empty"
  else
    let first_char = String.get name 0 in
    let last_char = String.get name (String.length name - 1) in
    if not (is_lowercase first_char && is_alpha first_char) then
      Error ("Package name must start with a lowercase letter. Try '"
      ^ String.lowercase_ascii name
      ^ "' instead")
    else if first_char = '-' || first_char = '_' then
      Error "Package name cannot start with hyphen or underscore"
    else if last_char = '-' || last_char = '_' then
      Error "Package name cannot end with hyphen or underscore"
    else if not (String.for_all is_valid_char name) then
      Error "Package name can only contain lowercase letters, numbers, hyphens, and underscores"
    else
      Ok name

let version_parse_error_to_string = fun err ->
  match err with
  | Version.Invalid_format msg -> msg
  | Version.Invalid_version_segment segment -> "invalid version segment: " ^ segment
  | Version.Invalid_pre_release_segment segment -> "invalid pre-release segment: " ^ segment

(** Package TOML parsing *)
let parse_name: (string * Toml.value) list -> string -> string = fun items fallback ->
  match List.assoc_opt "package" items with
  | Some (Toml.Table pkg_items) -> (
      match List.assoc_opt "name" pkg_items with
      | Some (Toml.String n) -> n
      | _ -> fallback
    )
  | _ -> fallback

let parse_publish_metadata: (string * Toml.value) list -> (publish_metadata, string) result = fun items ->
  let parse_version = fun ~package_name ->
    function
    | Toml.String raw_version -> (
        match Version.parse (String.trim raw_version) with
        | Ok version -> Ok (Some version)
        | Error err -> Error ("package '"
        ^ package_name
        ^ "' has invalid version '"
        ^ raw_version
        ^ "': "
        ^ version_parse_error_to_string err)
      )
    | _ -> Error ("package '" ^ package_name ^ "' has non-string version")
  in
  let parse_optional_string = fun ~package_name ~field ->
    function
    | Toml.String value -> Ok (Some value)
    | _ -> Error ("package '" ^ package_name ^ "' has non-string " ^ field)
  in
  let parse_public = fun ~package_name ->
    function
    | Toml.Bool value -> Ok (Some value)
    | _ -> Error ("package '" ^ package_name ^ "' has non-boolean public flag")
  in
  match List.assoc_opt "package" items with
  | Some (Toml.Table pkg_items) ->
      let package_name = parse_name items "<package>" in
      let version =
        match List.assoc_opt "version" pkg_items with
        | Some value -> parse_version ~package_name value
        | None -> Ok None
      in
      let description =
        match List.assoc_opt "description" pkg_items with
        | Some value -> parse_optional_string ~package_name ~field:"description" value
        | None -> Ok None
      in
      let license =
        match List.assoc_opt "license" pkg_items with
        | Some value -> parse_optional_string ~package_name ~field:"license" value
        | None -> Ok None
      in
      let is_public =
        match List.assoc_opt "public" pkg_items with
        | Some value -> parse_public ~package_name value
        | None -> Ok None
      in
      (
        match version, description, license, is_public with
        | Ok version, Ok description, Ok license, Ok is_public -> Ok {
          version;
          description;
          license;
          is_public
        }
        | (Error err, _, _, _)
        | (_, Error err, _, _)
        | (_, _, Error err, _)
        | (_, _, _, Error err) -> Error err
      )
  | Some _ ->
      Error "[package] must be a table"
  | None ->
      Ok default_publish_metadata

let resolve_workspace_dependency: string -> dependency list -> dependency = fun name workspace_deps ->
  match List.find_opt (fun (d: dependency) -> d.name = name) workspace_deps with
  | Some dep -> dep
  | None -> panic
    ("Dependency '" ^ name ^ "' with { workspace = true } not found in workspace dependencies")

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

let make_source = fun ?(workspace = false) ?(builtin = false) ?path ?source_locator ?ref_ ?version () ->
  {
    workspace;
    builtin;
    path;
    source_locator;
    ref_;
    version;
  }

let validate_dependency_source = fun ~dependency_name source ->
  if
    source.workspace
    && (source.builtin
    || Option.is_some source.path
    || Option.is_some source.source_locator
    || Option.is_some source.ref_
    || Option.is_some source.version)
  then
    Error ("dependency '" ^ dependency_name ^ "' cannot combine workspace = true with path, source, ref, or version")
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
    source.workspace
    || Option.is_some source.path
    || Option.is_some source.source_locator
    || Option.is_some source.version
  then
    Ok source
  else
    Ok { source with version = Some Version.any }

let parse_dependency:
  string -> Toml.value -> workspace_deps:dependency list -> (dependency, string) result = fun name value ~workspace_deps ->
  match value with
  | Toml.Table attrs -> (
      match List.assoc_opt "workspace" attrs with
      | Some (Toml.Bool true) -> (
          let source = {
            (resolve_workspace_dependency name workspace_deps).source
            with workspace = true
          } in
          validate_dependency_source ~dependency_name:name source
          |> Result.map (fun source -> { name; source })
        )
      | Some _ ->
          Error ("dependency '" ^ name ^ "' has non-boolean workspace flag")
      | _ -> (
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
          | Ok path, Ok source_locator, Ok ref_, Ok version -> validate_dependency_source
            ~dependency_name:name
            (make_source
              ~builtin:(is_builtin_dependency_name name)
              ?path
              ?source_locator
              ?ref_
              ?version
              ())
          |> Result.map (fun source -> { name; source })
        )
    )
  | Toml.String requirement -> (
      match validate_requirement ~dependency_name:name requirement with
      | Error _ as err -> err
      | Ok version -> validate_dependency_source
        ~dependency_name:name
        (make_source ~builtin:(is_builtin_dependency_name name) ~version ())
      |> Result.map (fun source -> { name; source })
    )
  | _ ->
      Error ("dependency '" ^ name ^ "' must be a string or table")

let parse_dependencies:
  (string * Toml.value) list -> workspace_deps:dependency list -> (dependency list, string) result = fun items ~workspace_deps ->
  let rec loop acc entries =
    match entries with
    | [] -> Ok (List.rev acc)
    | (name, value) :: rest -> (
        match parse_dependency name value ~workspace_deps with
        | Ok dep -> loop (dep :: acc) rest
        | Error _ as err -> err
      )
  in
  loop [] items

let parse_dependency_section = fun section_name items ~(workspace_deps:dependency list) ->
  match List.assoc_opt section_name items with
  | Some (Toml.Table dep_items) -> parse_dependencies dep_items ~workspace_deps
  | Some _ -> Error ("[" ^ section_name ^ "] must be a table")
  | None -> Ok []

let dependency_source_to_json = fun source ->
  let fields = [] in
  let fields =
    if source.workspace then
      ("workspace", Json.Bool true) :: fields
    else
      fields
  in
  let fields =
    if source.builtin then
      ("builtin", Json.Bool true) :: fields
    else
      fields
  in
  let fields =
    match source.path with
    | Some path -> ("path", Json.String (Path.to_string path)) :: fields
    | None -> fields
  in
  let fields =
    match source.source_locator with
    | Some source_locator -> ("source", Json.String source_locator) :: fields
    | None -> fields
  in
  let fields =
    match source.ref_ with
    | Some ref_ -> ("ref", Json.String ref_) :: fields
    | None -> fields
  in
  let fields =
    match source.version with
    | Some version -> ("version", Json.String (Version.requirement_to_string version)) :: fields
    | None -> fields
  in
  Json.Object (List.rev fields)

let dependency_source_of_json = fun json ->
  match json with
  | Json.String "workspace" ->
      Ok {
        workspace = true;
        builtin = false;
        path = None;
        source_locator = None;
        ref_ = None;
        version = None;
      }
  | Json.String source_path ->
      Ok {
        workspace = false;
        builtin = false;
        path = Some (Path.v source_path);
        source_locator = None;
        ref_ = None;
        version = None;
      }
  | Json.Object fields -> (
      match List.assoc_opt "kind" fields with
      | Some (Json.String "workspace") ->
          Ok {
            workspace = true;
            builtin = false;
            path = None;
            source_locator = None;
            ref_ = None;
            version = None;
          }
      | Some (Json.String "builtin") ->
          Ok {
            workspace = false;
            builtin = true;
            path = None;
            source_locator = None;
            ref_ = None;
            version = Some Version.any;
          }
      | Some (Json.String "path") -> (
          let path =
            match List.assoc_opt "path" fields with
            | Some (Json.String path) -> Ok (Some (Path.v path))
            | _ -> Error "path dependency source is missing a string path"
          in
          let source_locator =
            match List.assoc_opt "source" fields with
            | Some (Json.String locator) -> Ok (Some (normalize_source_locator locator))
            | Some Json.Null
            | None -> Ok None
            | Some _ -> Error "path dependency source has non-string source locator"
          in
          let ref_ =
            match List.assoc_opt "ref" fields with
            | Some (Json.String ref_) -> Ok (Some ref_)
            | Some Json.Null
            | None -> Ok None
            | Some _ -> Error "path dependency source has non-string ref"
          in
          let version =
            match List.assoc_opt "version" fields with
            | Some Json.Null
            | None -> Ok None
            | Some (Json.String requirement) -> validate_requirement ~dependency_name:"<json>" requirement
            |> Result.map (fun version -> Some version)
            | _ -> Error "path dependency source has non-string version requirement"
          in
          match path, source_locator, ref_, version with
          | Ok path, Ok source_locator, Ok ref_, Ok version ->
              validate_dependency_source ~dependency_name:"<json>"
                {
                  workspace = false;
                  builtin = false;
                  path;
                  source_locator;
                  ref_;
                  version;
                }
          | (Error err, _, _, _)
          | (_, Error err, _, _)
          | (_, _, Error err, _)
          | (_, _, _, Error err) -> Error err
        )
      | Some (Json.String "registry") -> (
          match List.assoc_opt "version" fields with
          | Some Json.Null
          | None ->
              Ok {
                workspace = false;
                builtin = false;
                path = None;
                source_locator = None;
                ref_ = None;
                version = Some Version.any;
              }
          | Some (Json.String requirement) ->
              validate_requirement ~dependency_name:"<json>" requirement |> Result.map
                (fun version ->
                  {
                    workspace = false;
                    builtin = false;
                    path = None;
                    source_locator = None;
                    ref_ = None;
                    version = Some version;
                  })
          | _ -> Error "registry dependency source has non-string version requirement"
        )
      | Some (Json.String kind) ->
          Error ("unknown dependency source kind: " ^ kind)
      | _ ->
          let workspace =
            match List.assoc_opt "workspace" fields with
            | Some (Json.Bool value) -> Ok value
            | Some _ -> Error "dependency source workspace flag must be boolean"
            | None -> Ok false
          in
          let builtin =
            match List.assoc_opt "builtin" fields with
            | Some (Json.Bool value) -> Ok value
            | Some _ -> Error "dependency source builtin flag must be boolean"
            | None -> Ok false
          in
          let path =
            match List.assoc_opt "path" fields with
            | Some (Json.String path) -> Ok (Some (Path.v path))
            | Some Json.Null -> Ok None
            | Some _ -> Error "dependency source path must be a string"
            | None -> Ok None
          in
          let source_locator =
            match List.assoc_opt "source" fields with
            | Some (Json.String locator) -> Ok (Some (normalize_source_locator locator))
            | Some Json.Null -> Ok None
            | Some _ -> Error "dependency source source must be a string"
            | None -> Ok None
          in
          let ref_ =
            match List.assoc_opt "ref" fields with
            | Some (Json.String ref_) -> Ok (Some ref_)
            | Some Json.Null -> Ok None
            | Some _ -> Error "dependency source ref must be a string"
            | None -> Ok None
          in
          let version =
            match List.assoc_opt "version" fields with
            | Some (Json.String requirement) -> validate_requirement ~dependency_name:"<json>" requirement
            |> Result.map (fun version -> Some version)
            | Some Json.Null -> Ok None
            | Some _ -> Error "dependency source version must be a string"
            | None -> Ok None
          in
          match workspace, builtin, path, source_locator, ref_, version with
          | Ok workspace, Ok builtin, Ok path, Ok source_locator, Ok ref_, Ok version ->
              validate_dependency_source ~dependency_name:"<json>"
                {
                  workspace;
                  builtin;
                  path;
                  source_locator;
                  ref_;
                  version;
                }
          | (Error err, _, _, _, _, _)
          | (_, Error err, _, _, _, _)
          | (_, _, Error err, _, _, _)
          | (_, _, _, Error err, _, _)
          | (_, _, _, _, Error err, _)
          | (_, _, _, _, _, Error err) -> Error err
    )
  | _ ->
      Error "dependency source must be a string or object"

let parse_foreign_dependency:
  string -> Toml.value -> package_path:Path.t -> (foreign_dependency, string) result = fun name value ~package_path ->
  match value with
  | Toml.Table attrs -> (
      let get_string key =
        match List.assoc_opt key attrs with
        | Some (Toml.String s) -> Ok s
        | Some _ -> Error ("Foreign dependency '" ^ name ^ "': '" ^ key ^ "' must be a string")
        | None -> Error ("Foreign dependency '" ^ name ^ "': missing required field '" ^ key ^ "'")
      in
      let get_string_list key =
        match List.assoc_opt key attrs with
        | Some (Toml.Array arr) ->
            let strings =
              List.filter_map
                (
                  function
                  | Toml.String s -> Some s
                  | _ -> None
                )
                arr
            in
            if List.length strings = List.length arr then
              Ok strings
            else
              Error ("Foreign dependency '" ^ name ^ "': '" ^ key ^ "' must be an array of strings")
        | Some _ ->
            Error ("Foreign dependency '" ^ name ^ "': '" ^ key ^ "' must be an array")
        | None ->
            Error ("Foreign dependency '" ^ name ^ "': missing required field '" ^ key ^ "'")
      in
      let get_string_list_opt key =
        match List.assoc_opt key attrs with
        | Some (Toml.Array arr) ->
            let strings =
              List.filter_map
                (
                  function
                  | Toml.String s -> Some s
                  | _ -> None
                )
                arr
            in
            if List.length strings = List.length arr then
              Some strings
            else
              None
        | _ -> None
      in
      let get_env () =
        match List.assoc_opt "env" attrs with
        | Some (Toml.Table env_items) ->
            List.filter_map
              (fun ((k, v)) ->
                match v with
                | Toml.String s -> Some (k, s)
                | _ -> None)
              env_items
        | _ -> []
      in
      match get_string "path", get_string_list "build_cmd", get_string_list "outputs" with
      | Ok path_str, Ok build_cmd, Ok outputs ->
          let dep_path = Path.(package_path / v path_str) in
          let output_paths = List.map Path.v outputs in
          let clean_cmd = get_string_list_opt "clean_cmd" in
          let test_cmd = get_string_list_opt "test_cmd" in
          let env = get_env () in
          (* Scan for foreign dependency source files *)
          let scan_foreign_inputs foreign_path =
            let walker =
              match Fs.Walker.create ~roots:[ foreign_path ] ~sort:true ~follow_symlinks:true () with
              | Ok walker -> walker
              | Error _ -> panic "foreign dependency walker configuration should be valid"
            in
            let walker =
              Fs.Walker.filter_entry walker
                ~f:(fun (entry: Fs.Walker.FileItem.t) ->
                  let path = Fs.Walker.FileItem.path entry in
                  if Int.equal (Fs.Walker.FileItem.depth entry) 0 then
                    true
                  else
                    match Path.strip_prefix path ~prefix:foreign_path with
                    | Error _ -> false
                    | Ok rel_path ->
                        let entry_name = Path.basename rel_path in
                        let exclude_dirs = [ "target"; "_build"; "build"; "dist"; "node_modules" ] in
                        let should_skip =
                          String.starts_with ~prefix:"." entry_name || List.mem entry_name exclude_dirs in
                        if should_skip then
                          false
                        else
                          match Fs.Walker.FileItem.kind entry with
                          | Directory -> true
                          | File -> String.ends_with ~suffix:".rs" entry_name
                          || String.ends_with ~suffix:".c" entry_name
                          || String.ends_with ~suffix:".h" entry_name
                          || String.ends_with ~suffix:".cpp" entry_name
                          || String.ends_with ~suffix:".hpp" entry_name
                          || entry_name = "Cargo.toml"
                          || entry_name = "Cargo.lock"
                          || entry_name = "build.rs"
                          || entry_name = "CMakeLists.txt"
                          || entry_name = "Makefile"
                          | Symlink
                          | Other -> false)
            in
            let iter = Fs.Walker.into_iter walker in
            let rec loop acc iter =
              match Iter.Iterator.next iter with
              | None, _ ->
                  List.rev acc
              | Some (Error _), iter' ->
                  loop acc iter'
              | Some (Ok (entry: Fs.Walker.FileItem.t)), iter' -> (
                  let path = Fs.Walker.FileItem.path entry in
                  match Fs.Walker.FileItem.kind entry with
                  | File -> (
                      match Path.strip_prefix path ~prefix:foreign_path with
                      | Ok rel_path -> loop (rel_path :: acc) iter'
                      | Error _ -> loop acc iter'
                    )
                  | Directory
                  | Symlink
                  | Other -> loop acc iter'
                )
            in
            loop [] iter
          in
          let inputs = scan_foreign_inputs dep_path in
          Log.debug
            ("[PACKAGE] Foreign dependency '"
            ^ name
            ^ "' found "
            ^ Int.to_string (List.length inputs)
            ^ " input files");
          Ok {
            name;
            path = dep_path;
            inputs;
            build_cmd;
            clean_cmd;
            test_cmd;
            outputs = output_paths;
            env;
          }
      | Error e, _, _ ->
          Error e
      | _, Error e, _ ->
          Error e
      | _, _, Error e ->
          Error e
    )
  | _ -> Error ("Foreign dependency '" ^ name ^ "' must be a table")

let parse_foreign_dependencies:
  (string * Toml.value) list -> package_path:Path.t -> (foreign_dependency list, string) result = fun items ~package_path ->
  Log.debug "[PACKAGE] parse_foreign_dependencies: checking for 'foreign-dependencies' key";
  Log.debug ("[PACKAGE] Available keys: " ^ String.concat ", " (List.map fst items));
  (* Collect all keys that start with "foreign-dependencies." *)
  let foreign_dep_items =
    List.filter_map
      (fun ((key, value)) ->
        if String.starts_with ~prefix:"foreign-dependencies." key then
          let prefix_len = String.length "foreign-dependencies." in
          let dep_name = String.sub key prefix_len (String.length key - prefix_len) in
          Some (dep_name, value)
        else
          None)
      items
  in
  if not (List.is_empty foreign_dep_items) then
    Log.debug
      ("[PACKAGE] Found " ^ Int.to_string (List.length foreign_dep_items) ^ " foreign dependencies via dotted keys");
  let nested_deps =
    match List.assoc_opt "foreign-dependencies" items with
    | Some (Toml.Table deps) ->
        Log.debug
          ("[PACKAGE] Found foreign-dependencies table with " ^ Int.to_string (List.length deps) ^ " entries");
        deps
    | Some _ ->
        Log.warn "[PACKAGE] foreign-dependencies exists but is not a table";
        []
    | None ->
        Log.debug "[PACKAGE] No 'foreign-dependencies' table found";
        []
  in
  (* Combine both sources *)
  let all_deps = foreign_dep_items @ nested_deps in
  if all_deps = [] then
    Ok []
  else
    let results =
      List.map (fun ((name, value)) -> parse_foreign_dependency name value ~package_path) all_deps
    in
    let errors =
      List.filter_map
        (fun r ->
          match r with
          | Error e -> Some e
          | Ok _ -> None)
        results
    in
    if errors != [] then
      Error (String.concat "; " errors)
    else
      Ok (
        List.filter_map
          (fun r ->
            match r with
            | Ok d -> Some d
            | Error _ -> None)
          results
      )

let parse_binary: Toml.value -> package_path:Path.t -> (binary, string) result = fun value ~package_path ->
  match value with
  | Toml.Table items -> (
      match (List.assoc_opt "name" items, List.assoc_opt "path" items) with
      | Some (Toml.String name), Some (Toml.String path_str) ->
          let bin_path = Path.v path_str in
          Ok { name; path = bin_path }
      | Some (Toml.String _), None ->
          Error "Binary entry missing required 'path' field"
      | None, Some (Toml.String _) ->
          Error "Binary entry missing required 'name' field"
      | Some (Toml.String _), Some _ ->
          Error "Binary 'path' field must be a string"
      | Some _, Some _ ->
          Error "Binary 'name' field must be a string"
      | Some _, None ->
          Error "Binary 'name' field must be a string"
      | None, Some _ ->
          Error "Binary 'path' field must be a string"
      | None, None ->
          Error "Binary entry missing required 'name' and 'path' fields"
    )
  | _ -> Error "Binary entry must be a table"

let parse_binaries: (string * Toml.value) list -> package_path:Path.t -> (binary list, string) result = fun items ~package_path ->
  match List.assoc_opt "bin" items with
  | None ->
      Ok []
  | Some (Toml.Array bin_entries) ->
      let results = List.map (parse_binary ~package_path) bin_entries in
      let errors =
        List.filter_map
          (fun r ->
            match r with
            | Error e -> Some e
            | Ok _ -> None)
          results
      in
      if errors != [] then
        Error (String.concat "; " errors)
      else
        Ok (
          List.filter_map
            (fun r ->
              match r with
              | Ok b -> Some b
              | Error _ -> None)
            results
        )
  | Some _ ->
      Error "[[bin]] must be an array of tables"

let parse_library_kind = fun lib_items ->
  match List.assoc_opt "kind" lib_items with
  | None -> Ok Runtime
  | Some (Toml.String "runtime") -> Ok Runtime
  | Some (Toml.String "macro") -> Error "Library 'kind = \"macro\"' has been replaced by [riot.macro.provider]"
  | Some (Toml.String kind) -> Error ("Library 'kind' field must be \"runtime\", got \""
  ^ kind
  ^ "\"")
  | Some _ -> Error "Library 'kind' field must be a string"
let parse_library:
  (string * Toml.value) list ->
  package_path:Path.t ->
  package_name:string ->
  (library option, string) result = fun items ~package_path ~package_name ->
  match List.assoc_opt "lib" items with
  | None ->
      (* Autodiscover: if src/<package_name>.ml exists, use it as library *)
      let default_lib_path = Path.(package_path / Path.v "src" / Path.v (package_name ^ ".ml")) in
      (
        match Fs.exists default_lib_path with
        | Ok true -> Ok (Some { path = default_lib_path })
        | Ok false
        | Error _ -> Ok None
      )
  | Some (Toml.Table lib_items) -> (
      match parse_library_kind lib_items with
      | Error _ as err -> err
      | Ok Runtime -> (
          match List.assoc_opt "path" lib_items with
          | Some (Toml.String path_str) ->
              let lib_path = Path.(package_path / Path.v path_str) in
              Ok (Some { path = lib_path })
          | None ->
              let default_path = Path.(package_path / Path.v "src" / Path.v (package_name ^ ".ml")) in
              Ok (Some { path = default_path })
          | Some _ ->
              Error "Library 'path' field must be a string"
        )
      | Ok Macro ->
          Error "Library 'kind = \"macro\"' has been replaced by [riot.macro.provider]"
    )
  | Some _ ->
      Error "[lib] must be a table"

let parse_compiler_config: (string * Toml.value) list -> compiler_config = fun items ->
  let profile_overrides =
    match List.assoc_opt "profile" items with
    | Some (Toml.Table profile_table) ->
        List.filter_map
          (fun ((profile_name, value)) ->
            match value with
            | Toml.Table profile_items -> Some (
              profile_name,
              Profile.override_from_toml profile_items
            )
            | _ -> None)
          profile_table
    | _ -> []
  in
  (* Parse [target.macos], [target.linux], etc. sections *)
  let target_overrides =
    match List.assoc_opt "target" items with
    | Some (Toml.Table target_table) ->
        List.filter_map
          (fun ((platform, value)) ->
            match value with
            | Toml.Table platform_items ->
                let profile_override = Profile.override_from_toml platform_items in
                Some (platform, { profile_override = Some profile_override })
            | _ -> None)
          target_table
    | _ -> []
  in
  { profile_overrides; target_overrides }

let provider_excluded_relpaths = fun ~(package_path:Path.t) providers ->
  let collect_relative_files ~root ~keep_file ~skip_dir =
    let walker =
      match Fs.Walker.create ~roots:[ root ] ~sort:true ~follow_symlinks:true () with
      | Ok walker -> walker
      | Error _ -> panic "package walker configuration should be valid"
    in
    let walker =
      Fs.Walker.filter_entry walker
        ~f:(fun (entry: Fs.Walker.FileItem.t) ->
          let path = Fs.Walker.FileItem.path entry in
          if Int.equal (Fs.Walker.FileItem.depth entry) 0 then
            true
          else
            match Path.strip_prefix path ~prefix:package_path with
            | Error _ -> false
            | Ok rel_path -> (
                match Fs.Walker.FileItem.kind entry with
                | Directory -> not (skip_dir rel_path)
                | File -> keep_file rel_path
                | Symlink
                | Other -> false
              ))
    in
    let iter = Fs.Walker.into_iter walker in
    let rec loop acc iter =
      match Iter.Iterator.next iter with
      | None, _ ->
          List.rev acc
      | Some (Error _), iter' ->
          loop acc iter'
      | Some (Ok (entry: Fs.Walker.FileItem.t)), iter' -> (
          let path = Fs.Walker.FileItem.path entry in
          match Fs.Walker.FileItem.kind entry with
          | File -> (
              match Path.strip_prefix path ~prefix:package_path with
              | Ok rel_path -> loop (rel_path :: acc) iter'
              | Error _ -> loop acc iter'
            )
          | Directory
          | Symlink
          | Other -> loop acc iter'
        )
    in
    loop [] iter
  in
  let ocaml_source_suffix path_str =
    String.ends_with ~suffix:".ml" path_str || String.ends_with ~suffix:".mli" path_str in
  let collect_provider_tree rel_path =
    let provider_parent = Path.dirname rel_path in
    let parent_basename = Path.basename provider_parent in
    let basename = Path.basename rel_path in
    if String.equal basename "riot_fix_rules.ml" && String.equal parent_basename "riot_fix_rules" then
      let provider_dir = Path.(package_path / provider_parent) in
      collect_relative_files
        ~root:provider_dir
        ~skip_dir:(fun _ -> false)
        ~keep_file:(fun rel_path -> ocaml_source_suffix (Path.to_string rel_path))
    else
      [ rel_path ]
  in
  providers |> List.filter_map
    (fun (provider: Fix_provider.t) ->
      match Path.strip_prefix provider.source_path ~prefix:package_path with
      | Ok rel_path -> Some (collect_provider_tree rel_path)
      | Error _ -> None) |> List.concat |> List.sort_uniq
    (fun left right ->
      String.compare (Path.to_string left) (Path.to_string right))

let macro_provider_excluded_relpaths = fun ~(package_path:Path.t) providers ->
  providers
  |> List.filter_map
    (fun (provider: Macro_provider.t) ->
      match Path.strip_prefix provider.source_path ~prefix:package_path with
      | Ok rel_path -> Some rel_path
      | Error _ -> None)
  |> List.sort_uniq
    (fun left right ->
      String.compare (Path.to_string left) (Path.to_string right))

let scan_sources ~(package_path:Path.t) ?(excluded_relpaths = []) (): sources =
  let excluded_relpath_strings = excluded_relpaths |> List.map Path.to_string in
  let should_skip_source_entry filename = String.starts_with ~prefix:"." (Path.basename filename) in
  let should_skip_test_support_path rel_path =
    let path_str = Path.to_string rel_path in
    String.starts_with ~prefix:"tests/fixtures/" path_str
    || String.starts_with ~prefix:"tests/generated/" path_str
    || String.starts_with ~prefix:"tests/diagnostics/" path_str
  in
  let collect_relative_files root =
    let walker =
      match Fs.Walker.create ~roots:[ root ] ~sort:true ~follow_symlinks:true () with
      | Ok walker -> walker
      | Error _ -> panic "package walker configuration should be valid"
    in
    let walker =
      Fs.Walker.filter_entry walker
        ~f:(fun (entry: Fs.Walker.FileItem.t) ->
          let path = Fs.Walker.FileItem.path entry in
          if Int.equal (Fs.Walker.FileItem.depth entry) 0 then
            true
          else
            match Path.strip_prefix path ~prefix:package_path with
            | Error _ -> false
            | Ok rel_path -> (
                match Fs.Walker.FileItem.kind entry with
                | Directory -> not
                  (should_skip_source_entry rel_path || should_skip_test_support_path rel_path)
                | File -> not
                  (should_skip_source_entry rel_path
                  || should_skip_test_support_path rel_path
                  || List.mem (Path.to_string rel_path) excluded_relpath_strings)
                | Symlink
                | Other -> false
              ))
    in
    let iter = Fs.Walker.into_iter walker in
    let rec loop acc iter =
      match Iter.Iterator.next iter with
      | None, _ ->
          List.rev acc
      | Some (Error _), iter' ->
          loop acc iter'
      | Some (Ok (entry: Fs.Walker.FileItem.t)), iter' -> (
          let path = Fs.Walker.FileItem.path entry in
          match Fs.Walker.FileItem.kind entry with
          | File -> (
              match Path.strip_prefix path ~prefix:package_path with
              | Ok rel_path -> loop (rel_path :: acc) iter'
              | Error _ -> loop acc iter'
            )
          | Directory
          | Symlink
          | Other -> loop acc iter'
        )
    in
    loop [] iter
  in
  let src_files = collect_relative_files Path.(package_path / Path.v "src") in
  let test_files = collect_relative_files Path.(package_path / Path.v "tests") in
  let native_files = collect_relative_files Path.(package_path / Path.v "native") in
  let example_files = collect_relative_files Path.(package_path / Path.v "examples") in
  let bench_files = collect_relative_files Path.(package_path / Path.v "bench") in
  {
    src = src_files;
    tests = test_files;
    native = native_files;
    examples = example_files;
    bench = bench_files;
  }

(** Autodiscover test binaries from test files ending in _tests.ml or -tests.ml *)
let autodiscover_test_binaries: sources -> package_path:Path.t -> binary list = fun sources ~package_path ->
  List.filter_map
    (fun test_file ->
      let filename = Path.basename test_file in
      if
        String.ends_with ~suffix:"_tests.ml" filename || String.ends_with ~suffix:"-tests.ml" filename
      then
        let binary_name = Path.remove_extension (Path.v filename) |> Path.to_string in
        (* test_file is already relative to package (e.g., tests/foo_tests.ml) *)
        let binary_path = test_file in
        Some { name = binary_name; path = binary_path }
      else
        None)
    sources.tests

(** Autodiscover a default runtime binary from src/main.ml when no explicit [[bin]] exists. *)
let autodiscover_main_binary: sources -> package_name:string -> binary list = fun sources ~package_name ->
  if List.exists
      (fun path ->
        Path.equal path (Path.v "src/main.ml"))
      sources.src then
    [ { name = package_name; path = Path.v "src/main.ml" } ]
  else
    []

(** Autodiscover example binaries from any .ml file in examples/ directory *)
let autodiscover_example_binaries: sources -> package_path:Path.t -> binary list = fun sources ~package_path ->
  List.filter_map
    (fun example_file ->
      let filename = Path.basename example_file in
      if String.ends_with ~suffix:".ml" filename then
        let binary_name = Path.remove_extension (Path.v filename) |> Path.to_string in
        (* example_file is already relative to package (e.g., examples/sqltool.ml) *)
        Some { name = binary_name; path = example_file }
      else
        None)
    sources.examples

(** Autodiscover benchmark binaries from bench files ending in _bench.ml *)
let autodiscover_bench_binaries: sources -> package_path:Path.t -> binary list = fun sources ~package_path ->
  List.filter_map
    (fun bench_file ->
      let filename = Path.basename bench_file in
      if String.ends_with ~suffix:"_bench.ml" filename then
        let binary_name = Path.remove_extension (Path.v filename) |> Path.to_string in
        (* bench_file is already relative to package (e.g., bench/foo_bench.ml) *)
        Some { name = binary_name; path = bench_file }
      else
        None)
    sources.bench

let merge_binaries: declared:binary list -> autodiscovered:binary list -> binary list = fun ~declared ~autodiscovered ->
  let seen_paths = declared |> List.map (fun (bin: binary) -> Path.to_string bin.path) in
  let _, discovered =
    List.fold_left
      (fun ((seen_paths, acc)) (bin: binary) ->
        let path = Path.to_string bin.path in
        if List.mem path seen_paths then
          (seen_paths, acc)
        else
          (path :: seen_paths, bin :: acc))
      (seen_paths, [])
      autodiscovered
  in
  declared @ List.rev discovered

let from_toml:
  Toml.value ->
  workspace_deps:dependency list ->
  workspace_dev_deps:dependency list ->
  workspace_build_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (t, string) result = fun toml ~workspace_deps ~workspace_dev_deps ~workspace_build_deps ~path ~relative_path ->
  match toml with
  | Toml.Table items -> (
      let fallback_name = Path.basename path in
      let name = parse_name items fallback_name in
      match parse_publish_metadata items with
      | Error _ as err -> err
      | Ok publish ->
          match parse_dependency_section "dependencies" items ~workspace_deps with
          | Error _ as err -> err
          | Ok dependencies ->
              match parse_dependency_section "dev-dependencies" items ~workspace_deps:workspace_dev_deps with
              | Error _ as err -> err
              | Ok dev_dependencies ->
                  match parse_dependency_section "build-dependencies" items ~workspace_deps:workspace_build_deps with
                  | Error _ as err -> err
                  | Ok build_dependencies ->
                      let binaries =
                        match parse_binaries items ~package_path:path with
                        | Ok bins -> bins
                        | Error msg ->
                            Log.warn ("[PACKAGE] Failed to parse binaries for " ^ name ^ ": " ^ msg);
                            []
                      in
                      let library =
                        match parse_library items ~package_path:path ~package_name:name with
                        | Ok lib -> lib
                        | Error msg ->
                            Log.warn ("[PACKAGE] Failed to parse library for " ^ name ^ ": " ^ msg);
                            None
                      in
                      let foreign =
                        match parse_foreign_dependencies items ~package_path:path with
                        | Ok deps -> deps
                        | Error msg ->
                            Log.warn
                              ("[PACKAGE] Failed to parse foreign dependencies for "
                              ^ name
                              ^ ": "
                              ^ msg);
                            []
                      in
                      let fix_providers = Fix_provider.parse_from_toml
                        items
                        ~package_name:name
                        ~package_path:path in
                      match Macro_provider.parse_from_toml items ~package_name:name ~package_path:path with
                      | Error _ as err -> err
                      | Ok macro_providers ->
                          let excluded_relpaths =
                            provider_excluded_relpaths ~package_path:path fix_providers
                            @ macro_provider_excluded_relpaths ~package_path:path macro_providers
                            |> List.sort_uniq
                              (fun left right ->
                                String.compare (Path.to_string left) (Path.to_string right))
                          in
                          let sources = scan_sources ~package_path:path ~excluded_relpaths () in
                          let compiler = parse_compiler_config items in
                          let main_binaries = autodiscover_main_binary sources ~package_name:name in
                          let test_binaries = autodiscover_test_binaries sources ~package_path:path in
                          let example_binaries = autodiscover_example_binaries sources ~package_path:path in
                          let bench_binaries = autodiscover_bench_binaries sources ~package_path:path in
                          Log.debug
                            ("[PACKAGE] "
                            ^ name
                            ^ ": discovered "
                            ^ Int.to_string (List.length main_binaries)
                            ^ " runtime binaries from src/main.ml");
                          Log.debug
                            ("[PACKAGE] "
                            ^ name
                            ^ ": discovered "
                            ^ Int.to_string (List.length test_binaries)
                            ^ " test binaries from "
                            ^ Int.to_string (List.length sources.tests)
                            ^ " test files");
                          Log.debug
                            ("[PACKAGE] "
                            ^ name
                            ^ ": discovered "
                            ^ Int.to_string (List.length example_binaries)
                            ^ " example binaries from "
                            ^ Int.to_string (List.length sources.examples)
                            ^ " example files");
                          Log.debug
                            ("[PACKAGE] "
                            ^ name
                            ^ ": discovered "
                            ^ Int.to_string (List.length bench_binaries)
                            ^ " benchmark binaries from "
                            ^ Int.to_string (List.length sources.bench)
                            ^ " benchmark files");
                          let runtime_binaries = merge_binaries ~declared:binaries ~autodiscovered:main_binaries in
                          let all_binaries = merge_binaries
                            ~declared:runtime_binaries
                            ~autodiscovered:((test_binaries @ example_binaries @ bench_binaries)) in
                          let commands =
                            match List.assoc_opt "command" items with
                            | Some (Toml.Array cmd_entries) -> Package_command.parse_from_toml
                              cmd_entries
                              ~package_name:name
                              ~package_path:path
                            | _ -> []
                          in
                          Ok (
                            canonicalize
                              {
                            name;
                            path;
                            relative_path;
                            dependencies;
                            dev_dependencies;
                            build_dependencies;
                            foreign_dependencies = foreign;
                            binaries = all_binaries;
                            library;
                            sources;
                            compiler;
                            commands;
                            fix_providers;
                            macro_providers;
                            publish;
                          }
                          )
    )
  | _ -> Error "TOML is not a table"

let to_json: t -> Json.t = fun pkg ->
  let dependencies_json = Json.Array (List.map
    (fun (dep: dependency) ->
      Json.Object [
        ("name", Json.String dep.name);
        ("source", dependency_source_to_json dep.source);
      ])
    pkg.dependencies) in
  let dev_dependencies_json = Json.Array (List.map
    (fun (dep: dependency) ->
      Json.Object [
        ("name", Json.String dep.name);
        ("source", dependency_source_to_json dep.source);
      ])
    pkg.dev_dependencies) in
  let build_dependencies_json = Json.Array (List.map
    (fun (dep: dependency) ->
      Json.Object [
        ("name", Json.String dep.name);
        ("source", dependency_source_to_json dep.source);
      ])
    pkg.build_dependencies) in
  let binaries_json = Json.Array (List.map
    (fun (bin: binary) ->
      Json.Object [
        ("name", Json.String bin.name);
        ("path", Json.String (Path.to_string bin.path));
      ])
    pkg.binaries) in
  let library_json =
    match pkg.library with
    | Some lib -> Json.Object [ ("path", Json.String (Path.to_string lib.path)) ]
    | None -> Json.Null
  in
  let fix_providers_json = Json.Array (List.map Fix_provider.to_json pkg.fix_providers) in
  let macro_providers_json = Json.Array (List.map Macro_provider.to_json pkg.macro_providers) in
  Json.Object [
    ("name", Json.String pkg.name);
    ("path", Json.String (Path.to_string pkg.path));
    ("relative_path", Json.String (Path.to_string pkg.relative_path));
    ("dependencies", dependencies_json);
    ("dev_dependencies", dev_dependencies_json);
    ("build_dependencies", build_dependencies_json);
    ("binaries", binaries_json);
    ("library", library_json);
    ("fix_providers", fix_providers_json);
    ("macro_providers", macro_providers_json);
    (
      "publish",
      Json.Object (
        [] |> (fun fields ->
          match pkg.publish.version with
          | Some version -> ("version", Json.String (Version.to_string version)) :: fields
          | None -> fields) |> (fun fields ->
          match pkg.publish.description with
          | Some description -> ("description", Json.String description) :: fields
          | None -> fields) |> (fun fields ->
          match pkg.publish.license with
          | Some license -> ("license", Json.String license) :: fields
          | None -> fields) |> (fun fields ->
          match pkg.publish.is_public with
          | Some is_public -> ("public", Json.Bool is_public) :: fields
          | None -> fields) |> List.rev
      )
    );
  ]

let from_json: Json.t -> (t, string) result = fun json ->
  match json with
  | Json.Object fields -> (
      match (
        List.assoc_opt "name" fields,
        List.assoc_opt "path" fields,
        List.assoc_opt "relative_path" fields
      ) with
      | (Some (Json.String name), Some (Json.String path_str), Some (Json.String rel_path_str)) -> (
          let parse_dependencies_field field_name =
            match List.assoc_opt field_name fields with
            | Some (Json.Array deps) ->
                let rec loop acc entries =
                  match entries with
                  | [] -> Ok (List.rev acc)
                  | entry :: rest -> (
                      match entry with
                      | Json.Object dep_fields -> (
                          match (
                            List.assoc_opt "name" dep_fields,
                            List.assoc_opt "source" dep_fields
                          ) with
                          | Some (Json.String dep_name), Some source_json -> (
                              match dependency_source_of_json source_json with
                              | Ok source -> loop ({ name = dep_name; source } :: acc) rest
                              | Error _ as err -> err
                            )
                          | _ -> Error ("Invalid dependency entry in '" ^ field_name ^ "'")
                        )
                      | _ -> Error ("Invalid dependency entry in '" ^ field_name ^ "'")
                    )
                in
                loop [] deps
            | _ -> Ok []
          in
          match Path.of_string path_str with
          | Error _ -> Error ("Invalid path in package JSON: " ^ path_str)
          | Ok path -> (
              match Path.of_string rel_path_str with
              | Error _ -> Error ("Invalid relative_path in package JSON: " ^ rel_path_str)
              | Ok relative_path -> (
                  match parse_dependencies_field "dependencies" with
                  | Error _ as err -> err
                  | Ok dependencies -> (
                      match parse_dependencies_field "dev_dependencies" with
                      | Error _ as err -> err
                      | Ok dev_dependencies -> (
                          match parse_dependencies_field "build_dependencies" with
                          | Error _ as err -> err
                          | Ok build_dependencies ->
                              let binaries =
                                match List.assoc_opt "binaries" fields with
                                | Some (Json.Array bins) ->
                                    List.filter_map
                                      (
                                        function
                                        | Json.Object bin_fields -> (
                                            match (
                                              List.assoc_opt "name" bin_fields,
                                              List.assoc_opt "path" bin_fields
                                            ) with
                                            | (Some (Json.String bin_name), Some (Json.String bin_path)) -> Some {
                                              name = bin_name;
                                              path = Path.v bin_path
                                            }
                                            | _ -> None
                                          )
                                        | _ -> None
                                      )
                                      bins
                                | _ -> []
                              in
                              let library =
                                match List.assoc_opt "library" fields with
                                | Some (Json.Object lib_fields) -> (
                                    match List.assoc_opt "path" lib_fields with
                                    | Some (Json.String lib_path) -> Some { path = Path.v lib_path }
                                    | _ -> None
                                  )
                                | _ -> None
                              in
                              let macro_providers =
                                match List.assoc_opt "macro_providers" fields with
                                | Some (Json.Array providers) ->
                                    let rec loop acc = function
                                      | [] -> Ok (List.rev acc)
                                      | provider_json :: rest -> (
                                          match Macro_provider.of_json provider_json with
                                          | Ok provider -> loop (provider :: acc) rest
                                          | Error _ as err -> err
                                        )
                                    in
                                    loop [] providers
                                | _ -> Ok []
                              in
                              let publish =
                                match List.assoc_opt "publish" fields with
                                | Some (Json.Object publish_fields) ->
                                    let version =
                                      match List.assoc_opt "version" publish_fields with
                                      | Some (Json.String raw_version) -> (
                                          match Version.parse raw_version with
                                          | Ok version -> Ok (Some version)
                                          | Error err -> Error ("Invalid package publish version in JSON: "
                                          ^ version_parse_error_to_string err)
                                        )
                                      | Some Json.Null
                                      | None ->
                                          Ok None
                                      | Some _ ->
                                          Error "Package publish version must be a string"
                                    in
                                    let description =
                                      match List.assoc_opt "description" publish_fields with
                                      | Some (Json.String description) -> Ok (Some description)
                                      | Some Json.Null
                                      | None -> Ok None
                                      | Some _ -> Error "Package publish description must be a string"
                                    in
                                    let license =
                                      match List.assoc_opt "license" publish_fields with
                                      | Some (Json.String license) -> Ok (Some license)
                                      | Some Json.Null
                                      | None -> Ok None
                                      | Some _ -> Error "Package publish license must be a string"
                                    in
                                    let is_public =
                                      match List.assoc_opt "public" publish_fields with
                                      | Some (Json.Bool value) -> Ok (Some value)
                                      | Some Json.Null
                                      | None -> Ok None
                                      | Some _ -> Error "Package publish public flag must be a boolean"
                                    in
                                    (
                                      match version, description, license, is_public with
                                      | Ok version, Ok description, Ok license, Ok is_public -> Ok {
                                        version;
                                        description;
                                        license;
                                        is_public
                                      }
                                      | (Error err, _, _, _)
                                      | (_, Error err, _, _)
                                      | (_, _, Error err, _)
                                      | (_, _, _, Error err) -> Error err
                                    )
                                | Some _ ->
                                    Error "Package publish metadata must be an object"
                                | None ->
                                    Ok default_publish_metadata
                              in
                              (
                                match macro_providers with
                                | Error _ as err -> err
                                | Ok macro_providers -> (
                                    match publish with
                                    | Error _ as err -> err
                                    | Ok publish ->
                                        Ok (
                                          canonicalize
                                            {
                                          name;
                                          path;
                                          relative_path;
                                          dependencies;
                                          dev_dependencies;
                                          build_dependencies;
                                          foreign_dependencies = [];
                                          binaries;
                                          library;
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
                                          macro_providers;
                                          publish;
                                        }
                                        )
                                  )
                              )
                        )
                    )
                )
            )
        )
      | _ -> Error "Invalid package JSON"
    )
  | _ -> Error "Package must be a JSON object"

(** Hash package metadata into a hasher state *)
module type Hash_writer = sig
  type state
  val write: state -> string -> unit

  val write_int: state -> int -> unit

  val write_float: state -> float -> unit

  val write_bool: state -> bool -> unit

  val write_list: (state -> 'a -> unit) -> state -> 'a list -> unit
end

let hash_with = fun (type s) ((module H : Hash_writer with type state = s)) state (pkg: t) ->
  let hash_string_option value =
    match value with
    | Some value ->
        H.write_bool state true;
        H.write state value
    | None -> H.write_bool state false
  in
  let hash_path_option value =
    match value with
    | Some value ->
        H.write_bool state true;
        H.write state (Path.to_string value)
    | None -> H.write_bool state false
  in
  let rec hash_version (version: Version.t) =
    H.write_int state version.major;
    H.write_int state version.minor;
    H.write_int state version.patch;
    H.write_list
      (fun state segment ->
        match segment with
        | Version.Numeric n ->
            H.write_bool state true;
            H.write_int state n
        | Version.Alphanumeric s ->
            H.write_bool state false;
            H.write state s)
      state
      version.pre;
    hash_string_option version.build
  in
  let hash_requirement requirement =
    match Version.view_requirement requirement with
    | Version.AnyRequirement ->
        H.write_int state 0
    | Version.ExactRequirement version ->
        H.write_int state 1;
        hash_version version
    | Version.NotEqualRequirement version ->
        H.write_int state 2;
        hash_version version
    | Version.GreaterThanRequirement version ->
        H.write_int state 3;
        hash_version version
    | Version.GreaterThanOrEqualRequirement version ->
        H.write_int state 4;
        hash_version version
    | Version.LessThanRequirement version ->
        H.write_int state 5;
        hash_version version
    | Version.LessThanOrEqualRequirement version ->
        H.write_int state 6;
        hash_version version
    | Version.TildeRequirement version ->
        H.write_int state 7;
        hash_version version
    | Version.PrefixMajorRequirement major ->
        H.write_int state 8;
        H.write_int state major
    | Version.PrefixMinorRequirement (major, minor) ->
        H.write_int state 9;
        H.write_int state major;
        H.write_int state minor
  in
  let hash_kind_override value =
    match value with
    | Profile.Inherit -> H.write_int state 0
    | Profile.Override kind ->
        H.write_int state 1;
        H.write_int state
          (
            match kind with
            | Ocaml_compiler.Bytecode -> 0
            | Native -> 1
          )
  in
  let hash_inline_override value =
    match value with
    | Profile.Inherit ->
        H.write_int state 0
    | Profile.Override (Some n) ->
        H.write_int state 1;
        H.write_int state n
    | Profile.Override None ->
        H.write_int state 2
  in
  let hash_bool_override value =
    match value with
    | Profile.Inherit -> H.write_int state 0
    | Profile.Override value ->
        H.write_int state 1;
        H.write_bool state value
  in
  let hash_string_list_override value =
    match value with
    | Profile.Inherit -> H.write_int state 0
    | Profile.Override values ->
        H.write_int state 1;
        H.write_list H.write state values
  in
  H.write state pkg.name;
  (* Dependencies metadata *)
  let hash_dependency (dep: dependency) =
    H.write state dep.name;
    H.write_bool state dep.source.workspace;
    H.write_bool state dep.source.builtin;
    hash_path_option dep.source.path;
    hash_string_option dep.source.source_locator;
    hash_string_option dep.source.ref_;
    (
      match dep.source.version with
      | Some version ->
          H.write_bool state true;
          hash_requirement version
      | None -> H.write_bool state false
    )
  in
  List.iter hash_dependency pkg.dependencies;
  List.iter hash_dependency pkg.dev_dependencies;
  (
    match pkg.publish.version with
    | Some version ->
        H.write_bool state true;
        hash_version version
    | None -> H.write_bool state false
  );
  (
    match pkg.publish.description with
    | Some description ->
        H.write_bool state true;
        H.write state description
    | None -> H.write_bool state false
  );
  (
    match pkg.publish.license with
    | Some license ->
        H.write_bool state true;
        H.write state license
    | None -> H.write_bool state false
  );
  (
    match pkg.publish.is_public with
    | Some is_public ->
        H.write_bool state true;
        H.write_bool state is_public
    | None -> H.write_bool state false
  );
  (* Binaries metadata *)
  List.iter
    (fun (bin: binary) ->
      H.write state bin.name;
      H.write state (Path.to_string bin.path))
    pkg.binaries;
  List.iter
    (fun (provider: Fix_provider.t) ->
      H.write state provider.name;
      H.write state (Path.to_string provider.source_path);
      H.write_list H.write state provider.rules)
    pkg.fix_providers;
  let sorted_macro_providers =
    List.sort
      (fun (a: Macro_provider.t) (b: Macro_provider.t) ->
        Macro_provider.compare a b)
      pkg.macro_providers
  in
  List.iter
    (fun (provider: Macro_provider.t) ->
      H.write state provider.package_name;
      H.write state (Path.to_string provider.source_path);
      H.write_list H.write state provider.module_path;
      H.write_list H.write state provider.macros)
    sorted_macro_providers;
  (* Library metadata *)
  (
    match pkg.library with
    | Some lib ->
        H.write_bool state true;
        H.write state (Path.to_string lib.path)
    | None -> H.write_bool state false
  );
  (* Compiler configuration - profile and target overrides *)
  let hash_override (override: profile_override) =
    hash_kind_override override.kind;
    hash_inline_override override.inline;
    hash_bool_override override.no_assert;
    hash_bool_override override.compact;
    hash_bool_override override.unsafe;
    hash_bool_override override.no_alias_deps;
    hash_string_list_override override.open_modules;
    hash_string_list_override override.cc_flags;
    hash_string_list_override override.ocamlc_flags;
  in
  List.iter
    (fun ((profile_name, override): string * profile_override) ->
      H.write state profile_name;
      hash_override override)
    pkg.compiler.profile_overrides;
  List.iter
    (fun ((platform_name, target): string * target_override) ->
      H.write state platform_name;
      (
        match target.profile_override with
        | Some override ->
            H.write_bool state true;
            hash_override override
        | None -> H.write_bool state false
      );)
    pkg.compiler.target_overrides;
  (* Source file contents - include explicit [[bin]] entries that may not be in source dirs *)
  let seen_source_files = HashSet.with_capacity 32 in
  let hash_source_file file_path =
    let path_str = Path.to_string file_path in
    if HashSet.insert seen_source_files path_str then
      let abs_path =
        if Path.is_absolute file_path then
          file_path
        else
          Path.(pkg.path / file_path)
      in
      let path_str = Path.to_string file_path in
      match Fs.read abs_path with
      | Ok content ->
          H.write state path_str;
          H.write state content
      | Error _ ->
          (* File read error - include path only *)
          H.write state path_str
  in
  List.iter hash_source_file pkg.sources.src;
  List.iter hash_source_file pkg.sources.native;
  List.iter hash_source_file pkg.sources.tests;
  List.iter hash_source_file pkg.sources.examples;
  List.iter hash_source_file pkg.sources.bench;
  List.iter
    (fun (bin: binary) ->
      let path_str = Path.to_string bin.path in
      if String.ends_with ~suffix:".ml" path_str || String.ends_with ~suffix:".mli" path_str then
        hash_source_file bin.path)
    pkg.binaries;
  (* Foreign dependency sources *)
  List.iter
    (fun (fdep: foreign_dependency) ->
      H.write state fdep.name;
      H.write state (Path.to_string fdep.path);
      List.iter (H.write state) fdep.build_cmd;
      (* Hash all input files *)
      List.iter
        (fun input_path ->
          let abs_path = Path.(fdep.path / input_path) in
          match Fs.read abs_path with
          | Ok content ->
              H.write state (Path.to_string input_path);
              H.write state content
          | Error _ -> H.write state (Path.to_string input_path))
        fdep.inputs)
    pkg.foreign_dependencies

let hash = fun state pkg -> hash_with (module Crypto.Sha256) state pkg

module Tests = struct
  let source = fun ?(workspace = false) ?(builtin = false) ?path ?source_locator ?ref_ ?version () ->
    {
      workspace;
      builtin;
      path;
      source_locator;
      ref_;
      version;
    }

  let publish = default_publish_metadata

  let test_parse_dependency_classes (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
std = { workspace = true }

[dev-dependencies]
propane = { workspace = true }

[build-dependencies]
fixme = { path = "../fixme" }
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let workspace_dep name = { name; source = source ~workspace:true () } in
    let pkg = from_toml
      toml
      ~workspace_deps:[ workspace_dep "std" ]
      ~workspace_dev_deps:[ workspace_dep "propane" ]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example")
      ~relative_path:(Path.v "packages/example")
    |> Result.expect ~msg:"expected package manifest" in
    if
      List.map (fun (dep: dependency) -> dep.name) pkg.dependencies = [ "std" ]
      && List.map (fun (dep: dependency) -> dep.name) pkg.dev_dependencies = [ "propane" ]
      && List.map (fun (dep: dependency) -> dep.name) pkg.build_dependencies = [ "fixme" ]
    then
      Ok ()
    else
      Error "expected dependency classes to round-trip" [@test]

  let test_parse_registry_requirement (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
std = ">= 1.2.3"
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let pkg = from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example")
      ~relative_path:(Path.v "packages/example")
    |> Result.expect ~msg:"expected package manifest" in
    match pkg.dependencies with
    | [ { source={
          workspace=false;
          builtin=false;
          path=None;
          source_locator=None;
          ref_=None;
          version=Some requirement;

        }; _;  } ] ->
        if String.equal (Version.requirement_to_string requirement) ">= 1.2.3" then
          Ok ()
        else
          Error "expected parsed dependency requirement to be preserved structurally"
    | _ -> Error "expected a registry dependency with a parsed requirement" [@test]

  let test_parse_builtin_dependency (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
stdlib = "*"
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let pkg = from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example")
      ~relative_path:(Path.v "packages/example")
    |> Result.expect ~msg:"expected package manifest" in
    match pkg.dependencies with
    | [ { name="stdlib"; source={ builtin=true; version=Some requirement; _ } } ] when requirement_is_any
      requirement -> Ok ()
    | _ -> Error "expected stdlib '*' to parse as a builtin dependency" [@test]

  let test_parse_github_dependency_shorthand (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
widgets = { github = "riot-tests/widgets" }
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let pkg = from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example")
      ~relative_path:(Path.v "packages/example")
    |> Result.expect ~msg:"expected package manifest" in
    match pkg.dependencies with
    | [
      {
        name="widgets";
        source={ source_locator=Some "github.com/riot-tests/widgets"; ref_=None; _ }
      }
    ] -> Ok ()
    | _ -> Error "expected github shorthand to normalize into a source locator" [@test]

  let test_parse_source_dependency_with_ref_and_path (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
widgets = { source = "https://github.com/riot-tests/monorepo/packages/widgets", ref = "main" }
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let pkg = from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example")
      ~relative_path:(Path.v "packages/example")
    |> Result.expect ~msg:"expected package manifest" in
    match pkg.dependencies with
    | [
      {
        name="widgets";
        source={
          source_locator=Some "github.com/riot-tests/monorepo/packages/widgets";
          ref_=Some "main";
          _;

        };

      }
    ] -> Ok ()
    | _ -> Error "expected source dependency to preserve locator and ref" [@test]

  let test_builtin_dependency_rejects_version_constraints (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
stdlib = ">= 1.0.0"
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    match from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example")
      ~relative_path:(Path.v "packages/example") with
    | Ok _ -> Error "expected builtin dependency version constraints to fail"
    | Error _ -> Ok () [@test]

  let test_invalid_registry_requirement_fails_manifest_parse (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
std = "definitely-not-semver"
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    match from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example")
      ~relative_path:(Path.v "packages/example") with
    | Ok _ -> Error "expected invalid semver requirement to fail package parsing"
    | Error _ -> Ok () [@test]

  let test_package_json_round_trips_registry_requirement (): (unit, string) result =
    let requirement = Version.parse_requirement ">= 1.2.3" |> Result.expect ~msg:"expected requirement to parse" in
    let package = {
      name = "example";
      path = Path.v "/tmp/example";
      relative_path = Path.v "packages/example";
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
      macro_providers = [];
      publish;
    }
    in
    match from_json (to_json package) with
    | Error err -> Error err
    | Ok decoded -> (
        match decoded.dependencies with
        | [ { source={
              workspace=false;
              builtin=false;
              path=None;
              source_locator=None;
              ref_=None;
              version=Some decoded_requirement;

            }; _ } ] ->
            if String.equal (Version.requirement_to_string decoded_requirement) ">= 1.2.3" then
              Ok ()
            else
              Error "expected registry requirement to survive package json roundtrip"
        | _ -> Error "expected registry dependency after package json roundtrip"
      ) [@test]

  let test_package_json_round_trips_source_dependency (): (unit, string) result =
    let package = {
      name = "example";
      path = Path.v "/tmp/example";
      relative_path = Path.v "packages/example";
      dependencies = [
        {
          name = "widgets";
          source = source ~source_locator:"github.com/riot-tests/widgets" ~ref_:"main" ()
        }
      ];
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
      macro_providers = [];
      publish;
    }
    in
    match from_json (to_json package) with
    | Ok {
      dependencies=[
        {
          name="widgets";
          source={ source_locator=Some "github.com/riot-tests/widgets"; ref_=Some "main"; _;  };

        }
      ];
      _;

    } -> Ok ()
    | Ok _ -> Error "expected source dependency to survive package json roundtrip"
    | Error err -> Error err [@test]

  let test_resolve_projects_runtime_and_build_edges (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "app"
version = "0.1.0"

[dependencies]
std = {}

[build-dependencies]
ppx = {}
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let package = from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/workspace/packages/app")
      ~relative_path:(Path.v "packages/app")
    |> Result.expect ~msg:"expected package manifest" in
    let lock_package: Lockfile.package = {
      id = { registry = None; name = "app"; version = None; sha256 = None };
      root = Some (Path.v "packages/app");
      provenance = Lockfile.Workspace;
      dependencies = [
        {
          name = "std";
          package = {
            registry = Some "pkgs.ml";
            name = "std";
            version = Some "0.1.0";
            sha256 = Some "deadbeef"
          }
        };
      ];
      build_dependencies = [
        {
          name = "ppx";
          package = {
            registry = Some "pkgs.ml";
            name = "ppx";
            version = Some "1.2.3";
            sha256 = Some "cafebabe"
          }
        };
      ];
      dev_dependencies = [];
    }
    in
    match resolve
      ~package
      ~lock_package
      ~manifest_path:Path.(package.path / Path.v "riot.toml")
      ~materialized_root:package.path with
    | Ok resolved ->
        if
          List.length resolved.runtime_resolved = 1
          && List.length resolved.build_resolved = 1
          && (List.hd resolved.runtime_resolved).resolved_id.name = "std"
          && (List.hd resolved.build_resolved).resolved_id.version = Some "1.2.3"
        then
          Ok ()
        else
          Error "expected resolved package projection to preserve exact ids"
    | Error err -> Error err [@test]

  let test_resolve_requires_all_declared_dependencies (): (unit, string) result =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "app"
version = "0.1.0"

[dependencies]
std = {}
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let package = from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/workspace/packages/app")
      ~relative_path:(Path.v "packages/app")
    |> Result.expect ~msg:"expected package manifest" in
    let lock_package: Lockfile.package = {
      id = { registry = None; name = "app"; version = None; sha256 = None };
      root = Some (Path.v "packages/app");
      provenance = Lockfile.Workspace;
      dependencies = [];
      build_dependencies = [];
      dev_dependencies = [];
    }
    in
    match resolve
      ~package
      ~lock_package
      ~manifest_path:Path.(package.path / Path.v "riot.toml")
      ~materialized_root:package.path with
    | Ok _ -> Error "expected resolve to fail when a declared dependency is missing from the lockfile"
    | Error _ -> Ok () [@test]

  let test_resolve_ignores_builtin_dependencies (): (unit, string) result =
    let package = {
      name = "app";
      path = Path.v "/workspace/packages/app";
      relative_path = Path.v "packages/app";
      dependencies = [ { name = "stdlib"; source = source ~builtin:true ~version:Version.any () } ];
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
      macro_providers = [];
      publish;
    }
    in
    let lock_package: Lockfile.package = {
      id = { registry = None; name = "app"; version = None; sha256 = None };
      root = Some (Path.v "packages/app");
      provenance = Lockfile.Workspace;
      dependencies = [];
      build_dependencies = [];
      dev_dependencies = [];
    }
    in
    match resolve
      ~package
      ~lock_package
      ~manifest_path:Path.(package.path / Path.v "riot.toml")
      ~materialized_root:package.path with
    | Ok resolved when resolved.runtime_resolved = [] -> Ok ()
    | Ok _ -> Error "expected builtin dependencies to stay out of the resolved lock graph"
    | Error err -> Error err [@test]

  let test_build_graph_dependencies_exclude_build_only_deps (): (unit, string) result =
    let pkg = {
      name = "example";
      path = Path.v "/tmp/example";
      relative_path = Path.v "packages/example";
      dependencies = [ { name = "std"; source = source ~workspace:true () } ];
      dev_dependencies = [ { name = "propane"; source = source ~workspace:true () } ];
      build_dependencies = [ { name = "fixme"; source = source ~workspace:true () } ];
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
      macro_providers = [];
      publish;
    }
    in
    let build_graph = build_graph_dependencies pkg |> List.map (fun (dep: dependency) -> dep.name) in
    let all = all_dependencies pkg |> List.map (fun (dep: dependency) -> dep.name) in
    if build_graph = [ "std"; "propane" ] && all = [ "std"; "propane"; "fixme" ] then
      Ok ()
    else
      Error "expected build graph dependencies to exclude build-only deps" [@test]
end
