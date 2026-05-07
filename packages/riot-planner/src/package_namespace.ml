open Std
open Riot_model

type t = {
  public_root: string;
  compiled_root: string;
}

let flag_var = "RIOT_EXPERIMENT_PACKAGE_NAMESPACES"

let enabled = fun () ->
  match Env.get Env.String ~var:flag_var with
  | Some "0"
  | Some "false"
  | Some "no" -> false
  | _ -> true

let mode_key = fun () ->
  if enabled () then
    "package-namespaces:v1"
  else
    "package-namespaces:off"

let sanitize_component = fun value ->
  String.map
    ~fn:(fun ch ->
      match ch with
      | 'A' .. 'Z'
      | 'a' .. 'z'
      | '0' .. '9' -> ch
      | _ -> '_')
    value

let package_version_component = fun (package: Package.t) ->
  match package.publish.version with
  | Some version -> "v" ^ sanitize_component (Version.to_string version)
  | None -> "vdev"

let package_hash_component = fun package ->
  let module H = Crypto.Sha256 in
  let state = H.create () in
  H.write state (Package_name.to_string package.Package.name);
  H.write state (Path.to_string package.path);
  (
    match package.publish.version with
    | Some version -> H.write state (Version.to_string version)
    | None -> H.write state "dev"
  );
  (
    match package.library with
    | Some library -> H.write state (Path.to_string library.path)
    | None -> H.write state "no-library"
  );
  let hex = Crypto.Digest.hex (H.finish state) in
  String.sub hex ~offset:0 ~len:(Int.min 10 (String.length hex))

let public_root = fun package -> Package.root_module_name package

let compiled_root = fun package ->
  let public_root = public_root package in
  if not (enabled ()) then
    public_root
  else
    public_root
    ^ "_"
    ^ package_version_component package
    ^ "_"
    ^ package_hash_component package

let for_package = fun package -> { public_root = public_root package; compiled_root = compiled_root package }

let planning_library_name = fun package ->
  if enabled () then
    compiled_root package
  else
    Package_name.to_string package.Package.name

let library_cmxa = fun package ->
  Module_name.(from_string (compiled_root package)
  |> cmxa)
