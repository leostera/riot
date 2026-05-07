open Std
open Std.Result.Syntax

type parsed = {
  dependencies: Riot_model.Package.dependency list;
  body: string;
  prelude: string;
}

let std_package_name =
  Riot_model.Package_name.from_string "std"
  |> Std.Result.expect ~msg:"expected std package name to be valid"

let source_of_registry_spec = fun (spec: Riot_deps.Registry_package_spec.t) ->
  let package_name = Riot_model.Package_name.to_string spec.name in
  Riot_model.Package.{
    workspace = false;
    builtin = Riot_model.Package.is_builtin_dependency_name package_name;
    path = None;
    source_locator = None;
    ref_ = None;
    version = spec.requirement;
  }

let dependency_of_registry_spec = fun (spec: Riot_deps.Registry_package_spec.t) ->
  Riot_model.Package.{
    name = spec.name;
    source = source_of_registry_spec spec;
  }

let dependency_of_package_name = fun package_name ->
  dependency_of_registry_spec Riot_deps.Registry_package_spec.{
    name = package_name;
    requirement = Some Std.Version.any;
  }

let dependency_names = fun dependencies ->
  Std.List.map dependencies ~fn:(fun (dependency: Riot_model.Package.dependency) -> dependency.name)

let dependency_name_in = fun dependency_name dependencies ->
  Std.List.any dependencies ~fn:(fun (dependency: Riot_model.Package.dependency) ->
    Riot_model.Package_name.equal dependency.name dependency_name)

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

let default_std_dependency = fun () -> dependency_of_package_name std_package_name

let ensure_std_dependency = fun dependencies ->
  let dependencies = unique_dependencies dependencies in
  if dependency_name_in std_package_name dependencies then
    dependencies
  else
    default_std_dependency () :: dependencies

let dependency_spec_error = fun spec error ->
  "invalid #use directive '"
  ^ spec
  ^ "': "
  ^ Riot_deps.Registry_package_spec.error_message error

let parse_dependency_spec = fun spec ->
  let spec = Std.String.trim spec in
  if Std.String.is_empty spec then
    Error "#use directive requires a registry package spec"
  else if Std.String.contains spec " "
          || Std.String.contains spec "\t"
          || Std.String.contains spec "\n"
          || Std.String.contains spec "\r"
  then
    Error ("invalid #use directive '" ^ spec ^ "': expected one registry package spec")
  else
    Riot_deps.Registry_package_spec.from_string spec
    |> Std.Result.map ~fn:dependency_of_registry_spec
    |> Std.Result.map_err ~fn:(dependency_spec_error spec)

let trim_line_end = fun line ->
  if Std.String.ends_with ~suffix:"\r" line then
    Std.String.sub line ~offset:0 ~len:(Std.String.length line - 1)
  else
    line

let split_lines = fun source ->
  Std.String.split source ~by:"\n"
  |> Std.List.map ~fn:trim_line_end

let strip_shebang_lines = fun lines ->
  match lines with
  | first :: rest when Std.String.starts_with ~prefix:"#!" first -> rest
  | _ -> lines

let parse_use_line = fun line ->
  let trimmed = Std.String.trim line in
  if not (Std.String.starts_with ~prefix:"#use" trimmed) then
    Error ("internal script parser error: not a #use line: " ^ line)
  else
    let after_directive_raw = Std.String.sub trimmed ~offset:4 ~len:(Std.String.length trimmed - 4) in
    if Std.String.is_empty after_directive_raw then
      Error "#use directive requires a registry package spec"
    else if not (Prelude.is_whitespace (Std.String.get_unchecked after_directive_raw ~at:0)) then
      Error ("invalid #use directive '" ^ trimmed ^ "': expected `#use <registry-spec>;;`")
    else
    let after_directive = Std.String.trim after_directive_raw in
    if Std.String.is_empty after_directive then
      Error "#use directive requires a registry package spec"
    else if not (Std.String.ends_with ~suffix:";;" after_directive) then
      Error "unterminated #use directive: expected ';;'"
    else
      let spec =
        Std.String.sub after_directive ~offset:0 ~len:(Std.String.length after_directive - 2)
        |> Std.String.trim
      in
      parse_dependency_spec spec

let body_contains_late_use = fun body_lines ->
  Std.List.find
    body_lines
    ~fn:(fun line -> Std.String.starts_with ~prefix:"#use" (Std.String.trim line))

let parse = fun source ->
  let lines = source |> split_lines |> strip_shebang_lines in
  let rec loop prelude_lines dependencies body_started body_lines = fun __tmp1 ->
    match __tmp1 with
    | [] ->
        let body_lines = Std.List.reverse body_lines in
        (
          match body_contains_late_use body_lines with
          | Some line ->
              Error (
                "late #use directive '"
                ^ Std.String.trim line
                ^ "': #use directives must appear before script code")
          | None ->
              Ok {
                dependencies = ensure_std_dependency dependencies;
                prelude = Std.String.concat "\n" (Std.List.reverse prelude_lines);
                body = Std.String.concat "\n" body_lines;
              }
        )
    | line :: rest ->
        let trimmed = Std.String.trim line in
        if body_started then
          loop prelude_lines dependencies true (line :: body_lines) rest
        else if Std.String.equal trimmed "" then
          loop (line :: prelude_lines) dependencies false body_lines rest
        else if Std.String.starts_with ~prefix:"#use" trimmed then
          let* dependency = parse_use_line line in
          loop (line :: prelude_lines) (upsert_dependency dependencies dependency) false body_lines rest
        else
          loop prelude_lines dependencies true (line :: body_lines) rest
  in
  loop [] [] false [] lines

let dependencies_of_package_names = fun package_names ->
  package_names
  |> Std.List.map ~fn:dependency_of_package_name
  |> unique_dependencies
