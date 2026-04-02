open Std

type generated_provider = {
  provider : Riot_model.Macro_provider.t;
  module_name : string;
  support_module_sources : (string * Path.t) list;
}

type dependency_source =
  | Consumer_workspace
  | Tool_workspace
  | External_path

type dependency = {
  name : string;
  path : Path.t;
  source : dependency_source;
}

type plan = {
  provider_hash : string;
  generated_dir : Path.t;
  workspace_root : Path.t;
  workspace_toml_path : Path.t;
  toolchain_toml_path : Path.t;
  build_dir_root : Path.t;
  package_dir : Path.t;
  package_toml_path : Path.t;
  src_dir : Path.t;
  library_path : Path.t;
  main_path : Path.t;
  binary_path : Path.t;
  package_name : string;
  binary_name : string;
  dependencies : dependency list;
  providers : generated_provider list;
}

let trace_enabled = fun () ->
  match Env.var String ~name:"RIOT_MACRO_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace = fun message ->
  if trace_enabled () then
    eprintln ("[macro-runner] " ^ message)

let is_shell_safe_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '-'
  | '.'
  | '/'
  | ':'
  | '+'
  | '='
  | ','
  | '@'
  | '%' -> true
  | _ -> false

let shell_quote = fun value ->
  if String.equal value "" then
    "''"
  else if String.for_all is_shell_safe_char value then
    value
  else
    "'" ^ String.concat "'\"'\"'" (String.split_on_char '\'' value) ^ "'"

let sanitize_component = fun text ->
  String.map
    (fun ch ->
      if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') then
        ch
      else
        '_')
    text

let generated_module_name = fun (provider: Riot_model.Macro_provider.t) ->
  "Provider_" ^ sanitize_component provider.package_name ^ "_" ^ sanitize_component provider.module_name

let provider_source_path = fun (provider: Riot_model.Macro_provider.t) ->
  if Path.is_absolute provider.source_path then
    provider.source_path
  else
    Path.(provider.package_path / provider.source_path)

let ocaml_module_name_of_path = fun path ->
  let base = Path.basename path |> Path.v |> Path.remove_extension |> Path.to_string in
  if String.length base = 0 then
    "Generated"
  else
    String.uppercase_ascii (String.sub base 0 1) ^ String.sub base 1 (String.length base - 1)

let support_module_sources = fun (provider: Riot_model.Macro_provider.t) ->
  let source_path = provider_source_path provider in
  let provider_dir = Path.dirname source_path in
  let provider_basename = Path.basename source_path in
  match Fs.read_dir provider_dir with
  | Error _ -> []
  | Ok iter ->
      Std.Iter.MutIterator.to_list iter
      |> List.filter_map
        (fun entry ->
          let source_path = Path.(provider_dir / entry) in
          let entry_name = Path.basename source_path in
          if
            String.equal entry_name provider_basename
            || not (String.ends_with ~suffix:".ml" entry_name)
          then
            None
          else
            Some (ocaml_module_name_of_path source_path, source_path))
      |> List.sort
        (fun (left_name, left_path) (right_name, right_path) ->
          match String.compare left_name right_name with
          | 0 -> String.compare (Path.to_string left_path) (Path.to_string right_path)
          | cmp -> cmp)

let file_content_hash = fun path ->
  match Fs.read path with
  | Ok source -> Crypto.hash_string source |> Crypto.Digest.hex
  | Error _ -> "missing"

let provider_fingerprint = fun (provider: Riot_model.Macro_provider.t) ->
  let source_path = provider_source_path provider in
  let support_hashes =
    support_module_sources provider
    |> List.map
      (fun (module_name, source_path) ->
        module_name ^ ":" ^ Path.to_string source_path ^ ":" ^ file_content_hash source_path)
    |> String.concat ","
  in
  String.concat
    ":"
    [
      Riot_model.Macro_provider.fingerprint provider;
      Path.to_string source_path;
      file_content_hash source_path;
      support_hashes;
    ]

let providers_hash = fun providers ->
  providers
  |> List.sort Riot_model.Macro_provider.compare
  |> List.map provider_fingerprint
  |> String.concat "\n"
  |> Crypto.hash_string
  |> Crypto.Digest.hex

let generated_provider = fun provider ->
  {
    provider;
    module_name = generated_module_name provider;
    support_module_sources = support_module_sources provider;
  }

let normalized_path_string = fun path -> Path.normalize path |> Path.to_string

let path_equal = fun left right ->
  String.equal (normalized_path_string left) (normalized_path_string right)

let dependency = fun ~source name path -> { name; path; source }

let option_or_else_lazy = fun fallback value ->
  match value with
  | Some _ -> value
  | None -> fallback ()

let scan_workspace_packages = fun workspace_root ->
  match Riot_model.Workspace_manager.scan workspace_root with
  | Ok (workspace, _errors) -> Riot_model.Workspace.(workspace.packages)
  | Error _ -> []

let has_workspace_package = fun workspace_root name ->
  match Fs.exists Path.(workspace_root / Path.v "packages" / Path.v name / Path.v "riot.toml") with
  | Ok true -> true
  | _ -> false

let tool_workspace_root = fun consumer_workspace_root ->
  if
    List.for_all (has_workspace_package consumer_workspace_root) [ "std"; "syn"; "macro"; "riot-model" ]
  then
    consumer_workspace_root
  else
    match Env.current_dir () with
    | Ok cwd
      when List.for_all (has_workspace_package cwd) [ "std"; "syn"; "macro"; "riot-model" ] -> cwd
    | _ -> consumer_workspace_root

let find_package_by_name = fun packages name ->
  List.find_opt (fun (pkg: Riot_model.Package.t) -> String.equal pkg.name name) packages

let find_package_by_path = fun packages path ->
  List.find_opt (fun (pkg: Riot_model.Package.t) -> path_equal pkg.path path) packages

let dependency_from_package = fun ~source (pkg: Riot_model.Package.t) ->
  dependency ~source pkg.name pkg.path

let resolve_dependency_path = fun ~package_path path ->
  if Path.is_absolute path then
    Path.normalize path
  else
    Path.normalize Path.(package_path / path)

let dependency_entries_for_workspace = fun workspace_root providers ->
  let consumer_packages = scan_workspace_packages workspace_root in
  let tool_workspace_root = tool_workspace_root workspace_root in
  let tool_packages =
    if path_equal tool_workspace_root workspace_root then
      []
    else
      scan_workspace_packages tool_workspace_root
  in
  let resolve_workspace_package ~source name =
    match source with
    | Consumer_workspace ->
        find_package_by_name consumer_packages name
        |> Option.map (dependency_from_package ~source:Consumer_workspace)
        |> option_or_else_lazy (fun () ->
          find_package_by_name tool_packages name
          |> Option.map (dependency_from_package ~source:Tool_workspace))
    | Tool_workspace ->
        find_package_by_name tool_packages name
        |> Option.map (dependency_from_package ~source:Tool_workspace)
        |> option_or_else_lazy (fun () ->
          find_package_by_name consumer_packages name
          |> Option.map (dependency_from_package ~source:Consumer_workspace))
    | External_path -> None
  in
  let package_for_dependency = fun (dep: dependency) ->
    match dep.source with
    | Consumer_workspace -> find_package_by_path consumer_packages dep.path
    | Tool_workspace -> find_package_by_path tool_packages dep.path
    | External_path -> None
  in
  let resolve_dependency_entry ~source ~package_path (dep: Riot_model.Package.dependency) =
    match dep.source with
    | { workspace=true; _ } -> resolve_workspace_package ~source dep.name
    | { builtin=true; _ } -> None
    | { path=Some path; _ } ->
        let abs_path = resolve_dependency_path ~package_path path in
        find_package_by_path
          (match source with
          | Consumer_workspace -> consumer_packages
          | Tool_workspace -> tool_packages
          | External_path -> [])
          abs_path
        |> Option.map
          (fun pkg ->
            dependency_from_package
              ~source:(
                match source with
                | Consumer_workspace -> Consumer_workspace
                | Tool_workspace -> Tool_workspace
                | External_path -> External_path)
              pkg)
        |> option_or_else_lazy (fun () -> Some (dependency ~source:External_path dep.name abs_path))
    | { path=None; _ } -> None
  in
  let provider_dependency_entries =
    providers
    |> List.concat_map
      (fun ({ provider; _ }: generated_provider) ->
        let provider_package =
          find_package_by_path consumer_packages provider.package_path
          |> Option.map (fun pkg -> Consumer_workspace, pkg)
          |> option_or_else_lazy (fun () ->
            find_package_by_path tool_packages provider.package_path
            |> Option.map (fun pkg -> Tool_workspace, pkg))
          |> option_or_else_lazy (fun () ->
            find_package_by_name consumer_packages provider.package_name
            |> Option.map (fun pkg -> Consumer_workspace, pkg))
          |> option_or_else_lazy (fun () ->
            find_package_by_name tool_packages provider.package_name
            |> Option.map (fun pkg -> Tool_workspace, pkg))
        in
        match provider_package with
        | None -> []
        | Some (source, pkg) ->
            Riot_model.Package.all_dependencies pkg
            |> List.filter_map (resolve_dependency_entry ~source ~package_path:pkg.path))
  in
  let core_dependencies =
    [ "std"; "syn"; "macro"; "riot-model" ]
    |> List.filter_map
      (fun name ->
        find_package_by_name tool_packages name
        |> Option.map (dependency_from_package ~source:Tool_workspace))
  in
  let rec expand seen acc = function
    | [] -> List.rev acc
    | dep :: rest ->
        if List.mem dep.name seen then
          expand seen acc rest
        else
          let next =
            match package_for_dependency dep with
            | None -> []
            | Some pkg ->
                Riot_model.Package.all_dependencies pkg
                |> List.filter_map (resolve_dependency_entry ~source:dep.source ~package_path:pkg.path)
          in
          expand (dep.name :: seen) (dep :: acc) (next @ rest)
  in
  expand [] [] (core_dependencies @ provider_dependency_entries)

let plan = fun ~workspace_root ~target_dir_root providers ->
  let hash = providers_hash providers in
  let generated_providers = List.map generated_provider providers in
  let dependencies = dependency_entries_for_workspace workspace_root generated_providers in
  let generated_dir =
    Path.(target_dir_root / Path.v "macro" / Path.v "macro-runner" / Path.v hash)
  in
  let workspace_root = Path.(generated_dir / Path.v "workspace") in
  let build_dir_root = Path.(generated_dir / Path.v "build") in
  let package_dir = Path.(workspace_root / Path.v "packages" / Path.v "macro-runner") in
  let src_dir = Path.(package_dir / Path.v "src") in
  let package_name = "macro-runner" in
  let binary_name = "macro-runner" in
  {
    provider_hash = hash;
    generated_dir;
    workspace_root;
    workspace_toml_path = Path.(workspace_root / Path.v "riot.toml");
    toolchain_toml_path = Path.(workspace_root / Path.v "ocaml-toolchain.toml");
    build_dir_root;
    package_dir;
    package_toml_path = Path.(package_dir / Path.v "riot.toml");
    src_dir;
    library_path = Path.(src_dir / Path.v "macro_runner.ml");
    main_path = Path.(src_dir / Path.v "main.ml");
    binary_path =
      Path.(
        build_dir_root
        / Path.v "debug"
        / Path.v (Riot_model.Riot_dirs.host_target ())
        / Path.v "out"
        / Path.v (package_name ^ "/" ^ binary_name)
      );
    package_name;
    binary_name;
    dependencies;
    providers = generated_providers;
  }

let workspace_root = fun plan -> plan.workspace_root

let embedded_provider_module_source = fun (provider: generated_provider) ->
  let source_path = provider_source_path provider.provider in
  let source =
    Fs.read source_path
    |> Result.expect
      ~msg:("failed to read macro provider source " ^ Path.to_string source_path)
  in
  String.concat
    "\n"
    [
      "module " ^ provider.module_name ^ " = struct";
      String.concat
        "\n"
        (List.map
          (fun (module_name, source_path) ->
            let support_source =
              Fs.read source_path
              |> Result.expect
                ~msg:("failed to read macro support source " ^ Path.to_string source_path)
            in
            String.concat "\n" [ "module " ^ module_name ^ " = struct"; support_source; "end"; "" ])
          provider.support_module_sources);
      source;
      "end";
      "";
    ]

let provider_line = fun (provider: generated_provider) ->
  "    " ^ provider.module_name ^ ".provider ();"

let workspace_toml_source = fun plan ->
  let members =
    [ "packages/" ^ plan.package_name ]
    @ List.map (fun (dep: dependency) -> "packages/" ^ dep.name) plan.dependencies
    |> List.sort_uniq String.compare
  in
  String.concat
    "\n"
    [
      "[workspace]";
      "members = ["
      ^ String.concat ", " (List.map (fun member -> "\"" ^ member ^ "\"") members)
      ^ "]";
      "";
      "[riot]";
      "target_dir = \"" ^ Path.to_string plan.build_dir_root ^ "\"";
      "";
    ]

let package_toml_source = fun plan ->
  let dependency_lines =
    plan.dependencies
    |> List.map
      (fun (dep: dependency) -> dep.name ^ " = { path = \"../" ^ dep.name ^ "\", version = \"*\" }")
  in
  String.concat
    "\n"
    [
      "[package]";
      "name = \"" ^ plan.package_name ^ "\"";
      "version = \"0.1.0\"";
      "";
      "[lib]";
      "path = \"src/macro_runner.ml\"";
      "";
      "[[bin]]";
      "name = \"" ^ plan.binary_name ^ "\"";
      "path = \"src/main.ml\"";
      "";
      "[dependencies]";
      String.concat "\n" dependency_lines;
      "";
    ]

let library_source = fun plan ->
  String.concat
    "\n"
    [
      "open Std";
      "";
      String.concat "\n" (List.map embedded_provider_module_source plan.providers);
      "let providers () =";
      "  [";
      String.concat "\n" (List.map provider_line plan.providers);
      "  ]";
      "";
      "let expand_file ~input_path ~output_path =";
      "  match Fs.read input_path with";
      "  | Error err -> Error (\"failed to read macro input: \" ^ IO.error_message err)";
      "  | Ok source ->";
      "      match Macro.expand_source ~providers:(providers ()) ~filename:input_path source with";
      "      | Error err -> Error (Macro.error_message err)";
      "      | Ok { Macro.source = expanded; _ } ->";
      "          match Fs.write expanded output_path with";
      "          | Ok () -> Ok ()";
      "          | Error err -> Error (\"failed to write macro output: \" ^ IO.error_message err)";
      "";
      "let main ~args =";
      "  match args with";
      "  | _program :: \"expand\" :: input_path :: output_path :: [] ->";
      "      (match expand_file ~input_path:(Path.v input_path) ~output_path:(Path.v output_path) with";
      "      | Ok () -> Ok ()";
      "      | Error err -> Error (Failure err))";
      "  | _ -> Error (Failure \"usage: macro-runner expand <input> <output>\")";
      "";
    ]

let main_source =
  String.concat
    "\n"
    [
      "open Std";
      "";
      "let () =";
      "  Actors.run ~main:Macro_runner.main ~args:Env.args ()";
      "";
    ]

let local_toolchain_source = fun workspace_root ->
  let direct_config = Path.(workspace_root / Path.v "ocaml-toolchain.toml") in
  let local_compiler = Path.(workspace_root / Path.v "vendor" / Path.v "ocaml" / Path.v "compiler") in
  match Fs.exists direct_config with
  | Ok true -> Some (`Copy direct_config)
  | _ -> (
      match Fs.is_dir local_compiler with
      | Ok true -> Some (`Generate local_compiler)
      | _ -> None
    )

let toolchain_toml_source = fun compiler_path ->
  String.concat
    "\n"
    [ "[toolchain]"; "version = { path = \"" ^ Path.to_string compiler_path ^ "\" }"; ""; ]

let write_file = fun path content ->
  Fs.write content path |> Result.expect ~msg:("failed to write " ^ Path.to_string path)

let remove_dir_if_exists = fun path ->
  match Fs.exists path with
  | Ok true ->
      Fs.remove_dir_all path
      |> Result.expect ~msg:("failed to clean generated macro runner dir " ^ Path.to_string path)
  | _ -> ()

let ensure_directories = fun plan ->
  List.iter
    (fun path ->
      Fs.create_dir_all path
      |> Result.expect ~msg:("failed to create generated macro runner dir " ^ Path.to_string path))
    [ plan.workspace_root; plan.package_dir; plan.src_dir ]

let rec copy_directory = fun ~src ~dst ->
  Fs.create_dir_all dst
  |> Result.expect ~msg:("failed to create copied macro dependency dir " ^ Path.to_string dst);
  match Fs.read_dir src with
  | Error err ->
      panic ("failed to read macro dependency dir "
      ^ Path.to_string src
      ^ ": "
      ^ IO.error_message err)
  | Ok iter ->
      Std.Iter.MutIterator.to_list iter
      |> List.iter
        (fun entry ->
          let src_path = Path.(src / entry) in
          let dst_path = Path.(dst / entry) in
          match Fs.is_dir src_path with
          | Ok true -> copy_directory ~src:src_path ~dst:dst_path
          | _ ->
              Fs.copy ~src:src_path ~dst:dst_path
              |> Result.expect
                ~msg:("failed to copy macro dependency file " ^ Path.to_string src_path))

let materialize_dependency_packages = fun plan ->
  List.iter
    (fun (dep: dependency) ->
      let destination = Path.(plan.workspace_root / Path.v "packages" / Path.v dep.name) in
      copy_directory ~src:dep.path ~dst:destination)
    plan.dependencies

let materialize_toolchain = fun workspace_root plan ->
  match local_toolchain_source workspace_root with
  | Some (`Copy source_path) ->
      Fs.copy ~src:source_path ~dst:plan.toolchain_toml_path
      |> Result.expect
        ~msg:("failed to copy " ^ Path.to_string source_path ^ " into macro runner workspace")
  | Some (`Generate compiler_path) ->
      write_file plan.toolchain_toml_path (toolchain_toml_source compiler_path)
  | None -> ()

let binary_path = fun plan -> plan.binary_path

let materialize = fun ~workspace_root ~target_dir_root providers ->
  let plan = plan ~workspace_root ~target_dir_root providers in
  trace ("materializing generated runner at " ^ Path.to_string plan.generated_dir);
  remove_dir_if_exists plan.workspace_root;
  ensure_directories plan;
  materialize_dependency_packages plan;
  write_file plan.workspace_toml_path (workspace_toml_source plan);
  materialize_toolchain workspace_root plan;
  write_file plan.package_toml_path (package_toml_source plan);
  write_file plan.library_path (library_source plan);
  write_file plan.main_path main_source;
  plan

let ensure_built = fun plan ->
  trace ("building generated runner package " ^ plan.package_name);
  let shell_command =
    "cd "
    ^ shell_quote (Path.to_string plan.workspace_root)
    ^ " && riot build "
    ^ shell_quote plan.package_name
  in
  let command =
    Command.make
      "/bin/sh"
      ~args:[ "-lc"; shell_command ]
  in
  trace ("generated runner build command: " ^ shell_command);
  match Command.status command with
  | Ok status when Int.equal status 0 ->
      trace ("generated runner build finished: " ^ Path.to_string plan.binary_path);
      Ok ()
  | Ok status ->
      Error ("failed to build macro runner: exited with status " ^ Int.to_string status)
  | Error (Command.SystemError error) -> Error ("failed to build macro runner: " ^ error)

let run_file = fun ~workspace_root ~target_dir_root providers ~input_path ~output_path ->
  trace
    ("expanding "
    ^ Path.to_string input_path
    ^ " with providers ["
    ^ String.concat ", " (List.map (fun (provider: Riot_model.Macro_provider.t) -> provider.package_name) providers)
    ^ "]");
  let plan = materialize ~workspace_root ~target_dir_root providers in
  match ensure_built plan with
  | Error _ as err -> err
  | Ok () -> (
      trace ("running generated runner " ^ Path.to_string plan.binary_path);
      let command =
        Command.make
          (Path.to_string plan.binary_path)
          ~cwd:(Path.to_string plan.workspace_root)
          ~args:
            [
              "expand";
              Path.to_string input_path;
              Path.to_string output_path;
            ]
      in
      match Command.output command with
      | Ok output when Int.equal output.Command.status 0 ->
          trace ("macro expansion finished for " ^ Path.to_string input_path);
          Ok ()
      | Ok output ->
          let details =
            match String.trim output.Command.stderr with
            | "" -> output.Command.stdout
            | stderr -> stderr
          in
          Error ("macro expansion runner failed: " ^ details)
      | Error (Command.SystemError error) -> Error ("failed to execute macro runner: " ^ error)
    )
