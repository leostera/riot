open Std

(** Build the static CLI. Workspace commands are resolved lazily after parse. *)
let build_cli = fun () ->
  let open ArgParser in
    let open Arg in
      let builtin_commands = [
        Add.command;
        Build.command;
        Remove.command;
        Clean.command;
        Completions.command;
        Fix_cmd.command;
        Riot_fmt.command;
        Riot_init.command;
        Install.command;
        Login.command;
        Logout.command;
        New.command;
        Publish.command;
        Run.command;
        Search.command;
        Snapshots.command;
        Test_cmd.command;
        Bench_cmd.command;
        Toolchain_cmd.command;
        Upgrade.command;
        Update_cmd.command;
        command "doc" |> about "Generate documentation";
        command "lsp" |> about "Start OCaml LSP server";
        command "version" |> about "Show riot version";
      ]
      in
      command "riot"
      |> version (Version_info.version_string ())
      |> about "OCaml build system and package manager"
      |> args
        [ flag "verbose" |> short 'v' |> long "verbose" |> help "Enable verbose output" |> count; ]
      |> subcommands builtin_commands

let set_verbosity = fun verbose ->
  let verbose =
    if verbose < 0 then
      0
    else
      verbose
  in
  match verbose with
  | 0 -> Log.(set_level Error)
  | 1 -> Log.(set_level Info)
  | 2 -> Log.(set_level Debug)
  | _ -> Log.(set_level Trace)

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "system call '"
  ^ syscall
  ^ "' returned invalid UTF-8 path: "
  ^ path
  | Path.SystemError error -> error

type workspace_scan =
  | NoWorkspace
  | ScanFailed of string
  | Loaded of Riot_model.Workspace.t * Riot_model.Workspace_manager.load_error list

(** Get workspace scan status *)
let scan_workspace = fun () ->
  match Env.current_dir () with
  | Error err -> ScanFailed ("failed to read current directory: " ^ path_error_message err)
  | Ok cwd -> (
      let workspace_manager = Riot_model.Workspace_manager.create () in
      match Riot_model.Workspace_manager.find_workspace_root workspace_manager cwd with
      | None -> NoWorkspace
      | Some _ -> (
          match Riot_model.Workspace_manager.scan workspace_manager cwd with
          | Error err -> ScanFailed err
          | Ok (workspace, load_errors) -> Loaded (workspace, load_errors)
        )
    )

let report_workspace_load_errors = fun load_errors ->
  List.iter
    (fun err ->
      eprintln ("\027[1;31mError\027[0m: " ^ Riot_model.Workspace_manager.load_error_to_string err))
    load_errors

let require_clean_workspace = fun workspace_scan_opt ->
  match workspace_scan_opt with
  | NoWorkspace ->
      eprintln "❌ Not in a riot workspace";
      Error (Failure "Not in a riot workspace")
  | ScanFailed err ->
      eprintln ("\027[1;31mError\027[0m: " ^ err);
      Error (Failure "Workspace scan failed")
  | Loaded (_workspace, load_errors) when List.length load_errors > 0 ->
      report_workspace_load_errors load_errors;
      Error (Failure "Workspace load failed")
  | Loaded (workspace, _) ->
      Ok workspace

let fail_not_in_workspace = fun () ->
  eprintln "❌ Not in a riot workspace";
  Error (Failure "Not in a riot workspace")

type current_manifest_status =
  | Missing_manifest of Path.t
  | Existing_manifest of Path.t

let current_manifest_status = fun () ->
  match Env.current_dir () with
  | Error err -> Error ("failed to read current directory: " ^ path_error_message err)
  | Ok cwd ->
      let manifest_path = Path.(cwd / Path.v "riot.toml") in
      (
        match Fs.exists manifest_path with
        | Ok true -> Ok (Existing_manifest cwd)
        | Ok false -> Ok (Missing_manifest cwd)
        | Error err -> Error ("failed to read riot.toml status: " ^ IO.error_message err)
      )

(** Try to execute a package command if it exists *)
let try_command = fun ?workspace_scan cmd_name remaining_args ->
  let workspace_scan =
    match workspace_scan with
    | Some workspace_scan -> workspace_scan ()
    | None -> scan_workspace ()
  in
  match workspace_scan with
  | NoWorkspace
  | ScanFailed _ -> None
  | Loaded (workspace, _load_errors) -> (
      (* Parse package:command format *)
      match String.split_on_char ':' cmd_name with
      | [package_name;command_name] -> (
          (* Find the command in the specified package *)
          let commands = Riot_model.Workspace.discover_commands workspace in
          match List.find_opt
            (fun (cmd: Riot_model.Package_command.t) ->
              cmd.package_name = package_name && cmd.name = command_name)
            commands with
          | None -> None
          | Some cmd ->
              Log.info ("Found command: " ^ cmd.package_name ^ ":" ^ cmd.name);
              Log.info ("Command binary path: " ^ Path.to_string cmd.command_binary);
              (* Build the package first to ensure command is up to date *)
              Log.info ("Building package: " ^ cmd.package_name);
              (
                match Build.build_command ~workspace (Some cmd.package_name) None with
                | Error err ->
                    Log.error ("Failed to build package: " ^ Exception.to_string err);
                    Some (Error err)
                | Ok () ->
                    (* Execute the command binary *)
                    match Command_executor.execute ~command_binary:cmd.command_binary ~args:remaining_args with
                    | Ok () -> Some (Ok ())
                    | Error err ->
                        Log.error ("Command execution failed: " ^ Exception.to_string err);
                        Some (Error err)
              )
        )
      | _ -> None
    )

let ensure_toolchain = fun workspace ->
  (* Check toolchain before starting server to provide better error messages *)
  let toolchain_config = Riot_model.Toolchain_config.from_workspace workspace in
  match Riot_toolchain.init ~config:toolchain_config with
  | Ok _ -> Ok ()
  | Error msg ->
      eprintln "\n❌ ERROR: Toolchain initialization failed!\n";
      eprintln msg;
      eprintln "";
      Error (Failure "Toolchain not available")

let initialize_runtime = fun () ->
  (* Load config BEFORE starting logger - handlers need config *)
  Std.Config.load_string
    {|
[[log.handler]]
type = "stdout"
format = "full"
|};
  Std.Log.set_level Info;
  (* Now start logger and telemetry *)
  let _ = Std.Log.start_link () in
  let _ = Std.Telemetry.start () in
  ()

let is_lsp_invocation = fun args ->
  let rec loop = function
    | [] -> false
    | arg :: rest when String.length arg > 0 && arg.[0] = '-' -> loop rest
    | "lsp" :: _ -> true
    | _ :: _ -> false
  in
  match args with
  | _program :: rest -> loop rest
  | [] -> false

let run = fun ~args ->
  let workspace_scan_cache = ref None in
  let get_workspace_scan () =
    match !workspace_scan_cache with
    | Some workspace_scan -> workspace_scan
    | None ->
        let workspace_scan = scan_workspace () in
        let _ =
          workspace_scan_cache := Some workspace_scan
        in
        workspace_scan
  in
  let workspace_opt () =
    match get_workspace_scan () with
    | Loaded (workspace, _) -> Some workspace
    | NoWorkspace
    | ScanFailed _ -> None
  in
  (* Check if first arg is a package command (format: package:command) before ArgParser *)
  match args with
  | _ :: "completions" :: "install" :: rest ->
      Completions.run_install_args rest
  | _ :: cmd :: rest when String.contains cmd ":" -> (
      (* This looks like a package command, try to execute it directly *)
      match try_command ?workspace_scan:(Some get_workspace_scan) cmd rest with
      | Some result -> result
      | None ->
          (* Not a valid package command, fall through to normal parsing *)
          let cli = build_cli () in
          match ArgParser.get_matches cli args with
          | Error err ->
              ArgParser.print_error err;
              Error (Failure "Argument parsing failed")
          | Ok _ ->
              ArgParser.print_error (ArgParser.UnknownSubcommand cmd);
              Error (Failure ("Unknown command: " ^ cmd))
    )
  | _ ->
      (* Normal command parsing *)
      let cli = build_cli () in
      match ArgParser.get_matches cli args with
      | Error err ->
          ArgParser.print_error err;
          Error (Failure "Argument parsing failed")
      | Ok matches -> (
          let verbose = ArgParser.get_count matches "verbose" in
          set_verbosity verbose;
          match ArgParser.get_subcommand matches with
          | Some ("build", build_matches) -> (
              match require_clean_workspace (get_workspace_scan ()) with
              | Ok workspace -> (
                  match ensure_toolchain workspace with
                  | Ok () -> Build.run ~workspace build_matches
                  | Error _ as e -> e
                )
              | Error _ as e -> e
            )
          | Some ("run", run_matches) -> (
              match require_clean_workspace (get_workspace_scan ()) with
              | Ok workspace -> (
                  match ensure_toolchain workspace with
                  | Ok () -> Run.run ~workspace run_matches
                  | Error _ as e -> e
                )
              | Error _ as e -> e
            )
          | Some ("search", search_matches) ->
              Search.run search_matches
          | Some ("snapshots", snapshots_matches) -> (
              match require_clean_workspace (get_workspace_scan ()) with
              | Ok workspace -> Snapshots.run ~workspace snapshots_matches
              | Error _ as e -> e
            )
          | Some ("test", test_matches) -> (
              match require_clean_workspace (get_workspace_scan ()) with
              | Ok workspace -> (
                  match ensure_toolchain workspace with
                  | Ok () -> Test_cmd.run ~workspace test_matches
                  | Error _ as e -> e
                )
              | Error _ as e -> e
            )
          | Some ("bench", bench_matches) -> (
              match require_clean_workspace (get_workspace_scan ()) with
              | Ok workspace -> (
                  match ensure_toolchain workspace with
                  | Ok () -> Bench_cmd.run ~workspace bench_matches
                  | Error _ as e -> e
                )
              | Error _ as e -> e
            )
          | Some ("add", add_matches) -> (
              match get_workspace_scan () with
              | Loaded (_workspace, load_errors) when not (List.is_empty load_errors) -> (
                  match require_clean_workspace (get_workspace_scan ()) with
                  | Error _ as e -> e
                  | Ok workspace -> Add.run ~workspace add_matches
                )
              | Loaded (workspace, _) ->
                  Add.run ~workspace add_matches
              | NoWorkspace -> (
                  match current_manifest_status () with
                  | Ok (Missing_manifest cwd) ->
                      Add.run_without_workspace ~cwd add_matches
                  | Ok (Existing_manifest _) ->
                      fail_not_in_workspace ()
                  | Error err ->
                      eprintln ("\027[1;31mError\027[0m: " ^ err);
                      Error (Failure err)
                )
              | ScanFailed _ -> (
                  match require_clean_workspace (get_workspace_scan ()) with
                  | Error _ as e -> e
                  | Ok workspace -> Add.run ~workspace add_matches
                )
            )
          | Some ("rm", remove_matches) -> (
              match get_workspace_scan () with
              | Loaded (_workspace, load_errors) when not (List.is_empty load_errors) -> (
                  match require_clean_workspace (get_workspace_scan ()) with
                  | Error _ as e -> e
                  | Ok workspace -> Remove.run ~workspace remove_matches
                )
              | Loaded (workspace, _) ->
                  Remove.run ~workspace remove_matches
              | NoWorkspace -> (
                  match current_manifest_status () with
                  | Ok (Missing_manifest _) ->
                      Remove.run_without_workspace remove_matches
                  | Ok (Existing_manifest _) ->
                      fail_not_in_workspace ()
                  | Error err ->
                      eprintln ("\027[1;31mError\027[0m: " ^ err);
                      Error (Failure err)
                )
              | ScanFailed _ -> (
                  match require_clean_workspace (get_workspace_scan ()) with
                  | Error _ as e -> e
                  | Ok workspace -> Remove.run ~workspace remove_matches
                )
            )
          | Some ("fmt", fmt_matches) ->
              let explicit_paths = ArgParser.get_many fmt_matches "path" in
              let workspace =
                if List.is_empty explicit_paths then
                  workspace_opt ()
                else
                  None
              in
              Riot_fmt.run ?workspace fmt_matches
          | Some ("clean", clean_matches) ->
              Clean.run clean_matches
          | Some ("completions", completions_matches) ->
              Completions.run completions_matches
          | Some ("fix", fix_matches) ->
              Fix_cmd.run fix_matches
          | Some ("login", login_matches) ->
              Login.run login_matches
          | Some ("logout", logout_matches) ->
              Logout.run logout_matches
          | Some ("init", init_matches) ->
              Riot_init.run init_matches
          | Some ("new", new_matches) ->
              New.run new_matches
          | Some ("publish", publish_matches) -> (
              match require_clean_workspace (get_workspace_scan ()) with
              | Ok workspace -> Publish.run workspace publish_matches
              | Error _ as e -> e
            )
          | Some ("install", install_matches) -> (
              match require_clean_workspace (get_workspace_scan ()) with
              | Ok workspace -> Install.run ~workspace install_matches
              | Error _ as e -> e
            )
          | Some ("update", update_matches) -> (
              match get_workspace_scan () with
              | Loaded (_workspace, load_errors) when not (List.is_empty load_errors) -> (
                  match require_clean_workspace (get_workspace_scan ()) with
                  | Error _ as e -> e
                  | Ok workspace -> Update_cmd.run ~workspace update_matches
                )
              | Loaded (workspace, _) ->
                  Update_cmd.run ~workspace update_matches
              | NoWorkspace -> (
                  match current_manifest_status () with
                  | Ok (Missing_manifest _) ->
                      Update_cmd.run_without_workspace update_matches
                  | Ok (Existing_manifest _) ->
                      fail_not_in_workspace ()
                  | Error err ->
                      eprintln ("\027[1;31mError\027[0m: " ^ err);
                      Error (Failure err)
                )
              | ScanFailed _ -> (
                  match require_clean_workspace (get_workspace_scan ()) with
                  | Error _ as e -> e
                  | Ok workspace -> Update_cmd.run ~workspace update_matches
                )
            )
          | Some ("toolchain", toolchain_matches) ->
              Toolchain_cmd.run toolchain_matches
          | Some ("upgrade", upgrade_matches) ->
              Upgrade.run upgrade_matches
          | Some ("lsp", _) ->
              Riot_lsp.run ()
          | Some ("version", _) ->
              println (Version_info.version_string ());
              Ok ()
          | None ->
              Ok ()
          | Some (cmd, _matches) -> (
              (* Check if this is a package command *)
              (* Extract remaining args after the command name *)
              let remaining_args =
                match List.tl args with
                | cmd_arg :: rest when cmd_arg = cmd -> rest
                | _ -> []
              in
              match try_command ?workspace_scan:(Some get_workspace_scan) cmd remaining_args with
              | Some result -> result
              | None ->
                  ArgParser.print_error (ArgParser.UnknownSubcommand cmd);
                  Error (Failure ("Unknown command: " ^ cmd))
            )
        )

let main = fun ~args ->
  if not (is_lsp_invocation args) then
    initialize_runtime ();
  run ~args
