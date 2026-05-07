open Std
open Std.Result.Syntax

(** Build the static CLI. Workspace commands are resolved lazily after parse. *)
let build_cli = fun () ->
  let open ArgParser in
  let open ArgParser.Arg in
  let builtin_commands = [
    Add.command;
    Build.command;
    Remove.command;
    Clean.command;
    Completions.command;
    Fix_cmd.command;
    Fuzz_cmd.command;
    Riot_fmt.command;
    Info_cmd.command;
    Riot_init.command;
    Install.command;
    Login.command;
    Logout.command;
    Lsp_cmd.command;
    New.command;
    Plan.command;
    Publish.command;
    Run.command;
    Trace_cmd.command;
    Search.command;
    Snapshots.command;
    Test_cmd.command;
    Bench_cmd.command;
    Toolchain_cmd.command;
    Upgrade.command;
    Update_cmd.command;
    Yank.command;
    Doc.command;
    command "version"
    |> about "Show riot version";
  ]
  in
  command "riot"
  |> version (Version_info.version_string ())
  |> about "OCaml build system and package manager"
  |> args
    [
      flag "verbose"
      |> short 'v'
      |> long "verbose"
      |> help "Enable verbose output"
      |> count;
    ]
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

let path_error_message = fun __tmp1 ->
  match __tmp1 with
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } ->
      "system call '" ^ syscall ^ "' returned invalid UTF-8 path: " ^ path
  | Path.SystemError error -> error

type workspace_scan =
  | NoWorkspace
  | ScanFailed of Info_cmd.workspace_scan_error
  | Loaded of Riot_model.Workspace_manifest.t * Riot_model.Workspace_manager.load_error list

let cli_trace_origin = ref (Time.Instant.now ())

let reset_cli_trace_origin = fun () -> cli_trace_origin := Time.Instant.now ()

let cli_elapsed_us = fun () ->
  Time.Instant.elapsed !cli_trace_origin
  |> Time.Duration.to_micros

let cli_trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_CLI_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace_cli = fun message ->
  if cli_trace_enabled () then
    eprintln
      ("riot-cli +" ^ Int.to_string (cli_elapsed_us ()) ^ "us " ^ message)

let normalize_args = fun __tmp1 ->
  match __tmp1 with
  | executable :: "docs" :: rest -> executable :: "doc" :: rest
  | executable :: "toolchains" :: rest -> executable :: "toolchain" :: rest
  | executable :: "help" :: [] -> [ executable; "--help" ]
  | args -> args

(** Get workspace scan status *)
let scan_workspace = fun () ->
  match Env.current_dir () with
  | Error err -> ScanFailed (Info_cmd.CurrentDirReadFailed err)
  | Ok cwd ->
      let workspace_manager = Riot_model.Workspace_manager.create () in
      (
        match Riot_model.Workspace_manager.scan workspace_manager cwd with
        | Ok (workspace, load_errors) -> Loaded (workspace, load_errors)
        | Error Riot_model.Workspace_manager.NoWorkspaceRootFound -> NoWorkspace
        | Error err -> ScanFailed (Info_cmd.WorkspaceScanFailed err)
      )

let report_workspace_load_errors = fun load_errors ->
  List.for_each
    load_errors
    ~fn:(fun err ->
      eprintln
        ("\027[1;31mError\027[0m: " ^ Riot_model.Workspace_manager.load_error_to_string err))

let require_clean_workspace = fun workspace_scan_opt ->
  match workspace_scan_opt with
  | NoWorkspace ->
      eprintln "❌ Not in a riot workspace";
      Error (Failure "Not in a riot workspace")
  | ScanFailed err ->
      eprintln ("\027[1;31mError\027[0m: " ^ Info_cmd.workspace_scan_error_message err);
      Error (Failure "Workspace scan failed")
  | Loaded (_workspace, load_errors) when List.length load_errors > 0 ->
      report_workspace_load_errors load_errors;
      Error (Failure "Workspace load failed")
  | Loaded (workspace, _) -> Ok workspace

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

let ensure_workspace = fun ?overrides (workspace: Riot_model.Workspace_manifest.t) ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let* registry =
    Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:"pkgs.ml" ()
    |> Result.map_err ~fn:(fun err -> Failure (Pkgs_ml.Registry_cache.create_error_message err))
  in
  Riot_deps.ensure_workspace
    ?overrides
    ~workspace_manager
    ~mode:Riot_deps.Dep_solver.Refresh
    ~registry
    ~workspace
    ()
  |> Result.map_err ~fn:(fun err -> Failure (Riot_model.Pm_error.message err))

let workspace_overrides_of_build_matches = fun matches ->
  match ArgParser.get_one matches "target-dir" with
  | None -> None
  | Some target_dir -> Some (Riot_model.Workspace.with_target_dir (Path.v target_dir))

let workspace_load_error_message = fun load_errors ->
  String.concat
    "\n"
    (List.map load_errors ~fn:Riot_model.Workspace_manager.load_error_to_string)

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
      match String.split cmd_name ~by:":" with
      | [ package_name; command_name ] -> (
          match Riot_model.Package_name.from_string package_name with
          | Error _ -> None
          | Ok package_name -> (
              (* Find the command in the specified package *)
              let commands = Riot_model.Workspace_manifest.discover_commands workspace in
              match List.find
                commands
                ~fn:(fun (cmd: Riot_model.Package_command.t) ->
                  Riot_model.Package_name.equal cmd.package_name package_name
                  && cmd.name = command_name) with
              | None -> None
              | Some cmd ->
                  Log.info
                    ("Found command: "
                    ^ Riot_model.Package_name.to_string cmd.package_name
                    ^ ":"
                    ^ cmd.name);
                  Log.info ("Command binary path: " ^ Path.to_string cmd.command_binary);
                  (* Build the package first to ensure command is up to date *)
                  Log.info
                    ("Building package: " ^ Riot_model.Package_name.to_string cmd.package_name);
                  (
                    match ensure_workspace workspace with
                    | Error err ->
                        Log.error
                          ("Failed to ensure workspace for build: " ^ Exception.to_string err);
                        Some (Error err)
                    | Ok workspace -> (
                        match Build.build_command ~workspace (Some cmd.package_name) None with
                        | Error err ->
                            Log.error ("Failed to build package: " ^ Exception.to_string err);
                            Some (Error err)
                        | Ok () ->
                            (* Execute the command binary *)
                            match Command_executor.execute
                              ~command_binary:cmd.command_binary
                              ~args:remaining_args with
                            | Ok () -> Some (Ok ())
                            | Error err ->
                                Log.error ("Command execution failed: " ^ Exception.to_string err);
                                Some (Error err)
                      )
                  )
            )
        )
      | _ -> None
    )

let ensure_toolchain = fun (workspace: Riot_model.Workspace_manifest.t) ->
  (* Check toolchain before starting server to provide better error messages *)
  let toolchain_config =
    Riot_model.Toolchain_config.from_root ~root:workspace.Riot_model.Workspace_manifest.root
  in
  match Riot_toolchain.init ~config:toolchain_config with
  | Ok _ -> Ok ()
  | Error msg ->
      eprintln "\n❌ ERROR: Toolchain initialization failed!\n";
      eprintln msg;
      eprintln "";
      Error (Failure "Toolchain not available")

let initialize_runtime = fun () ->
  (* Load config BEFORE starting logger - handlers need config *)
  Std.Config.load_string {|
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
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> false
    | arg :: rest when String.length arg > 0 && String.get_unchecked arg ~at:0 = '-' -> loop rest
    | "lsp" :: _ -> true
    | _ :: _ -> false
  in
  match args with
  | _program :: rest -> loop rest
  | [] -> false

let render_init_event = fun __tmp1 ->
  match __tmp1 with
  | Riot_init.WorkspaceInitializationStarted { name; target_dir } ->
      println "";
      println ("Creating workspace '" ^ name ^ "' in '" ^ Path.to_string target_dir ^ "'");
      println ""
  | Riot_init.ScaffoldCreated { path } -> println ("✓ Created " ^ path)
  | Riot_init.WorkspaceInitializationCompleted { next_steps; package_hints } ->
      println "";
      println "✓ Workspace initialized successfully!";
      println "";
      println "Next steps:";
      List.for_each next_steps ~fn:(fun step -> println ("  " ^ step));
      println "";
      List.enumerate package_hints
      |> List.for_each
        ~fn:(fun (idx, (kind, command)) ->
          if idx > 0 then
            println "";
          let kind_name =
            match kind with
            | Riot_init.Library -> "library"
            | Riot_init.Binary -> "binary"
          in
          println ("To add a new " ^ kind_name ^ " package run");
          println ("  " ^ command));
      println ""

let run = fun ~args ->
  let () = reset_cli_trace_origin () in
  let () = Pkgs_ml.Registry.set_riot_agent (Some (Version_info.agent_string ())) in
  let normalized_args = normalize_args args in
  let workspace_scan_cache = ref None in
  let get_workspace_scan () =
    match !workspace_scan_cache with
    | Some workspace_scan -> workspace_scan
    | None ->
        let () = trace_cli "scan-workspace-start" in
        let workspace_scan = scan_workspace () in
        let () =
          match workspace_scan with
          | Loaded (workspace, load_errors) ->
              trace_cli
                ("scan-workspace-loaded packages="
                ^ Int.to_string (List.length workspace.packages)
                ^ " load_errors="
                ^ Int.to_string (List.length load_errors))
          | NoWorkspace -> trace_cli "scan-workspace-no-workspace"
          | ScanFailed err ->
              trace_cli
                ("scan-workspace-failed reason=" ^ Info_cmd.workspace_scan_error_message err)
        in
        let _ =
          workspace_scan_cache := Some workspace_scan
        in
        let () = trace_cli "workspace-scan-cache-store" in
        workspace_scan
  in
  (* Check if first arg is a package command (format: package:command) before ArgParser *)
  match normalized_args with
  | _ :: "completions" :: "install" :: rest -> Completions.run_install_args rest
  | _ :: cmd :: rest when String.contains cmd ":" -> (
      (* This looks like a package command, try to execute it directly *)
      match try_command ?workspace_scan:(Some get_workspace_scan) cmd rest with
      | Some result -> result
      | None ->
          (* Not a valid package command, fall through to normal parsing *)
          let cli = build_cli () in
          match ArgParser.get_matches cli normalized_args with
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
      match ArgParser.get_matches cli normalized_args with
      | Error err ->
          ArgParser.print_error err;
          Error (Failure "Argument parsing failed")
      | Ok matches -> (
          let verbose = ArgParser.get_count matches "verbose" in
          set_verbosity verbose;
          match ArgParser.get_subcommand matches with
          | Some ("build", build_matches) ->
              let overrides = workspace_overrides_of_build_matches build_matches in
              let* workspace = require_clean_workspace (get_workspace_scan ()) in
              let () = trace_cli "ensure-toolchain-start" in
              let* () = ensure_toolchain workspace in
              let () = trace_cli "ensure-toolchain-done" in
              let () = trace_cli "build-prepare-start" in
              let* workspace = ensure_workspace ?overrides workspace in
              let () = trace_cli "build-prepare-done" in
              let () = trace_cli "build-run-start" in
              Build.run ~workspace build_matches
          | Some ("plan", plan_matches) ->
              let overrides = workspace_overrides_of_build_matches plan_matches in
              let* workspace = require_clean_workspace (get_workspace_scan ()) in
              let () = trace_cli "plan-prepare-start" in
              let* workspace = ensure_workspace ?overrides workspace in
              let () = trace_cli "plan-prepare-done" in
              Plan.run ~workspace plan_matches
          | Some ("run", run_matches) -> (
              let workspace_scan = get_workspace_scan () in
              let (workspace, workspace_error) =
                match workspace_scan with
                | Loaded (workspace, load_errors) when List.is_empty load_errors -> (
                    match ensure_workspace workspace with
                    | Ok workspace -> (Some workspace, None)
                    | Error err -> (None, Some (Exception.to_string err))
                  )
                | Loaded (workspace, load_errors) -> (
                    let _ = workspace in
                    (None, Some (workspace_load_error_message load_errors))
                  )
                | NoWorkspace -> (None, None)
                | ScanFailed err -> (None, Some (Info_cmd.workspace_scan_error_message err))
              in
              Run.run_with_workspace_info ~workspace ~workspace_error run_matches
            )
          | Some ("trace", trace_matches) -> (
              if Trace_cmd.is_summary trace_matches then
                Trace_cmd.run_with_workspace_info
                  ~workspace:None
                  ~workspace_error:None
                  trace_matches
              else
                let workspace_scan = get_workspace_scan () in
                let (workspace, workspace_error) =
                  match workspace_scan with
                  | Loaded (workspace, load_errors) when List.is_empty load_errors -> (
                      match ensure_workspace workspace with
                      | Ok workspace -> (Some workspace, None)
                      | Error err -> (None, Some (Exception.to_string err))
                    )
                  | Loaded (workspace, load_errors) -> (
                      let _ = workspace in
                      (None, Some (workspace_load_error_message load_errors))
                    )
                  | NoWorkspace -> (None, None)
                  | ScanFailed err -> (None, Some (Info_cmd.workspace_scan_error_message err))
                in
                Trace_cmd.run_with_workspace_info ~workspace ~workspace_error trace_matches
            )
          | Some ("search", search_matches) -> Search.run search_matches
          | Some ("snapshots", snapshots_matches) ->
              let* workspace = require_clean_workspace (get_workspace_scan ()) in
              let* workspace = ensure_workspace workspace in
              Snapshots.run ~workspace snapshots_matches
          | Some ("test", test_matches) ->
              let* workspace = require_clean_workspace (get_workspace_scan ()) in
              let* () = ensure_toolchain workspace in
              let* workspace = ensure_workspace workspace in
              Test_cmd.run ~workspace test_matches
          | Some ("fuzz", fuzz_matches) ->
              let* workspace = require_clean_workspace (get_workspace_scan ()) in
              let* () = ensure_toolchain workspace in
              let* workspace = ensure_workspace workspace in
              Fuzz_cmd.run ~workspace fuzz_matches
          | Some ("bench", bench_matches) ->
              let* workspace = require_clean_workspace (get_workspace_scan ()) in
              let* () = ensure_toolchain workspace in
              let* workspace = ensure_workspace workspace in
              Bench_cmd.run ~workspace bench_matches
          | Some ("add", add_matches) -> (
              match get_workspace_scan () with
              | Loaded (_workspace, load_errors) when not (List.is_empty load_errors) -> (
                  match require_clean_workspace (get_workspace_scan ()) with
                  | Error _ as e -> e
                  | Ok workspace -> Add.run ~workspace add_matches
                )
              | Loaded (workspace, _) -> Add.run ~workspace add_matches
              | NoWorkspace -> (
                  match current_manifest_status () with
                  | Ok (Missing_manifest cwd) -> Add.run_without_workspace ~cwd add_matches
                  | Ok (Existing_manifest _) -> fail_not_in_workspace ()
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
              | Loaded (workspace, _) -> Remove.run ~workspace remove_matches
              | NoWorkspace -> (
                  match current_manifest_status () with
                  | Ok (Missing_manifest _) -> Remove.run_without_workspace remove_matches
                  | Ok (Existing_manifest _) -> fail_not_in_workspace ()
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
                  match get_workspace_scan () with
                  | Loaded (workspace, _load_errors) ->
                      ensure_workspace workspace
                      |> Result.to_option
                  | NoWorkspace
                  | ScanFailed _ -> None
                else
                  None
              in
              Riot_fmt.run ?workspace fmt_matches
          | Some ("info", info_matches) -> (
              let workspace_scan =
                match get_workspace_scan () with
                | NoWorkspace -> Info_cmd.NoWorkspace
                | ScanFailed err -> Info_cmd.ScanFailed err
                | Loaded (workspace, load_errors) -> Info_cmd.Loaded (workspace, load_errors)
              in
              Info_cmd.run ~workspace_scan info_matches
            )
          | Some ("clean", clean_matches) -> (
              match require_clean_workspace (get_workspace_scan ()) with
              | Error _ as e -> e
              | Ok workspace -> (
                  match ensure_workspace workspace with
                  | Ok workspace -> Clean.run ~workspace clean_matches
                  | Error _ as e -> e
                )
            )
          | Some ("doc", doc_matches) ->
              let workspace =
                match get_workspace_scan () with
                | Loaded (workspace, load_errors) when List.is_empty load_errors ->
                    ensure_workspace workspace
                    |> Result.to_option
                | _ -> None
              in
              (
                match workspace with
                | Some workspace ->
                    Doc.run ~workspace doc_matches
                    |> Result.map_err ~fn:(fun err -> Failure err)
                | None -> fail_not_in_workspace ()
              )
          | Some ("completions", completions_matches) -> Completions.run completions_matches
          | Some ("fix", fix_matches) -> Fix_cmd.run fix_matches
          | Some ("login", login_matches) -> Login.run login_matches
          | Some ("logout", logout_matches) -> Logout.run logout_matches
          | Some ("lsp", lsp_matches) -> Lsp_cmd.run lsp_matches
          | Some ("yank", yank_matches) -> Yank.run yank_matches
          | Some ("init", init_matches) -> Riot_init.run ~on_event:render_init_event init_matches
          | Some ("new", new_matches) -> New.run new_matches
          | Some ("publish", publish_matches) -> (
              match require_clean_workspace (get_workspace_scan ()) with
              | Ok workspace -> (
                  match ensure_workspace workspace with
                  | Ok workspace -> Publish.run workspace publish_matches
                  | Error _ as e -> e
                )
              | Error _ as e -> e
            )
          | Some ("install", install_matches) -> (
              let workspace_scan = get_workspace_scan () in
              let (workspace, workspace_error) =
                match workspace_scan with
                | Loaded (workspace, load_errors) when List.is_empty load_errors -> (
                    match ensure_workspace workspace with
                    | Ok workspace -> (Some workspace, None)
                    | Error err -> (None, Some (Exception.to_string err))
                  )
                | Loaded (workspace, load_errors) -> (
                    let _ = workspace in
                    (None, Some (workspace_load_error_message load_errors))
                  )
                | NoWorkspace -> (None, None)
                | ScanFailed err -> (None, Some (Info_cmd.workspace_scan_error_message err))
              in
              Install.run_with_workspace_info ~workspace ~workspace_error install_matches
            )
          | Some ("update", update_matches) -> (
              match get_workspace_scan () with
              | Loaded (_workspace, load_errors) when not (List.is_empty load_errors) -> (
                  match require_clean_workspace (get_workspace_scan ()) with
                  | Error _ as e -> e
                  | Ok workspace -> Update_cmd.run ~workspace update_matches
                )
              | Loaded (workspace, _) -> Update_cmd.run ~workspace update_matches
              | NoWorkspace -> (
                  match current_manifest_status () with
                  | Ok (Missing_manifest _) -> Update_cmd.run_without_workspace update_matches
                  | Ok (Existing_manifest _) -> fail_not_in_workspace ()
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
          | Some ("toolchain", toolchain_matches) -> Toolchain_cmd.run toolchain_matches
          | Some ("upgrade", upgrade_matches) -> Upgrade.run upgrade_matches
          | Some ("version", _) ->
              println (Version_info.version_string ());
              Ok ()
          | None -> Ok ()
          | Some (cmd, _matches) -> (
              (* Check if this is a package command *)
              (* Extract remaining args after the command name *)
              let remaining_args =
                match args with
                | _program :: cmd_arg :: rest when cmd_arg = cmd -> rest
                | _ -> []
              in
              match try_command ?workspace_scan:(Some get_workspace_scan) cmd remaining_args with
              | Some result -> result
              | None ->
                  ArgParser.print_error (ArgParser.UnknownSubcommand cmd);
                  Error (Failure ("Unknown command: " ^ cmd))
            )
        )

let main ~args =
  if not (is_lsp_invocation args) then
    initialize_runtime ();
  let result = run ~args in
  let () = trace_cli "main-return" in
  result
