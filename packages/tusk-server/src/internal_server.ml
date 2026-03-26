(** Internal server - Main server loop for handling requests *)

open Std

open Tusk_model

type server_state = {
  workspace : Workspace.t;
  toolchain : Tusk_toolchain.t;
  store : Tusk_store.Store.t;
  concurrency : int;
  package_graph : Tusk_planner.Package_graph.t;
  load_errors : Workspace_manager.load_error list;
}

let build_state ~(workspace : Workspace.t) ~config =
  let _ = config in
  let (workspace, load_errors) =
    Workspace_manager.scan workspace.root
    |> Result.expect ~msg:"tusk_server: workspace scan failed"
  in

  if List.length load_errors > 0 then (
    Log.warn
      ("Workspace loaded with " ^ Int.to_string (List.length load_errors)
      ^ " package load errors:");
    List.iter
      (fun err -> Log.warn ("  " ^ Workspace_manager.load_error_to_string err))
      load_errors);

  let toolchain_config = Toolchain_config.from_workspace workspace in
  let toolchain =
    match Tusk_toolchain.init ~config:toolchain_config with
    | Ok t -> t
    | Error msg ->
        println "\n❌ ERROR: Toolchain initialization failed!\n";
        println msg;
        println "";
        exit 1
  in

  let store = Tusk_store.Store.create ~workspace in

  let package_graph =
    match Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime workspace with
    | Ok graph -> graph
    | Error (Tusk_planner.Package_graph.MissingPackages { missing }) ->
        Log.warn "Package graph has missing dependencies at startup:";
        List.iter
          (fun { Tusk_planner.Package_graph.package; dependency } ->
            Log.warn ("  " ^ package ^ " requires: " ^ dependency))
          missing;
        Log.warn "Build operations will report this error to clients.";
        let ws = Workspace.make ~root:workspace.root ~packages:[] () in
        Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime ws
        |> Result.expect ~msg:"Failed to create empty package graph"
  in

  {
    workspace;
    toolchain;
    store;
    concurrency = System.available_parallelism;
    package_graph;
    load_errors;
  }

(** Main server loop - handle all incoming requests *)
let rec loop state =
  let selector msg =
    match msg with
    | Protocol.ServerRequest req -> `select (`Request req)
    | Protocol.UpdatePackageGraph pg -> `select (`UpdateGraph pg)
    | _ -> `skip
  in

  Log.info "[INTERNAL_SERVER] Server loop ready, awaiting next request...";
  match receive ~selector () with
  | `UpdateGraph package_graph ->
      Log.info "[INTERNAL_SERVER] Received updated package graph from build worker";
      loop { state with package_graph; load_errors = [] }
  | `Request (Protocol.Ping { client_pid }) ->
      Log.debug "Server loop received: Ping";
      handle_ping state client_pid
  | `Request (Protocol.Build { client_pid; target; scope; target_arch; session_id }) ->
      Log.debug "Server loop received: Build";
      handle_build state client_pid target scope target_arch session_id
  | `Request (Protocol.ScanWorkspace { client_pid; current_dir }) ->
      Log.debug "Server loop received: ScanWorkspace";
      handle_scan_workspace state client_pid current_dir
  | `Request (Protocol.GetWorkspaceConfig { client_pid }) ->
      Log.debug "Server loop received: GetWorkspaceConfig";
      handle_get_workspace_config state client_pid
  | `Request (Protocol.GetPackageInfo { client_pid; package_name }) ->
      Log.debug "Server loop received: GetPackageInfo";
      handle_get_package_info state client_pid package_name
  | `Request (Protocol.GetPackageGraph { client_pid }) ->
      Log.debug "Server loop received: GetPackageGraph";
      handle_get_package_graph state client_pid
  | `Request (Protocol.FindExecutable { client_pid; name }) ->
      Log.debug ("Server loop received: FindExecutable(" ^ name ^ ")");
      handle_find_executable state client_pid name
  | `Request (Protocol.FindArtifact { client_pid; package; kind; name }) ->
      Log.debug
        ("Server loop received: FindArtifact(package=" ^ package ^ ", kind="
        ^ kind ^ ", name=" ^ name ^ ")");
      handle_find_artifact state client_pid package kind name
  | `Request (Protocol.FormatFile { client_pid; file_path; check_only }) ->
      Log.debug "Server loop received: FormatFile";
      handle_format_file state client_pid file_path check_only
  | `Request (Protocol.FormatCode { client_pid; code; file_path }) ->
      Log.debug "Server loop received: FormatCode";
      handle_format_code state client_pid code file_path
  | `Request (Protocol.FormatAll { client_pid; mode }) ->
      Log.debug "Server loop received: FormatAll";
      handle_format_all state client_pid mode
  | `Request (Protocol.NewPackage { client_pid; path; name; is_library }) ->
      Log.debug "Server loop received: NewPackage";
      handle_new_package state client_pid path name is_library

(** Handler for ping message *)
and handle_ping state client_pid =
  Log.debug
    ("handle_ping: Received Ping from " ^ Pid.to_string client_pid);
  send client_pid (Protocol.ServerResponse Protocol.Pong);
  Log.debug "handle_ping: Pong sent, continuing loop";
  loop state

(** Handler for scan workspace message *)
and handle_scan_workspace state client_pid current_dir =
  let (workspace, load_errors) =
    Workspace_manager.scan current_dir
    |> Result.expect ~msg:"tusk_server: workspace scan failed"
  in
  let package_graph = 
    match Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime workspace with
    | Ok graph -> graph
    | Error _ -> 
        (* Create empty graph as fallback *)
        let ws = Workspace.make ~root:workspace.root ~packages:[] () in
        Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime ws |> Result.expect ~msg:"Failed to create empty package graph"
  in
  let new_state = { state with workspace; package_graph; load_errors } in
  send client_pid
    (Protocol.ServerResponse
       (Protocol.BuildCompleted
          {
            session_id = Session_id.make ();
            completed_at = Datetime.now ();
            stats = Protocol.BuildStats.make ();
            results = [];
          }));
  loop new_state

(** Handler for getting workspace configuration *)
and handle_get_workspace_config state client_pid =
  Log.debug
    ("Server: Received GetWorkspaceConfig from " ^ Pid.to_string client_pid);
  send client_pid
    (Protocol.ServerResponse
       (Protocol.WorkspaceConfig
          { workspace = state.workspace; toolchain = state.toolchain }));
  loop state

(** Handler for getting package information *)
and handle_get_package_info state client_pid package_name =
  Log.debug
    ("Server: Received GetPackageInfo for " ^ package_name ^ " from "
    ^ Pid.to_string client_pid);

  let package_opt =
    List.find_opt
      (fun (pkg : Package.t) -> pkg.name = package_name)
      state.workspace.packages
  in

  (match package_opt with
  | None ->
      Log.debug ("Server: Package " ^ package_name ^ " not found");
      send client_pid
        (Protocol.ServerResponse
           (Protocol.PackageInfo
              {
                package =
                  {
                    name = package_name;
                    path = Path.of_string "" |> Result.expect ~msg:"Failed to create empty path";
                    relative_path = Path.of_string "" |> Result.expect ~msg:"Failed to create empty relative path";
                    dependencies = [];
                    dev_dependencies = [];
                    build_dependencies = [];
                    foreign_dependencies = [];
                    binaries = [];
                    library = None;
                    sources = { src = []; native = []; tests = []; examples = []; bench = [] };
                    compiler = { profile_overrides = []; target_overrides = [] };
                    commands = [];
                    fix_providers = [];
                  };
                sources = [];
                dependencies = [];
              }))
  | Some package ->
      let dep_nodes =
        Tusk_planner.Package_graph.get_dependencies state.package_graph package
      in
      let dependencies =
        List.map Tusk_planner.Package_graph.get_package dep_nodes
      in
      let all_sources =
        List.concat
          [ package.sources.src; package.sources.native; package.sources.tests ]
      in
      send client_pid
        (Protocol.ServerResponse
           (Protocol.PackageInfo
              { package; sources = all_sources; dependencies })));
  loop state

(** Handler for getting the package graph *)
and handle_get_package_graph state client_pid =
  Log.debug
    ("Server: Received GetPackageGraph from " ^ Pid.to_string client_pid);
  let sorted_packages =
    Tusk_planner.Package_graph.(
      topological_sort state.package_graph |> List.map get_package)
  in
  send client_pid
    (Protocol.ServerResponse (Protocol.PackageGraph { nodes = sorted_packages }));
  loop state

and handle_find_executable state client_pid name =
  Log.debug ("Server: handle_find_executable " ^ name);
  (* Only search in workspace member packages, not external dependencies *)
  let workspace_packages = 
    List.filter Package.is_workspace_member state.workspace.packages 
  in
  let found =
    List.find_map
      (fun (pkg : Package.t) ->
        List.find_opt
          (fun (bin : Package.binary) -> bin.name = name)
          pkg.binaries
        |> Option.map (fun _ -> pkg))
      workspace_packages
  in
  (match found with
  | Some pkg ->
      send client_pid
        (Protocol.ServerResponse
           (Protocol.ExecutableFound { package = pkg.name; binary = name }))
  | None ->
      send client_pid (Protocol.ServerResponse Protocol.ExecutableNotFound));
  loop state

and handle_find_artifact state client_pid package kind name =
  Log.info
    ("Server: handle_find_artifact package=" ^ package ^ " kind=" ^ kind
    ^ " name=" ^ name);

  (* Find the package in the workspace *)
  let pkg_opt =
    List.find_opt
      (fun (p : Package.t) -> p.name = package)
      state.workspace.packages
  in
  let response =
    match pkg_opt with
    | None ->
        Log.info ("Server: Package '" ^ package ^ "' not found in workspace");
        Protocol.ServerResponse
          (Protocol.ArtifactNotFound
             { error = "Package '" ^ package ^ "' not found" })
    | Some pkg ->
        (* Artifact resolution order:
           1) promoted package outputs in out/ (fast path for current command flow)
           2) package export manifest -> immutable action artifact path
           3) legacy package-hash artifact lookup fallback

           This keeps compatibility while making export manifests the primary
           package-level discovery metadata. *)
        let resolve_from_package_hash () =
          let package_node_opt =
            Tusk_planner.Package_graph.get_package_node state.package_graph pkg
          in
          match package_node_opt with
          | None ->
              Log.info
                ("Server: Package '" ^ package ^ "' not found in package graph");
              Protocol.ServerResponse
                (Protocol.ArtifactNotFound
                   { error = "Package '" ^ package ^ "' not in build graph" })
          | Some package_node -> (
              let hash_opt = Tusk_planner.Package_graph.get_hash package_node in
              match hash_opt with
              | None ->
                  Log.info
                    ("Server: Package '" ^ package ^ "' has no hash (not built)");
                  Protocol.ServerResponse
                    (Protocol.ArtifactNotFound
                       { error = "Package '" ^ package ^ "' has not been built" })
              | Some hash -> (
                  match Tusk_store.Store.get state.store hash with
                  | None ->
                      Log.info
                        ("Server: No artifact found for package '" ^ package
                        ^ "' hash " ^ Std.Crypto.Digest.hex hash);
                      Protocol.ServerResponse
                        (Protocol.ArtifactNotFound
                           { error = "Package '" ^ package ^ "' has not been built" })
                  | Some artifact ->
                      let binary_path =
                        List.find_opt
                          (fun file_path -> Path.basename file_path = name)
                          artifact.Tusk_store.Artifact.files
                      in
                      match binary_path with
                      | Some file_path ->
                          let artifact_dir =
                            Tusk_store.Store.get_artifact_dir state.store artifact
                          in
                          let full_path = Path.(artifact_dir / file_path) in
                          Log.info
                            ("Server: Artifact found at "
                            ^ Path.to_string full_path);
                          Protocol.ServerResponse
                            (Protocol.ArtifactFound { path = full_path })
                      | None ->
                          Log.info
                            ("Server: Artifact '" ^ name
                            ^ "' not found in package '" ^ package ^ "' files");
                          Protocol.ServerResponse
                            (Protocol.ArtifactNotFound
                               {
                                 error =
                                   "Artifact '" ^ name ^ "' not found in package '" ^ package ^ "'";
                               })))
        in
        let promoted_artifact_dir =
          Path.(
            Tusk_model.Tusk_dirs.out_dir ~workspace_root:state.workspace.root
            / Path.v package)
        in
        let promoted_artifact_path =
          Path.(promoted_artifact_dir / Path.v name)
        in

        match Fs.exists promoted_artifact_path with
        | Ok true ->
            Log.info
              ("Server: Found promoted artifact at "
              ^ Path.to_string promoted_artifact_path);
            Protocol.ServerResponse
              (Protocol.ArtifactFound { path = promoted_artifact_path })
        | _ ->
            let profile = "debug" in
            let target = Tusk_model.Tusk_dirs.host_target () in
            (match
               Tusk_store.Store.find_package_export_path state.store
                 ~package:pkg.name ~profile ~target ~name
             with
            | Some export_path -> (
                match Fs.exists export_path with
                | Ok true ->
                    Log.info
                      ("Server: Found export manifest artifact at "
                      ^ Path.to_string export_path);
                    Protocol.ServerResponse
                      (Protocol.ArtifactFound { path = export_path })
                | _ ->
                    Log.warn
                      ("Server: Export manifest pointed to missing path "
                      ^ Path.to_string export_path ^ ", falling back to package hash lookup");
                    resolve_from_package_hash ())
            | None -> resolve_from_package_hash ())
  in
  Log.debug "Server: Sending response";
  send client_pid response;
  Log.debug "Server: Response sent, continuing loop";
  loop state

and handle_format_file state client_pid file_path check_only =
  Log.debug
    ("Server: Received FormatFile from " ^ Pid.to_string client_pid ^ " for "
    ^ Path.to_string file_path ^ " (check_only=" ^ Bool.to_string check_only
    ^ ")");

  let ocamlformat = Tusk_toolchain.ocamlformat state.toolchain in
  let response =
    match
      Tusk_toolchain.Ocamlformat.format_file ocamlformat ~file_path ~check_only
    with
    | Tusk_toolchain.Ocamlformat.Formatted { code; changed } ->
        Protocol.FormatResult { formatted_code = code; changed }
    | Tusk_toolchain.Ocamlformat.Error err ->
        Protocol.FormatError { error = err }
  in
  send client_pid (Protocol.ServerResponse response);
  loop state

and handle_format_code state client_pid code file_path =
  Log.debug
    ("Server: Received FormatCode from " ^ Pid.to_string client_pid);

  let ocamlformat = Tusk_toolchain.ocamlformat state.toolchain in
  let response =
    match
      Tusk_toolchain.Ocamlformat.format_code ocamlformat ~code ~file_path
    with
    | Tusk_toolchain.Ocamlformat.Formatted { code; changed } ->
        Protocol.FormatResult { formatted_code = code; changed }
    | Tusk_toolchain.Ocamlformat.Error err ->
        Protocol.FormatError { error = err }
  in
  send client_pid (Protocol.ServerResponse response);
  loop state

and handle_format_all state client_pid mode =
  Log.debug
    ("Server: Received FormatAll from " ^ Pid.to_string client_pid ^ " (mode="
    ^ (match mode with `check -> "check" | `write -> "write") ^ ")");

  send client_pid
    (Protocol.ServerResponse
       (Protocol.FormatError
          { error = "FormatAll not yet implemented with worker pool" }));
  loop state

and handle_new_package state client_pid path name is_library =
  Log.debug
    ("Server: Received NewPackage from " ^ Pid.to_string client_pid ^ " for "
    ^ name ^ " at " ^ Path.to_string path);

  let src_dir = Path.(path / Path.v "src") in

  match Fs.create_dir_all src_dir with
  | Error _ ->
      send client_pid
        (Protocol.ServerResponse
           (Protocol.PackageCreationError
              { error = "Failed to create src directory" }));
      loop state
  | Ok () -> (
      let module_name =
        String.split_on_char '-' name
        |> List.map String.capitalize_ascii
        |> String.concat ""
      in

      let main_ml = Path.(src_dir / Path.v (module_name ^ ".ml")) in
      let main_mli = Path.(src_dir / Path.v (module_name ^ ".mli")) in

      let ml_content =
        if is_library then
          "open Std\n\n(** Main module for " ^ name ^ " library *)\n"
        else "open Std\n\nlet () = println \"Hello, World!\"\n"
      in

      let mli_content =
        if is_library then Some ("(** " ^ name ^ " library interface *)\n")
        else None
      in

      let package_toml = Path.(path / Path.v "tusk.toml") in
      let toml_content =
        "[package]\nname = \"" ^ name
        ^ "\"\nversion = \"0.1.0\"\n\n[dependencies]\nstd = \"*\"\n# Add dependencies here\n\n"
        ^ (if is_library then "" else "[package.bin]\nmain = \"" ^ name ^ "\"")
        ^ "\n"
      in

      let write_mli =
        match mli_content with
        | None -> Ok ()
        | Some content -> Fs.write content main_mli
      in

      match
        ( Fs.write ml_content main_ml,
          Fs.write toml_content package_toml,
          write_mli )
      with
      | Ok (), Ok (), Ok () ->
          Log.debug "Server: Rescanning workspace after package creation";
          let (updated_workspace, updated_load_errors) =
            Workspace_manager.scan state.workspace.root
            |> Result.expect
                 ~msg:"Failed to rescan workspace after package creation"
          in
          Log.debug
            ("Server: Workspace rescanned, found "
            ^ Int.to_string (List.length updated_workspace.packages)
            ^ " packages");

          let updated_package_graph =
            Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime updated_workspace
            |> Result.expect ~msg:"Failed to create package graph after rescan"
          in
          let updated_state =
            {
              state with
              workspace = updated_workspace;
              package_graph = updated_package_graph;
              load_errors = updated_load_errors;
            }
          in

          send client_pid
            (Protocol.ServerResponse
               (Protocol.PackageCreated { path = Path.to_string path; name }));
          loop updated_state
      | _ ->
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PackageCreationError
                  { error = "Failed to write package files" }));
           loop state)

(** Handler for build message - spawns worker and continues loop immediately *)
and handle_build state client_pid target scope target_arch session_id =
  Log.debug
    ("Server: handle_build called for target: "
    ^ (match target with
      | Protocol.All -> "All"
      | Protocol.Package p -> "Package(" ^ p ^ ")")
    ^ (match target_arch with
      | Some arch -> ", arch: " ^ arch
      | None -> ""));

  (* Rescan workspace to pick up any tusk.toml changes *)
  let (workspace, load_errors) =
    Workspace_manager.scan state.workspace.root
    |> Result.expect ~msg:"tusk_server: workspace scan failed"
  in
  let package_graph = 
    match Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime workspace with
    | Ok graph -> graph
    | Error _ -> 
        let ws = Workspace.make ~root:workspace.root ~packages:[] () in
        Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime ws |> Result.expect ~msg:"Failed to create empty package graph"
  in
  let updated_state = { state with workspace; package_graph; load_errors } in

  let server_pid = self () in
  Build_server.start ~workspace:updated_state.workspace ~load_errors:updated_state.load_errors
    ~toolchain:updated_state.toolchain ~store:updated_state.store
    ~concurrency:updated_state.concurrency ~session_id ~client_pid ~server_pid
    ~target ~scope ~target_arch;

  Log.info "[INTERNAL_SERVER] Build worker spawned, continuing server loop";
  loop updated_state

let start_local ~workspace ~config =
  try
    let state = build_state ~workspace ~config in
    let server_pid =
      spawn (fun () ->
          let _ = loop state in
          Ok ())
    in
    Ok server_pid
  with exn ->
    Error exn
