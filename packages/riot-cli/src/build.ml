open Std
open Std.Collections
open Std.Result.Syntax
open Riot_model
open Riot_build

module Build_telemetry = Riot_build.Internal.Telemetry_events

type build_scope = Riot_build.Request.scope =
  | Runtime
  | Dev

type dev_artifacts = Riot_build.Request.dev_artifacts = {
  tests: bool;
  examples: bool;
  benches: bool;
}

let out = eprintln

module Ui = Jollyroger.Terminal

let terminal = Ui.make ()

let status_line = fun status message -> Ui.status_line terminal status message

let out_status = fun status message -> out (status_line status message)

type output_mode =
  | Human
  | Json

type build_progress = {
  mutable built_count: int;
  mutable cached_count: int;
  mutable failed_count: int;
  mutable skipped_count: int;
}

type render_state = {
  mutable target_count: int option;
  profile_name: string option;
}

let create_render_state = fun ?profile () -> { target_count = None; profile_name = profile }

type request = {
  workspace: Workspace.t;
  packages: Riot_model.Package_name.t list;
  targets: Riot_model.Target.request;
  scope: build_scope;
  dev_artifacts: dev_artifacts;
  profile: Riot_model.Profile.t;
  requested_parallelism: int option;
  output_mode: output_mode;
  show_finished_summary: bool;
}

let build_trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_BUILD_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace_build = fun message ->
  let _ = message in
  ()

let trace_build_probe = fun ~started_at message ->
  let _ = started_at in
  let _ = message in
  ()

let build_request_label = fun (request: request) ->
  let packages =
    match request.packages with
    | [] -> "all"
    | packages ->
        packages
        |> List.map ~fn:Riot_model.Package_name.to_string
        |> String.concat ","
  in
  let targets =
    match request.targets with
    | Riot_model.Target.Host -> "host"
    | Riot_model.Target.All -> "all"
    | Riot_model.Target.Pattern pattern -> pattern
    | Riot_model.Target.Exact targets ->
        Riot_model.Target.Set.to_list targets
        |> List.map ~fn:Riot_model.Target.to_string
        |> String.concat ","
  in
  "Build(" ^ packages ^ "; targets=" ^ targets ^ "; profile=" ^ request.profile.name ^ ")"

let json_clock_origin = ref None

let reset_json_clock = fun ~started_at -> json_clock_origin := Some started_at

let json_clock_origin_or_set = fun started_at ->
  match !json_clock_origin with
  | Some origin -> origin
  | None ->
      json_clock_origin := Some started_at;
      started_at

let elapsed_us_since_json_origin = fun instant ->
  let origin = json_clock_origin_or_set instant in
  Time.Instant.saturating_duration_since ~earlier:origin instant
  |> Time.Duration.to_micros

let event_elapsed_us = fun () -> elapsed_us_since_json_origin (Time.Instant.now ())

let stamp_json_event = fun ?timestamp (json: Data.Json.t) ->
  match json with
  | Data.Json.Object fields ->
      let emitted_at_us = event_elapsed_us () in
      let fields =
        if
          Option.is_some (List.find fields ~fn:(fun (name, _) -> String.equal name "emitted_at_us"))
        then
          fields
        else
          fields @ [ ("emitted_at_us", Data.Json.Int emitted_at_us); ]
      in
      let fields =
        match timestamp with
        | Some (name, instant) ->
            if
              Option.is_some
                (List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name))
            then
              fields
            else
              fields @ [ (name, Data.Json.Int (elapsed_us_since_json_origin instant)); ]
        | None ->
            if
              Option.is_some
                (List.find fields ~fn:(fun (name, _) -> String.equal name "created_at_us"))
            then
              fields
            else
              fields @ [ ("created_at_us", Data.Json.Int emitted_at_us); ]
      in
      Data.Json.Object fields
  | other -> other

let write_json_event = fun ?timestamp (json: Data.Json.t) ->
  println
    (Data.Json.to_string (stamp_json_event ?timestamp json))

let write_serde_json_event = fun serializer value ->
  match Serde_json.to_string serializer value with
  | Ok content -> println content
  | Error err ->
      write_json_event
        (Data.Json.Object [
          ("type", Data.Json.String "JsonEncodingFailed");
          ("error", Data.Json.String (Serde.Error.to_string err));
        ])

let write_build_event_json = fun event ->
  match Riot_build.Event.to_json event with
  | Some json -> write_json_event ?timestamp:(Riot_build.Event.timestamp event) json
  | None -> ()

let path_has_prefix = fun ~prefix path -> String.starts_with ~prefix (Path.to_string path)

let package_has_dev_artifact = fun ~(prefix:string) (package: Riot_model.Package.t) sources ->
  not (List.is_empty sources)
  || List.any
    package.binaries
    ~fn:(fun (binary: Riot_model.Package.binary) -> path_has_prefix ~prefix binary.path)

let workspace_artifact_labels = fun (package: Riot_model.Package.t) ->
  if not (Riot_model.Package.is_workspace_member package) then
    []
  else
    [
      ("test", package_has_dev_artifact ~prefix:"tests/" package package.sources.tests);
      ("example", package_has_dev_artifact ~prefix:"examples/" package package.sources.examples);
      ("bench", package_has_dev_artifact ~prefix:"bench/" package package.sources.bench);
    ]
    |> List.filter_map
      ~fn:(fun (label, enabled) ->
        if enabled then
          Some label
        else
          None)

let profile_details = fun profile ->
  match profile with
  | Some profile -> [ profile ]
  | None -> []

let display_package_details = fun
  ?profile ?build_target ?(show_target = false) (package: Riot_model.Package.t) ->
  let version_details =
    if Riot_model.Package.is_workspace_member package then
      []
    else
      match package.publish.version with
      | Some version -> [ Std.Version.to_string version ]
      | None -> []
  in
  let target_details =
    match build_target with
    | Some target when show_target -> [ Riot_model.Target.to_string target ]
    | _ -> []
  in
  ((profile_details profile @ version_details) @ workspace_artifact_labels package) @ target_details

let display_package_name = fun
  ?profile ?build_target ?(show_target = false) (package: Riot_model.Package.t) ->
  let name = Riot_model.Package_name.to_string package.name in
  let details = display_package_details ?profile ?build_target ~show_target package in
  match details with
  | [] -> name
  | details -> name ^ " (" ^ String.concat ", " details ^ ")"

let labeled_multiline_lines = fun ~label value ->
  match String.split value ~by:"\n" with
  | [] -> [ label ^ ":" ]
  | first :: rest ->
      (label ^ ": " ^ first) :: List.map
        rest
        ~fn:(fun line ->
          if String.equal line "" then
            ""
          else
            "  " ^ line)

let error_line = fun message -> status_line Ui.Error message

let display_planner_file = fun path ->
  let path_text = Path.to_string path in
  if
    Path.is_absolute path
    || String.starts_with ~prefix:"./" path_text
    || String.starts_with ~prefix:"../" path_text
  then
    path_text
  else
    "./" ^ path_text

let planning_error_lines = fun __tmp1 ->
  match __tmp1 with
  | Riot_planner.Planning_error.CyclicDependency { cycle } ->
      [
        error_line "cyclic dependency detected while planning modules";
        "Riot found a cycle in the module graph, so it cannot choose a safe compile order.";
        "cycle: " ^ String.concat " -> " cycle;
        "examples:";
        "  - move shared types or helpers into a lower-level module";
        "  - replace one side of the cycle with a parameter, callback, or interface";
      ]
  | Riot_planner.Planning_error.ScanFailed { path; reason } ->
      [
        error_line "failed to scan package sources";
        "Riot could not read the source tree it needs to plan this package.";
        "path: " ^ Path.to_string path;
      ]
      @ labeled_multiline_lines ~label:"reason" reason
  | Riot_planner.Planning_error.DependencyAnalysisFailed { reason } ->
      [
        error_line "dependency analysis failed";
        "Riot could not parse or analyze a source file while discovering module dependencies.";
      ]
      @ labeled_multiline_lines ~label:"reason" reason
  | Riot_planner.Planning_error.GraphBuildFailed { reason } ->
      [
        error_line "failed to build the module graph";
        "Riot analyzed the package sources but could not assemble a valid build graph.";
      ]
      @ labeled_multiline_lines ~label:"reason" reason
  | Riot_planner.Planning_error.SourceDependsOnUndeclaredPackageModule {
      package_name;
      source;
      requested_module;
      allowed_modules;
      suggested_modules;
    } ->
      let allowed_modules =
        match allowed_modules with
        | [] -> "<none>"
        | allowed_modules -> String.concat ", " allowed_modules
      in
      let suggestion_lines =
        match suggested_modules with
        | [] -> []
        | [ suggestion ] -> [ "did you mean: " ^ suggestion ]
        | suggestions -> [ "did you mean one of: " ^ String.concat ", " suggestions ]
      in
      [
        error_line (requested_module ^ " is not available to package " ^ package_name);
        "The source file imports "
        ^ requested_module
        ^ ", but Riot only exposes modules from this package and its direct dependencies.";
        "package: " ^ package_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "available direct modules: " ^ allowed_modules;
      ]
      @ suggestion_lines
      @ [
        "examples:";
        "  - add the package that provides " ^ requested_module ^ " to [dependencies]";
        "  - or depend through one of the exposed modules above if that is the public API you meant";
      ]
  | Riot_planner.Planning_error.TargetDependsOnInternalLibraryModule {
      target_name;
      source;
      requested_module;
      internal_module;
      public_module;
    } ->
      [
        error_line ("target " ^ target_name ^ " imports private module " ^ requested_module);
        "The target source reaches "
        ^ internal_module
        ^ ", which is internal to this package library.";
        "target: " ^ target_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "internal module: " ^ internal_module;
        "public module: " ^ public_module;
        "examples:";
        "  - use " ^ public_module ^ "." ^ requested_module ^ " instead";
        "  - move shared target code behind " ^ public_module ^ " or a shared helper module";
      ]
  | Riot_planner.Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
      target_name;
      source;
      requested_module;
      internal_module;
      public_module;
    } ->
      let public_leaf =
        internal_module
        |> String.split ~by:"__"
        |> List.reverse
        |> List.head
        |> Option.unwrap_or ~default:requested_module
      in
      [
        error_line ("target " ^ target_name ^ " imports private module " ^ requested_module);
        "The target source reaches "
        ^ internal_module
        ^ ", which is a namespaced implementation detail of this package library.";
        "target: " ^ target_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "internal module: " ^ internal_module;
        "public module: " ^ public_module;
        "examples:";
        "  - use " ^ public_module ^ "." ^ public_leaf ^ " instead";
        "  - move shared target code behind " ^ public_module ^ " or a shared helper module";
      ]
  | Riot_planner.Planning_error.TargetDependsOnOtherTargetRoot {
      target_name;
      source;
      requested_module;
      other_target_name;
      other_target_module;
      public_module;
    } ->
      [
        error_line ("target " ^ target_name ^ " imports target entrypoint " ^ other_target_module);
        "The target source reaches another target root. Target entrypoints are private and are not reusable modules.";
        "target: " ^ target_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "other target: " ^ other_target_name;
        "other target module: " ^ other_target_module;
        "public module: " ^ public_module;
        "examples:";
        "  - move shared code behind " ^ public_module;
        "  - move shared code into a helper module that both targets can import";
      ]
  | Riot_planner.Planning_error.InvalidExecutableMain {
      package_name;
      target_name;
      file;
      error;
      _;
    } ->
      let file = display_planner_file file in
      let (headline, reason_lines) =
        match error with
        | Riot_planner.Planning_error.MissingMain -> (
          "`" ^ target_name ^ "` has no executable entry point",
          [ "But we could not find one." ]
        )
        | Riot_planner.Planning_error.MultipleMainDefinitions { count } -> (
          "`" ^ target_name ^ "` has more than one executable entry point",
          [
            "But we found " ^ Int.to_string count ^ " top-level `main` definitions.";
            "Executable targets must define exactly one.";
          ]
        )
        | Riot_planner.Planning_error.InvalidMainParameters { parameters } ->
            let parameters =
              match parameters with
              | [] -> "<none>"
              | parameters -> String.concat ", " parameters
            in
            (
              "`" ^ target_name ^ "` has an invalid executable entry point",
              [
                "But the `main` function we found does not have that shape.";
                "found parameters: " ^ parameters;
              ]
            )
      in
      [
        error_line headline;
        "";
        "Riot is building this target as an executable:";
        "";
        "    package: " ^ package_name;
        "    target:  " ^ target_name;
        "    file:    " ^ file;
        "";
        "To start the program, Riot needs this file to define a top-level";
        "`main` function with this shape:";
        "";
        "    let main ~args =";
        "      ...";
        "      Ok ()";
        "";
      ]
      @ reason_lines
  | Riot_planner.Planning_error.Exception { exn } ->
      [
        error_line "unexpected planner exception";
        "Riot hit an unexpected exception while planning this package.";
      ]
      @ labeled_multiline_lines ~label:"reason" (Exception.to_string exn)

let build_unit_planning_error_lines = fun __tmp1 ->
  match __tmp1 with
  | Riot_build.Internal.Build_unit_plan.MissingPackages { missing } ->
      let missing_lines =
        missing
        |> List.map
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Riot_planner.Build_unit_graph.Root package ->
                "missing: root -> " ^ Riot_model.Package_name.to_string package
            | Dependency { package; dependency } ->
                "missing: "
                ^ Riot_model.Package_name.to_string package
                ^ " -> "
                ^ Riot_model.Package_name.to_string dependency)
      in
      [
        error_line "missing package dependencies";
        "Riot found package dependency edges that do not point at a loaded workspace or resolved package.";
      ]
      @ missing_lines
      @ [
        "examples:";
        "  - add the missing package to the workspace";
        "  - add a registry, path, or workspace dependency entry for the missing package";
      ]
  | CycleDetected { cycle } ->
      [
        error_line "cyclic build-unit dependency detected";
        "Riot found a cycle in the package artifact graph, so it cannot choose a safe build order.";
        "cycle: " ^ String.concat " -> " (List.map cycle ~fn:Riot_planner.Build_unit.key_to_string);
        "examples:";
        "  - move shared code into a lower-level package";
        "  - remove a build/dev dependency edge that points back to its consumer";
      ]

let workspace_load_error_line = fun __tmp1 ->
  match __tmp1 with
  | Riot_model.Workspace_manager.PackageNotFound { package; path; dependant = None } ->
      "missing package: " ^ package ^ " (" ^ path ^ ")"
  | Riot_model.Workspace_manager.PackageNotFound { package; path; dependant = Some dependant } ->
      "missing package: " ^ package ^ " (required by " ^ dependant ^ ", " ^ path ^ ")"
  | Riot_model.Workspace_manager.PackageTomlReadFailed { package; path } ->
      "failed to read package toml: " ^ package ^ " (" ^ path ^ ")"
  | Riot_model.Workspace_manager.PackageTomlParseFailed { package; path } ->
      "failed to parse package toml: " ^ package ^ " (" ^ path ^ ")"
  | Riot_model.Workspace_manager.PackageFromTomlFailed { package; path; error } ->
      "failed to load package manifest: "
      ^ package
      ^ " ("
      ^ path
      ^ "): "
      ^ Riot_model.Package_manifest.error_message error

let out_prefixed_payload = fun ~prefix payload ->
  match String.split payload ~by:"\n" with
  | [] -> ()
  | first_line :: rest ->
      out (prefix ^ first_line);
      rest
      |> List.for_each ~fn:out

let telemetry_package_error_message = fun __tmp1 ->
  match __tmp1 with
  | Build_telemetry.PlanningFailed planning_error ->
      Riot_planner.Planning_error.to_string planning_error
  | Build_telemetry.ExecutionFailed { message }
  | Build_telemetry.ActionExecutionFailed { message } -> message
  | Build_telemetry.ActionOutputsNotCreated { missing } ->
      "missing outputs: " ^ String.concat ", " (List.map missing ~fn:Path.to_string)
  | Build_telemetry.ActionDependenciesFailed { failed } ->
      "failed dependencies: "
      ^ String.concat ", " (List.map failed ~fn:Graph.SimpleGraph.Node_id.to_string)

let show_target_in_package_labels = fun __tmp1 ->
  match __tmp1 with
  | Some { target_count = Some target_count } -> target_count > 1
  | Some { target_count = None }
  | None -> false

let render_profile = fun ?render_state ?profile () ->
  match profile with
  | Some _ -> profile
  | None -> (
      match render_state with
      | Some state -> state.profile_name
      | None -> None
    )

let display_build_package_name = fun ?render_state ?profile ~build_target package ->
  display_package_name
    ?profile:(render_profile ?render_state ?profile ())
    ~build_target
    ~show_target:(show_target_in_package_labels render_state)
    package

let count_part = fun ?plural count singular ->
  let label =
    if count = 1 then
      singular
    else
      match plural with
      | Some plural -> plural
      | None -> singular
  in
  Int.to_string count ^ " " ^ label

let non_zero_count_part = fun count label ->
  if count = 0 then
    None
  else
    Some (count_part count label)

let build_count_parts = fun
  ~built_count ~cached_count ~skipped_count ~failed_count ?(error_count = 0) () ->
  [
    non_zero_count_part built_count "built";
    non_zero_count_part cached_count "cached";
    non_zero_count_part skipped_count "skipped";
    non_zero_count_part failed_count "failed";
    non_zero_count_part error_count "errored";
  ]
  |> List.filter_map ~fn:(fun value -> value)

let build_count_summary = fun
  ~built_count ~cached_count ~skipped_count ~failed_count ?error_count () ->
  match build_count_parts ~built_count ~cached_count ~skipped_count ~failed_count ?error_count () with
  | [] -> "nothing to do"
  | parts -> String.concat ", " parts

let record_build_event_progress = fun progress event ->
  match event with
  | Riot_build.Event.Telemetry (
    Build_telemetry.BuildCompleted { status = `Fresh; _ }
  ) ->
      progress.built_count <- progress.built_count + 1
  | Riot_build.Event.Telemetry (
    Build_telemetry.BuildCompleted { status = `Cached; _ }
  ) ->
      progress.cached_count <- progress.cached_count + 1
  | Riot_build.Event.Telemetry (Build_telemetry.BuildSkipped _) ->
      progress.skipped_count <- progress.skipped_count + 1
  | Riot_build.Event.Telemetry (Build_telemetry.BuildFailed _) ->
      progress.failed_count <- progress.failed_count + 1
  | _ -> ()

type build_dashboard_package = {
  key: string;
  package: Riot_model.Package.t;
  build_target: Riot_model.Target.t;
  mutable action_count: int;
  mutable completed_actions: int;
  mutable planning_source_count: int;
  mutable planned_sources: int;
  planning_sources: (string, string) HashMap.t;
  planning_source_order: string Vector.t;
  running_actions: (string, string) HashMap.t;
  running_action_order: string Vector.t;
  mutable status: build_dashboard_row_status;
}

and build_dashboard_row_status =
  | Planning
  | Preparing
  | Queued
  | Blocked
  | Finalizing
  | Waiting

type build_dashboard_state = {
  active: (string, build_dashboard_package) HashMap.t;
  active_order: string Vector.t;
  profile_name: string option;
  target_count: int option;
  package_count: int;
  total_action_count: int;
  completed_action_count: int;
  built_count: int;
  cached_count: int;
  failed_count: int;
  skipped_count: int;
}

type build_dashboard_row_view = {
  key: string;
  label: string;
  action_count: int;
  completed_actions: int;
  planning_source_count: int;
  planned_sources: int;
  actions: build_dashboard_action_view list;
  status: string;
  status_kind: build_dashboard_row_status;
}

and build_dashboard_action_view = {
  action_key: string;
  action_label: string;
}

type build_dashboard_board_view = {
  completed_action_count: int;
  total_action_count: int;
  summary: string;
  rows: build_dashboard_row_view list;
}

type build_dashboard_view =
  | Empty
  | Board of build_dashboard_board_view

type build_dashboard = {
  mutable state: build_dashboard_state;
  mutable last_view: build_dashboard_view option;
  mutable last_line_count: int;
  mutable last_rendered_at: Time.Instant.t option;
}

type human_build_renderer =
  | LoggedBuildEvents
  | BuildDashboard of build_dashboard

type event_renderer = {
  mode: output_mode;
  profile: string option;
  render_state: render_state;
  human_renderer: human_build_renderer option;
  seen_registry_updates: string HashSet.t;
}

let is_interactive_stderr = fun () -> Tty.is_tty (Tty.stderr_fd ())

let build_dashboard_create_state = fun ?profile () ->
  {
    active = HashMap.with_capacity ~size:16;
    active_order = Vector.with_capacity ~size:16;
    profile_name = profile;
    target_count = None;
    package_count = 0;
    total_action_count = 0;
    completed_action_count = 0;
    built_count = 0;
    cached_count = 0;
    failed_count = 0;
    skipped_count = 0;
  }

let create_build_dashboard = fun ?profile () ->
  {
    state = build_dashboard_create_state ?profile ();
    last_view = None;
    last_line_count = 0;
    last_rendered_at = None;
  }

let create_human_build_renderer = fun ?profile () ->
  if is_interactive_stderr () then
    BuildDashboard (create_build_dashboard ?profile ())
  else
    LoggedBuildEvents

let build_dashboard_clear = fun dashboard ->
  if dashboard.last_line_count > 0 then (
    eprint
      (Tty.Escape_seq.cursor_up_seq dashboard.last_line_count ^ Tty.Escape_seq.erase_display_seq 0);
    dashboard.last_line_count <- 0
  );
  dashboard.last_view <- None

let build_dashboard_action_label = fun action ->
  let path_label = fun prefix path -> prefix ^ " " ^ Path.basename path in
  match action with
  | Riot_planner.Action.CompileInterface { source; _ } -> path_label "compile" source
  | Riot_planner.Action.CompileImplementation { source; _ } -> path_label "compile" source
  | Riot_planner.Action.GenerateInterface { source; _ } -> path_label "interface" source
  | Riot_planner.Action.CompileC { source; _ } -> path_label "compile" source
  | Riot_planner.Action.CreateLibrary { outputs; _ } -> (
      match outputs with
      | output :: _ -> path_label "archive" output
      | [] -> "archive"
    )
  | Riot_planner.Action.CreateExecutable { outputs; _ } -> (
      match outputs with
      | output :: _ -> path_label "link" output
      | [] -> "link"
    )
  | Riot_planner.Action.CreateSharedLibrary { outputs; _ } -> (
      match outputs with
      | output :: _ -> path_label "link" output
      | [] -> "link shared library"
    )
  | Riot_planner.Action.CopyFile { source; _ } -> path_label "copy" source
  | Riot_planner.Action.WriteFile { destination; _ } -> path_label "write" destination
  | Riot_planner.Action.BuildForeignDependency { name; _ } -> "build " ^ name

let build_dashboard_node_label = fun (action: Riot_planner.Action_node.t) ->
  match (Riot_planner.Action_node.value action).actions with
  | first :: _ -> build_dashboard_action_label first
  | [] -> "build"

let build_dashboard_min_render_interval_ms = 80

let build_dashboard_package_key = fun state ~build_target package ->
  let profile = Option.unwrap_or ~default:"" state.profile_name in
  Riot_model.Package_name.to_string package.Riot_model.Package.name
  ^ "|"
  ^ profile
  ^ "|"
  ^ Riot_model.Target.to_string build_target

let build_dashboard_package_label = fun state ~build_target package ->
  let show_target =
    match state.target_count with
    | Some target_count -> target_count > 1
    | None -> false
  in
  let name = Riot_model.Package_name.to_string package.Riot_model.Package.name in
  let details = display_package_details ?profile:state.profile_name ~build_target ~show_target package in
  match details with
  | [] -> name
  | details -> name ^ Ui.muted terminal (" (" ^ String.concat ", " details ^ ")")

let build_dashboard_get_package = fun state ~build_target package ->
  let key = build_dashboard_package_key state ~build_target package in
  match HashMap.get state.active ~key with
  | Some row -> row
  | None ->
      let row = {
        key;
        package;
        build_target;
        action_count = 0;
        completed_actions = 0;
        planning_source_count = 0;
        planned_sources = 0;
        planning_sources = HashMap.with_capacity ~size:4;
        planning_source_order = Vector.with_capacity ~size:4;
        running_actions = HashMap.with_capacity ~size:4;
        running_action_order = Vector.with_capacity ~size:4;
        status = Waiting;
      }
      in
      let _ = HashMap.insert state.active ~key ~value:row in
      Vector.push state.active_order ~value:key;
      row

let build_dashboard_find_package = fun state ~build_target package ->
  let key = build_dashboard_package_key state ~build_target package in
  HashMap.get state.active ~key

let build_dashboard_remove_package = fun state ~build_target package ->
  let key = build_dashboard_package_key state ~build_target package in
  let _ = HashMap.remove state.active ~key in
  ()

let build_dashboard_count_summary = fun state ->
  build_count_summary
    ~built_count:state.built_count
    ~cached_count:state.cached_count
    ~skipped_count:state.skipped_count
    ~failed_count:state.failed_count
    ()

let build_dashboard_package_progress = fun state ->
  state.built_count + state.cached_count + state.skipped_count + state.failed_count

let build_dashboard_set_action_count = fun state ~build_target package ~action_count ->
  let row = build_dashboard_get_package state ~build_target package in
  let previous_action_count = row.action_count in
  row.action_count <- action_count;
  HashMap.clear row.planning_sources;
  Vector.clear row.planning_source_order;
  if HashMap.is_empty row.running_actions then
    row.status <- Preparing;
  {
    state with
    total_action_count = Int.max 0 (state.total_action_count + action_count - previous_action_count);
  }

let build_dashboard_action_key = fun (action: Riot_planner.Action_node.t) ->
  Graph.SimpleGraph.Node_id.to_string
    (Riot_planner.Action_node.id action)

let build_dashboard_running_action_views = fun row ->
  let actions = ref [] in
  Vector.for_each
    row.running_action_order
    ~fn:(fun key ->
      match HashMap.get row.running_actions ~key with
      | Some label ->
          actions := {
            action_key = key;
            action_label = label;
          } :: !actions
      | None -> ());
  List.reverse !actions

let build_dashboard_planning_source_views = fun row ->
  let sources = ref [] in
  Vector.for_each
    row.planning_source_order
    ~fn:(fun key ->
      match HashMap.get row.planning_sources ~key with
      | Some label ->
          sources := {
            action_key = key;
            action_label = label;
          } :: !sources
      | None -> ());
  List.reverse !sources

let build_dashboard_set_planning_source = fun row source ->
  let key = Path.to_string source in
  let label = "plan " ^ Path.basename source in
  HashMap.clear row.planning_sources;
  Vector.clear row.planning_source_order;
  Vector.push row.planning_source_order ~value:key;
  let _ = HashMap.insert row.planning_sources ~key ~value:label in
  ()

let build_dashboard_mark_action_started = fun
  state ~build_target (action: Riot_planner.Action_node.t) ->
  match build_dashboard_find_package state ~build_target (Riot_planner.Action_node.value action).package with
  | Some row ->
      let key = build_dashboard_action_key action in
      let label = build_dashboard_node_label action in
      if not (HashMap.has_key row.running_actions ~key) then
        Vector.push row.running_action_order ~value:key;
      HashMap.clear row.planning_sources;
      Vector.clear row.planning_source_order;
      let _ = HashMap.insert row.running_actions ~key ~value:label in
      state
  | None -> state

let build_dashboard_mark_action_completed = fun state ~build_target package action ->
  match build_dashboard_find_package state ~build_target package with
  | Some row ->
      let previous_completed_actions = row.completed_actions in
      row.completed_actions <- if row.action_count > 0 then
        Int.min row.action_count (row.completed_actions + 1)
      else
        row.completed_actions + 1;
      let action_key = build_dashboard_action_key action in
      let _ = HashMap.remove row.running_actions ~key:action_key in
      if HashMap.is_empty row.running_actions then
        row.status <- if row.action_count > 0 && row.completed_actions >= row.action_count then
          Finalizing
        else if row.completed_actions = 0 then
          Queued
        else
          Queued;
      {
        state with
        completed_action_count = state.completed_action_count + row.completed_actions
        - previous_completed_actions;
      }
  | None -> state

let build_dashboard_complete_package_actions = fun state ~build_target package ->
  match build_dashboard_find_package state ~build_target package with
  | Some row when row.action_count > row.completed_actions ->
      let remaining_actions = row.action_count - row.completed_actions in
      row.completed_actions <- row.action_count;
      row.status <- Finalizing;
      { state with completed_action_count = state.completed_action_count + remaining_actions }
  | Some _
  | None -> state

let build_dashboard_truncate = fun ~width text ->
  if String.width text <= width then
    text
  else if width <= 1 then
    String.truncate_width ~width:(Int.max 0 width) text
  else
    String.truncate_width ~width ~tail:"..." text

let build_dashboard_terminal_width = fun () ->
  match Tty.Size.get () with
  | Ok { cols; _ } -> Int.max 40 cols
  | Error _ -> 120

let build_dashboard_update = fun state event ->
  match event with
  | Riot_build.Event.Phase (Riot_build.Event.TargetsResolved { target_count }) ->
      { state with target_count = Some target_count }
  | Riot_build.Event.Phase (Riot_build.Event.PackagePlanningStarted { package_count; _ }
  | Riot_build.Event.PackagePlanningFinished { package_count; _ }
  | Riot_build.Event.PackageExecutionStarted { package_count; _ }) ->
      { state with package_count }
  | Riot_build.Event.Phase (
    Riot_build.Event.PackagePlanStarted { package; build_target; source_count; _ }
  ) ->
      let row = build_dashboard_get_package state ~build_target package in
      row.planning_source_count <- source_count;
      row.planned_sources <- 0;
      HashMap.clear row.planning_sources;
      Vector.clear row.planning_source_order;
      row.status <- Planning;
      state
  | Riot_build.Event.Phase (
    Riot_build.Event.PackagePlanSourceStarted {
      package;
      build_target;
      source;
      source_index;
      source_count;
      _;
    }
  ) ->
      let row = build_dashboard_get_package state ~build_target package in
      row.planning_source_count <- source_count;
      row.planned_sources <- source_index;
      build_dashboard_set_planning_source row source;
      row.status <- Planning;
      state
  | Riot_build.Event.Phase (
    Riot_build.Event.PackagePlanFinished { package; build_target; source_count; _ }
  ) ->
      (
        match build_dashboard_find_package state ~build_target package with
        | Some row ->
            row.planning_source_count <- source_count;
            row.planned_sources <- source_count;
            HashMap.clear row.planning_sources;
            Vector.clear row.planning_source_order;
            row.status <- Preparing
        | None -> ()
      );
      state
  | Riot_build.Event.Telemetry (
    Build_telemetry.CompilationStarted { package; build_target; action_count; _ }
  ) ->
      build_dashboard_set_action_count state ~build_target package ~action_count
  | Riot_build.Event.Telemetry (Build_telemetry.SandboxCreated { package; build_target; _ }
  | Build_telemetry.SandboxInputsCopied { package; build_target; _ }
  | Build_telemetry.SandboxDependenciesCopied { package; build_target; _ }) ->
      (
        match build_dashboard_find_package state ~build_target package with
        | Some row -> row.status <- Preparing
        | None -> ()
      );
      state
  | Riot_build.Event.Telemetry (
    Build_telemetry.PackageExecutionPrepared { package; build_target; _ }
  ) ->
      (
        match build_dashboard_find_package state ~build_target package with
        | Some row -> row.status <- Queued
        | None -> ()
      );
      state
  | Riot_build.Event.Phase (
    Riot_build.Event.PackageActionGraphPlanned { package; build_target; action_count; _ }
  ) ->
      build_dashboard_set_action_count state ~build_target package ~action_count
  | Riot_build.Event.Telemetry (
    Build_telemetry.ActionStarted { action; build_target; _ }
  ) ->
      build_dashboard_mark_action_started state ~build_target action
  | Riot_build.Event.Telemetry (
    Build_telemetry.ActionCompleted { action; build_target; _ }
  ) ->
      build_dashboard_mark_action_completed state ~build_target (Riot_planner.Action_node.value action).package action
  | Riot_build.Event.Telemetry (
    Build_telemetry.ActionFailed { action; build_target; _ }
  ) ->
      build_dashboard_mark_action_completed state ~build_target (Riot_planner.Action_node.value action).package action
  | Riot_build.Event.Telemetry (
    Build_telemetry.BuildCompleted { package; build_target; status = `Fresh; _ }
  ) ->
      let state = build_dashboard_complete_package_actions state ~build_target package in
      build_dashboard_remove_package state ~build_target package;
      { state with built_count = state.built_count + 1 }
  | Riot_build.Event.Telemetry (
    Build_telemetry.BuildCompleted { package; build_target; status = `Cached; _ }
  ) ->
      build_dashboard_remove_package state ~build_target package;
      { state with cached_count = state.cached_count + 1 }
  | Riot_build.Event.Telemetry (
    Build_telemetry.BuildSkipped { package; build_target; _ }
  ) ->
      build_dashboard_remove_package state ~build_target package;
      { state with skipped_count = state.skipped_count + 1 }
  | Riot_build.Event.Telemetry (
    Build_telemetry.BuildFailed { package; build_target; _ }
  ) ->
      build_dashboard_remove_package state ~build_target package;
      { state with failed_count = state.failed_count + 1 }
  | _ -> state

let build_dashboard_row_view = fun state (row: build_dashboard_package) ->
  let running_actions = build_dashboard_running_action_views row in
  let actions =
    match (row.status, running_actions) with
    | (Planning, []) -> build_dashboard_planning_source_views row
    | _ -> running_actions
  in
  let status =
    match (row.status, actions) with
    | (Planning, _) -> "planning"
    | (_, _ :: _) -> "running"
    | (Preparing, []) -> "preparing"
    | (Queued, []) -> "queued"
    | (Blocked, []) -> "blocked"
    | (Finalizing, []) -> "finalizing"
    | (Waiting, []) -> "waiting"
  in
  {
    key = row.key;
    label = build_dashboard_package_label state ~build_target:row.build_target row.package;
    action_count = row.action_count;
    completed_actions = row.completed_actions;
    planning_source_count = row.planning_source_count;
    planned_sources = row.planned_sources;
    actions;
    status;
    status_kind = row.status;
  }

let build_dashboard_row_is_active = fun (row: build_dashboard_package) ->
  not (HashMap.is_empty row.running_actions)
  || not (HashMap.is_empty row.planning_sources)
  || match row.status with
  | Planning -> true
  | Finalizing -> true
  | Preparing
  | Queued
  | Blocked
  | Waiting -> false

let build_dashboard_render_state = fun state ->
  let package_done_count = build_dashboard_package_progress state in
  if package_done_count = 0 && state.total_action_count = 0 && HashMap.length state.active = 0 then
    Empty
  else
    let rows = ref [] in
    let seen = HashSet.create () in
    Vector.for_each
      state.active_order
      ~fn:(fun key ->
        if not (HashSet.contains seen ~value:key) then (
          let _ = HashSet.insert seen ~value:key in
          match HashMap.get state.active ~key with
          | Some row when build_dashboard_row_is_active row ->
              rows := build_dashboard_row_view state row :: !rows
          | Some _
          | None -> ()
        ));
    let rows = List.reverse !rows in
    if List.is_empty rows && state.completed_action_count = 0 && state.total_action_count = 0 then
      Empty
    else
      Board {
        completed_action_count = state.completed_action_count;
        total_action_count = state.total_action_count;
        summary = build_dashboard_count_summary state;
        rows;
      }

let rec build_dashboard_row_views_equal = fun left right ->
  match (left, right) with
  | ([], []) -> true
  | (left :: left_rest, right :: right_rest) ->
      String.equal left.key right.key
      && String.equal left.label right.label
      && left.action_count = right.action_count
      && left.completed_actions = right.completed_actions
      && left.planning_source_count = right.planning_source_count
      && left.planned_sources = right.planned_sources
      && left.actions = right.actions
      && String.equal left.status right.status
      && left.status_kind = right.status_kind
      && build_dashboard_row_views_equal left_rest right_rest
  | ([], _ :: _)
  | (_ :: _, []) -> false

let build_dashboard_board_views_equal = fun left right ->
  left.completed_action_count = right.completed_action_count
  && left.total_action_count = right.total_action_count
  && String.equal left.summary right.summary
  && build_dashboard_row_views_equal left.rows right.rows

let build_dashboard_views_equal = fun left right ->
  match (left, right) with
  | (Empty, Empty) -> true
  | (Board left, Board right) -> build_dashboard_board_views_equal left right
  | (Empty, Board _)
  | (Board _, Empty) -> false

let build_dashboard_row_line = fun ~width ~is_last row ->
  let running_action_count = List.length row.actions in
  let progress =
    match row.status_kind with
    | Planning ->
        if row.planning_source_count > 0 then
          "[" ^ Int.to_string row.planned_sources ^ "/" ^ Int.to_string row.planning_source_count ^ "]"
        else
          "[0/?]"
    | Preparing
    | Queued
    | Blocked
    | Finalizing
    | Waiting ->
        let visible_action =
          if row.action_count > 0 then
            Int.min row.action_count (row.completed_actions + running_action_count)
          else
            row.completed_actions + running_action_count
        in
        if row.action_count > 0 then
          "[" ^ Int.to_string visible_action ^ "/" ^ Int.to_string row.action_count ^ "]"
        else
          "[" ^ Int.to_string row.completed_actions ^ "/?]"
  in
  let branch =
    if is_last then
      "└── "
    else
      "├── "
  in
  let suffix =
    match (row.status_kind, row.actions) with
    | (Planning, _) -> " " ^ Ui.status_label terminal Ui.Plan
    | (_, []) -> " " ^ row.status
    | _ -> ""
  in
  build_dashboard_truncate ~width (branch ^ row.label ^ " " ^ progress ^ suffix)

let build_dashboard_action_line = fun ~width ~parent_is_last ~is_last action ->
  let prefix =
    if parent_is_last then
      "    "
    else
      "│   "
  in
  let branch =
    if is_last then
      "└── "
    else
      "├── "
  in
  build_dashboard_truncate ~width (prefix ^ branch ^ action.action_label)

let build_dashboard_push_action_lines = fun ~width ~parent_is_last lines actions ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | [ action ] ->
        Vector.push
          lines
          ~value:(build_dashboard_action_line ~width ~parent_is_last ~is_last:true action)
    | action :: rest ->
        Vector.push
          lines
          ~value:(build_dashboard_action_line ~width ~parent_is_last ~is_last:false action);
        loop rest
  in
  loop actions

let build_dashboard_push_row_lines = fun ~width lines rows ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | [ row ] ->
        Vector.push lines ~value:(build_dashboard_row_line ~width ~is_last:true row);
        build_dashboard_push_action_lines
          ~width
          ~parent_is_last:true
          lines
          row.actions
    | row :: rest ->
        Vector.push lines ~value:(build_dashboard_row_line ~width ~is_last:false row);
        build_dashboard_push_action_lines
          ~width
          ~parent_is_last:false
          lines
          row.actions;
        loop rest
  in
  loop rows

let build_dashboard_view_lines = fun view ->
  match view with
  | Empty -> Vector.with_capacity ~size:0
  | Board board ->
      let width = build_dashboard_terminal_width () in
      let lines = Vector.with_capacity ~size:8 in
      Vector.push
        lines
        ~value:(build_dashboard_truncate ~width ("["
        ^ Int.to_string board.completed_action_count
        ^ "/"
        ^ Int.to_string board.total_action_count
        ^ "] actions  "
        ^ board.summary));
      build_dashboard_push_row_lines ~width lines board.rows;
      lines

let build_dashboard_throttle_allows_render = fun dashboard ->
  match dashboard.last_rendered_at with
  | None -> true
  | Some rendered_at ->
      Time.Duration.to_millis (Time.Instant.elapsed rendered_at)
      >= build_dashboard_min_render_interval_ms

let build_dashboard_draw = fun ?(force = false) dashboard ->
  let view = build_dashboard_render_state dashboard.state in
  let lines = build_dashboard_view_lines view in
  if Vector.is_empty lines then
    if force then
      build_dashboard_clear dashboard
    else
      ()
  else
    let view_matches =
      match dashboard.last_view with
      | Some previous -> build_dashboard_views_equal previous view
      | None -> false
    in
    if (not force) && (view_matches || not (build_dashboard_throttle_allows_render dashboard)) then
      ()
    else (
      build_dashboard_clear dashboard;
      Vector.for_each lines ~fn:(fun line -> eprint (line ^ "\n"));
      dashboard.last_line_count <- Vector.length lines;
      dashboard.last_view <- Some view;
      dashboard.last_rendered_at <- Some (Time.Instant.now ())
    )

let human_renderer_clear = fun __tmp1 ->
  match __tmp1 with
  | LoggedBuildEvents -> ()
  | BuildDashboard dashboard -> build_dashboard_clear dashboard

let write_build_telemetry_event = fun ?render_state ?profile ?human_renderer ~mode event ->
  match mode with
  | Json -> write_build_event_json (Riot_build.Event.Telemetry event)
  | Human -> (
      match human_renderer with
      | Some (BuildDashboard dashboard) -> (
          match event with
          | Build_telemetry.BuildFailed {
              package;
              build_target;
              error = PlanningFailed planning_error;
              _;
            } ->
              build_dashboard_clear dashboard;
              out_status
                Ui.Error
                (display_build_package_name ?render_state ?profile ~build_target package);
              planning_error_lines planning_error
              |> List.for_each ~fn:(fun line -> out ("  " ^ line))
          | Build_telemetry.BuildFailed { package; build_target; error; _ } ->
              build_dashboard_clear dashboard;
              out_status
                Ui.Error
                (display_build_package_name ?render_state ?profile ~build_target package
                ^ ": "
                ^ telemetry_package_error_message error)
          | Build_telemetry.PackageOcamlcWarnings { package; build_target; messages; _ } ->
              build_dashboard_clear dashboard;
              messages
              |> List.for_each
                ~fn:(fun message ->
                  out_prefixed_payload
                    ~prefix:(status_line
                      Ui.Warning
                      (display_build_package_name ?render_state ?profile ~build_target package
                      ^ ": "))
                    message)
          | _ -> build_dashboard_draw dashboard
        )
      | Some LoggedBuildEvents
      | None -> (
          match event with
          | Build_telemetry.CompilationStarted { package; build_target; _ } ->
              out_status
                Ui.Building
                (display_build_package_name ?render_state ?profile ~build_target package)
          | Build_telemetry.BuildCompleted { package; build_target; status = `Fresh; _ } ->
              out_status
                Ui.Built
                (display_build_package_name ?render_state ?profile ~build_target package)
          | Build_telemetry.BuildCompleted { package; build_target; status = `Cached; _ } ->
              out_status
                Ui.Cached
                (display_build_package_name ?render_state ?profile ~build_target package)
          | Build_telemetry.BuildSkipped { package; build_target; reason; _ } ->
              out_status
                Ui.Skipped
                (display_build_package_name ?render_state ?profile ~build_target package
                ^ ": "
                ^ reason)
          | Build_telemetry.BuildFailed {
              package;
              build_target;
              error = PlanningFailed planning_error;
              _;
            } ->
              out_status
                Ui.Error
                (display_build_package_name ?render_state ?profile ~build_target package);
              planning_error_lines planning_error
              |> List.for_each ~fn:(fun line -> out ("  " ^ line))
          | Build_telemetry.BuildFailed { package; build_target; error; _ } ->
              out_status
                Ui.Error
                (display_build_package_name ?render_state ?profile ~build_target package
                ^ ": "
                ^ telemetry_package_error_message error)
          | Build_telemetry.PackageOcamlcWarnings { package; build_target; messages; _ } ->
              messages
              |> List.for_each
                ~fn:(fun message ->
                  out_prefixed_payload
                    ~prefix:(status_line
                      Ui.Warning
                      (display_build_package_name ?render_state ?profile ~build_target package
                      ^ ": "))
                    message)
          | Build_telemetry.PackageStarted _
          | Build_telemetry.WorkspacePlanStarted _
          | Build_telemetry.WorkspacePlanCompleted _
          | Build_telemetry.WorkspaceManifestFilterCompleted _
          | Build_telemetry.WorkspaceGraphCreated _
          | Build_telemetry.WorkspaceTargetGraphFiltered _
          | Build_telemetry.WorkspaceTopologicalSortCompleted _
          | Build_telemetry.PlanningWorkspaceStarted _
          | Build_telemetry.PlanningWorkspaceCompleted _
          | Build_telemetry.PackagePlanningResult _
          | Build_telemetry.PackagePlanningBreakdown _
          | Build_telemetry.SandboxCreated _
          | Build_telemetry.SandboxInputsCopied _
          | Build_telemetry.SandboxDependenciesCopied _
          | Build_telemetry.PackageExecutionPrepared _
          | Build_telemetry.ActionStarted _
          | Build_telemetry.ActionCommandStarted _
          | Build_telemetry.ActionCompleted _
          | Build_telemetry.ActionFailed _
          | Build_telemetry.CacheHit _
          | Build_telemetry.CacheMiss _
          | Build_telemetry.WorkspaceStarted _
          | Build_telemetry.WorkspaceCompleted _ -> ()
          | _ -> ()
        )
    )

let write_build_phase_event_with_renderer = fun ?render_state ?human_renderer ~mode phase ->
  (
    match (render_state, phase) with
    | (Some (state: render_state), Riot_build.Event.TargetsResolved { target_count }) ->
        state.target_count <- Some target_count
    | _ -> ()
  );
  match mode with
  | Json -> write_build_event_json (Riot_build.Event.Phase phase)
  | Human -> (
      match human_renderer with
      | Some (BuildDashboard dashboard) -> (
          match phase with
          | Riot_build.Event.BuildLockWaiting _ ->
              build_dashboard_clear dashboard;
              out_status Ui.Running "build lock is taken, waiting..."
          | Riot_build.Event.PackageExecutionFinished { built_count; failed_count; error_count; _ } ->
              if failed_count > 0 || error_count > 0 then (
                build_dashboard_clear dashboard;
                out_status
                  Ui.Error
                  ("execution failed: "
                  ^ build_count_summary
                    ~built_count
                    ~cached_count:0
                    ~skipped_count:0
                    ~failed_count
                    ~error_count
                    ())
              ) else
                build_dashboard_draw dashboard
          | _ -> build_dashboard_draw dashboard
        )
      | Some LoggedBuildEvents
      | None -> (
          match phase with
          | Riot_build.Event.TargetsResolved _
          | Riot_build.Event.ToolchainsEnsured _
          | Riot_build.Event.ToolchainsValidated _
          | Riot_build.Event.RuntimeStarting
          | Riot_build.Event.RuntimeStarted -> ()
          | Riot_build.Event.BuildLockWaiting _ ->
              out_status Ui.Running "build lock is taken, waiting..."
          | Riot_build.Event.PackagePlanningStarted _ -> ()
          | Riot_build.Event.PackagePlanStarted _ -> ()
          | Riot_build.Event.PackagePlanSourceStarted _ -> ()
          | Riot_build.Event.PackagePlanFinished _ -> ()
          | Riot_build.Event.PackagePlanningFinished _ -> ()
          | Riot_build.Event.PackageActionGraphPlanned _ -> ()
          | Riot_build.Event.BuildLanesPreparationStarted _
          | Riot_build.Event.BuildLanesPreparationFinished _
          | Riot_build.Event.BuildUnitPlanCreated _
          | Riot_build.Event.BuildLanePreparationStarted _
          | Riot_build.Event.BuildLaneLockAcquired _
          | Riot_build.Event.BuildLaneToolchainInitialized _
          | Riot_build.Event.BuildLaneStoreCreated _
          | Riot_build.Event.BuildLanePreparationFinished _ -> ()
          | Riot_build.Event.PackageExecutionStarted { package_count; _ } ->
              let _ = package_count in
              ()
          | Riot_build.Event.PackageExecutionFinished { built_count; failed_count; error_count; _ } ->
              if failed_count > 0 || error_count > 0 then
                out_status
                  Ui.Error
                  ("execution failed: "
                  ^ build_count_summary
                    ~built_count
                    ~cached_count:0
                    ~skipped_count:0
                    ~failed_count
                    ~error_count
                    ())
          | Riot_build.Event.TargetBuildStarted _
          | Riot_build.Event.TargetBuildFinished _
          | Riot_build.Event.CacheGenerationRecordingStarted _
          | Riot_build.Event.CacheGenerationRecorded _
          | Riot_build.Event.ReturningResults _ -> ()
        )
    )

let command_error_event_to_json = fun kind details ->
  Data.Json.Object (("type", Data.Json.String kind) :: details)

let format_pm_event = fun ~seen_registry_updates kind ->
  match kind with
  | Riot_model.Event.RegistryIndexUpdating { registry } ->
      if HashSet.contains seen_registry_updates ~value:registry then
        None
      else
        (
          let _ = HashSet.insert seen_registry_updates ~value:registry in
          Some (status_line Ui.Running ("updating " ^ registry ^ " index"))
        )
  | Riot_model.Event.PackageResolvedForBuild _ -> None
  | Riot_model.Event.PackageDownloadStarted { package; version; _ } ->
      Some (status_line
        Ui.Running
        ("fetching " ^ Riot_model.Package_name.to_string package ^ " " ^ version))
  | Riot_model.Event.PackageDownloadQueued { package; version; _ } ->
      Some (status_line
        Ui.Running
        ("queued " ^ Riot_model.Package_name.to_string package ^ " (" ^ version ^ ")"))
  | Riot_model.Event.DependencyResolutionStarted _
  | Riot_model.Event.DependencyResolutionRefreshingLock _
  | Riot_model.Event.DependencyResolutionFailed _
  | Riot_model.Event.DependencyUniverseBuilding _
  | Riot_model.Event.DependencyUniverseBuilt _
  | Riot_model.Event.PackageMetadataFetchStarted _
  | Riot_model.Event.PackageMetadataFetchFinished _
  | Riot_model.Event.PackageMetadataFetchFailed _
  | Riot_model.Event.SourceDependencyMaterializationFinished _
  | Riot_model.Event.LockfileReadStarted _
  | Riot_model.Event.LockfileReadFinished _
  | Riot_model.Event.LockfileReadFailed _
  | Riot_model.Event.LockfileWriteStarted _
  | Riot_model.Event.LockfileWriteFinished _
  | Riot_model.Event.LockfileWriteFailed _
  | Riot_model.Event.DependencyResolutionFinished _
  | Riot_model.Event.DependencyResolutionUsingExistingLock _
  | Riot_model.Event.DependencyResolutionUnlocking _
  | Riot_model.Event.PackageManifestFetchStarted _
  | Riot_model.Event.PackageManifestFetchFinished _
  | Riot_model.Event.PackageManifestFetchFailed _
  | Riot_model.Event.PackageDownloadSkipped _
  | Riot_model.Event.PackageMaterializationStarted _
  | Riot_model.Event.PackageMaterializationFinished _
  | Riot_model.Event.PackageMaterializationFailed _ -> None
  | Riot_model.Event.SourceDependencyMaterializationStarted { source_locator; ref_ } ->
      Some (
        status_line
          Ui.Running
          (
            "installing " ^ (
              match ref_ with
              | Some ref_ -> source_locator ^ "#" ^ ref_
              | None -> source_locator
            )
          )
      )
  | Riot_model.Event.DependencyManifestUpdated {
      path;
      section;
      operation;
      dependency;
    } ->
      let verb =
        match operation with
        | `Add -> "added"
        | `Remove -> "removed"
      in
      Some (status_line Ui.Success (verb ^ " " ^ dependency ^ " (" ^ section ^ ") in " ^ path))
  | Riot_model.Event.PackageVersionLocked { package; version } ->
      Some (status_line
        Ui.Success
        ("locked " ^ Riot_model.Package_name.to_string package ^ " (" ^ version ^ ")"))
  | Riot_model.Event.PackageVersionsUnchanged _ ->
      Some (status_line Ui.Success "dependencies are already up to date")
  | Riot_model.Event.PackageVersionUpdated { package; from_version; to_version } ->
      Some (status_line
        Ui.Success
        (Riot_model.Package_name.to_string package
        ^ " updated ("
        ^ from_version
        ^ " -> "
        ^ to_version
        ^ ")"))
  | kind -> Some (Riot_model.Event.display kind)

let write_pm_event = fun ~mode ~seen_registry_updates event ->
  match mode with
  | Json -> write_build_event_json (Riot_build.Event.Pm event)
  | Human -> (
      match format_pm_event ~seen_registry_updates event.kind with
      | Some message -> out message
      | None -> ()
    )

let write_command_error = fun ~mode kind details human_message ->
  match mode with
  | Json -> write_json_event (command_error_event_to_json kind details)
  | Human -> out_status Ui.Error human_message

let build_failure_detail_lines = fun (failure: Riot_build.Build_result.failure) ->
  let package_name = Riot_model.Package_name.to_string failure.package_name in
  match failure.reason with
  | Riot_build.Build_result.PackagePlanningFailed planning_error ->
      planning_error_lines planning_error
  | _ -> [ error_line (package_name ^ " failed"); failure.message ]

let write_failure_blocks = fun failures ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | [ failure ] ->
        out "";
        build_failure_detail_lines failure
        |> List.for_each ~fn:out;
        out ""
    | failure :: rest ->
        out "";
        build_failure_detail_lines failure
        |> List.for_each ~fn:out;
        loop rest
  in
  loop failures

let write_build_failed_error = fun ~mode errors ->
  match mode with
  | Json ->
      write_json_event
        (command_error_event_to_json
          "BuildFailed"
          [
            (
              "errors",
              Data.Json.Array (List.map errors ~fn:Riot_build.Build_result.failure_to_json)
            );
          ])
  | Human -> (
      match errors with
      | [] -> out_status Ui.Error "build failed"
      | [ failure ] -> write_failure_blocks [ failure ]
      | failures ->
          out_status Ui.Error "build failed";
          write_failure_blocks failures
    )

let build_args =
  let open ArgParser in
  let open ArgParser.Arg in
  [
    option "package"
    |> short 'p'
    |> long "package"
    |> multiple
    |> help
      "Build a specific package. Repeat to build multiple packages; omit to build all packages.";
    option "target"
    |> short 'x'
    |> long "target"
    |> help "Target architecture (exact triple, pattern like 'linux'/'aarch64', or 'all')";
    flag "all-targets"
    |> long "all-targets"
    |> help "Build for all configured targets";
    flag "tests"
    |> long "tests"
    |> help "Also compile test binaries";
    flag "examples"
    |> long "examples"
    |> help "Also compile example binaries";
    flag "benches"
    |> long "benches"
    |> help "Also compile benchmark binaries";
    flag "all"
    |> long "all"
    |> help "Also compile tests, examples, and benchmark binaries";
    flag "release"
    |> long "release"
    |> help "Use the release build profile";
    option "jobs"
    |> short 'j'
    |> long "jobs"
    |> help "Limit parallel workers";
    option "target-dir"
    |> long "target-dir"
    |> help "Override the workspace target directory for this command";
    flag "json"
    |> long "json"
    |> help "Emit machine-readable JSONL events";
  ]

let command =
  let open ArgParser in
  command "build"
  |> about "Build packages"
  |> args build_args

let target_request_of_matches = fun matches ->
  if ArgParser.get_flag matches "all-targets" then
    Riot_model.Target.All
  else
    match ArgParser.get_one matches "target" with
    | Some value -> Riot_model.Target.parse value
    | None -> Riot_model.Target.Host

let output_mode_of_matches = fun matches ->
  if ArgParser.get_flag matches "json" then
    Json
  else
    Human

let dev_artifacts_of_matches = fun matches ->
  if ArgParser.get_flag matches "all" then
    { tests = true; examples = true; benches = true }
  else
    {
      tests = ArgParser.get_flag matches "tests";
      examples = ArgParser.get_flag matches "examples";
      benches = ArgParser.get_flag matches "benches";
    }

let scope_of_dev_artifacts = fun (dev_artifacts: dev_artifacts) ->
  if dev_artifacts.tests || dev_artifacts.examples || dev_artifacts.benches then
    Dev
  else
    Runtime

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    Riot_model.Profile.release
  else
    Riot_model.Profile.debug

let requested_parallelism_of_matches = fun matches ->
  match ArgParser.get_one matches "jobs" with
  | None -> Ok None
  | Some value -> (
      match Int.parse value with
      | Some workers -> Ok (Some workers)
      | None -> Error (Failure ("invalid --jobs value: " ^ value))
    )

let parse_package_names = fun package_names ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | package_name :: rest -> (
        match Riot_model.Package_name.from_string package_name with
        | Ok package_name -> loop (package_name :: acc) rest
        | Error error ->
            Error (Failure ("invalid package name '"
            ^ package_name
            ^ "': "
            ^ Riot_model.Package_name.error_message error))
      )
  in
  loop [] package_names

let make_request = fun
  ~workspace
  ?(scope = Runtime)
  ?(dev_artifacts = {tests = true; examples = true; benches = true})
  ?(profile = Riot_model.Profile.debug)
  ?(mode = Human)
  ?(show_finished_summary = true)
  ?(requested_parallelism = None)
  ~packages
  ~targets
  () ->
  {
    workspace;
    packages;
    targets;
    scope;
    dev_artifacts;
    profile;
    requested_parallelism;
    output_mode = mode;
    show_finished_summary;
  }

let request_of_matches = fun ~workspace matches ->
  match parse_package_names (ArgParser.get_many matches "package") with
  | Error _ as err -> err
  | Ok packages -> (
      match requested_parallelism_of_matches matches with
      | Error _ as err -> err
      | Ok requested_parallelism ->
          let dev_artifacts = dev_artifacts_of_matches matches in
          Ok (make_request
            ~workspace
            ~scope:(scope_of_dev_artifacts dev_artifacts)
            ~dev_artifacts
            ~profile:(profile_of_matches matches)
            ~mode:(output_mode_of_matches matches)
            ~requested_parallelism
            ~packages
            ~targets:(target_request_of_matches matches)
            ())
    )

let write_building_target_event = fun ~mode ~target ~host ->
  let target_name = Riot_model.Target.to_string target in
  match mode with
  | Json -> write_build_event_json (Riot_build.Event.BuildingTarget { target; host })
  | Human ->
      if not host then
        out_status Ui.Running ("cross-compiling for " ^ target_name)

let scaled_size_string = fun bytes divisor suffix ->
  let whole = Int64.div bytes divisor in
  let remainder = Int64.rem bytes divisor in
  let fraction = Int64.div (Int64.mul remainder 10L) divisor in
  Int64.to_string whole ^ "." ^ Int64.to_string fraction ^ " " ^ suffix

let size_to_string = fun size_bytes ->
  let kib = 1_024L in
  let mib = Int64.mul kib 1_024L in
  let gib = Int64.mul mib 1_024L in
  let tib = Int64.mul gib 1_024L in
  if Int64.compare size_bytes tib != Order.LT then
    scaled_size_string size_bytes tib "TiB"
  else if Int64.compare size_bytes gib != Order.LT then
    scaled_size_string size_bytes gib "GiB"
  else if Int64.compare size_bytes mib != Order.LT then
    scaled_size_string size_bytes mib "MiB"
  else if Int64.compare size_bytes kib != Order.LT then
    scaled_size_string size_bytes kib "KiB"
  else
    Int64.to_string size_bytes ^ " B"

let format_cache_gc_cleanup = fun (summary: Riot_store.Cache_gc.summary) ->
  Int.to_string summary.deleted_entries
  ^ " cache entries and "
  ^ Int.to_string summary.deleted_generations
  ^ " generations ("
  ^ size_to_string summary.size_before_bytes
  ^ " -> "
  ^ size_to_string summary.size_after_bytes
  ^ ")"

type cache_gc_progress = {
  total_entries: int;
  step: int;
  mutable removed_entries: int;
}

let cache_gc_progress = ref None

let max_cache_gc_progress_dots = 40

let start_cache_gc_progress = fun total_entries ->
  if total_entries > 0 then (
    let step =
      let raw = (total_entries + max_cache_gc_progress_dots - 1) / max_cache_gc_progress_dots in
      if raw > 0 then
        raw
      else
        1
    in
    eprint (status_line Ui.Running "removing cache entries ");
    cache_gc_progress := Some { total_entries; step; removed_entries = 0 }
  )

let tick_cache_gc_progress = fun () ->
  match !cache_gc_progress with
  | None -> ()
  | Some progress ->
      progress.removed_entries <- progress.removed_entries + 1;
      if
        progress.removed_entries = progress.total_entries
        || progress.removed_entries mod progress.step = 0
      then
        eprint "."

let close_cache_gc_progress = fun () ->
  match !cache_gc_progress with
  | None -> ()
  | Some _ ->
      eprintln "";
      cache_gc_progress := None

let write_cache_gc_event = fun ~mode event ->
  match mode with
  | Json -> write_serde_json_event Riot_store.Cache_gc.event_serializer event
  | Human -> (
      match event with
      | Riot_store.Cache_gc.GcStarted { trigger = Riot_store.Cache_gc.Manual } ->
          out_status
            Ui.Running
            "running tracked cache GC (build root kept; use --force to remove it)"
      | Riot_store.Cache_gc.GcStarted { trigger = Riot_store.Cache_gc.Post_build } -> ()
      | Riot_store.Cache_gc.GcCacheScanStarted { trigger = Riot_store.Cache_gc.Manual; build_root } ->
          out_status
            Ui.Running
            ("scanning tracked cache entries under " ^ Path.to_string build_root)
      | Riot_store.Cache_gc.GcCacheScanStarted { trigger = Riot_store.Cache_gc.Post_build; _ } -> ()
      | Riot_store.Cache_gc.GcCacheEntryScanStarted { trigger = Riot_store.Cache_gc.Manual; _ } ->
          ()
      | Riot_store.Cache_gc.GcCacheEntryScanStarted { trigger = Riot_store.Cache_gc.Post_build; _ } ->
          ()
      | Riot_store.Cache_gc.GcCacheEntryScanned { trigger = Riot_store.Cache_gc.Manual; _ } -> ()
      | Riot_store.Cache_gc.GcCacheEntryScanned { trigger = Riot_store.Cache_gc.Post_build; _ } ->
          ()
      | Riot_store.Cache_gc.GcCacheScanCompleted {
          trigger = Riot_store.Cache_gc.Manual;
          entry_count;
          total_size_bytes;
        } ->
          out_status
            Ui.Success
            ("found "
            ^ Int.to_string entry_count
            ^ " tracked cache entries ("
            ^ size_to_string total_size_bytes
            ^ ")")
      | Riot_store.Cache_gc.GcCacheScanCompleted { trigger = Riot_store.Cache_gc.Post_build; _ } ->
          ()
      | Riot_store.Cache_gc.GcPlanComputed {
          trigger = Riot_store.Cache_gc.Manual;
          deleted_entries;
          deleted_generations;
          reclaimable_bytes;
        } ->
          out_status
            Ui.Running
            ("removing "
            ^ Int.to_string deleted_entries
            ^ " cache entries and "
            ^ Int.to_string deleted_generations
            ^ " generations; reclaiming "
            ^ size_to_string reclaimable_bytes);
          start_cache_gc_progress deleted_entries
      | Riot_store.Cache_gc.GcPlanComputed { trigger = Riot_store.Cache_gc.Post_build; _ } -> ()
      | Riot_store.Cache_gc.GcCacheEntryDeleteStarted { trigger = Riot_store.Cache_gc.Manual; _ } ->
          tick_cache_gc_progress ()
      | Riot_store.Cache_gc.GcCacheEntryDeleteStarted { trigger = Riot_store.Cache_gc.Post_build; _ } ->
          ()
      | Riot_store.Cache_gc.GcGenerationDeleteStarted { trigger = Riot_store.Cache_gc.Manual; _ } ->
          ()
      | Riot_store.Cache_gc.GcGenerationDeleteStarted { trigger = Riot_store.Cache_gc.Post_build; _ } ->
          ()
      | Riot_store.Cache_gc.GcSkipped { trigger = Riot_store.Cache_gc.Post_build; _ } -> ()
      | Riot_store.Cache_gc.GcSkipped { summary; _ } ->
          out_status
            Ui.Skipped
            ("tracked cache is already within policy ("
            ^ size_to_string summary.size_after_bytes
            ^ "). Build root kept; use --force to remove it.")
      | Riot_store.Cache_gc.GcCompleted { summary; _ } ->
          close_cache_gc_progress ();
          out_status
            Ui.Success
            ("cleaned tracked cache: " ^ format_cache_gc_cleanup summary ^ ". Build root kept.")
      | Riot_store.Cache_gc.GcFailed { error; _ } ->
          close_cache_gc_progress ();
          out_status Ui.Error ("cache GC failed: " ^ error)
      | Riot_store.Cache_gc.ForceCleanStarted { build_root } ->
          out_status Ui.Running ("removing build root " ^ Path.to_string build_root)
      | Riot_store.Cache_gc.ForceCleanCompleted { build_root } ->
          out_status Ui.Success ("removed build root " ^ Path.to_string build_root)
      | Riot_store.Cache_gc.ForceCleanFailed { build_root; error } ->
          out_status
            Ui.Error
            ("failed to remove build root " ^ Path.to_string build_root ^ ": " ^ error)
    )

let write_build_event_with_renderer = fun
  ?render_state ?profile ?human_renderer ~mode ~seen_registry_updates event ->
  Option.for_each
    human_renderer
    ~fn:(fun renderer ->
      match renderer with
      | BuildDashboard dashboard -> dashboard.state <- build_dashboard_update dashboard.state event
      | LoggedBuildEvents -> ());
  match event with
  | Riot_build.Event.Pm event ->
      Option.for_each human_renderer ~fn:human_renderer_clear;
      write_pm_event ~mode ~seen_registry_updates event
  | Riot_build.Event.BuildingTarget { target; host } -> (
      match (mode, human_renderer) with
      | (Human, Some (BuildDashboard _)) -> ()
      | _ ->
          Option.for_each human_renderer ~fn:human_renderer_clear;
          write_building_target_event ~mode ~target ~host
    )
  | Riot_build.Event.CacheGc event ->
      Option.for_each human_renderer ~fn:human_renderer_clear;
      write_cache_gc_event ~mode event
  | Riot_build.Event.Telemetry event ->
      write_build_telemetry_event ?render_state ?profile ?human_renderer ~mode event
  | Riot_build.Event.Phase phase ->
      write_build_phase_event_with_renderer ?render_state ?human_renderer ~mode phase

let write_build_phase_event = fun ?render_state ~mode phase ->
  write_build_phase_event_with_renderer
    ?render_state
    ~mode
    phase

let write_build_event = fun ?render_state ?profile ~mode ~seen_registry_updates event ->
  write_build_event_with_renderer
    ?render_state
    ?profile
    ~mode
    ~seen_registry_updates
    event

let create_event_renderer = fun ?profile ~mode () ->
  let profile_name = Option.map profile ~fn:(fun (profile: Riot_model.Profile.t) -> profile.name) in
  let render_state = create_render_state ?profile:profile_name () in
  let human_renderer =
    match mode with
    | Human -> Some (create_human_build_renderer ?profile:profile_name ())
    | Json -> None
  in
  {
    mode;
    profile = profile_name;
    render_state;
    human_renderer;
    seen_registry_updates = HashSet.create ();
  }

let finish_event_renderer = fun renderer ->
  Option.for_each renderer.human_renderer ~fn:human_renderer_clear

let event_stream_finished = fun __tmp1 ->
  match __tmp1 with
  | Riot_build.Event.Phase (Riot_build.Event.ReturningResults _) -> true
  | Riot_build.Event.Telemetry (Build_telemetry.BuildFailed _) -> true
  | _ -> false

let render_event = fun renderer event ->
  write_build_event_with_renderer
    ~render_state:renderer.render_state
    ?profile:renderer.profile
    ?human_renderer:renderer.human_renderer
    ~mode:renderer.mode
    ~seen_registry_updates:renderer.seen_registry_updates
    event;
  if event_stream_finished event then
    finish_event_renderer renderer

let write_package_not_found_error = fun ~mode ~package_name ~available_packages ->
  let package_name = Riot_model.Package_name.to_string package_name in
  let available_packages = List.map available_packages ~fn:Riot_model.Package_name.to_string in
  if mode = Json then
    write_json_event
      (command_error_event_to_json
        "PackageNotFound"
        [
          ("package_name", Data.Json.String package_name);
          (
            "available_packages",
            Data.Json.Array (List.map available_packages ~fn:(fun pkg -> Data.Json.String pkg))
          );
        ])
  else (
    out_status Ui.Error ("package '" ^ package_name ^ "' not found");
    out "";
    out "Available packages:";
    List.for_each available_packages ~fn:(fun pkg -> out (Jollyroger.Layout.bullet ~indent:2 pkg))
  )

let write_packages_not_found_error = fun ~mode ~package_names ~available_packages ->
  let package_names = List.map package_names ~fn:Riot_model.Package_name.to_string in
  let available_packages = List.map available_packages ~fn:Riot_model.Package_name.to_string in
  if mode = Json then
    write_json_event
      (command_error_event_to_json
        "PackagesNotFound"
        [
          (
            "package_names",
            Data.Json.Array (List.map package_names ~fn:(fun pkg -> Data.Json.String pkg))
          );
          (
            "available_packages",
            Data.Json.Array (List.map available_packages ~fn:(fun pkg -> Data.Json.String pkg))
          );
        ])
  else (
    out_status Ui.Error ("packages not found: " ^ String.concat ", " package_names);
    out "";
    out "Available packages:";
    List.for_each available_packages ~fn:(fun pkg -> out (Jollyroger.Layout.bullet ~indent:2 pkg))
  )

let write_build_error = fun ~mode err ->
  match err with
  | Riot_build.TargetSelectionFailed { pattern; available_targets } ->
      write_command_error
        ~mode
        "NoTargetsMatched"
        [
          ("pattern", Data.Json.String pattern);
          (
            "available_targets",
            Data.Json.Array (List.map
              available_targets
              ~fn:(fun target -> Data.Json.String (Riot_model.Target.to_string target)))
          );
        ]
        (Riot_build.error_message err)
  | Riot_build.PackageNotFound { package_name; available_packages } ->
      write_package_not_found_error ~mode ~package_name ~available_packages
  | Riot_build.PackagesNotFound { package_names; available_packages } ->
      write_packages_not_found_error ~mode ~package_names ~available_packages
  | Riot_build.ToolchainInstallFailed { target; error } ->
      write_command_error
        ~mode
        "ToolchainInstallFailed"
        [
          ("target", Data.Json.String (Riot_model.Target.to_string target));
          ("reason", Data.Json.String (Riot_build.toolchain_install_error_message error));
        ]
        (Riot_build.error_message err)
  | Riot_build.ToolchainInitializationFailed { target; error } ->
      write_command_error
        ~mode
        "ToolchainInitializationFailed"
        [
          ("target", Data.Json.String (Riot_model.Target.to_string target));
          ("reason", Data.Json.String (Riot_build.toolchain_initialization_error_message error));
        ]
        (Riot_build.error_message err)
  | Riot_build.BuildFailed { errors } -> write_build_failed_error ~mode errors
  | Riot_build.BuildUnitPlanningFailed planning_error -> (
      match mode with
      | Json ->
          write_command_error
            ~mode
            "BuildUnitPlanningFailed"
            [ ("reason", Data.Json.String (Riot_build.error_message err)); ]
            (Riot_build.error_message err)
      | Human ->
          build_unit_planning_error_lines planning_error
          |> List.for_each ~fn:out
    )
  | Riot_build.CycleDetected { cycle_nodes } ->
      write_command_error
        ~mode
        "CycleDetected"
        [ ("cycle_nodes", Data.Json.Array (List.map cycle_nodes ~fn:Data.Json.string)); ]
        (Riot_build.error_message err)
  | Riot_build.BuildAlreadyRunning { lock_path } ->
      write_command_error
        ~mode
        "BuildAlreadyRunning"
        [ ("lock_path", Data.Json.String (Path.to_string lock_path)); ]
        (Riot_build.error_message err)
  | Riot_build.InvalidRequestedParallelism value ->
      write_command_error
        ~mode
        "InvalidRequestedParallelism"
        [ ("value", Data.Json.Int value); ]
        (Riot_build.error_message err)
  | Riot_build.UnexpectedError { reason } ->
      write_command_error ~mode "UnexpectedError" [ ("reason", Data.Json.String reason); ] reason

let should_build_fix_provider_runner = fun (request: request) ->
  match request.scope with
  | Runtime -> false
  | Dev ->
      request.dev_artifacts.tests && request.dev_artifacts.examples && request.dev_artifacts.benches

let workspace_fix_providers = fun (workspace: Riot_model.Workspace.t) ->
  workspace.packages
  |> List.filter ~fn:Riot_model.Package_manifest.is_workspace_member
  |> List.map ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.fix_providers)
  |> List.concat

let workspace_member_package_names = fun (workspace: Riot_model.Workspace.t) ->
  workspace.packages
  |> List.filter ~fn:Riot_model.Package_manifest.is_workspace_member
  |> List.map ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.name)

let selected_fix_providers = fun (request: request) ->
  let providers = workspace_fix_providers request.workspace in
  match request.packages with
  | [] -> providers
  | selected ->
      providers
      |> List.filter
        ~fn:(fun (provider: Riot_model.Fix_provider.t) ->
          List.any
            selected
            ~fn:(fun package_name ->
              Riot_model.Package_name.equal
                provider.package_name
                package_name))

let run_request = fun (request: request) ->
  trace_build
    (
      "run_request request=" ^ build_request_label request ^ " scope=" ^ match request.scope with
      | Runtime -> "runtime"
      | Dev ->
          "dev" ^ " mode=" ^ match request.output_mode with
          | Human -> "human"
          | Json -> "json"
    );
  let seen_registry_updates = HashSet.create () in
  let start_time = Time.Instant.now () in
  reset_json_clock ~started_at:start_time;
  let progress = {
    built_count = 0;
    cached_count = 0;
    failed_count = 0;
    skipped_count = 0;
  }
  in
  let render_state = create_render_state ~profile:request.profile.name () in
  let human_renderer =
    match request.output_mode with
    | Human -> Some (create_human_build_renderer ~profile:request.profile.name ())
    | Json -> None
  in
  let attempted_build = ref false in
  let pm_session_id = Riot_model.Session_id.make () in
  let emit_pm_kind kind =
    Option.for_each human_renderer ~fn:human_renderer_clear;
    write_pm_event
      ~mode:request.output_mode
      ~seen_registry_updates
      (Riot_model.Event.create ~session_id:pm_session_id ~level:Riot_model.Event.Info kind)
  in
  let on_build_event event =
    match event with
    | Riot_build.Event.Pm kind -> emit_pm_kind kind.kind
    | _ ->
        attempted_build := true;
        record_build_event_progress progress event;
        write_build_event_with_renderer
          ~render_state
          ?human_renderer
          ~mode:request.output_mode
          ~seen_registry_updates
          event
  in
  let build_request = fun
    ?(synthetic_tools = []) ~workspace ~packages ~targets ~scope ~dev_artifacts ~profile () ->
    Riot_build.build
      ~on_event:on_build_event
      (Riot_build.Request.make
        ~workspace
        ~packages
        ~targets
        ~scope
        ~dev_artifacts
        ~profile
        ~synthetic_tools
        ~requested_parallelism:request.requested_parallelism
        ())
    |> Result.map ~fn:(fun _output -> ())
  in
  let fix_provider_runner_plan = fun () ->
    let providers = selected_fix_providers request in
    if List.is_empty providers then
      None
    else
      Some (
        Riot_fix.Fixme_runner.materialize
          ~workspace_root:request.workspace.root
          ~target_dir_root:request.workspace.target_dir_root
          providers
      )
  in
  let runner_plan =
    if should_build_fix_provider_runner request then
      fix_provider_runner_plan ()
    else
      None
  in
  let build_workspace =
    match runner_plan with
    | Some plan -> Riot_fix.Fixme_runner.attach_to_workspace request.workspace plan
    | None -> request.workspace
  in
  let build_packages =
    match (runner_plan, request.packages) with
    | (Some _, []) -> workspace_member_package_names request.workspace
    | _ -> request.packages
  in
  let synthetic_tools =
    match runner_plan with
    | Some plan ->
        [
          Riot_planner.Build_unit_graph.{
            package = plan.package_name;
            name = plan.binary_name;
          };
        ]
    | None -> []
  in
  let result =
    build_request
      ~workspace:build_workspace
      ~packages:build_packages
      ~targets:request.targets
      ~scope:request.scope
      ~dev_artifacts:request.dev_artifacts
      ~profile:request.profile
      ~synthetic_tools
      ()
    |> Result.map_err
      ~fn:(fun err ->
        Option.for_each human_renderer ~fn:human_renderer_clear;
        write_build_error ~mode:request.output_mode err;
        Failure (Riot_build.error_message err))
  in
  if request.show_finished_summary && !attempted_build then (
    match request.output_mode with
    | Json -> ()
    | Human ->
        Option.for_each human_renderer ~fn:human_renderer_clear;
        let duration = Time.Instant.duration_since ~earlier:start_time (Time.Instant.now ()) in
        let formatted_duration = Time.Duration.to_secs_string ~precision:2 duration in
        if progress.failed_count = 0 && progress.skipped_count = 0 then
          out_status
            Ui.Success
            ("finished in "
            ^ formatted_duration
            ^ "s ("
            ^ build_count_summary
              ~built_count:progress.built_count
              ~cached_count:progress.cached_count
              ~skipped_count:progress.skipped_count
              ~failed_count:progress.failed_count
              ()
            ^ ")")
        else if progress.failed_count > 0 then
          out_status
            Ui.Error
            ("finished in "
            ^ formatted_duration
            ^ "s ("
            ^ build_count_summary
              ~built_count:progress.built_count
              ~cached_count:progress.cached_count
              ~skipped_count:progress.skipped_count
              ~failed_count:progress.failed_count
              ()
            ^ ")")
        else
          out_status
            Ui.Warning
            ("finished in "
            ^ formatted_duration
            ^ "s ("
            ^ build_count_summary
              ~built_count:progress.built_count
              ~cached_count:progress.cached_count
              ~skipped_count:progress.skipped_count
              ~failed_count:progress.failed_count
              ()
            ^ ")")
  );
  if request.show_finished_summary && !attempted_build then
    trace_build_probe ~started_at:start_time "summary-finished";
  match result with
  | Ok _ ->
      trace_build_probe ~started_at:start_time "run-request-return-ok";
      Ok ()
  | Error err ->
      trace_build_probe
        ~started_at:start_time
        ("run-request-return-error reason=" ^ Exception.to_string err);
      Error err

let print_workspace_load_errors = fun errors ->
  List.for_each
    errors
    ~fn:(fun err -> out_status Ui.Error (Workspace_manager.load_error_to_string err))

type loaded_workspace = {
  workspace: Workspace.t;
}

let load_workspace_strict = fun cwd ->
  let workspace_manager = Workspace_manager.create () in
  let* registry =
    Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:"pkgs.ml" ()
    |> Result.map_err ~fn:(fun err -> Failure (Pkgs_ml.Registry_cache.create_error_message err))
  in
  match Workspace_manager.scan workspace_manager cwd with
  | Error err -> Error (Failure (Workspace_manager.scan_error_message err))
  | Ok (_workspace, load_errors) when List.length load_errors > 0 ->
      print_workspace_load_errors load_errors;
      Error (Failure "Workspace load failed")
  | Ok (workspace, _) ->
      let* workspace =
        Riot_deps.ensure_workspace
          ~workspace_manager
          ~mode:Riot_deps.Dep_solver.Refresh
          ~registry
          ~workspace
          ()
        |> Result.map_err ~fn:(fun err -> Failure (Riot_model.Pm_error.message err))
      in
      Ok { workspace }

let build_command = fun
  ~workspace
  ?(scope = Runtime)
  ?(dev_artifacts = {tests = true; examples = true; benches = true})
  ?(profile = "debug")
  ?(mode = Human)
  ?(show_finished_summary = true)
  ?(requested_parallelism = None)
  package_opt
  target_arch ->
  let packages =
    package_opt
    |> Option.to_list
  in
  run_request
    (
      make_request
        ~workspace
        ~scope
        ~dev_artifacts
        ~profile:(
          match profile with
          | "release" -> Riot_model.Profile.release
          | "fuzz" -> Riot_model.Profile.fuzz
          | _ -> Riot_model.Profile.debug
        )
        ~mode
        ~show_finished_summary
        ~requested_parallelism
        ~packages
        ~targets:(
          match target_arch with
          | Some target -> Riot_model.Target.parse target
          | None -> Riot_model.Target.Host
        )
        ()
    )

let build_packages_command = fun
  ~workspace
  ?(scope = Runtime)
  ?(dev_artifacts = {tests = true; examples = true; benches = true})
  ?(mode = Human)
  ?(show_finished_summary = true)
  ?(requested_parallelism = None)
  package_names
  target_arch ->
  match parse_package_names package_names with
  | Error _ as err -> err
  | Ok packages ->
      run_request
        (
          make_request
            ~workspace
            ~scope
            ~dev_artifacts
            ~mode
            ~show_finished_summary
            ~requested_parallelism
            ~packages
            ~targets:(
              match target_arch with
              | Some target -> Riot_model.Target.parse target
              | None -> Riot_model.Target.Host
            )
            ()
        )

let run = fun ~workspace matches ->
  match request_of_matches ~workspace matches with
  | Error _ as err -> err
  | Ok request -> run_request request
