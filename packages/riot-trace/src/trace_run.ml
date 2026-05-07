open Std
open Std.Result.Syntax

type perf_options = Riot_model.Workspace_operational_config.perf_trace_policy = {
  sample_rate_hz: int option;
  call_graph: string option;
  call_graph_stack_size: int option;
}
type xctrace_options = Riot_model.Workspace_operational_config.xctrace_trace_policy = {
  template_: string option;
  time_limit: string option;
  window: string option;
}
type trace_options = {
  perf: perf_options;
  xctrace: xctrace_options;
}
type output_policy =
  | Fail_if_exists
  | Overwrite
  | Append

type trace_request = {
  output: Path.t;
  output_policy: output_policy;
  profiler: Profiler.t;
  options: trace_options;
}

type run_request = {
  workspace: Riot_model.Workspace.t;
  package_name: Riot_model.Package_name.t option;
  binary_name: string;
  profile: string;
  trace: trace_request;
  args: string list;
}

type source_run_request = {
  source_spec: string;
  binary_name: string;
  profile: string;
  trace: trace_request;
  update: bool;
  args: string list;
}

type event =
  | Build of Riot_build.Event.t
  | TracingBinary of {
      package: Riot_model.Package_name.t;
      binary: string;
      profiler: string;
      output: Path.t;
    }

type error =
  | Run of Riot_run.run_error
  | ProfilerUnavailable of {
      profiler: string;
      reason: string;
    }
  | UnsupportedProfilerOption of {
      profiler: string;
      option: string;
      reason: string;
    }
  | OutputAlreadyExists of Path.t
  | ProcessExited of int
  | SystemError of string

let no_event: event -> unit = fun _ -> ()

let default_options = {
  perf = Riot_model.Workspace_operational_config.default_perf_trace_policy;
  xctrace = Riot_model.Workspace_operational_config.default_xctrace_trace_policy;
}

let unavailable_error = fun (unavailable: Profiler.unavailable) ->
  ProfilerUnavailable {
    profiler = unavailable.profiler;
    reason = unavailable.reason;
  }

let error_message = fun __tmp1 ->
  match __tmp1 with
  | Run err -> Riot_run.run_error_message err
  | ProfilerUnavailable { profiler; reason } ->
      "profiler '" ^ profiler ^ "' is unavailable: " ^ reason
  | UnsupportedProfilerOption { profiler; option; reason } ->
      "profiler '" ^ profiler ^ "' does not support " ^ option ^ ": " ^ reason
  | OutputAlreadyExists output ->
      "trace output already exists: "
      ^ Path.to_string output
      ^ " (pass --force to replace it or --append to add a run when supported)"
  | ProcessExited code -> "process exited with " ^ Int.to_string code
  | SystemError msg -> msg

let event_to_json = fun __tmp1 ->
  match __tmp1 with
  | Build event -> Riot_build.Event.to_json event
  | TracingBinary { package; binary; profiler; output } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "TracingBinary");
        ("package", Data.Json.String (Riot_model.Package_name.to_string package));
        ("binary", Data.Json.String binary);
        ("profiler", Data.Json.String profiler);
        ("output", Data.Json.String (Path.to_string output));
      ])

let ensure_trace_output_parent = fun output ->
  match Path.parent output with
  | None -> Ok ()
  | Some parent ->
      Fs.create_dir_all parent
      |> Result.map_err
        ~fn:(fun err ->
          SystemError
            ("failed to create trace output directory '"
            ^ Path.to_string parent
            ^ "': "
            ^ IO.error_message err))

let option_arg = fun name value ->
  match value with
  | None -> []
  | Some value -> [ name; value ]

let perf_frequency = fun (options: perf_options) ->
  options.sample_rate_hz
  |> Option.unwrap_or ~default:99
  |> Int.to_string

let perf_call_graph = fun (options: perf_options) ->
  let call_graph =
    options.call_graph
    |> Option.unwrap_or ~default:"dwarf"
  in
  match options.call_graph_stack_size with
  | None -> call_graph
  | Some stack_size -> call_graph ^ "," ^ Int.to_string stack_size

let xctrace_template = fun (options: xctrace_options) ->
  options.template_
  |> Option.unwrap_or ~default:"Time Profiler"

let inspect_output_exists = fun output ->
  Fs.exists output
  |> Result.map_err
    ~fn:(fun err ->
      SystemError
        ("failed to inspect trace output path '"
        ^ Path.to_string output
        ^ "': "
        ^ IO.error_message err))

let remove_existing_output = fun output ->
  let remove metadata =
    if Fs.Metadata.is_dir metadata then
      Fs.remove_dir_all output
    else
      Fs.remove_file output
  in
  Fs.symlink_metadata output
  |> Result.and_then ~fn:remove
  |> Result.map_err
    ~fn:(fun err ->
      SystemError
        ("failed to remove existing trace output '"
        ^ Path.to_string output
        ^ "': "
        ^ IO.error_message err))

let prepare_trace_output = fun ~profiler trace ->
  let* exists = inspect_output_exists trace.output in
  match trace.output_policy with
  | Fail_if_exists ->
      if exists then
        Error (OutputAlreadyExists trace.output)
      else
        Ok false
  | Overwrite ->
      if exists then
        let* () = remove_existing_output trace.output in
        Ok false
      else
        Ok false
  | Append -> (
      match profiler with
      | Profiler.Xctrace -> Ok exists
      | Profiler.Perf ->
          Error
            (UnsupportedProfilerOption {
              profiler = "perf";
              option = "--append";
              reason = "perf traces are written as a single perf.data file";
            })
      | Profiler.Auto ->
          Error (ProfilerUnavailable { profiler = "auto"; reason = "unresolved profiler" })
    )

let ensure_profiler_supported_on_host = fun profiler ->
  let host = Riot_model.Riot_dirs.host_target () in
  match profiler with
  | Profiler.Perf when not (String.equal host.Riot_model.Target.os "linux") ->
      Error
        (ProfilerUnavailable {
          profiler = "perf";
          reason = "perf recording is only supported on Linux hosts in this prototype";
        })
  | Profiler.Xctrace when not (String.equal host.Riot_model.Target.os "darwin") ->
      Error
        (ProfilerUnavailable {
          profiler = "xctrace";
          reason = "xctrace recording is only supported on Darwin hosts";
        })
  | Profiler.Auto ->
      Error (ProfilerUnavailable { profiler = "auto"; reason = "unresolved profiler" })
  | Profiler.Perf
  | Profiler.Xctrace -> Ok ()

let preflight_output = fun ~profiler trace ->
  match trace.output_policy with
  | Fail_if_exists ->
      let* exists = inspect_output_exists trace.output in
      if exists then
        Error (OutputAlreadyExists trace.output)
      else
        Ok ()
  | Append -> (
      match profiler with
      | Profiler.Xctrace -> Ok ()
      | Profiler.Perf ->
          Error
            (UnsupportedProfilerOption {
              profiler = "perf";
              option = "--append";
              reason = "perf traces are written as a single perf.data file";
            })
      | Profiler.Auto ->
          Error (ProfilerUnavailable { profiler = "auto"; reason = "unresolved profiler" })
    )
  | Overwrite -> Ok ()

let preflight = fun (trace: trace_request) ->
  let* profiler =
    Profiler.effective trace.profiler
    |> Result.map_err ~fn:unavailable_error
  in
  let* () = ensure_profiler_supported_on_host profiler in
  preflight_output ~profiler trace

let profiler_command = fun ~(binary_path:Path.t) ~(args:string list) (trace:trace_request) ->
  let* () = ensure_trace_output_parent trace.output in
  let output = Path.to_string trace.output in
  let* profiler =
    Profiler.effective trace.profiler
    |> Result.map_err ~fn:unavailable_error
  in
  let* () = ensure_profiler_supported_on_host profiler in
  let* output_exists = prepare_trace_output ~profiler trace in
  match profiler with
  | Profiler.Perf ->
      Ok (
        Profiler.to_string profiler,
        Command.make
          "perf"
          ~args:(
            [
              "record";
              "--freq";
              perf_frequency trace.options.perf;
              "--call-graph";
              perf_call_graph trace.options.perf;
              "--output";
              output;
              "--";
              Path.to_string binary_path;
            ]
            @ args
          )
      )
  | Profiler.Xctrace ->
      let append_args =
        if output_exists then
          [ "--append-run" ]
        else
          []
      in
      Ok (
        Profiler.to_string profiler,
        Command.make
          "xcrun"
          ~args:(
            [
              "xctrace";
              "record";
              "--template";
              xctrace_template trace.options.xctrace;
              "--output";
              output;
            ]
            @ option_arg "--time-limit" trace.options.xctrace.time_limit
            @ option_arg "--window" trace.options.xctrace.window
            @ append_args
            @ [
              "--no-prompt";
              "--target-stdout";
              "-";
              "--launch";
              "--";
              Path.to_string binary_path;
            ]
            @ args
          )
      )
  | Profiler.Auto ->
      Error (ProfilerUnavailable { profiler = "auto"; reason = "unresolved profiler" })

let run_built_binary = fun ~on_event ~(trace:trace_request) (built:Riot_run.built_binary) ->
  let* (profiler, cmd) = profiler_command ~binary_path:built.path ~args:built.args trace in
  on_event
    (TracingBinary {
      package = built.package_name;
      binary = built.binary_name;
      profiler;
      output = trace.output;
    });
  match Command.status cmd with
  | Ok 0 -> Ok ()
  | Ok code -> Error (ProcessExited code)
  | Error (Command.SystemError msg) -> Error (SystemError msg)

let bridge_run_event = fun ~on_event __tmp1 ->
  match __tmp1 with
  | Riot_run.Build event -> on_event (Build event)
  | Riot_run.RunningBinary _ -> ()

let run = fun ?(on_event = no_event) (request: run_request) ->
  let* built =
    Riot_run.build_binary
      ~on_event:(bridge_run_event ~on_event)
      {
        workspace = request.workspace;
        package_name = request.package_name;
        binary_name = request.binary_name;
        profile = request.profile;
        args = request.args;
      }
    |> Result.map_err ~fn:(fun err -> Run err)
  in
  run_built_binary ~on_event ~trace:request.trace built

let run_source = fun ?(on_event = no_event) (request: source_run_request) ->
  let* built =
    Riot_run.build_source_binary
      ~on_event:(bridge_run_event ~on_event)
      {
        source_spec = request.source_spec;
        binary_name = request.binary_name;
        profile = request.profile;
        update = request.update;
        args = request.args;
      }
    |> Result.map_err ~fn:(fun err -> Run err)
  in
  run_built_binary ~on_event ~trace:request.trace built
