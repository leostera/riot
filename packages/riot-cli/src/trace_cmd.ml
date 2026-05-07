open Std
open Std.Result.Syntax
open Riot_model
open Riot_build
open ArgParser

module Trace_runtime = Riot_trace
module Run_runtime = Riot_run

let out = eprintln

let summary_command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "summary"
  |> about "Print flat trace summary tables"
  |> args
    [
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output";
      option "filter"
      |> short 'f'
      |> long "filter"
      |> value_name "GLOB"
      |> help "Only show frames whose function name matches GLOB";
      positional "path"
      |> help "Trace artifact path to summarize";
    ]

let call_tree_command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "call-tree"
  |> about "Print an inclusive trace call tree"
  |> args
    [
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output";
      option "filter"
      |> short 'f'
      |> long "filter"
      |> value_name "GLOB"
      |> help "Only show call-tree branches containing frames whose function name matches GLOB";
      positional "path"
      |> help "Trace artifact path to inspect";
    ]

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "trace"
  |> about "Run a binary under a profiler"
  |> ArgParser.allow_trailing_args
  |> ArgParser.allow_no_subcommand
  |> args
    [
      positional "name"
      |> required false
      |> help
        "Binary name or remote source to trace. Use -p/--package to disambiguate local binaries, or the legacy [package:]binary form";
      option "package"
      |> short 'p'
      |> long "package"
      |> help "Trace a binary from a specific package";
      flag "list"
      |> long "list"
      |> help "List runnable binaries in the current workspace";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output for --list";
      flag "release"
      |> long "release"
      |> help "Use the release build profile";
      option "output"
      |> short 'o'
      |> long "output"
      |> value_name "PATH"
      |> help "Write trace output to PATH";
      flag "force"
      |> long "force"
      |> help "Replace an existing trace output path";
      flag "append"
      |> long "append"
      |> help "Append a run to an existing trace output when supported";
      option "profiler"
      |> long "profiler"
      |> value_name "NAME"
      |> help "Profiler backend to use (auto, perf, xctrace)";
      option "sample-rate"
      |> long "sample-rate"
      |> value_name "HZ"
      |> help "Sampling frequency in hertz for profilers that expose it";
      option "time-limit"
      |> long "time-limit"
      |> value_name "DURATION"
      |> help "Limit recording time when the profiler supports it";
      option "window"
      |> long "window"
      |> value_name "DURATION"
      |> help "Keep only the final recording window when the profiler supports it";
      option "xctrace-template"
      |> long "xctrace-template"
      |> value_name "NAME_OR_PATH"
      |> help "xctrace template name or path";
      option "perf-call-graph"
      |> long "perf-call-graph"
      |> value_name "MODE"
      |> help "perf call graph mode";
      option "perf-call-graph-stack-size"
      |> long "perf-call-graph-stack-size"
      |> value_name "BYTES"
      |> help "perf DWARF call graph stack dump size";
      flag "update"
      |> long "update"
      |> help "Refresh a cached remote source before tracing";
      trailing "-- [args]..."
      |> help "Arguments to pass to the binary";
      flag "verbose"
      |> short 'v'
      |> long "verbose"
      |> help "Enable verbose output for trace"
      |> count;
    ]
  |> subcommand summary_command
  |> subcommand call_tree_command

let is_summary = fun matches ->
  match ArgParser.get_subcommand matches with
  | Some ("summary", _)
  | Some ("call-tree", _) -> true
  | _ -> false

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    "release"
  else
    "debug"

let trailing_args = fun matches ->
  let args = ArgParser.trailing_args matches in
  match args with
  | "--" :: rest -> rest
  | _ -> args

type target =
  | Local of {
      package_name: Riot_model.Package_name.t option;
      binary_name: string;
    }
  | Remote_source of { source_spec: string; binary_name: string }

type implicit_local_target = {
  package_name: Riot_model.Package_name.t;
  binary_name: string;
}

let no_runnable_binaries_message = fun ?package_name () ->
  let hint = "create one with `riot new --bin ./packages/my-binary`" in
  match package_name with
  | Some package_name ->
      "package '"
      ^ Riot_model.Package_name.to_string package_name
      ^ "' has no runnable binaries; "
      ^ hint
  | None -> "no runnable binaries found; pass a binary name or " ^ hint

let parse_package_name = fun package_name ->
  Riot_model.Package_name.from_string package_name
  |> Result.map_err
    ~fn:(fun error ->
      Failure ("invalid package name '"
      ^ package_name
      ^ "': "
      ^ Riot_model.Package_name.error_message error))

let parse_local_target = fun ?package_filter name ->
  match String.split name ~by:":" with
  | [ package_name; binary_name ] ->
      let* package_name = parse_package_name package_name in
      let* () =
        match package_filter with
        | Some expected_package when not
          (Riot_model.Package_name.equal expected_package package_name) ->
            Error (Failure ("conflicting package filters: got --package "
            ^ Riot_model.Package_name.to_string expected_package
            ^ " and binary target "
            ^ name))
        | _ -> Ok ()
      in
      Ok (Local { package_name = Some package_name; binary_name })
  | _ -> Ok (Local { package_name = package_filter; binary_name = name })

let split_remote_binary = fun raw ->
  match String.last_index raw '@' with
  | Some idx when idx = String.length raw - 1 ->
      Error (Failure ("invalid remote target '" ^ raw ^ "': expected binary name after @"))
  | Some idx when idx > 0 && idx < String.length raw - 1 ->
      Ok (
        String.sub raw ~offset:0 ~len:idx,
        Some (String.sub raw ~offset:(idx + 1) ~len:(String.length raw - idx - 1))
      )
  | _ -> Ok (raw, None)

let default_remote_binary_name = Run.default_remote_binary_name

let parse_target = fun ?package_filter name ->
  if Riot_deps.Git_dependency.looks_like_remote_spec name then
    match package_filter with
    | Some _ -> Error (Failure "--package cannot be used with remote source targets")
    | None -> (
        match split_remote_binary name with
        | Error _ as err -> err
        | Ok (source_spec, binary_name) ->
            Ok (Remote_source {
              source_spec;
              binary_name = Option.unwrap_or
                ~default:(default_remote_binary_name source_spec)
                binary_name;
            })
      )
  else
    parse_local_target ?package_filter name

let implicit_local_targets = fun ?package_filter (workspace: Riot_model.Workspace.t) ->
  let package_matches_filter (pkg: Riot_model.Package.t) =
    match package_filter with
    | Some expected_package -> Riot_model.Package_name.equal expected_package pkg.name
    | None -> true
  in
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Run workspace
  |> List.filter ~fn:Package.is_workspace_member
  |> List.filter ~fn:package_matches_filter
  |> List.flat_map
    ~fn:(fun (pkg: Riot_model.Package.t) ->
      Riot_model.Package.binaries_for_scope Riot_model.Package.Normal pkg
      |> List.map
        ~fn:(fun (bin: Riot_model.Package.binary) -> {
          package_name = pkg.name;
          binary_name = bin.name;
        }))

let resolve_implicit_local_target = fun ?package_filter (workspace: Riot_model.Workspace.t) ->
  match implicit_local_targets ?package_filter workspace with
  | [ { package_name; binary_name } ] -> Ok { package_name; binary_name }
  | [] -> (
      match package_filter with
      | Some package_name -> Error (no_runnable_binaries_message ~package_name ())
      | None -> Error (no_runnable_binaries_message ())
    )
  | targets ->
      let rendered =
        targets
        |> List.map
          ~fn:(fun { package_name; binary_name } ->
            Riot_model.Package_name.to_string package_name ^ ":" ^ binary_name)
        |> String.concat ", "
      in
      Error ("multiple runnable binaries found; pass a binary name or --package (" ^ rendered ^ ")")

let optional_cli_string = fun matches name ->
  ArgParser.get_one matches name
  |> Option.and_then
    ~fn:(fun value ->
      let value = String.trim value in
      if String.equal value "" then
        None
      else
        Some value)

let parse_optional_positive_int_arg = fun matches name ->
  match ArgParser.get_one matches name with
  | None -> Ok None
  | Some value -> (
      match Int.parse value with
      | Some parsed when parsed > 0 -> Ok (Some parsed)
      | Some _
      | None -> Error (Failure ("invalid --" ^ name ^ " value: " ^ value))
    )

let output_policy_of_matches = fun matches ->
  let force = ArgParser.get_flag matches "force" in
  let append = ArgParser.get_flag matches "append" in
  match (force, append) with
  | (true, true) -> Error (Failure "--force and --append cannot be used together")
  | (true, false) -> Ok Trace_runtime.Overwrite
  | (false, true) -> Ok Trace_runtime.Append
  | (false, false) -> Ok Trace_runtime.Fail_if_exists

let trace_request_of_matches = fun ~binary_name ~(operational_config:Riot_model.Workspace_operational_config.t) matches ->
  let trace_config = operational_config.trace in
  let* profiler =
    match Option.or_ (optional_cli_string matches "profiler") trace_config.profiler with
    | None -> Ok Trace_runtime.Auto
    | Some profiler ->
        Trace_runtime.profiler_from_string profiler
        |> Result.map_err ~fn:(fun message -> Failure message)
  in
  let* sample_rate_hz = parse_optional_positive_int_arg matches "sample-rate" in
  let* perf_call_graph_stack_size =
    parse_optional_positive_int_arg matches "perf-call-graph-stack-size"
  in
  let* output_policy = output_policy_of_matches matches in
  let output =
    match ArgParser.get_one matches "output" with
    | None -> Ok (Trace_runtime.default_output_path ~binary_name)
    | Some output ->
        Path.from_string output
        |> Result.map_err ~fn:(fun _ -> Failure ("invalid --output path: " ^ output))
  in
  let options =
    Trace_runtime.{
      perf = {
        sample_rate_hz = Option.or_ sample_rate_hz trace_config.perf.sample_rate_hz;
        call_graph = Option.or_ (optional_cli_string matches "perf-call-graph") trace_config.perf.call_graph;
        call_graph_stack_size = Option.or_
          perf_call_graph_stack_size
          trace_config.perf.call_graph_stack_size;
      };
      xctrace = {
        template_ = Option.or_
          (optional_cli_string matches "xctrace-template")
          trace_config.xctrace.template_;
        time_limit = Option.or_
          (optional_cli_string matches "time-limit")
          trace_config.xctrace.time_limit;
        window = Option.or_
          (optional_cli_string matches "window")
          trace_config.xctrace.window;
      };
    }
  in
  Result.map output ~fn:(fun output -> Trace_runtime.{ output; output_policy; profiler; options })

let json_requested_for_child = fun args -> List.any args ~fn:(fun arg -> String.equal arg "--json")

let write_json_event = fun (json: Data.Json.t) -> println (Data.Json.to_string json)

let trace_error_to_json = fun (err: Trace_runtime.trace_error) ->
  let details =
    match err with
    | Trace_runtime.Run run_error ->
        [
          ("kind", Data.Json.String "run_error");
          ("reason", Data.Json.String (Run_runtime.run_error_message run_error));
        ]
    | Trace_runtime.ProfilerUnavailable { profiler; reason } ->
        [
          ("kind", Data.Json.String "profiler_unavailable");
          ("profiler", Data.Json.String profiler);
          ("reason", Data.Json.String reason);
        ]
    | Trace_runtime.UnsupportedProfilerOption { profiler; option; reason } ->
        [
          ("kind", Data.Json.String "unsupported_profiler_option");
          ("profiler", Data.Json.String profiler);
          ("option", Data.Json.String option);
          ("reason", Data.Json.String reason);
        ]
    | Trace_runtime.OutputAlreadyExists output ->
        [
          ("kind", Data.Json.String "output_already_exists");
          ("path", Data.Json.String (Path.to_string output));
        ]
    | Trace_runtime.ProcessExited status ->
        [
          ("kind", Data.Json.String "process_exited");
          ("status", Data.Json.String (Int.to_string status));
        ]
    | Trace_runtime.SystemError reason ->
        [ ("kind", Data.Json.String "system_error"); ("reason", Data.Json.String reason); ]
  in
  Data.Json.Object (("type", Data.Json.String "trace.error")
  :: ("message", Data.Json.String (Trace_runtime.trace_error_message err))
  :: details)

let write_trace_event = fun ~mode (event: Trace_runtime.trace_event) ->
  match mode with
  | Build.Json ->
      Trace_runtime.trace_event_to_json event
      |> Option.for_each ~fn:write_json_event
  | Build.Human -> (
      match event with
      | Trace_runtime.Build _ -> ()
      | Trace_runtime.TracingBinary { package; binary; profiler; output } ->
          out
            ("    \027[1;32mTracing\027[0m "
            ^ Riot_model.Package_name.to_string package
            ^ ":"
            ^ binary
            ^ " with "
            ^ profiler
            ^ " -> "
            ^ Path.to_string output)
    )

let write_trace_error = fun ~mode (err: Trace_runtime.trace_error) ->
  match mode with
  | Build.Json -> write_json_event (trace_error_to_json err)
  | Build.Human -> (
      match err with
      | Trace_runtime.ProcessExited _ -> ()
      | Trace_runtime.Run (Run_runtime.ProcessExited _) -> ()
      | err -> out ("error: " ^ Trace_runtime.trace_error_message err)
    )

let write_workspace_error = fun ~mode message ->
  match mode with
  | Build.Json ->
      write_json_event
        (Data.Json.Object [
          ("type", Data.Json.String "trace.error");
          ("kind", Data.Json.String "workspace_error");
          ("message", Data.Json.String message);
        ])
  | Build.Human -> out ("error: " ^ message)

let binary_source_label = fun
  ~(workspace:Riot_model.Workspace.t) (binary: Run_runtime.runnable_binary) ->
  match Path.strip_prefix binary.source_path ~prefix:workspace.root with
  | Ok relative_path -> Path.to_string relative_path
  | Error _ -> Path.to_string binary.source_path

let write_binary_list = fun ~(workspace:Riot_model.Workspace.t) binaries ->
  binaries
  |> List.for_each
    ~fn:(fun (binary: Run_runtime.runnable_binary) ->
      println
        (Riot_model.Package_name.to_string binary.package_name
        ^ ":"
        ^ binary.binary_name
        ^ " ("
        ^ binary_source_label ~workspace binary
        ^ ")"))

let write_binary_list_json = fun ~(workspace:Riot_model.Workspace.t) binaries ->
  let binary_kind (binary: Run_runtime.runnable_binary) =
    let path = binary_source_label ~workspace binary in
    if List.contains (String.split path ~by:"/") ~value:"examples" then
      "example"
    else
      "binary"
  in
  let binary_json (binary: Run_runtime.runnable_binary) =
    Data.Json.Object [
      ("kind", Data.Json.String (binary_kind binary));
      ("package", Data.Json.String (Riot_model.Package_name.to_string binary.package_name));
      ("binary", Data.Json.String binary.binary_name);
      ("path", Data.Json.String (binary_source_label ~workspace binary));
      (
        "selector",
        Data.Json.String (Riot_model.Package_name.to_string binary.package_name
        ^ ":"
        ^ binary.binary_name)
      );
    ]
  in
  write_json_event
    (Data.Json.Object [
      ("type", Data.Json.String "TraceList");
      ("binaries", Data.Json.Array (List.map binaries ~fn:binary_json));
    ])

let render_ms = fun weight_ns ->
  Float.to_string ~precision:2 (Float.from_int weight_ns /. 1_000_000.0) ^ "ms"

let render_duration_compact = fun weight_ns ->
  let ms = Float.from_int weight_ns /. 1_000_000.0 in
  if ms >= 1000.0 then
    Float.to_string ~precision:2 (ms /. 1000.0) ^ "s"
  else
    Float.to_string ~precision:2 ms ^ "ms"

let percent_value = fun ~total_weight_ns weight_ns ->
  if total_weight_ns <= 0 then
    0.0
  else
    Float.from_int weight_ns *. 100.0 /. Float.from_int total_weight_ns

let style_text = fun style text -> Tty.Style.styled style text

let trace_gray_style =
  Tty.Style.default
  |> Tty.Style.fg (Tty.Color.ansi256 245)

let percent_style = fun color ->
  let style = Tty.Style.default |> Tty.Style.bold in
  match color with
  | Some color -> style |> Tty.Style.fg color
  | None -> style

let percent_color = fun percent ->
  if percent > 60.0 then
    Some (Tty.Color.ansi 1)
  else if percent > 40.0 then
    Some (Tty.Color.ansi256 208)
  else if percent > 25.0 then
    Some (Tty.Color.ansi 3)
  else
    None

let color_percent = fun ~total_weight_ns weight_ns ->
  let percent = percent_value ~total_weight_ns weight_ns in
  let rendered = Float.to_string ~precision:1 percent ^ "%" in
  style_text (percent_style (percent_color percent)) rendered

let dim_gray = fun value -> style_text trace_gray_style value

let truncate_cell = fun ~width value ->
  if width <= 0 then
    ""
  else if String.width value <= width then
    value
  else if width <= 1 then
    String.truncate_width ~width:(Int.max 0 width) ~tail:"" value
  else
    String.truncate_width ~width ~tail:"~" value

let cell_left = fun width value ->
  truncate_cell ~width value
  |> String.pad_right ~width ' '

let cell_right = fun width value ->
  truncate_cell ~width value
  |> String.pad_left ~width ' '

let color_percent_cell = fun ~total_weight_ns ~width weight_ns ->
  let percent = percent_value ~total_weight_ns weight_ns in
  let rendered = Float.to_string ~precision:1 percent ^ "%" in
  style_text
    (percent_style (percent_color percent))
    (cell_right width rendered)

type frame_filter = {
  pattern: string;
  matcher: Glob.t;
}

let offset_suffix = fun offset ->
  Option.map offset ~fn:(fun offset -> " at offset " ^ Int.to_string offset)
  |> Option.unwrap_or ~default:""

let glob_error_message = fun __tmp1 ->
  match __tmp1 with
  | Glob.Empty -> "empty frame filter"
  | Glob.Invalid_glob { input; message; offset } ->
      "invalid frame filter '"
      ^ input
      ^ "': "
      ^ message
      ^ offset_suffix offset
  | Glob.Invalid_regex { message; offset } ->
      "invalid frame filter regex: "
      ^ message
      ^ offset_suffix offset

let frame_filter_from_string = fun pattern ->
  Glob.create [ pattern ]
  |> Result.map ~fn:(fun matcher -> { pattern; matcher })
  |> Result.map_err ~fn:(fun err -> Failure (glob_error_message err))

let frame_filter_of_matches = fun matches ->
  match ArgParser.get_one matches "filter" with
  | None -> Ok None
  | Some pattern -> frame_filter_from_string pattern |> Result.map ~fn:Option.some

let glob_matches = fun filter value ->
  match Glob.matches filter.matcher ~str:value with
  | Ok matched -> matched
  | Error _ -> false

let cost_matches_filter = fun filter (cost: Trace_runtime.call_cost) ->
  match filter with
  | None -> true
  | Some filter -> glob_matches filter cost.name

let filter_costs = fun filter costs ->
  List.filter costs ~fn:(cost_matches_filter filter)

let table_limit = 40

let summary_table_terminal_width = fun () ->
  match Tty.Size.get () with
  | Ok { cols; _ } -> Int.max 60 cols
  | Error _ -> 88

let summary_table_function_width = fun () ->
  let fixed_width = 35 in
  let available = summary_table_terminal_width () - fixed_width in
  Int.max 24 (Int.min 48 available)

let repeat_text = fun ~count text ->
  if count <= 0 then
    ""
  else
    List.init ~count ~fn:(fun _ -> text)
    |> String.concat ""

let summary_table_rule = fun ~left ~join ~right widths ->
  "  "
  ^ left
  ^ (
    widths
    |> List.map ~fn:(fun width -> repeat_text ~count:(width + 2) "─")
    |> String.concat join
  )
  ^ right

let summary_table_row = fun cells ->
  "  │ "
  ^ String.concat " │ " cells
  ^ " │"

let write_function_table = fun ~title ~total_weight_ns ~filter costs ->
  let costs =
    filter_costs filter costs
    |> List.take ~len:table_limit
  in
  let percent_width = 6 in
  let total_width = 9 in
  let samples_width = 5 in
  let function_width = summary_table_function_width () in
  let widths = [ percent_width; total_width; samples_width; function_width ] in
  println "";
  println title;
  if List.is_empty costs then
    println "  no sampled frames"
  else (
    println (summary_table_rule ~left:"┌" ~join:"┬" ~right:"┐" widths);
    println
      (summary_table_row [
        cell_right percent_width "%cpu";
        cell_right total_width "total";
        cell_right samples_width "samp";
        cell_left function_width "function";
      ]);
    println (summary_table_rule ~left:"├" ~join:"┼" ~right:"┤" widths);
    let rec loop index = fun __tmp1 ->
      match __tmp1 with
      | [] -> println (summary_table_rule ~left:"└" ~join:"┴" ~right:"┘" widths)
      | (cost: Trace_runtime.call_cost) :: rest ->
          println
            (summary_table_row [
              color_percent_cell
                ~total_weight_ns
                ~width:percent_width
                cost.total_weight_ns;
              cell_right total_width (render_duration_compact cost.total_weight_ns);
              cell_right samples_width (Int.to_string cost.samples);
              cell_left function_width cost.name;
            ]);
          loop (index + 1) rest
    in
    loop 1 costs
  )

let rec filter_call_tree_node = fun filter (node: Trace_runtime.call_tree_node) ->
  if glob_matches filter node.name then
    Some node
  else
    let children = List.filter_map node.children ~fn:(filter_call_tree_node filter) in
    if List.is_empty children then
      None
    else
      Some { node with children; hidden_children = 0 }

let filter_call_tree = fun filter (profile: Trace_runtime.profile_summary) ->
  match filter with
  | None -> (profile.call_tree, profile.hidden_call_tree_roots)
  | Some filter ->
      (
        List.filter_map profile.call_tree ~fn:(filter_call_tree_node filter),
        0
      )

let rec write_call_tree_node = fun ~root_total_ns ~prefix ~is_last (node: Trace_runtime.call_tree_node) ->
  let branch =
    if is_last then
      "└── "
    else
      "├── "
  in
  println
    (prefix
    ^ branch
    ^ truncate_cell ~width:84 node.name
    ^ " "
    ^ color_percent ~total_weight_ns:root_total_ns node.total_weight_ns
    ^ " "
    ^ dim_gray
      ("[total="
      ^ render_ms node.total_weight_ns
      ^ " self="
      ^ render_ms node.self_weight_ns
      ^ "]"));
  let next_prefix =
    prefix
    ^
    if is_last then
      "    "
    else
      "│   "
  in
  let child_count = List.length node.children in
  let rec loop index = fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | child :: rest ->
        let last_child = Int.equal index child_count && Int.equal node.hidden_children 0 in
        write_call_tree_node ~root_total_ns ~prefix:next_prefix ~is_last:last_child child;
        loop (index + 1) rest
  in
  loop 1 node.children;
  if node.hidden_children > 0 then
    println (next_prefix ^ "└── ... " ^ Int.to_string node.hidden_children ^ " more")

let write_call_tree = fun ~filter (profile: Trace_runtime.profile_summary) ->
  let (call_tree, hidden_call_tree_roots) = filter_call_tree filter profile in
  println "";
  println "call tree";
  println ("  total_cpu_ms=" ^ Float.to_string ~precision:2 (Trace_runtime.Profile.weight_ms profile.total_weight_ns));
  if List.is_empty call_tree then
    match filter with
    | Some filter -> println ("  no frames matched " ^ filter.pattern)
    | None -> println "  no sampled frames"
  else
    let root_count = List.length call_tree in
    let rec loop index = fun __tmp1 ->
      match __tmp1 with
      | [] -> ()
      | node :: rest ->
          let is_last = Int.equal index root_count && Int.equal hidden_call_tree_roots 0 in
          write_call_tree_node ~root_total_ns:profile.total_weight_ns ~prefix:"  " ~is_last node;
          loop (index + 1) rest
    in
    loop 1 call_tree;
  if hidden_call_tree_roots > 0 then
    println ("  └── ... " ^ Int.to_string hidden_call_tree_roots ^ " more")

let limit_costs = fun costs -> List.take costs ~len:table_limit

let summary_for_table = fun ~filter (summary: Trace_runtime.summary) ->
  {
    summary with
    profile =
      Option.map
        summary.profile
        ~fn:(fun (profile: Trace_runtime.profile_summary) -> {
          profile with
          top_self =
            filter_costs filter profile.top_self
            |> limit_costs;
          top_total =
            filter_costs filter profile.top_total
            |> limit_costs;
        });
  }

let summary_for_call_tree = fun ~filter (summary: Trace_runtime.summary) ->
  {
    summary with
    profile =
      Option.map
        summary.profile
        ~fn:(fun (profile: Trace_runtime.profile_summary) ->
          let (call_tree, hidden_call_tree_roots) = filter_call_tree filter profile in
          { profile with call_tree; hidden_call_tree_roots });
  }

let write_call_tree_summary = fun ~json ~filter (summary: Trace_runtime.summary) ->
  let summary = summary_for_call_tree ~filter summary in
  if json then
    match Trace_runtime.summary_call_tree_to_json_string summary with
    | Ok content -> println content
    | Error err ->
        write_json_event
          (Data.Json.Object [
            ("type", Data.Json.String "trace.call_tree.error");
            ("message", Data.Json.String ("failed to encode trace call tree JSON: " ^ Serde.Error.to_string err));
          ])
  else (
    println ("trace: " ^ Path.to_string summary.path);
    println
      ("exists: "
      ^ (if summary.exists then
          "true"
        else
          "false"));
    println
      ("format: "
      ^ Option.unwrap_or ~default:"unknown" summary.format);
    match summary.profile with
    | None -> ()
    | Some profile ->
        println ("samples: " ^ Int.to_string profile.sample_count);
        println ("sampled cpu: " ^ render_ms profile.total_weight_ns);
        write_call_tree ~filter profile
  )

let write_summary_json = fun ~filter summary ->
  let summary = summary_for_table ~filter summary in
  match Trace_runtime.summary_table_to_json_string summary with
  | Ok content -> println content
  | Error err ->
      write_json_event
        (Data.Json.Object [
          ("type", Data.Json.String "trace.summary.error");
          ("message", Data.Json.String ("failed to encode trace summary JSON: " ^ Serde.Error.to_string err));
        ])

let write_summary = fun ~json ~filter (summary: Trace_runtime.summary) ->
  if json then
    write_summary_json ~filter summary
  else (
    println ("trace: " ^ Path.to_string summary.path);
    println
      ("exists: "
      ^ (if summary.exists then
          "true"
        else
          "false"));
    println
      ("format: "
      ^ Option.unwrap_or ~default:"unknown" summary.format);
    match summary.profile with
    | None -> ()
    | Some profile ->
        println ("samples: " ^ Int.to_string profile.sample_count);
        println ("sampled cpu: " ^ render_ms profile.total_weight_ns);
        write_function_table
          ~title:"top functions by total time"
          ~total_weight_ns:profile.total_weight_ns
          ~filter
          profile.top_total
  )

let write_summary_error = fun ~json err ->
  let message = Trace_runtime.summary_error_message err in
  if json then
    write_json_event
      (Data.Json.Object [
        ("type", Data.Json.String "trace.summary.error");
        ("message", Data.Json.String message);
      ])
  else
    out ("error: " ^ message)

let write_call_tree_error = fun ~json err ->
  let message = Trace_runtime.summary_error_message err in
  if json then
    write_json_event
      (Data.Json.Object [
        ("type", Data.Json.String "trace.call_tree.error");
        ("message", Data.Json.String message);
      ])
  else
    out ("error: " ^ message)

let run_summary = fun matches ->
  let json = ArgParser.get_flag matches "json" in
  match frame_filter_of_matches matches with
  | Error (Failure message as err) ->
      if json then
        write_json_event
          (Data.Json.Object [
            ("type", Data.Json.String "trace.summary.error");
            ("message", Data.Json.String message);
          ])
      else
        out ("error: " ^ message);
      Error err
  | Error err -> Error err
  | Ok filter ->
      let path =
        ArgParser.get_one matches "path"
        |> Option.unwrap_or ~default:""
      in
      match Path.from_string path with
  | Error _ ->
      let message = "invalid trace path: " ^ path in
      if json then
        write_json_event
          (Data.Json.Object [
            ("type", Data.Json.String "trace.summary.error");
            ("message", Data.Json.String message);
          ])
      else
        out ("error: " ^ message);
      Error (Failure message)
  | Ok path -> (
      match Trace_runtime.summarize path with
      | Ok summary ->
          write_summary ~json ~filter summary;
          Ok ()
      | Error err ->
          write_summary_error ~json err;
          Error (Failure (Trace_runtime.summary_error_message err))
    )

let run_call_tree = fun matches ->
  let json = ArgParser.get_flag matches "json" in
  match frame_filter_of_matches matches with
  | Error (Failure message as err) ->
      if json then
        write_json_event
          (Data.Json.Object [
            ("type", Data.Json.String "trace.call_tree.error");
            ("message", Data.Json.String message);
          ])
      else
        out ("error: " ^ message);
      Error err
  | Error err -> Error err
  | Ok filter ->
      let path =
        ArgParser.get_one matches "path"
        |> Option.unwrap_or ~default:""
      in
      match Path.from_string path with
  | Error _ ->
      let message = "invalid trace path: " ^ path in
      if json then
        write_json_event
          (Data.Json.Object [
            ("type", Data.Json.String "trace.call_tree.error");
            ("message", Data.Json.String message);
          ])
      else
        out ("error: " ^ message);
      Error (Failure message)
  | Ok path -> (
      match Trace_runtime.summarize path with
      | Ok summary ->
          write_call_tree_summary ~json ~filter summary;
          Ok ()
      | Error err ->
          write_call_tree_error ~json err;
          Error (Failure (Trace_runtime.summary_error_message err))
    )

let load_operational_config = fun workspace ->
  match workspace with
  | None -> Ok Riot_model.Workspace_operational_config.default
  | Some (workspace: Riot_model.Workspace.t) ->
      Riot_model.Workspace_operational_config.load ~workspace_root:workspace.root
      |> Result.map_err ~fn:Riot_model.Workspace_operational_config.message

let run_trace_with_workspace_info = fun ~workspace ~workspace_error matches ->
  let seen_registry_updates = Collections.HashSet.create () in
  let extra = trailing_args matches in
  let _verbose = ArgParser.get_count matches "verbose" in
  let list_mode = ArgParser.get_flag matches "list" in
  let json_mode = ArgParser.get_flag matches "json" in
  let* pkg_filter =
    match ArgParser.get_one matches "package" with
    | None -> Ok None
    | Some package_name ->
        parse_package_name package_name
        |> Result.map ~fn:Option.some
  in
  let update = ArgParser.get_flag matches "update" in
  let profile = profile_of_matches matches in
  let output_mode =
    if list_mode && json_mode then
      Build.Json
    else if json_requested_for_child extra then
      Build.Json
    else
      Build.Human
  in
  if json_mode && not list_mode then
    let message =
      "riot trace --json is only supported with --list; use `riot trace -- --json` to forward JSON to the child binary"
    in
    write_workspace_error ~mode:Build.Json message;
    Error (Failure message)
  else if list_mode then
    match workspace with
    | None ->
        let message = Option.unwrap_or ~default:"Not in a riot workspace" workspace_error in
        write_workspace_error ~mode:output_mode message;
        Error (Failure message)
    | Some workspace ->
        if Option.is_some (ArgParser.get_one matches "name") then
          let message = "riot trace --list does not accept a binary name" in
          write_workspace_error ~mode:output_mode message;
          Error (Failure message)
        else if not (List.is_empty extra) then
          let message = "riot trace --list does not accept forwarded arguments" in
          write_workspace_error ~mode:output_mode message;
          Error (Failure message)
        else
          let binaries = Run_runtime.list_binaries workspace ?package_filter:pkg_filter () in
          (
            match output_mode with
            | Build.Json -> write_binary_list_json ~workspace binaries
            | Build.Human -> write_binary_list ~workspace binaries
          );
        Ok ()
  else
    match load_operational_config workspace with
    | Error message ->
        write_workspace_error ~mode:output_mode message;
        Error (Failure message)
    | Ok operational_config ->
    let on_event (event: Trace_runtime.trace_event) =
      match event with
      | Trace_runtime.Build build_event ->
          Build.write_build_event ~mode:output_mode ~profile ~seen_registry_updates build_event
      | _ -> write_trace_event ~mode:output_mode event
    in
    let resolved_target =
      match ArgParser.get_one matches "name" with
      | Some name -> parse_target ?package_filter:pkg_filter name
      | None -> (
          match workspace with
          | Some workspace ->
              resolve_implicit_local_target ?package_filter:pkg_filter workspace
              |> Result.map
                ~fn:(fun { package_name; binary_name } ->
                  Local { package_name = Some package_name; binary_name })
              |> Result.map_err ~fn:(fun err -> Failure err)
          | None ->
              Error (Failure (Option.unwrap_or ~default:"Not in a riot workspace" workspace_error))
        )
    in
    match resolved_target with
    | Error (Failure message as err) ->
        write_workspace_error ~mode:output_mode message;
        Error err
    | Error _ as err -> err
    | Ok target ->
        let result =
          match target with
          | Remote_source { source_spec; binary_name } ->
              (
                match trace_request_of_matches ~binary_name ~operational_config matches with
                | Error (Failure message) -> Error (`Cli message)
                | Error err -> Error (`Cli (Exception.to_string err))
                | Ok trace ->
                    let* () =
                      Trace_runtime.preflight trace
                      |> Result.map_err ~fn:(fun err -> `Trace err)
                    in
                    Trace_runtime.run_source
                      ~on_event
                      {
                        source_spec;
                        binary_name;
                        profile;
                        trace;
                        update;
                        args = extra;
                      }
                    |> Result.map_err ~fn:(fun err -> `Trace err)
              )
          | Local { package_name; binary_name } -> (
              match trace_request_of_matches ~binary_name ~operational_config matches with
              | Error (Failure message) -> Error (`Cli message)
              | Error err -> Error (`Cli (Exception.to_string err))
              | Ok trace -> (
                  let* () =
                    Trace_runtime.preflight trace
                    |> Result.map_err ~fn:(fun err -> `Trace err)
                  in
                  match workspace with
                  | Some workspace ->
                      Trace_runtime.run
                        ~on_event
                        {
                          workspace;
                          package_name;
                          binary_name;
                          profile;
                          trace;
                          args = extra;
                        }
                      |> Result.map_err ~fn:(fun err -> `Trace err)
                  | None ->
                      Error (`Cli (Option.unwrap_or
                        ~default:"Not in a riot workspace"
                        workspace_error))
                )
            )
        in
        match result with
        | Ok () -> Ok ()
        | Error (`Cli message) ->
            write_workspace_error ~mode:output_mode message;
            Error (Failure message)
        | Error (`Trace err) ->
            write_trace_error ~mode:output_mode err;
            Error (Failure (Trace_runtime.trace_error_message err))

let run_with_workspace_info = fun ~workspace ~workspace_error matches ->
  match ArgParser.get_subcommand matches with
  | Some ("summary", summary_matches) -> run_summary summary_matches
  | Some ("call-tree", call_tree_matches) -> run_call_tree call_tree_matches
  | _ -> run_trace_with_workspace_info ~workspace ~workspace_error matches

let run = fun ~workspace matches ->
  run_with_workspace_info
    ~workspace:(Some workspace)
    ~workspace_error:None
    matches
