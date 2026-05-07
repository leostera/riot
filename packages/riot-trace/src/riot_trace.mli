open Std

module Profiler = Profiler

module Profile = Profile

module Summary = Trace_summary

module Execution = Trace_run

module Internal: sig
  module Xctrace: module type of Xctrace
end

type profiler = Profiler.t =
  | Auto
  | Perf
  | Xctrace

type perf_options = Trace_run.perf_options = {
  sample_rate_hz: int option;
  call_graph: string option;
  call_graph_stack_size: int option;
}
type xctrace_options = Trace_run.xctrace_options = {
  template_: string option;
  time_limit: string option;
  window: string option;
}
type trace_options = Trace_run.trace_options = {
  perf: perf_options;
  xctrace: xctrace_options;
}
type output_policy = Trace_run.output_policy =
  | Fail_if_exists
  | Overwrite
  | Append

type trace_request = Trace_run.trace_request = {
  output: Path.t;
  output_policy: output_policy;
  profiler: profiler;
  options: trace_options;
}
type run_request = Trace_run.run_request = {
  workspace: Riot_model.Workspace.t;
  package_name: Riot_model.Package_name.t option;
  binary_name: string;
  profile: string;
  trace: trace_request;
  args: string list;
}
type source_run_request = Trace_run.source_run_request = {
  source_spec: string;
  binary_name: string;
  profile: string;
  trace: trace_request;
  update: bool;
  args: string list;
}
type trace_event = Trace_run.event =
  | Build of Riot_build.Event.t
  | TracingBinary of {
      package: Riot_model.Package_name.t;
      binary: string;
      profiler: string;
      output: Path.t;
    }
type trace_error = Trace_run.error =
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

type call_cost = Profile.call_cost = {
  name: string;
  samples: int;
  total_samples: int;
  self_weight_ns: int;
  total_weight_ns: int;
}
type call_tree_node = Profile.call_tree_node = {
  name: string;
  self_samples: int;
  total_samples: int;
  self_weight_ns: int;
  total_weight_ns: int;
  children: call_tree_node list;
  hidden_children: int;
}
type profile_summary = Profile.t = {
  sample_count: int;
  total_weight_ns: int;
  top_self: call_cost list;
  top_total: call_cost list;
  call_tree: call_tree_node list;
  hidden_call_tree_roots: int;
}
type summary = Trace_summary.t = {
  path: Path.t;
  exists: bool;
  format: string option;
  profile: profile_summary option;
}
type summary_error = Trace_summary.error =
  | SummarySystemError of {
      path: Path.t;
      reason: string;
    }

val profiler_from_string: string -> (profiler, string) result

val profiler_to_string: profiler -> string

val default_output_path: binary_name:string -> Path.t

val default_options: trace_options

val trace_error_message: trace_error -> string

val trace_event_to_json: trace_event -> Data.Json.t option

val preflight: trace_request -> (unit, trace_error) result

val run: ?on_event:(trace_event -> unit) -> run_request -> (unit, trace_error) result

val run_source:
  ?on_event:(trace_event -> unit) -> source_run_request -> (unit, trace_error) result

val summarize: Path.t -> (summary, summary_error) result

val summary_serializer: summary Serde.Ser.t

val summary_table_serializer: summary Serde.Ser.t

val summary_call_tree_serializer: summary Serde.Ser.t

val summary_to_json_string: summary -> (string, Serde.error) result

val summary_table_to_json_string: summary -> (string, Serde.error) result

val summary_call_tree_to_json_string: summary -> (string, Serde.error) result

val summary_error_message: summary_error -> string
