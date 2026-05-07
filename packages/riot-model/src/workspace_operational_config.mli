open Std

(** Repository-local operational config loaded from [.riot/config.toml]. *)
type cache_policy = {
  keep_generations: int;
  max_size_bytes: int64;
}
type test_policy = {
  small_test_timeout: Time.Duration.t option;
  flaky_max_retries: int;
}
type perf_trace_policy = {
  sample_rate_hz: int option;
  call_graph: string option;
  call_graph_stack_size: int option;
}
type xctrace_trace_policy = {
  template_: string option;
  time_limit: string option;
  window: string option;
}
type trace_policy = {
  profiler: string option;
  perf: perf_trace_policy;
  xctrace: xctrace_trace_policy;
}
type t = {
  cache: cache_policy;
  test: test_policy;
  trace: trace_policy;
}
type value_error =
  | MissingNumberPrefix
  | UnsupportedUnit of string
  | InvalidNumber of string
  | NegativeValue
type cache_error =
  | KeepGenerationsMustBePositiveInt
  | MaxSizeMustBeString
  | InvalidMaxSize of value_error
type test_error =
  | SmallTestTimeoutMustBeDurationString
  | SmallTestTimeoutMustBeNonNegativeInt
  | InvalidSmallTestTimeout of value_error
  | FlakyMaxRetriesMustBeNonNegativeInt
  | FlakyMaxRetriesMustBeInt
type trace_error =
  | TraceProfilerMustBeString
  | PerfSampleRateMustBePositiveInt
  | PerfSampleRateMustBeInt
  | PerfCallGraphMustBeString
  | PerfCallGraphStackSizeMustBePositiveInt
  | PerfCallGraphStackSizeMustBeInt
  | XctraceTemplateMustBeString
  | XctraceTimeLimitMustBeDurationString
  | InvalidXctraceTimeLimit of value_error
  | XctraceWindowMustBeDurationString
  | InvalidXctraceWindow of value_error
type invalid_config_error =
  | RiotMustBeTable
  | RiotCacheMustBeTable
  | RiotTestMustBeTable
  | RiotTraceMustBeTable
  | RiotTracePerfMustBeTable
  | RiotTraceXctraceMustBeTable
  | CacheConfig of cache_error
  | TestConfig of test_error
  | TraceConfig of trace_error
type error =
  | ReadFailed of {
      path: Path.t;
      error: IO.error;
    }
  | ParseFailed of {
      path: Path.t;
      error: Std.Data.Toml.error;
    }
  | InvalidConfig of {
      path: Path.t;
      error: invalid_config_error;
    }

val default_cache_policy: cache_policy

val default_test_policy: test_policy

val default_perf_trace_policy: perf_trace_policy

val default_xctrace_trace_policy: xctrace_trace_policy

val default_trace_policy: trace_policy

val default: t

val message: error -> string

val load: workspace_root:Path.t -> (t, error) result
