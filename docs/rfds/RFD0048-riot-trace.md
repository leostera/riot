# RFD0048 - Riot Trace

- Feature Name: `riot_trace`
- Start Date: `2026-05-08`
- Status: `presented`
- RFD PR: [leostera/riot-new#19](https://github.com/leostera/riot-new/pull/19)
- Riot Issue: N/A

## Summary
[summary]: #summary

This RFD proposes `riot trace`: a built-in profiling workflow that runs Riot
binaries under host profilers and turns their output into Riot-readable trace
summaries.

- tracing is a first-class command, separate from `riot run`, but shaped so users
  can switch from `riot run` to `riot trace` with minimal command changes
- `riot-trace` owns profiler selection, trace output policy, summary parsing,
  and future format conversion; `riot-cli` owns command routing and rendering
- the first useful version is sampled native profiling that shows application
  functions, not compiler-inserted spans around every expression
- `.riot/config.toml` can provide repository-local trace defaults such as
  profiler choice and sampling options
- compiler instrumentation, full Perfetto export, allocation tracing, and
  every-branch spans are future work, not requirements for the first version

## Motivation
[motivation]: #motivation

Riot users can build and run binaries, but they do not yet have a Riot-native
way to ask "where did this run spend time?" The current workflow is to leave
Riot, choose a platform profiler by hand, remember the correct invocation shape,
find the produced artifact, and then interpret tool-specific output.

That is too much friction for the questions Riot contributors regularly need to
answer:

- which application functions dominate a command?
- why does a run behave differently on a multicore runtime?
- did a change move time from one package or runtime layer into another?
- can a trace be summarized without opening a GUI profiler?

The pain is structural because Riot already owns the build profile, binary
selection, package graph, and command arguments. External profiler wrappers do
not know those things. If profiling stays outside Riot, every useful workflow
must keep reimplementing the same glue: build the right binary, pick a profiler,
run it with child arguments, protect output paths, and normalize names enough to
make the result readable.

The first prototype showed that the useful first step is not compiler XRay-style
instrumentation. Riot should start with the workflow that native ecosystems
already use successfully: sampled profiling through host tools such as
`xctrace`, `perf`, or similar backends, followed by a Riot-owned summary view.
That makes profiling available without changing application source or the
vendored compiler.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Suppose a contributor normally runs a binary like this:

```sh
riot run syn -- --help
```

With this proposal, the profiling version is:

```sh
riot trace syn -- --help
```

Riot builds the selected binary with the selected profile, chooses a profiler
backend, runs the binary under that profiler, and writes a trace file. By
default the output path is timestamped:

```text
./syn_2026-05-08T12:34:56.000Z.trace
```

Users can be explicit when they need reproducibility:

```sh
riot trace syn --output .riot/profiles/syn-help.trace --profiler xctrace -- --help
```

Once a trace exists, contributors should not need to open a heavy profiler UI
just to answer the first question. They can ask Riot for a compact table:

```sh
riot trace summary .riot/profiles/syn-help.trace
```

Or for the messy inclusive view:

```sh
riot trace call-tree .riot/profiles/syn-help.trace
```

Both views can be filtered by function name:

```sh
riot trace summary -f '*Prelude*' .riot/profiles/syn-help.trace
riot trace call-tree -f '*Prelude*' .riot/profiles/syn-help.trace
```

The summary command is the tidy view: top functions by total sampled time. The
call tree command is the exploratory view: nested stacks, thread roots, hidden
children, and branches that help explain why a function is expensive.

Repository defaults live in `.riot/config.toml`, because tracing policy is about
how Riot behaves in a repository. It is not package metadata and it should not
change what the project is:

```toml
[riot.trace]
profiler = "auto"

[riot.trace.xctrace]
template = "Time Profiler"
time_limit = "10s"

[riot.trace.perf]
sample_rate_hz = 199
call_graph = "dwarf"
```

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

The public command surface is:

```text
riot trace [OPTIONS] [name] [-- args...]
riot trace summary [OPTIONS] <path>
riot trace call-tree [OPTIONS] <path>
```

`riot trace` should follow `riot run` selection semantics for local binaries:

- optional binary name
- optional `-p` or `--package`
- `--list` and `--json` for binary discovery
- `--release` for the release build profile
- trailing child arguments after `--`

The command intentionally does not live as `riot run --trace`. Tracing changes
the outer execution wrapper, output policy, and failure modes. A separate verb
keeps normal runs simple while letting tracing add profiler-specific controls.

`riot-trace` should own the reusable tracing behavior:

- profiler parsing and `auto` resolution
- default trace output naming
- output policy: fail, overwrite, append when supported
- preflight checks that happen before building where possible
- backend command construction for `xctrace`, `perf`, and future profilers
- summary parsing and JSON serialization
- future conversion between profiler-specific artifacts and common formats

`riot-cli` should stay thin:

- parse command-line flags
- load `.riot/config.toml`
- resolve the target using `riot-run` style behavior
- render human tables, call trees, and JSON events

The first summary model is sampled and function-oriented. It records:

- whether the trace path exists
- detected format from the path
- total sampled CPU time
- top functions by total time
- inclusive call-tree nodes

`summary` and `call-tree` intentionally share the same parsing backend but
render different views. `summary` optimizes for a compact answer. `call-tree`
optimizes for investigation.

## Drawbacks
[drawbacks]: #drawbacks

- Riot gains another top-level command and another package boundary
- profiler behavior varies by host OS, so exact outputs are not portable
- sampled profiling can miss very short executions or rare paths
- trace parsing can become expensive for large artifacts
- users may expect source-level or allocation-level instrumentation before Riot
  has implemented it

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why a separate `riot trace` command?

Because tracing is not just running. It adds profiler selection, output
artifacts, preflight behavior, summary subcommands, and future conversion. A
separate command lets users keep `riot run` predictable while making profiling
easy to discover.

### Why not compiler instrumentation first?

Compiler instrumentation is attractive because it could eventually show every
function call, allocation, branch, and user span. It is also a much larger
semantic and runtime commitment. The sampled-profiler path answers the first
useful question, "show me all my functions", with less risk and without
requiring changes to the vendored compiler.

### Why not only shell out to cargo-flamegraph-style wrappers?

Riot can and should learn from those wrappers, but Riot owns package and binary
selection. Keeping this in Riot means the same command understands workspaces,
build profiles, forwarded arguments, repository config, and Riot's output
conventions.

### What if Riot does nothing?

Profiling remains possible, but it stays tool-specific and manual. Contributors
will keep paying coordination cost every time they need a trace, and Riot will
not have a common place for trace summaries, filters, or future conversions.

## Prior art
[prior-art]: #prior-art

Native ecosystems commonly make sampled profiling available through thin tool
wrappers:

- Rust projects often use `cargo flamegraph` or direct `perf` workflows
- C and C++ projects commonly use `perf`, Instruments, DTrace, or platform
  profilers around native binaries
- Go has `pprof` as a standard profiling analysis workflow
- Zig and other native toolchains usually lean on platform profilers rather than
  requiring every project to adopt a language-specific tracing runtime

The lesson for Riot is that the first profiling workflow should be native,
sampled, and cheap to adopt. Riot can add richer instrumentation later once the
command and artifact model are stable.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- What should the stable JSON schema for trace summaries and call trees be?
- How much name normalization should Riot do beyond filtering raw addresses and
  leaving recognizable function names?
- Should Riot convert traces to Perfetto directly, or first define a neutral
  Riot profile model and export from there?
- What trace options should be common across profilers, and which should stay
  backend-specific?
- How should CI use tracing without making tests flaky or host-dependent?

## Future possibilities
[future-possibilities]: #future-possibilities

Future work can build on the same package and command boundary:

- export Perfetto-compatible artifacts from sampled profile summaries
- ingest Linux `perf.data`, Darwin `.trace`, and other host formats into one
  Riot profile model
- add `Std.Telemetry.with_span` / `Std.Telemetry.Span` spans for explicit user
  instrumentation
- extend the vendored compiler to insert function-entry spans in selected build
  profiles
- record allocation samples when a profiler backend supports them
- compare two traces and report regressions in top functions or call-tree paths
- let `riot bench` attach traces to benchmark history when requested
