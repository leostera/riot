# riot-test AGENTS

`riot-test` owns reusable test selection, suite binary discovery, and the
parent-side contract for invoking `Std.Test.Cli` test binaries.

## Rules

1. Keep command-line rendering and argument parser definitions out of this package.
   Callers should pass typed requests and render events themselves.
2. Keep `Std.Test.Cli` harness details here: `list-tests`, `run-tests`,
   `run-fuzz-case`, `--json`, `--ctx`, suite context JSON, parsed suite output,
   and fuzz corpus/mutator metadata from discovery JSON.
3. Keep package and suite filtering shared here so `riot test`, `riot bench`, and
   `riot fuzz` do not fork their selector semantics.
4. Keep suite execution event-oriented. Long-running subprocesses should surface
   heartbeat/progress events instead of requiring callers to scrape child output.
5. When adding parallel suite or case execution, use an owner-managed task pool
   like `Std.Test.Runner`: workers report readiness, the owner feeds pending work,
   and events/results are serialized by the owner.
