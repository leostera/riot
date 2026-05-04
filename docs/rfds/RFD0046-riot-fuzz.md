# RFD0046 - Riot Fuzz

- Feature Name: `riot_fuzz`
- Start Date: `2026-05-04`
- Status: `implemented`

## Summary
[summary]: #summary

This RFD records Riot's first built-in coverage-guided fuzzing system: `Std.Test`
can declare fuzz cases, `riot fuzz` builds those cases in a fuzz profile and
drives them with coverage feedback, and `riot test` replays saved fuzz inputs as
ordinary regression tests.

- fuzz cases are regular `Std.Test` cases with corpus and mutator metadata, but
  fuzz exploration runs through a separate `riot fuzz` command and a separate
  fuzz build profile

- Riot reuses vendored OCaml AFL instrumentation and owns a small native
  AFL-compatible forkserver boundary instead of requiring users to install a
  separate AFL or libFuzzer toolchain

- durable artifacts live under
  `.riot/fuzzing/<package>/<suite>/<case>/{corpus,crashes,crash-artifacts}` and
  are intentionally replayable by normal `riot test`

- `riot-fuzz` owns discovery, mutation, coverage accounting, corpus persistence,
  crash triage, corpus minimization, and campaign parallelism; `riot-cli` stays
  a thin renderer

- sanitizer integration, advanced structure-aware mutator APIs, and CI-scale
  campaign scheduling are out of scope for this first implementation

## Motivation
[motivation]: #motivation

Riot has a growing amount of parser, formatter, typechecker, runtime, build, and
standard-library code. Normal examples, unit tests, fixture tests, snapshots,
and property tests are necessary, but they do not cover one important feedback
loop:

- continuously mutate real inputs, execute actual Riot code, observe which code
  paths were reached, and keep the inputs that expand behavior or reproduce a
  crash.

Property testing is not enough for this job. A property test starts with a typed
generator and asks whether generated values satisfy an invariant. That is a good
fit for algebraic APIs, data structures, and deterministic transformations. It
is a weaker fit for parser and compiler frontends where the interesting inputs
are often malformed, half-valid, tiny, or hostile in ways that a friendly
generator will not naturally produce.

Fuzzing pays for a different kind of confidence:

- it explores inputs based on executed coverage rather than only on a generator
  distribution

- it finds panics, uncaught exceptions, timeouts, and recovery bugs even when
  there is no obvious semantic property beyond "do not crash"

- it turns discovered inputs into durable regression artifacts

- it gives parser, formatter, and typechecker work a way to continuously search
  the weird input space that humans are bad at enumerating

The first Riot targets that need this are `syn` and `typ`.

For `syn`, the input surface is untrusted source text. The parser and AST views
must handle incomplete files, malformed declarations, odd recovery nodes, empty
identifier positions, and attribute suffixes without raising. Fixture tests
cover known examples, but they do not systematically search for broken recovery
paths.

For `typ`, the input surface is parsed Riot/OCaml source lowered into the
prototype typechecker. The important early property is not "infer the perfect
type for every fuzz input". The important property is:

- parsing, lowering, and inference should produce a result or diagnostics
  instead of crashing the process.

Riot should fuzz actual Riot code, not generated stand-ins. That means the
fuzzer should build normal Riot test binaries with the fuzz profile, execute the
real test harness entrypoint, and preserve the normal package/suite/case
selection semantics.

There is also an adoption problem. If fuzzing requires every contributor to
install AFL++, cargo-fuzz, libFuzzer, several OCaml compiler variants, and a
package-local script for each target, it will not become part of the normal Riot
workflow. Riot already owns:

- workspace discovery
- package selection
- test suite discovery
- test binary invocation
- build profiles
- the vendored OCaml compiler
- durable workspace-local artifacts

Fuzzing belongs in that system. Contributors should learn one shape:

```text
riot fuzz ...
riot test ...
```

The first command searches. The second command replays what was found.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Suppose `syn` wants to fuzz parser recovery for implementation files.

The test suite declares a fuzz case in normal `Std.Test` code:

```ocaml
let fuzz_tests =
  Test.[
    fuzz
      "parser recovers arbitrary implementation input"
      ~seeds:[
        "";
        "let x = 0\n";
        "type t = A | B\n";
        "module M = struct let y = fun x -> x end\n";
      ]
      ~corpus:(Test.Fuzz.Corpus.dir fixture_root ~extensions:[ ".ml"; ".mli"; ])
      ~mutator:ocaml_source_mutator
      test_fuzz_parse_implementation;
  ]
```

To a normal test run, this still looks like a test case:

```text
riot test -f "syn:fixture_tests:parser recovers arbitrary implementation input"
```

`riot test` does not start an open-ended campaign. It replays:

- inline seed inputs declared in the test
- file corpus inputs declared by the test
- generated corpus inputs saved under `.riot/fuzzing`
- crash inputs saved under `.riot/fuzzing`

That is what makes fuzzing useful after the campaign ends. A crash found at
night becomes a normal regression case the next morning.

To search for new behavior, contributors use `riot fuzz`:

```text
riot fuzz --list -f "syn:fixture_tests"
riot fuzz -f "syn:fixture_tests:parser recovers arbitrary implementation input" --duration 1h
```

The first command builds the selected suites in the `fuzz` profile and lists the
fuzz cases. The second command runs a coverage-guided campaign for the selected
case.

During a campaign, Riot writes interesting inputs to:

```text
.riot/fuzzing/syn/fixture_tests/parser_recovers_arbitrary_implementation_input/corpus/
```

Crashes go to:

```text
.riot/fuzzing/syn/fixture_tests/parser_recovers_arbitrary_implementation_input/crashes/
```

Captured stdout, stderr, and status metadata for crash triage go to:

```text
.riot/fuzzing/syn/fixture_tests/parser_recovers_arbitrary_implementation_input/crash-artifacts/
```

Those directories are not hidden scratch space. They are the durable record of
what the fuzzer learned.

When the corpus gets too large, contributors can minimize it:

```text
riot fuzz minimize-corpus -f "syn:fixture_tests"
```

Minimization replays corpus files in size order and deletes inputs that do not
add coverage beyond already-kept inputs.

The typechecker workflow is similar:

```ocaml
let test_fuzz_parse_lower_and_infer = fun _ctx source ->
  let parse_result = Syn.parse ~filename:(Path.v "fuzz.ml") source in
  let model_source = Typ.Model.Source.make ~text:source in
  match Typ.Ast.from_parse_result ~source:model_source parse_result with
  | Error _diagnostics -> Ok ()
  | Ok ast ->
      let _infer_result = Typ.Infer.check ast in
      Ok ()
```

This is deliberately not a property test about the exact inferred type. It is a
stability fuzz case over the actual parser, lowering layer, and inference entry.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Public test surface

`Std.Test` gains:

```ocaml
Test.fuzz :
  ?size:Test.size ->
  ?reliability:Test.reliability ->
  ?seeds:string list ->
  ?corpus:Test.Fuzz.Corpus.t ->
  ?mutator:Test.Fuzz.Mutator.t ->
  string ->
  (Test.ctx -> string -> (unit, string) result) ->
  Test.test_case
```

`Test.Fuzz.Corpus` describes replayable inputs:

- inline byte/string inputs
- file inputs from a fixture directory
- merged corpuses

`Test.Fuzz.Mutator` describes mutation hints:

- dictionary entries
- maximum input length
- whether splicing is allowed

The test case records this metadata so `Std.Test.Cli list-tests --json` can
expose it to `riot-test` and `riot-fuzz`.

### Test binary contract

`Std.Test.Cli` now has three relevant modes:

- `list-tests --json`, which lists fuzz cases and their corpus/mutator metadata
- `run-tests`, which replays seeds and saved fuzz corpus/crash inputs as normal
  tests
- `run-fuzz-case <query> --input <path> --json`, which executes exactly one fuzz
  case with exactly one input

`run-fuzz-case` is the boundary used by `riot-fuzz`. It keeps fuzz execution in
the same suite binary and context model as normal tests, while making the hot
campaign loop operate one input at a time.

### `riot-test`

`riot-test` owns reusable selection and suite-binary interaction:

- package and suite filtering
- selector parsing
- suite binary discovery
- building selected suites
- suite context JSON
- parsing `list-tests --json`
- invoking `run-tests` and `run-fuzz-case` compatible binaries

This keeps `riot-cli` and `riot-fuzz` from duplicating the `riot test` selector
semantics.

`list_tests` accepts an event callback. That lets `riot fuzz --list` and
`riot test --list` forward build events while a selected test suite is being
built. Listing fuzz cases should not go silent just because discovery needs a
fuzz-profile build.

### `riot-fuzz`

`riot-fuzz` owns the fuzzing engine.

The package is split by responsibility:

- `Case` converts `riot-test` listed suites into selected fuzz cases and target
  descriptors
- `Corpus` loads seed, generated, and crash inputs
- `Mutation` mutates bytes with dictionary and splicing hints
- `Coverage` reads and compares AFL coverage maps
- `Afl` owns the forkserver protocol and child status model
- `Runner` runs campaigns, persists interesting inputs, triages crashes, and
  minimizes corpuses
- `Capture` records replay stdout/stderr/status through `Std.Command`
- `Lock` serializes workspace fuzz commands with `_build/fuzz.lock`

The primary engine shape is AFL-compatible:

1. build the test suite in the `fuzz` profile
2. start the suite binary under the forkserver boundary
3. write one generated input to a temporary file
4. execute `run-fuzz-case ... --input <path>`
5. reset and read the shared coverage map
6. save the input if it adds new coverage or crashes
7. repeat until the run or duration budget is exhausted

The coverage map is the same kind of 64 KiB edge map expected by AFL-style
instrumentation. The vendored OCaml compiler already has AFL support under
`vendor/ocaml/runtime/afl.c` and `vendor/ocaml/asmcomp/afl_instrument.ml`;
Riot's job is to expose that through a workspace profile and a native runtime
boundary.

### CLI surface

`riot fuzz` supports:

```text
riot fuzz [selection flags]
riot fuzz --list [selection flags]
riot fuzz --duration 1h [selection flags]
riot fuzz --runs 10000 [selection flags]
riot fuzz --concurrency 4 [selection flags]
riot fuzz --replay <path> [selection flags]
riot fuzz minimize-corpus [selection flags]
```

Selection follows the same shape as `riot test`:

- repeated `-p` / `--package` package filters
- `-f` / `--filter` substring or `package:suite:case` selector

The command renders human output by default and JSONL with `--json`.

Build events are forwarded through the normal Riot build renderer. Fuzz events
are owned by `riot-fuzz` and include:

- campaign started/completed
- campaign progress
- input executed
- corpus saved
- crash found
- crash triaged
- replay completed
- corpus minimized

### Parallelism and domains

Campaign-level parallelism belongs in `riot-fuzz`, not `riot-cli`.

`riot fuzz --concurrency N` runs independent fuzz cases or campaign tasks in
parallel. Individual fuzz-case binaries default to:

```text
RIOT_SCHEDULERS=1
```

That keeps one fuzz input from running with multiple OCaml domains by default,
which makes coverage accounting and crash triage easier to reason about. Riot
can still fuzz many cases in parallel by starting multiple processes.

### Durable artifacts and replay

The durable artifact layout is:

```text
.riot/fuzzing/<package>/<suite>/<case>/corpus/
.riot/fuzzing/<package>/<suite>/<case>/crashes/
.riot/fuzzing/<package>/<suite>/<case>/crash-artifacts/
```

`riot fuzz` writes these artifacts.

`riot test` reads them.

That asymmetry is important. Normal tests should not perform open-ended
mutation, but they should replay everything the campaign found. A failing saved
input should fail `riot test` until the underlying bug is fixed or the artifact
is intentionally removed.

### Initial results

The bootstrap immediately produced useful findings.

For `syn`, the committed fuzz state contains `6,527` saved corpus inputs under
`.riot/fuzzing/syn`. The parser campaign found crash and recovery paths around
malformed identifiers, first-class modules, record patterns, path expressions,
and type attribute suffix traversal. The resulting hardening changed AST views
to use optional identifier extraction where recovery trees may not contain a
valid identifier, and fixed attribute-suffix handling for wrapped type
expressions.

For `typ`, the committed fuzz state contains `31,182` saved corpus inputs and
`4` crash inputs under `.riot/fuzzing/typ`. The typechecker campaign found
lowering paths that assumed identifier token lists were non-empty. The first
fixes made lowering return diagnostics/build failures for malformed first-class
modules, package constraints, unpack expressions, and module aliases instead of
raising.

The `typ` results should be read as evidence that the fuzzer is already useful,
not as a claim that the typechecker is now stable. The campaign also exposed
ongoing inference and snapshot drift that needs separate typechecker work.

## Drawbacks
[drawbacks]: #drawbacks

Fuzzing has real costs.

The first cost is repository size. Durable corpuses are valuable because they
make bugs replayable, but they can add thousands of small files quickly. Riot
needs minimization and review discipline so corpus growth remains useful rather
than becoming noise.

The second cost is build complexity. Fuzz tests require a different compilation
mode from ordinary tests because coverage-guided fuzzing needs instrumentation.
That means Riot must maintain a `fuzz` profile and keep its compiler/runtime
integration working across supported targets.

The third cost is execution time. Fuzz campaigns are intentionally open-ended or
budgeted by time/runs. They do not belong in every pre-commit path. The normal
fast path is:

```text
riot test
```

which replays saved inputs, not:

```text
riot fuzz
```

The fourth cost is false confidence. A fuzzer is not a proof. It searches the
input space it can reach with its current corpus, mutator, and instrumentation.
Weak mutators can leave important structured inputs unexplored.

The fifth cost is native/runtime surface area. The AFL-compatible forkserver and
coverage map need native code, process management, timeouts, and crash capture.
That is more complex than a pure `Std.Test` helper.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Do nothing

Riot could keep relying on unit tests, fixture tests, snapshots, and property
tests.

That would avoid the implementation cost, but it would keep the central problem:
parser and compiler crash inputs would only be found when a human happened to
write them down. The first syn and typ campaigns already found bugs, so this is
not a hypothetical gap.

### Treat fuzzing as property testing

Riot could extend `propane` and model fuzzing as another generator mode.

That is the wrong abstraction. Property testing and fuzzing share the idea of
many generated inputs, but they optimize for different feedback:

- property testing asks whether generated values satisfy a property
- coverage-guided fuzzing asks which generated inputs execute new code paths or
  crash the program

Conflating them would make both systems less clear. Fuzzing needs a different
build profile, coverage map, corpus persistence, crash triage, and replay
contract. It deserves its own package and command.

### Require external AFL++

Riot could shell out to `afl-fuzz` and document an installation requirement.

That would reuse a mature fuzzer, but it would make fuzzing feel external to
Riot. Contributors would need separate tool installation, separate output
directories, and separate selector glue. Riot would still have to build the test
suite correctly, map cases to binaries, pass suite context, and preserve corpus
artifacts in a Riot-aware layout.

The selected design borrows the AFL execution model and instrumentation shape,
but keeps the user workflow Riot-native.

### Ship AFL++ as part of the Riot toolchain

Riot could vendor or distribute AFL++ binaries.

That is heavier than the first implementation needs. It increases release,
platform, and security maintenance work before Riot has proven the exact fuzzing
workflow it wants. A small package-owned fuzzer over the vendored OCaml
instrumentation is enough to validate the command shape, corpus layout, replay
contract, and initial mutator model.

### Use libFuzzer-style in-process fuzzing

Riot could pursue a libFuzzer-style in-process loop similar to what Rust
commonly reaches through `cargo-fuzz`.

That has strong performance advantages when the language/runtime/toolchain fits
it well, but Riot's immediate integration point is the vendored OCaml compiler's
AFL instrumentation. A forkserver-style process boundary is also easier to
isolate for crashes, timeouts, and `RIOT_SCHEDULERS=1` execution.

This RFD does not rule out an in-process fuzzer later. It chooses the
lower-integration-risk path for the bootstrap.

### Generate separate fuzz target packages

Riot could ask packages to create standalone fuzz binaries by hand.

That would work for a few packages, but it duplicates test selection, context
handoff, corpus metadata, and package filtering. The selected design lets fuzz
cases live next to the tests that already exercise the same APIs.

## Prior art
[prior-art]: #prior-art

Go's fuzzing support is the closest workflow precedent. Go uses normal test
files to declare fuzz tests, runs seed corpus entries during ordinary `go test`,
and switches into coverage-guided fuzzing with `go test -fuzz`. It also writes
failing inputs into `testdata/fuzz` so future `go test` runs replay them as
regressions.

Reference: <https://go.dev/doc/security/fuzz/>

Zig is a useful design comparison because its standard library is actively
working through the relationship between `std.testing.fuzz`, unit tests, corpus
collection, and build-system discovery. The linked Zig issue argues for making
unit tests contribute corpus inputs while exposing optimized per-fuzz-function
executables in fuzz mode. Riot takes the same lesson that fuzz declarations
should live with tests, but keeps the fuzz execution command separate.

Reference: <https://github.com/ziglang/zig/issues/25352>

AFL++ is the main prior art for the execution model. It is a coverage-guided
fuzzer with seed input directories, output directories, crash/hang discovery,
timeouts, dictionaries, parallel fuzzing, and resume/minimization workflows.
Riot intentionally borrows the AFL-compatible instrumentation and forkserver
shape.

Reference: <https://aflplusplus-aflplusplus.mintlify.app/commands/afl-fuzz>

Rust's `cargo-fuzz` is useful as a packaging comparison. It is a Cargo
subcommand that invokes libFuzzer for Rust projects rather than a fuzzer itself.
That separation is a good reminder that the language package manager should own
the project workflow even when the fuzzing engine is separate. Riot follows the
same workflow lesson, but uses its vendored OCaml instrumentation instead of
libFuzzer.

Reference: <https://rust-fuzz.github.io/book/cargo-fuzz.html>

Riot's own prior art is `riot test`. Fuzzing works because Riot already has a
structured test binary contract, JSON suite discovery, package filtering, build
profiles, and workspace-local artifacts. This RFD extends that system instead
of creating a parallel one.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- What is the right public API for reusable structure-aware mutators? Syn and
  typ both need source-aware mutation; raw byte mutation plus dictionaries is a
  useful start but not the end state.

- How aggressively should Riot minimize committed corpuses? We need enough
  inputs to preserve coverage and regressions without committing redundant
  noise.

- Should crash triage learn semantic deduplication beyond input hashes and
  process status?

- Which sanitizer profiles should Riot expose separately from fuzzing? Thread,
  leak, and memory access sanitizers are valuable, but they are expensive enough
  that they should be explicit test modes rather than default behavior.

- How should multicore fuzzing evolve? The first implementation defaults target
  processes to `RIOT_SCHEDULERS=1` and parallelizes at the process level. A
  future domain-aware coverage model may let individual fuzz cases run
  multicore safely.

- What belongs in CI? `riot test` should replay corpuses in normal CI, but
  open-ended `riot fuzz --duration ...` campaigns probably belong in scheduled
  or dedicated jobs.

- Should Riot eventually support a libFuzzer-compatible or in-process engine in
  addition to the AFL-compatible forkserver?

## Future possibilities
[future-possibilities]: #future-possibilities

The most immediate next step is to harden mutators.

`syn` should grow source-aware mutators that understand token boundaries,
balanced delimiters, attributes, declarations, and recovery-sensitive grammar
fragments. `typ` should grow mutators that bias toward type declarations,
patterns, modules, labeled arguments, GADTs, first-class modules, and other
constructs that exercise lowering and inference.

Riot should also fuzz `ArgParser`, formatter entrypoints, dependency metadata
parsing, package manifest parsing, lockfile parsing, and selected wire-format
codecs. These are all input-heavy surfaces where "do not crash" is valuable even
before deeper semantic properties are available.

Longer-term, `riot fuzz` can grow:

- richer progress dashboards
- corpus coverage summaries
- crash deduplication and symbolized reports
- CI-friendly campaign summaries
- import/export for external corpuses
- package-local fuzz dictionaries
- profile combinations such as fuzz-plus-sanitizer builds
- scheduled campaign helpers for workspaces that want continuous fuzzing

The broader direction is that Riot should treat fuzzing artifacts the same way
it treats snapshots: visible, reviewable, durable evidence that the code has
seen a specific edge case before.
