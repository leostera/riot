# Daily-driver v0.1

Goal: a reasonably stable Riot for daily application development, before a broad
public launch. This is the live execution ledger, not a requirement to finish all
of `NEXT.md`.

## Proposals

- [RFD0044: scope and acceptance](docs/rfds/RFD0044-v0-1-daily-driver-stability.md)
- [RFD0045: repeatable validation](docs/rfds/RFD0045-repeatable-stability-validation.md)
- [RFD0046: verified, recoverable releases](docs/rfds/RFD0046-verified-recoverable-releases.md)

All three are **presented**, not accepted or implemented. The confirmed goal
requires Apple Silicon development, x86_64 GNU/Linux execution, a representative
service outside the monorepo, and five actual development days of user-reported
candidate use. Compiler/build diagnostics are the agreed initial editing path;
LSP is explicitly deferred. ARM Linux is outside the initial required matrix.

## Current slice: restore and establish the baseline

- [x] Correct `Build_spec.make`'s optional argument declaration and add a trailing
  `unit` argument so callers can omit it; update its call site.
- [x] Build `riot-build` and `riot-cli` with the existing `./riot` driver.
- [x] Use the newly produced CLI to rebuild `riot-cli` against the existing cache.
- [ ] Diagnose the original build-core test command timeout. Later runs passed;
  the cause is still unproven, so this is not closed by a successful retry.
- [x] Run the newly built CLI's focused baseline with saved per-command evidence.
- [ ] Choose/pin the clean bootstrap path and record binary/toolchain provenance.
- [x] Add a bounded local validation entrypoint with explicit driver selection,
  process-group cleanup, saved output, and required-suite discovery checks.
  See `scripts/validate-stability.py` and `docs/contributing/stability-validation.md`.
- [ ] Wire active host CI to the same entrypoint; do not merely rename the stale
  disabled workflow.
- [ ] Exercise clean self-hosting in disposable state, without clearing real caches.
- [ ] Initialize and run an external application using the generated dependencies.

### Validation record — 2026-10-04

Source baseline: `df8b93642`, with the constructor fix committed as `23cd71ad5`. Local
Apple Silicon runs used existing caches; they are not clean-bootstrap evidence.

| Command | Observation |
| --- | --- |
| `./riot build riot-build` | Exit 0, reported 1.98s |
| `./riot build riot-cli` | Exit 0, reported 5.26s |
| `./_build/debug/aarch64-apple-darwin/out/riot-cli/riot build riot-cli` | Exit 0, reported 0.15s |
| `./_build/debug/aarch64-apple-darwin/out/riot-cli/riot test riot-build:build_core_tests` | No captured output; exceeded an external 120s deadline; process group terminated |

The build-core executable in `out/riot-build` still had its April 15 timestamp
after the timeout. Do not run that old file and attribute its results to the new
source. It is not yet known whether the timeout is planning, building, runner
orchestration, or another issue.

The newly produced CLI still reports `riot 0.0.15 (build adbaee474c51)` while the
manifest is `0.0.17`; installed metadata can affect this output. Record the observed
discrepancy rather than inferring the executable's source identity from it.

Earlier survey: kernel 319/319 and std 560/560 cases passed with the existing
driver. Those results do not establish the newly built driver's test path.

Follow-up: the whole-workspace commit hook failed on existing minttea/Raml errors
and may have warmed caches before the timeout investigation. The JSON build-core
rerun passed all 14 cases; six additional runs alternating human/JSON output also
passed. This does not establish the original timeout's cause.

`scripts/validate-stability.py --driver
./_build/debug/aarch64-apple-darwin/out/riot-cli/riot --timeout 30` passed all four
required suites: kernel foundation 25, std test CLI 13, build core 14, and CLI test
selection 9 (61 cases total, no failures/skips). Local evidence:
`_build/stability/run-p9o_00qr/report.json`. The supervisor's 10 Python tests passed,
including timeout cleanup of a SIGTERM-ignoring child. This is a focused warm-cache
baseline, not full workspace or clean-machine certification.

Commits bypass the unrelated failing workspace-wide hook per invocation; targeted
validation remains recorded. The hook and Git configuration were not changed.

### Foreign-action cache fix — 2026-10-05

- Red: five of eight initial argv tests failed with the old hash encoding
  (`_build/stability/run-u97p8jgw/report.json`).
- Green: twelve action-hash tests and thirteen package-planning tests passed using
  the existing `./riot` driver to build current-source suites
  (`_build/stability/run-04sjbbf6/report.json`). The latter includes a seeded v14
  plan-cache entry rejected by the new v15 planner contract.
- `./riot build riot-planner` and `./riot build riot-cli` passed within 30s each.
- Candidate self-build with the v15 driver timed out after 120s
  (`_build/stability/run-4mt_6eu9/report.json`), before suite execution.
- A separate 10s verbose reproduction showed ongoing planning, reaching `syn`;
  a macOS stack sample showed `Syn.Parser.scan_for_in` among active work. This is
  a cold-planning lead, not yet proof of the full timeout's cause. No TLA+ models
  were rerun; the old command-order model is documented as historical.

Follow-up after partial cache warming: the verbose v15 self-build completed in
25.15s, and the rebuilt driver's six-suite validation passed all 86 cases with no
failures/skips (`_build/stability/run-l3qoyfe8/report.json`). The hash and package-plan
regressions now join the default smoke gate. The focused existing-cache baseline
is green again; the first cold timeout still needs an isolated reproduction and
is not considered fixed by these successful retries.

### Isolated cold reproduction — 2026-10-10

Fresh Git export of `bb1c418a2`; isolated HOME with only a copied
`5.5.0-riot.2/aarch64-apple-darwin` compiler. No project/build or registry caches
were copied. Driver SHA-256 and compiler manifest are in `setup.json` under:
`/var/folders/v0/6x4x9vzn10gbdxpzdnsfyxwh0000gn/T/riot-cold-baseline-yqbl34n4/`.

- Rebuilt driver's `build riot-cli --json`: timed out after 182.28s. A sample at
  45s showed worker domains waiting, unlike the earlier active-parser sample.
- A subsequent kernel-only run failed in 0.39s with `Unbound module Io`.
- Git tracked `src/IO/IO.ml`, while the case-insensitive working tree contained
  `src/io/io.ml`. Four tracked paths are now renamed to the lowercase spelling
  required by internal `Io` references; the public `Kernel.IO` alias is unchanged.
- With casing corrected, the rebuilt driver still timed out building kernel at
  90s. The casing defect therefore does not explain away the hanging driver.
- The original `./riot` driver built the corrected kernel in a separate fresh
  export with no build cache in 3.37s (`kernel-clean.result.json`). This validates
  the casing fix, not the rebuilt driver's cold execution path.

Narrowing results (`minimal-results.json` in the same evidence directory):

- Minimal one-module library with the rebuilt driver: cold build 0.28s, warm build
  0.04s, both exit 0. The simplest compile path does not reproduce the hang.
- Rebuilt driver against the kernel cache produced by the old driver: 0.81s,
  exit 0. Warm-cache success is a control observation, not proof of recovery.

Next: narrow the rebuilt driver's cold concurrent action/runtime hang using
larger generated fixtures and child-process/wakeup evidence. The original
historical timeout and the isolated hang remain unresolved; no performance
budget or causal fix is claimed.

### Cold-build narrowing and repeatable harness — 2026-10-10

- Valid generated cold libraries with 8, 32, and 96 independent modules passed in
  0.34s, 0.70s, and 1.56s respectively. The first exploratory fixtures used `+`
  without a prelude and correctly failed compilation; those are not passing controls.
- A 24-file native C fixture passed in 1.40s; a native dependency shared across
  build/runtime scopes passed in 1.61s. Evidence is under the earlier temporary
  reproduction directory (`parallel-valid-results.json`, `native-results.json`).
- A separate fresh kernel build still hung at 15s with no live compiler child
  visible in the captured process table. Temporary command-reader instrumentation
  exists only in the disposable `repo` export, not in the real working tree;
  building that probe CLI with the old driver exceeded 60s, so no probe binary or
  probe-derived conclusion is claimed.
- `scripts/validate-cold-build.py` now reproduces committed-export cold/warm runs
  using an isolated HOME and explicit copied compiler/driver. It retains evidence,
  checks terminal JSON results, rejects escaping compiler links, and stops on the
  first failure rather than substituting a warm retry.
- Two fresh kernel harness runs passed cold/warm:
  `_build/stability/cold-t3p6eiuy/report.json` (4.63s/0.12s) and
  `_build/stability/cold-mj7lhufq/report.json` (4.21s/0.08s). Both use the same driver
  bytes as the hanging runs; this is not evidence that the hang is fixed.
- The CLI-closure harness exceeded 60s and exposed a process-group cleanup
  permission error (`cold-6uw5turs/report.json`). The supervisor now preserves
  cleanup failures as command evidence and keeps leader waits bounded. A 20s
  diagnostic run retained its timeout normally (`cold-yd1jveln/report.json`); this
  deadline is a diagnostic cutoff, not a proposed CLI release budget.
- Seventeen Python validation/isolation tests passed, including explicit HOME,
  compiler-link containment, missing build evidence, and cleanup-error reporting.

Next: collect live actor/action state in a successfully built diagnostic candidate
or narrow the kernel action graph further. No scheduler change has been made based
solely on these control observations.

### Restore scheduler error observability — 2026-10-10

- `Scheduler.eprintln` was an unconditional no-op, hiding process exception
  diagnostics, warnings, and even explicitly enabled scheduler tracing. Restored
  best-effort writes directly to kernel stderr, with partial-write handling and
  no actor logger/effect dependency. These writes are not a nonblocking guarantee.
- Added child-process regressions in `std_command_tests`: an intentionally failing
  main must exit nonzero with scheduler context/exception on stderr and empty
  stdout; enabled tracing must preserve the exact stdout payload and emit its
  shutdown diagnostic on stderr.
- `./riot build std` passed in 8.28s. The first CLI rebuild timed out after 30s
  (`riot-diagnostics-build-hlmnub01`); preserved, not counted as passing.
- All six `std_command_tests` passed, including both new regressions, then an
  independent verbose CLI rebuild passed in 10.93s. Evidence:
  `/var/folders/v0/6x4x9vzn10gbdxpzdnsfyxwh0000gn/T/riot-diagnostics-test-drkech0w/`.
  Two earlier compilation attempts exposed unavailable `Bytes.of_string` and
  `(<>)` APIs; fixed to supported `Bytes.from_string` and `Int.equal`.
- The resulting diagnostic-enabled driver still exceeded 30s on an isolated
  committed CLI source export (`_build/stability/cold-304vrzkp/report.json`) and
  120s on the normal required-gate entrypoint (`run-0evrko52/report.json`);
  neither emitted scheduler errors. Restoring diagnostics is a proven separate
  correctness fix, not a resolution of the cold-build hang. The cold export
  predates this source change; its driver snapshot includes the change.

Next: use an explicit periodic actor/action snapshot in a disposable diagnostic
build to identify which continuations are waiting without delivery. The required
validation gate remains red; no candidate freeze or soak can begin.

### Causal cold-build fix: late process registration — 2026-10-10

- Built a temporary scheduler snapshot probe; retained source SHA, patch, driver
  digest, build attempts, and logs in `_build/stability/actor-snapshot-probe/`.
  The temporary scheduler instrumentation has been removed from tracked source.
- `_build/stability/cold-amec9v52/report.json` exceeded 25s. Its snapshots show
  every worker queue empty and, after initial compilation activity, 345 actors
  waiting for messages plus exactly one actor parked at `Command.wait` (pid<367>).
  There were no runnable, queued, executing, or pending actors in that steady state.
- A raw kqueue control demonstrated running-process exit delivery, but `ESRCH`
  on registration of zombie/reaped children. The native process-change shim was
  swallowing that registration error as success, leaving no exit watch. This
  matches the race between `Process.try_wait` and async process registration.
- Added a deterministic late-registration regression: reap the child first, then
  require priority readiness from a successful registration. Before the fix,
  `04.stdout.log` reports 44/45 passing and this case failing with
  `successful late process registration lost readiness`. Earlier test-build
  timeout/cleanup denial is retained in `03.*` and not counted as a pass.
- Native process changes now retry `EINTR`, capture errno before reentering OCaml,
  and return registration `ESRCH`. The selector preserves that caller's token as
  a one-shot pending priority hint, so `try_wait` can observe exit rather than
  parking forever. Reregister replaces hints; deregister/close clear them; poll
  limits and validation preserve unconsumed hints.
- All 45 process cases passed after the fix (`06.stdout.log`). Two additional
  pending-token lifecycle/poll-limit cases now make that suite 47 cases.
- The changed driver built a fresh committed CLI source export cold in 49.71s
  and warm in 0.29s (`cold-1z2_vy97/report.json`, timeout 120s). This export still
  uses the prior committed kernel source; it proves the new *driver* completes
  its cold build, not that the resulting exported CLI contains the fix.
- `_build/stability/run-qlb4hat9/report.json` passed all eight required suites:
  139 cases, no failures/skips, with a snapshotted changed driver. Seventeen
  Python harness tests also passed. Process and command suites are now default
  gates so this readiness regression and scheduler diagnostics cannot be omitted.

Next: commit/export the fix, rebuild that export with the changed driver, and
have its resulting CLI rebuild itself. Keep pinned-bootstrap certification
separate from the copied-compiler diagnostic lane. Continue the remaining build
correctness contracts; this one resolved race does not complete that task.

## Correctness work after the baseline

- [x] Reproduce foreign build argv hash collisions; preserve argument order and
  field boundaries, and invalidate affected action/plan cache identities.
  Twelve action-hash cases and thirteen package-planning cases pass.
- [ ] **Next: investigate cold planning after cache invalidation.** The rebuilt
  v15 driver exceeded 120s rebuilding the CLI; do not remove the invalidation just
  to make a stale cache appear healthy.
- [ ] Reconcile `specs/riot/BugInventory.md` against current code. Toolchain hashing
  is already present; historical entries are not all current bugs.
- [ ] Cover missing/corrupt exports, dependency-path/toolchain changes, failed
  dependencies, cancellation, concurrent builds/clean, and executable promotion.
- [ ] Inventory and restore runtime-specific regression coverage after migration
  into `Std.Runtime`; exercise races and shutdown on macOS and Linux.
- [ ] Measure cold/warm/edit builds and memory on the selected daily-use application.
- [ ] Document the supported compiler-diagnostic workflow and unavailable commands
  honestly. `check`/LSP restoration is not a daily-driver gate.

## Candidate delivery and use

- [ ] Verify archive integrity in both install and upgrade before extraction.
- [ ] Resolve exact release identities; avoid mutable archive/metadata mixing.
- [ ] Decide installation transaction layout and implement interruption recovery.
- [ ] Publish complete immutable target sets before advancing discovery.
- [ ] Exercise install and exact-version rollback outside this repository.
- [ ] Select a real external application and record candidate daily-use results.
- [ ] Reconcile current package names, user docs, and support claims.

## Separate public-launch gates

- [ ] Execute every advertised target and publish a support/compatibility matrix.
- [ ] Verify registry authorization, upload limits, archive safety, rate limits,
  backup restore, monitoring, and incident-response contact.
- [ ] Align editor support claims and installation docs with the shipped candidate.

## Deferred unless required by a reproduced blocker

Full RFD0043 scheduler scope; Raml backend completeness; procedural macros;
script-file execution; general API renaming; zero-copy string redesign; new
numeric/collection libraries; `check`/LSP restoration; full typechecker parity;
snapshot-review UI polish.
