# Daily-driver v0.1

Goal: a reasonably stable Riot for daily application development, before a broad
public launch. This is the live execution ledger, not a requirement to finish all
of `NEXT.md`.

## Proposals

- [RFD0044: scope and acceptance](docs/rfds/RFD0044-v0-1-daily-driver-stability.md)
- [RFD0045: repeatable validation](docs/rfds/RFD0045-repeatable-stability-validation.md)
- [RFD0046: verified, recoverable releases](docs/rfds/RFD0046-verified-recoverable-releases.md)

All three are **presented**, not accepted or implemented. Initial scope assumptions
for discussion: Apple Silicon development, x86_64 GNU/Linux execution, a five-day
candidate soak. Compiler/build diagnostics are the agreed initial editing path;
LSP is explicitly deferred. Decide whether the first real application requires ARM
Linux.

## Current slice: restore and establish the baseline

- [x] Correct `Build_spec.make`'s optional argument declaration and add a trailing
  `unit` argument so callers can omit it; update its call site.
- [x] Build `riot-build` and `riot-cli` with the existing `./riot` driver.
- [x] Use the newly produced CLI to rebuild `riot-cli` against the existing cache.
- [ ] **Next: diagnose the build-core test command timeout.** Do not call the
  baseline green until a newly built suite runs and reports its results.
- [ ] Choose/pin the clean bootstrap path and record binary/toolchain provenance.
- [ ] Add a bounded local validation entrypoint with explicit driver selection,
  process-tree cleanup, saved output, and required-suite discovery checks.
- [ ] Wire active host CI to the same entrypoint; do not merely rename the stale
  disabled workflow.
- [ ] Exercise clean self-hosting in disposable state, without clearing real caches.
- [ ] Initialize and run an external application using the generated dependencies.

### Validation record — 2026-10-04

Source baseline: `df8b93642`, plus the uncommitted constructor fix above. Local
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

## Correctness work after the baseline

- [ ] Reproduce foreign build command-order hash collisions in a regression test;
  preserve order in hashing and invalidate affected cache identities.
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
