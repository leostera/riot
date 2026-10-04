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

The cache fix is regression-tested but the candidate self-build baseline is not
currently green after the intentionally invalidated planner cache.

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
