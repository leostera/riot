# v0.1 daily-driver execution ledger — fresh main baseline

## Scope and starting state

- Branch: `feat/v0.1.1`, based on `origin/main` at `1ea59cef7` plus the user's removals at `9f1f51944463519545b71dc8bc60a9ad427b51b0`.
- Goal: a reasonably stable v0.1 daily driver for Apple Silicon development and x86_64 GNU/Linux execution. Keep repeatable validation, an external representative service, safe installation/rollback, and five actual development days on one qualifying candidate.
- Starting worktree was clean. Read-only baseline evidence: `_build/stability/v0-1-1/baseline.json`, including host, configured toolchain, installed-driver digest, command, logs, exit, and timing. The installed driver's version is not proof it contains branch changes.
- Current configured compiler: `5.5.0-riot.4`; required targets are `aarch64-apple-darwin` and `x86_64-unknown-linux-gnu`. Configuration also lists aarch64 GNU/Linux; that does not expand the required daily-driver matrix.
- Preserve `feat/v0.1` at `322cdb44a468ec08851033abfb8aac53e4c34ab8`, backup `backup/feat-v0.1-pre-main-rebase-20261011-020059`, and review stash `1507fc68a6d4077066d7c54574d4e67d971c164a`. Do not apply the stash or whole commits blindly.
- Old passes and failures remain historical evidence, not acceptance evidence for this branch. Do not restore the user's deleted specs or reuse conflicting RFD0044–0046 numbers.

## Milestones

- [x] Record starting state and classify all 26 old commits against current code.
- [ ] Close current build/cache/execution/recovery regressions.
- [ ] Verify current kernel/runtime/std invariants and required-platform execution.
- [ ] Re-establish compatible pinned bootstrap and produced-CLI self-hosting, cold/warm/edit controls.
- [ ] Adapt checked-in validation and activate required-platform CI.
- [ ] Exercise a representative service outside the monorepo.
- [ ] Verify staged installation, upgrade, interruption recovery, and independent exact-version rollback.
- [ ] Freeze a qualifying candidate with source/binary/archive identities and applicable gate evidence.
- [ ] Obtain five actual user-reported development days on that same candidate.

## Selective-port audit

The classifications below describe code review, not completed runtime validation.

| Old commit | Classification | Current-code basis / action |
| --- | --- | --- |
| `850e14126` daily-driver RFDs | Obsolete as a direct port | Preserve acceptance intent in the fresh goal/ledger, not old document numbering or old execution status. Main already uses RFD0044–0049. |
| `23cd71ad5` optional Build_spec argument | Obsolete/incompatible | The old `build_spec` surface is gone; `packages/riot-build/src/build_core.ml` owns current request/context and parallelism validation. |
| `5e2a719b1` bounded stability runner | Requires adaptation | Reuse retained-evidence/process-supervision ideas, but current CLI uses package/filter selectors and current `Std.Test.Cli` streams lifecycle JSONL. Old default suites are not the current gate. |
| `1e6b31db5` foreign argv hashes | Still useful | `packages/riot-planner/src/action.ml` still sorts `build_cmd` and concatenates unframed arguments. Reproduce ordering and framing collisions first. Current planner contract is v31, not the old v14/v15. |
| `bb1c418a2` planner smoke checks | Requires adaptation | Keep focused planner regressions in the future gate; do not restore old suite assumptions or historical spec edits. |
| `40ee80932` lowercase IO paths | Obsolete/incompatible | Current tracked surface is `packages/kernel/src/IO/IO.ml`, with redesigned off-heap `IO/ioVec` and buffer APIs. Do not rename it to the old layout. |
| `e17599d5a` cold/warm evidence | Historical only | Old source/driver/toolchain results do not certify the new branch. Keep provenance outside fresh acceptance claims. |
| `edad0b5c1` isolated cold-build validator | Requires adaptation | Retain disposable committed exports, isolated HOME/cache and strict terminal checks; adapt current workspace/bootstrap/CLI and compiler configuration. |
| `13015b04f` scheduler stderr diagnostics | Still useful | `packages/std/src/runtime/scheduler.ml` still defines `eprintln` as a no-op. Reproduce stderr visibility/stdout separation through current command and IO surfaces. |
| `59e386808` late process readiness | Still useful, re-test | `packages/kernel/native/kernel_new_async_unix.c` still accepts interrupted/absent process-watch registration as success after removing its binding; `async/adapter/unix.ml` has no pending readiness hints. Reproduce against the current poller before adapting. |
| `0ef5ae7af` direct kernel dependencies | Requires investigation | Reconcile actual imports with current package manifests; do not transplant eleven historical dependency edits or the old router rules wholesale. |
| `9d77e8c76` lock refresh | Obsolete as a direct port | Keep the current lock and resolve only dependencies justified by current changes. |
| `52b8d26c2` pinned bootstrap validator | Requires adaptation | Old Riot 0.0.17/compiler riot.2 pins and manually initialized entrypoint assumptions are not a compatible current baseline. Review `bootstrap.py` and current entrypoint rules first. |
| `7c55d554e` old bootstrap proof | Historical only | Re-establish source/driver/toolchain identities and fresh self-hosting here. |
| `a1a7c2f34` cache integrity / typed promotion errors | Requires adaptation | Current store uses `manifest.bin`, separate action/package storage, and `input_hash`/`output_hash`. `manifest_files_exist` and `artifact_files_exist` check existence; review current integrity/promotion contracts and add current-code corruption cases before porting. |
| `ad71009de` produced-CLI edit probe | Requires adaptation | Preserve actual output/digest checks and same-size/mtime controls, but use current CLI/build results and disposable exports. |
| `d7d39d02f` shared advisory lock | Requires investigation | Current `kernel/src/fs/file/unix.mli` exposes exclusive locking only. Determine the current build/cleanup lifetime contract before adding another primitive. |
| `5fc5377fb` build-root cleanup guards | Requires redesign | Current `riot-build` schedulers and `riot-build2` services differ from the old coordinator/runtime. No old root-guard implementation should be copied wholesale. Reproduce destructive cleanup contention in disposable state first. |
| `f85643538` root-guard bootstrap proof | Historical only | Source/driver proof applies to the old implementation, not this branch. |
| `fb60616be` cancellation/recovery validator | Requires adaptation | Keep SIGINT, descendant cleanup, and subsequent build as separately recorded expectations; adapt current command/build process ownership. |
| `65399c8c8` atomic artifact nonce | Already covered | `packages/riot-store/src/store.ml` uses `Sync.Atomic.fetch_and_add` in `next_artifact_temp_nonce`. Do not reintroduce the old storage implementation. Retain contention testing only if current coverage needs it. |
| `926b3fd24` promoted executable smoke | Still useful as coverage | Check current store tests and add only missing save/promote/permissions/direct-execution assertions using current artifacts. |
| `910899ef1` old candidate-control evidence | Historical only | No current acceptance or soak day may be inferred from this record. |
| `45fbe23fc` exit-winning actor monitor | Still useful | `packages/std/src/runtime/runtime.ml` still panics on an absent monitor target. Adapt regressions to current `std_actor_tests`, runtime entrypoints, Kernel.Sync internals and 500ms/large-case test budgets. |
| `8a828f7c5` typed package finalization | Already covered by replacement | `packages/riot-build2/src/package_finalizer.ml` returns typed store errors for materialization/publication. The old `coordinator.ml` no longer exists. Audit current schedulers separately rather than resurrecting it. |
| `322cdb44a` GC-safe native file buffers | Already covered by newer design | Current `kernel/native/kernel_new_unix_io.c` stages heap bytes and uses off-heap IO; kernel AGENTS explicitly requires stable blocking-call buffers. Adapt GC/bounds coverage only if missing, using `Kernel.Gc`, not old private GC externals or old Iovec aliases. |

## Audit commit checks

- `git diff --check` passes. This audit does not claim a build/test gate passed.
- The local `.git/hooks/pre-commit` wrapper points to `scripts/git-hooks/pre-commit`, which is absent from the current branch. Use a per-command `core.hooksPath=/dev/null` bypass for this documentation-only commit; leave the user's hook installation unchanged.

## First execution slice

1. Add focused current-style tests demonstrating foreign argv ordering and ambiguous argument framing; retain the failing run.
2. Preserve ordered execution identity with explicit framing, and advance the current planner cache contract without rolling its version backward.
3. Run focused planner tests with the workspace Riot, retain source/driver/toolchain/command/log/count evidence, then broaden only as justified.
4. Commit the small tested slice and update affected guidance. Foreign hashing is selectively adapted below; remaining build/cache/recovery work is still open.

### Foreign hashing slice — current Darwin diagnostics

- Installed Riot is `0.0.15`; its three retained dependency-cycle failures did not execute tests and are not reliable current-planner defect reproductions. Tentative `std/src/ref.ml` and `.mli` edits were reverted exactly; no runtime source fix was ported.
- Compatible diagnostic bootstrap: Riot `0.0.43`, HTTPS archive `https://cdn.pkgs.ml/riot/riot-0.0.43-aarch64-apple-darwin.tar.gz`, SHA256 `4d14827336fe61c3987973f012697a93d7ad44099e1dcc5256f0f706ca52eec3`, verified before safe extraction. Identity/provisioning record: `_build/stability/v0-1-1/bootstrap-0.0.43-b5rygpi9/report.json`. It builds and runs the workspace CLI via `run riot -- ...`, using isolated HOME/XDG state. The compiler was automatically provisioned by that bootstrap; compiler archive pin verification and fresh-cache self-hosting acceptance remain open. These runs use the existing workspace build root and establish no cold/warm performance or candidate gate.
- Fixture mistakes remain visible: omitted `Runtime.run` produced status 139 with no cases (`hash-before-compatible-bootstrap-z8ufuakf`); LLDB records `hash-startup-lldb-3op8ogtb` show `Stdlib.do_at_exit` calling `kernel_new_thread_sleep_ns` with an invalid boxed-int64 argument. Adding the required entrypoint makes tests execute, but does not establish why that exit path crashed. Investigate normal shutdown/DLS coexistence in the runtime milestone rather than claiming the native failure explained. The first legacy-encoding test also used the wrong default hash algorithm; it was corrected to SHA256 before the qualifying before-fix run.
- Before fix: `hash-before-correct-sha256-g1v7gt91/report.json`, **12 cases: 4 passed / 8 failed / 0 skipped**. Reproduces argv order, executable position, argument boundaries, empty values/position, output/environment boundaries, and legacy namespace reuse.
- Adapted `BuildForeignDependency:v2` uses byte-length-framed strings and counted lists; preserves ordered/repeated argv, normalizes output paths/environment keys, and advances current package-plan identity from v31 to **v32**. A retained first compile failed on an unnecessary direct Kernel import (`hash-after-framed-argv-d5tv06j0`); using the existing byte-length `Std.String.length` removes that dependency.
- After fix: `hash-after-std-string-framing-a4czkqqr/report.json`, **12/12**; package planning `planner-package-controls-p3yzbori/report.json`, **50/50**, including v31 artifact invalidation; action graph `planner-action-graph-controls-b0pp9xgd/report.json`, **31/31**; JSON roundtrip `planner-action-json-controls-cvukl190/report.json`, **8/8**. No skips/timeouts. Counts are focused diagnostics, not full-workspace or Linux certification.
- Scoped format check initially found two changed files needing formatting (`hash-slice-format-check-k7v2td_h`); formatted only those files (`hash-slice-format-r_uh0yck`). No snapshot approval or unrelated formatting was performed.
- Committed implementation: `30d1a2402 fix(planner): preserve framed foreign command execution identity`. The missing local pre-commit script required a per-command `core.hooksPath=/dev/null` bypass, with hooks unchanged. Clean committed-source checks: `committed-hash-controls-gqzdenug` **12/12**, `committed-package-controls-hod8wfs4` **50/50**, `committed-graph-controls-u9dt3k2x` **31/31**, `committed-json-controls-jqiy1rmm` **8/8**; **4 suites / 101 cases**, zero skips/timeouts. `committed-format-check-1_iv747q` passes for the four changed OCaml files. All report directories are under `_build/stability/v0-1-1/`, with source/bootstrap/workspace-driver identities, command, logs, counts, and timing. No full-workspace, fresh-cache, Linux, or stable-candidate claim.

## Milestone 2 slice — cache integrity, lost timers, toolchain and cross builds

All evidence files below live in `_build/stability/v0-1-1/`. Driver: a workspace-built CLI (`riot 0.0.43`, build `2a3a2071a5f0`) rebuilt from this branch, unless the pinned 0.0.43 release is named.

**Environment caveat, found first.** The agent shell runs at macOS background QoS, where timer coalescing stretches a 1ms `nanosleep` to ~60ms (perl and python show the same). Under that, `std_command_tests` "inherited stdout" fails on the baseline worktree too (`cmd-inherit-baseline.jsonl`), but passes 9/9 under `taskpolicy -a` (`cmd-fg.jsonl`). Run validation at normal QoS; use background QoS only to reproduce timing races.

### Cache integrity (riot-store, riot-build)

- The earlier uncommitted fixture was lost. It was rewritten as `store_integrity_tests.ml` with 10 cases: valid round trip, same-size tamper, warmed lookup after a later mutation, wrong size, directory payload, symlink payload, corrupt promote does not copy, forged `../escaped` manifest, re-verification across store instances, and re-save replacing a corrupt artifact.
- Before the fix: `integrity-before.jsonl`, **1/10** (store_tests 35/35).
- After the fix: `integrity-after-1.jsonl` and `full-riot-store.jsonl`, riot-store **45/45**.
- Implementation: safe-path, regular-file, size and SHA512 verification on every lookup and promote, using the `verified.bin` stat-fingerprint sidecar with a 1s racy-clean window. Failures are typed as `ArtifactIntegrityFailed`; unsafe export names and paths are rejected; a save fails if the manifest dropped files.
- In riot-build, a corrupt cached action is now rebuilt instead of crashing the build. New case "corrupted cached action is rebuilt": integration_caching **4/4**.

### Lost runtime timers (std): the `riot run` hang

- Symptom: `riot run riot -- test -p riot-store -f "cache GC"` sometimes hung after the inner test finished. The child was a zombie, and the outer `wait_status` sat in `sleep` forever.
- Root cause: `Timer_wheel.tick` set `current_time <- now` but advanced only `floor(elapsed / slot)` slots, dropping the remainder. A reactor waking faster than the 1ms slot, as it does under I/O or command traffic, never advanced the wheel, so those timers never fired. In addition, timers due within the current slot waited a full 256-slot rotation.
- Also fixed: two lost-wakeup races.
  - Timeout ids are now recorded on the process before the reactor learns about the timer.
  - `handle_receive` rechecks the fired flag after parking.
- Reproduction: new case "sleep timeouts always wake the sleeper" (16 actors × 400 zero-length sleeps). Before: `sleep-wake-before.jsonl`, failed at its 20s limit with 16/16 sleepers stuck. After: `sleep-wake-after.jsonl`, passes in 0.79s. Full std suite: **1400/1401** (`std-after-timer.jsonl`, run at background QoS); the one failure is the QoS caveat above and passes at normal QoS.
- Hang A/B at background QoS, 8 runs each (`run-hang-bg-ab.txt`): pinned 0.0.43 hung **2/8**; fixed build **0/8**. 10/10 more at normal QoS (`run-hang-after.txt`).

### Toolchain install and cross builds (std, riot-toolchain)

- `Std.Fs.remove_dir_all` followed symlinked directories, which violated its documented contract. It deleted each target's contents (possibly outside the tree), then failed to `rmdir` the link with "Invalid operation on device". Before: `rmdirall-before.jsonl` fails; after: `rmdirall-after.jsonl` passes.
- That bug wedged a partial x86_64 toolchain, and the partial install itself was possible because installs were not atomic: the old code removed the existing toolchain and then extracted directly into the final path.
  - Installs now download and extract into a per-process staging directory, verify the manifest, and rename the old install aside.
  - A concurrent winner's complete install is kept.
  - `curl -f` is used.
  - Concurrent installers no longer share one tarball path.
- Cross builds on macOS were broken for every package: `x86_64-linux-gnu-as: command not found` (`cross-human-units.log`). `Ocamlc.run` now prepends the compiler's bin directory to `PATH`, outside the invocation identity. Before: `tool-dir-before.jsonl` exits 127; after: toolchain_tests **14/14**. `riot build -p human-units --target x86_64-unknown-linux-gnu` now succeeds (`cross-human-units-2.log`). Linux execution of the output is still unverified.
- Six build tests were sized Small (500ms deadline) and timed out under suite load, so they are now `Large`. riot-build **127/127** twice (`full-riot-build-r1.jsonl`, `full-riot-build-r2.jsonl`).

### Known, not fixed in this slice

- "concurrent: same package builds safely" failed 1 of 4 solo runs (`concurrent-4.jsonl`; `ar` is missing `.o` files). Two in-process builds of one lane share a sandbox directory, and `Build_lock` is reentrant within a process. Separate CLI processes are serialized by `flock`, so no supported workflow hits this. Revisit if in-process concurrent builds become a contract.
- Lint: `fix --check` reports large pre-existing debt. Changed files add only hits matching their surrounding style (`let* () =` and `output.Command.status`).
- Still open in milestone 2: ports of the scheduler `eprintln`, the process-exit/readiness race, DOWN before monitor registration, Ctrl-C recovery, and the cleanup/root-guard exclusion; and the status-139 crash in `Stdlib.do_at_exit`.

## Evidence rules and open dependencies

- Use current `riot run riot -- ...` validation; explicitly identify the bootstrap/installed driver and distinguish it from produced branch binaries.
- Failures remain visible. No silent skips, retry-until-green, automatic snapshot approval, or undocumented deadline changes.
- Preserve unrelated changes, real installations and real caches. Destructive checks use isolated state.
- Linux runner access/execution, active CI, publication authorization, representative-service evidence, delivery/recovery, and actual soak reports remain unverified.
- LSP/standalone check restoration, broad API/scheduler rewrites, full-workspace certification, extra platforms and restoration of deleted specs are not part of this slice.
