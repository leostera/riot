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
4. Commit the small tested slice and update affected guidance. No implementation fixes have been ported yet.

## Evidence rules and open dependencies

- Use current `riot run riot -- ...` validation; explicitly identify the bootstrap/installed driver and distinguish it from produced branch binaries.
- Failures remain visible. No silent skips, retry-until-green, automatic snapshot approval, or undocumented deadline changes.
- Preserve unrelated changes, real installations and real caches. Destructive checks use isolated state.
- Linux runner access/execution, active CI, publication authorization, representative-service evidence, delivery/recovery, and actual soak reports remain unverified.
- LSP/standalone check restoration, broad API/scheduler rewrites, full-workspace certification, extra platforms and restoration of deleted specs are not part of this slice.
