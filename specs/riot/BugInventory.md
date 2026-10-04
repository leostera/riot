# Riot Bug Inventory

This file is the running list of `riot` properties that currently look buggy
under the extracted specs.

The inventory began as spec triage. Daily-driver stabilization now adds executable
regressions and records fixes without erasing historical counterexamples.

Workflow:

- extract one small spec slice from the current implementation
- run TLC against a passing smoke model and, when useful, a failing bug model
- if the spec exposes a likely design bug, add the property here
- reproduce prioritized findings in OCaml tests and record their resolution

## Status Meanings

- `suspected`: the property looks wrong from code reading or partial modeling,
  but we do not yet have a tight failing spec
- `spec-failing`: the current extracted TLA+ slice produces a counterexample
- `spec-failing + impl-reproduced`: the spec fails and we have also reproduced
  the same behavior directly against the current implementation

- `resolved + regression-tested`: the implementation fix passes executable
  regressions; the historical model may still describe the pre-fix behavior

## Tracked Bug Properties

### 1. Action Cache Must Respect Command Order

Status: `resolved + regression-tested`

Spec slice (historical pre-fix semantics):
- `ActionCache.tla`
- `ActionCacheCommandOrderBug.cfg`

Property:
- Different foreign executable/argv lists must not alias in the action cache.
  `build_cmd` is one executable followed by arguments, not a shell-command sequence.

Reproduced behavior:
- Sorting `build_cmd` made `["cp", "source", "destination"]` and
  `["cp", "destination", "source"]` share a hash.
- Unframed strings also conflated argument boundaries and empty arguments.
- Five cases in `packages/riot-planner/tests/action_hash_tests.ml` failed before
  the fix. The original abstract `Prep`/`Compile` model represents the ordering
  issue, not the executor's literal command invocation contract.

Resolution:
- `BuildForeignDependency:v2` preserves argv order and frames string lengths and
  list counts, including the other foreign-action fields.
- Planner artifact contract v15 invalidates v14 plan bundles and package artifacts
  so saved action hashes cannot bypass the new encoding.
- Twelve action-hash tests pass, including order, boundaries, empties, legacy
  invalidation, normalized outputs/environment keys, and JSON roundtrips.
- The existing stale-plan regression now seeds a v14 entry and verifies replanning.

Primary source area:
- `packages/riot-planner/src/action.ml`
- `packages/riot-planner/src/package_planner.ml`
- `packages/riot-build/src/action_executor.ml`

The TLA+ slice is retained unchanged as a historical counterexample, not evidence
that the new encoding is still faulty. Other action variants have not received a
complete encoding audit in this fix.

### 2. Plan Bundle Cache Key Must Invalidate On Toolchain Change

Status: `spec-failing`

Spec slice:
- `PlanBundleToolchainInvalidation.tla`
- `PlanBundleToolchainInvalidationBug.cfg`

Property:
- A persisted plan bundle must not be reused across toolchain changes unless
  the restored action hashes are recomputed for the new toolchain.

Why it looks buggy:
- `compute_input_hash` decides the planner bundle cache key
- that key currently includes build context, package metadata, workspace-local
  dependency details, and transitive dependency hashes, but not the toolchain
- `Action_node.make` does include the toolchain hash in every action-node hash
- so a warm-plan cache hit can restore an action graph whose stored hashes
  were computed under an older toolchain

Primary source area:
- `packages/riot-planner/src/package_planner.ml`
- `packages/riot-planner/src/action_node.ml`

Counterexample shape:
- the first plan stores an action hash derived from `toolchain-v1`
- the second plan uses `toolchain-v2` but computes the same plan-bundle key
- the planner takes a cache hit and restores the old `toolchain-v1` action
  hash instead of replanning or rehashing

Deferred follow-up:
- write an OCaml regression test only after we finish the spec pass

### 3. Action Scheduler Must Count Skipped Nodes Toward Completion

Status: `spec-failing`

Spec slice:
- `ActionSchedulerCompletionAccounting.tla`
- `ActionSchedulerCompletionAccountingBug.cfg`

Property:
- When a dependent node is marked `Skipped` because an upstream dependency
  failed, the scheduler/executor interaction must still count that node toward
  global completion.

Why it looks buggy:
- `Action_queue.next` can mark a node `Skipped` immediately after seeing a
  failed dependency
- `action_executor.execute` increments `completed_count` only on worker
  `ActionCompleted` messages
- skipped nodes are therefore recorded in `queue.completed` without increasing
  `completed_count`
- the executor can become quiescent with no ready work and no busy workers,
  but still believe it is waiting for more completions

Primary source area:
- `packages/riot-executor/src/action_queue.ml`
- `packages/riot-executor/src/action_executor.ml`

Counterexample shape:
- `A` runs and fails
- dependent node `B` is marked `Skipped` inside `Action_queue.next`
- `queue.completed` contains both `A` and `B`
- `completed_count` is still `1`, so the executor is under-counted at
  quiescence

Deferred follow-up:
- write an OCaml regression test only after we finish the spec pass

### 4. Package Cache Short-Circuit Must Materialize Every Export

Status: `spec-failing`

Spec slice:
- `PackageCoordinatorCacheShortCircuit.tla`
- `PackageCoordinatorCacheShortCircuitBug.cfg`

Property:
- A package-level cache short-circuit must not report the package as cached
  unless every declared export is present in the target directory afterwards.

Why it looks buggy:
- `maybe_short_circuit_cached_package` short-circuits on the package hash
  artifact and then asks the store to materialize missing exports
- `materialize_package_exports` currently logs a warning and returns `Ok ()`
  when an export source is missing from the action-level store
- the coordinator treats that `Ok ()` as successful rematerialization and
  marks the package `Cached`
- the target directory can therefore stay incomplete even though the package
  is reported as a cache hit

Primary source area:
- `packages/riot-executor/src/coordinator.ml`
- `packages/riot-store/src/store.ml`

Counterexample shape:
- the package hash artifact exists
- `lib.cmxa` can be materialized but `lib.cmxs` is missing from the store
- the coordinator still sets the package status to `Cached`
- the target directory ends with only `{"lib.cmxa"}` instead of all exports

Deferred follow-up:
- write an OCaml regression test only after we finish the spec pass

### 5. Pending Package Failure Propagation Must Update The Package Graph

Status: `spec-failing`

Spec slice:
- `PackageCoordinatorPendingFailurePropagation.tla`
- `PackageCoordinatorPendingFailurePropagationBug.cfg`

Property:
- Once a pending package is resolved to a failed result because one of its
  dependencies failed, the returned `package_graph` must no longer report that
  package as `Unplanned`.

Why it looks buggy:
- `try_plan_pending_packages` revisits packages parked in `pending_planning`
- in the `deps_failed` branch it inserts a failed `package_results` entry and
  removes the package from `pending_planning`
- but it does not update the corresponding `package_graph` node
- the result table and returned graph can therefore disagree about the same
  package's state

Primary source area:
- `packages/riot-executor/src/coordinator.ml`
- `packages/riot-planner/src/package_graph.ml`

Counterexample shape:
- `Dep` is already failed
- pending package `Pkg` is revisited and resolved to a failed result
- `Pkg` leaves `pending_planning`
- `package_graph_state["Pkg"]` still says `unplanned`

Deferred follow-up:
- write an OCaml regression test only after we finish the spec pass
