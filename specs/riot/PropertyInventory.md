# Riot Property Inventory

This file is the extraction backlog for `riot`.

The goal is not to list every helper function. The goal is to enumerate the
current semantic properties that are worth modeling in TLA+, grouped into
small stateful slices we can audit and evolve with the codebase.

## How To Use This

- Treat each section as one candidate spec module or one tightly related spec
  family.
- Keep the slice names stable so we can mark progress over time.
- When a slice is modeled, add the `.tla` and `.cfg` files next to
  `ActionCache.tla` and update the status here.
- Prefer extracting laws from the current implementation and tests, not from
  what we wish the system looked like.
- If a slice exposes a likely bug, record that property in
  `BugInventory.md`.
- During daily-driver stabilization, promote prioritized findings into OCaml
  regression tests and distinguish historical model semantics from fixed code.

## Slice Backlog

### 1. `ActionCache`

Status: `modeled; foreign argv collision fixed with executable regressions`

The historical model still describes the pre-fix hash. Current foreign actions
preserve executable/argv order and use length/count framing; planner contract v15
invalidates cached graphs using the old hashes. See `action_hash_tests.ml` and
`BugInventory.md` for the reproduction and resolution.

Primary sources:
- `packages/riot-planner/src/action.ml`
- `packages/riot-planner/src/action_node.ml`
- `packages/riot-executor/src/action_executor.ml`
- `packages/riot-store/src/store.ml`

Properties:
- Action cache keys are content-derived, not execution-order-derived.
- Action-node hashes form a Merkle-style invalidation boundary over action
  fields, source contents, declared outputs, and dependency hashes.
- A cache hit materializes immutable stored outputs into the sandbox instead of
  re-running the action.
- A cache miss executes the action, verifies required outputs, and stores the
  result in the immutable artifact store.
- Different hashes stay isolated.
- Rewriting the same hash does not replace the first stored artifact.

### 2. `PackagePlanning`

Status: `partially modeled`

Primary sources:
- `packages/riot-planner/src/package_planner.ml`
- `packages/riot-planner/src/Riot_planner.mli`
- `packages/riot-planner/tests/package_planning_tests.ml`

Properties:
- A package cannot be fully planned until its dependency artifacts are
  available.
- Planner bundle cache hits restore the planned graphs instead of rebuilding
  them.
- Stale planner artifact versions are ignored and force a rebuild of the plan
  graphs.
- Planner bundle cache keys must remain aligned with action-hash invalidation
  behavior.

Implemented slice:
- `PlanBundleVersionGate.tla` covers the warm-plan cache acceptance gate
  around bundle presence, version matching, package identity, and parse
  success.
- `PlanBundleToolchainInvalidation.tla` covers the mismatch between planner
  bundle reuse and toolchain-sensitive action hashes.

Still open:
- planner bundle cache rehydration for the remaining execution-relevant state
- dependency state classification for `MissingDependencies` vs
  `FailedDependencies`
- dependency summaries sourced from immutable store paths
- input-hash composition over build context, package metadata, workspace-local
  dependency details, and transitive dependency hashes

Current bug found:
- The current planner bundle cache key does not include toolchain identity,
  even though action-node hashes do. A warm-plan cache hit can therefore
  restore stale action hashes after a toolchain change.

### 3. `ActionScheduler`

Status: `partially modeled`

Primary sources:
- `packages/riot-executor/src/action_queue.ml`
- `packages/riot-executor/src/action_executor.ml`
- `packages/riot-executor/tests/action_queue_workspace_graph_tests.ml`

Properties:
- An action is ready only when all of its dependencies succeeded or were
  satisfied from cache.
- Failed dependencies cause dependents to be skipped instead of executed.
- Requeueing a blocked node also requeues the missing dependencies it is still
  waiting on.
- Completion means every node is accounted for, with no ready, later, or busy
  work left behind.
- Empty graphs terminate immediately.
- Independent actions can continue even when other independent actions fail.

Implemented slice:
- `ActionSchedulerCompletionAccounting.tla` covers one worker-level
  interaction between `Action_queue.next` and `action_executor.execute`:
  skipped dependents are inserted into the completed table, while the outer
  executor loop tracks a separate `completed_count`.

Still open:
- cache-hit readiness vs execution readiness
- blocked-node requeue behavior with missing dependencies
- later-queue transfer rules
- multi-worker scheduling and fairness
- termination/accounting on larger mixed success/failure graphs

Current bug found:
- Skipped nodes are not counted toward global completion in the current
  scheduler/executor interaction. The queue can record a node as `Skipped`
  without producing a worker completion message, so the executor's
  `completed_count` can lag behind the completed table and leave the system
  quiescent but under-counted.

### 4. `PackageCoordinator`

Status: `partially modeled`

Primary sources:
- `packages/riot-executor/src/coordinator.ml`
- `packages/riot-executor/src/package_builder.ml`
- `packages/riot-executor/tests/coordinator_tests.ml`

Properties:
- Package-level cache hits only succeed when every declared export can be
  rematerialized.
- Pending packages are rechecked when dependency results become available.
- Failed dependency reasons survive into final package results.
- Package graph state and package result state must stay in sync.

Implemented slice:
- `PackageCoordinatorCacheShortCircuit.tla` covers the package-level cache
  short-circuit path between coordinator cache hits and store export
  rematerialization.
- `PackageCoordinatorPendingFailurePropagation.tla` covers the pending-package
  wakeup path when dependency results become available.

Still open:
- final package status derivation for built-vs-cached-vs-failed outcomes
- lazy activation rules for pending packages
- package-export manifest deduplication and rematerialization rules

Current bugs found:
- The current coordinator can treat a package as cached even when one export
  never rematerializes.
- The current pending-package failure path can leave the package graph node
  stale while the result table already records the failure.

## Removed From TLA+ Scope

Static graph-shape laws, scanner tagging rules, alias wiring, and direct JSON
round-trips are now tracked as ordinary OCaml-test contracts instead of TLA+
spec slices.
