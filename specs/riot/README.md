# Riot Specs

This directory contains readable TLA+ models for the current `riot` build
system design.

The spec set is intentionally narrow now: only stateful, interaction-heavy, or
cache-sensitive behavior stays here. Static graph-shape laws and direct
serialization contracts belong in normal OCaml tests.

## Layout

- `ActionCache.tla`: action-level cache semantics for `BuildForeignDependency`
  actions.
- `ActionCache.cfg`: smoke config for the action-cache slice.
- `ActionCacheCommandOrderBug.cfg`: historical failing config for the pre-v2
  foreign-action command-order cache collision.
- `PlanBundleVersionGate.tla`: warm-plan cache acceptance rules for persisted
  plan bundles.
- `PlanBundleVersionGate.cfg`: smoke config for the accepted bundle path.
- `PlanBundleVersionGateStaleVersion.cfg`: stale-version rebuild config.
- `PlanBundleToolchainInvalidation.tla`: planner-cache invalidation versus
  toolchain-sensitive action hashes.
- `PlanBundleToolchainInvalidation.cfg`: smoke config for the no-toolchain
  change baseline.
- `PlanBundleToolchainInvalidationBug.cfg`: failing config for stale hashes
  surviving a toolchain change.
- `ActionSchedulerCompletionAccounting.tla`: scheduler/executor completion
  accounting when dependents are skipped.
- `ActionSchedulerCompletionAccounting.cfg`: smoke config for the one-node
  completion baseline.
- `ActionSchedulerCompletionAccountingBug.cfg`: failing config for skipped
  nodes not advancing global completion.
- `PackageCoordinatorCacheShortCircuit.tla`: package-level cache hits and
  export rematerialization.
- `PackageCoordinatorCacheShortCircuit.cfg`: smoke config for the fully
  rematerializable package-cache-hit path.
- `PackageCoordinatorCacheShortCircuitBug.cfg`: failing config for incomplete
  export rematerialization.
- `PackageCoordinatorPendingFailurePropagation.tla`: pending-package wakeup
  when dependency results become available.
- `PackageCoordinatorPendingFailurePropagation.cfg`: smoke config for a
  successful pending-package wakeup.
- `PackageCoordinatorPendingFailurePropagationBug.cfg`: failing config for a
  pending package that stays stale in the graph after dependency failure.
- `BugInventory.md`: the running list of bug-shaped properties found by the
  extracted specs.
- `PropertyInventory.md`: the current TLA+ backlog for the remaining
  stateful/cached coordination slices.

## Why Start Here

The current cache path is split across a few packages:

- `riot-planner` computes action hashes and plan-bundle cache keys.
- `riot-build` now owns action execution, cache hit vs miss, and materialization
  orchestration. Historical models refer to the former `riot-executor` boundary.
- `riot-store` keeps the immutable action artifact store.

That makes action-level caching a good first slice: it is small enough to
model readably, but central enough that a hash-design mistake becomes a real
build correctness bug.

## How To Work On The Spec

- Keep the model readable first. Use PlusCal when it helps make the
  state changes obvious.
- Keep each slice small and named after one semantic concern.
- For each slice, keep one smoke config and add a `*Bug.cfg` only when the
  current implementation-shaped semantics look wrong.
- Update `PropertyInventory.md` and `BugInventory.md` whenever a slice changes
  the current picture.
- Commit spec work often with conventional commits such as
  `spec(riot): model scheduler readiness requeue`.

## Validation Commands

Run these from the repo root:

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  pcal.trans specs/riot/<Slice>.tla
```

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC specs/riot/<Slice>.tla -config specs/riot/<Slice>.cfg
```

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC specs/riot/<Slice>.tla -config specs/riot/<Slice>Bug.cfg
```

## Current Findings

- `ActionCacheCommandOrderBug.cfg` preserves the historical command-order hash
  collision. The implementation now uses framed, ordered foreign argv and planner
  artifact contract v15. Validate the fix with
  `riot test riot-planner:action_hash_tests` and
  `riot test riot-planner:package_planning_tests`; the old TLA+ config intentionally
  retains its pre-fix semantics.
- `PlanBundleToolchainInvalidationBug.cfg` exposes the planner-cache key
  mismatch between toolchain-insensitive bundle reuse and toolchain-sensitive
  action hashes.
- `ActionSchedulerCompletionAccountingBug.cfg` exposes skipped nodes not being
  counted toward executor completion.
- `PackageCoordinatorCacheShortCircuitBug.cfg` exposes incomplete export
  rematerialization being treated as a cache hit.
- `PackageCoordinatorPendingFailurePropagationBug.cfg` exposes a stale
  package-graph node after dependency failure resolution.
