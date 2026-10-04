# riot spec AGENTS

This directory contains readable formal models for the `packages/riot-*` build
system.

## Source Map

- `packages/riot-planner/src/action.ml`: action hashing rules
- `packages/riot-planner/src/action_node.ml`: action-node hash composition
- `packages/riot-planner/src/package_planner.ml`: planner bundle caching and
  dependency-aware package hashes
- `packages/riot-build/src/action_executor.ml`: action cache lookup,
  execution, verification, and store writes
- `packages/riot-build/src/action_queue.ml`: action dependency scheduling
- `packages/riot-build/src/coordinator.ml`: workspace/package orchestration
  and package-level cache short-circuiting
- `packages/riot-store/src/store.ml`: immutable artifact store, plan bundle
  store, and package export materialization
- `packages/riot-*/tests/*.ml`: executable expectations that should stay aligned
  with the modeled design intent

## Rules

1. Prefer small slice specs over one monolithic build-system model. Slice by
   semantic concern such as action hashing, package cache reuse, or scheduler
   fairness.
2. Default to PlusCal when it keeps the algorithm more readable than hand-written
   `Next` actions. Keep the readable algorithm block as the primary thing humans
   audit.
3. Comment every abstraction boundary. If a field from the OCaml code is omitted,
   say why that omission is safe for the slice.
4. Keep the current code shape visible in the model: use names like
   `ActionHash`, `CacheHit`, `CacheMiss`, `materialized`, and `cacheOwner`
   instead of generic mathy placeholders.
5. Separate structure laws from semantic laws. Type/bounds invariants should
   pass in the smoke config even when a bug-reproduction config intentionally
   fails a higher-level property.
6. When a slice is centered on a likely design bug, keep one passing smoke config
   and one failing `*Bug.cfg` that demonstrates the mismatch explicitly.
7. Update `README.md` whenever the modeled source map, abstraction boundaries,
   or validation commands change.

## Validate

From the repo root:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  pcal.trans specs/riot/ActionCache.tla

java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/riot/ActionCache.tla \
  -config specs/riot/ActionCache.cfg
```

Run bug configs separately. Some preserve historical pre-fix semantics; consult
`BugInventory.md` before treating a counterexample as a current implementation bug.
For the fixed foreign-action encoding, run `riot test riot-planner:action_hash_tests`
and `riot test riot-planner:package_planning_tests` as well.
