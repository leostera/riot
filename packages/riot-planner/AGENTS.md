# riot-planner AGENTS

`riot-planner` turns workspaces and packages into dependency-aware build plans.

## Rules

1. Planning is where graph shape and invalidation rules live. Keep execution concerns out.
2. Preserve deterministic planning output for the same workspace inputs.
3. Scoped package nodes (`pkg.build`, `pkg.runtime`, `pkg.dev`) and their dependency edges are planner-owned behavior; keep those rules explicit.
4. `build-dependencies` should only participate in the build-scope graph. Runtime and dev products should not accidentally inherit build-only edges.
5. Changes here often need matching updates in `riot-model` and `riot-build`.
6. Prefer explicit plan and error types over implicit sentinel values.
7. Keep default library planning limited to `.cma`/`.cmxa` outputs. Do not reintroduce unconditional `.cmxs` shared-library actions unless there is an explicit runtime consumer and an opt-in surface for it.
8. Package-plan cache keys must include all compiler inputs that can change produced artifacts, including the resolved toolchain identity for cross builds.
9. `CreateLibrary` inputs must be `.cmx` from OCaml module deps plus `.o` from `Native` C deps only. Do not feed ML companion `.o` files into library archive planning.
10. Resolved profile-owned compile flags must flow into planned OCaml compile actions. If release/debug profile settings change emitted compiler args, the action graph and planner artifact version must change with them.
11. Warm cached packages should short-circuit from the hash-addressed artifact manifest when possible. Do not require full module/action graph decode on cache hits unless execution really needs the full plan.
12. Foreign `build_cmd` is an executable followed by ordered argv, not a set. Hash it with count/length framing so reordered, empty, and differently split arguments remain distinct. Foreign action fields use the versioned `BuildForeignDependency:v2` encoding; outputs and distinct environment keys remain order-normalized. Bump the planner artifact contract when action hashing changes, because cached plan bundles carry already-computed action hashes.

## Validate

`timeout 30 riot build riot-planner`
