# riot-planner AGENTS

`riot-planner` turns workspaces and packages into dependency-aware build plans.

## Rules

1. Planning is where graph shape and invalidation rules live. Keep execution concerns out.
2. Preserve deterministic planning output for the same workspace inputs.
3. Scoped package nodes (`pkg.build`, `pkg.runtime`, `pkg.dev`) and their dependency edges are planner-owned behavior; keep those rules explicit.
4. `build-dependencies` should only participate in the build-scope graph. Runtime and dev products use their own dependency closures.
5. Changes here often need matching updates in `riot-model` and `riot-build`.
6. Prefer explicit plan and error types over implicit sentinel values.
7. Keep default library planning limited to `.cma`/`.cmxa` outputs. Add `.cmxs` shared-library actions only when there is an explicit runtime consumer and an opt-in surface.
8. Package-plan cache keys must include all compiler inputs that can change produced artifacts, including the resolved toolchain identity for cross builds.
9. `CreateLibrary` inputs must be `.cmx` from OCaml module deps plus `.o` from `Native` C deps only.
10. Resolved profile-owned compile flags must flow into planned OCaml compile actions. If release/debug profile settings change emitted compiler args, the action graph and planner artifact version must change with them.
11. Warm cached packages should short-circuit from the hash-addressed artifact manifest when possible. Decode the full module/action graph only when execution needs the full plan.
12. Library planning should trim unreachable modules from the final action graph by following resolved `Syn.Deps` edges from the concrete library root.
13. Binary targets should consume a target-private closure derived from their source module and resolved deps. Analyze binary sources as modules, with library-owned modules linked through the library product.
14. Source-root selection is scope-owned behavior. Runtime nodes only analyze runtime roots, dev nodes analyze dev roots, and build nodes stay empty until an explicit build-time source model exists. Derive planner source groups from the already projected package for that scope.
15. Package-layout validation runs after reachability has produced the actual planned closure. Reject target code that reaches library-internal modules or another target's root module; shared helper modules are fine, target entrypoints are not.
16. Executable target entry files are validated during dependency wiring. Binary, test, example, and bench entry modules must define exactly one top-level `let main ~args = ...` binding.
17. Package input hashes must consume dependency `output_hash` values, not dependency lookup keys. This keeps downstream packages rebuilding when a cached dependency artifact changes under the same planned input.
18. Package root relocation is planner-owned. Keep public package roots separate from compiled roots through `Package_namespace`, and include relocation mode in package input hashes when changing this behavior.
