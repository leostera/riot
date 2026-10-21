# riot-build AGENTS

`riot-build` is the in-process build session and orchestration layer used by command packages and `riot-cli`.

## Rules

1. Keep this package one-shot and local, with in-process entrypoints.
2. Public entrypoints should expose typed requests, events, errors, build locks, and `Build_result.t`; `riot-cli` translates CLI matches and renders events.
3. Consume `Riot_model.Workspace.t`, the build-ready workspace shape produced by `riot-deps.ensure_workspace`. Commands that need partial-workspace behavior should model that explicitly at their own boundary.
4. Respect build lanes everywhere. Profiles, targets, artifact selectors, build locks, and output paths must flow from the request and `workspace.target_dir_root`, not from host defaults or hardcoded `_build` paths.
5. Downstream command packages should consume `Build_result.t` directly. `run`, `install`, `test`, `bench`, and other commands should receive the artifacts they need from the build result.
6. Keep command-specific orchestration outside this package. External source/registry loading belongs in `riot-run` / `riot-install`; suite execution and aggregation belong above `riot-build`; scaffolding belongs in `riot-init`.
7. Keep package/action execution inside the current schedulers: `package_scheduler`, `action_scheduler`, and shared `graph_scheduler`.
8. Keep scheduler internals behind build-domain facades. Public events should expose package phases and build summaries, not generic graph nodes, worker tables, or mutation details.
9. Successful builds record reachable cache generations through `riot-store`. Keep that bookkeeping lightweight and best-effort so cache metadata failures stay separate from build success.
10. Build progress should flow through the explicit `on_event` callback as `Riot_model.Event.t`; do not add global telemetry wrappers or package-local event variants.
11. Actor-owned helpers should defer actor creation until runtime use so test binaries and embedded callers can boot their runtime first.
12. Keep planner failures typed through the public build error path so `riot-cli` can render useful structured diagnostics.
13. Cached action/package results carry full `Riot_store.Artifact.t` values. Use `artifact.input_hash` for store paths and export materialization; use `artifact.output_hash` only when strengthening downstream invalidation.
14. An action cache hit only counts once `promote_action` into the sandbox succeeds. If integrity verification rejects the cached artifact, log it and rebuild the action instead of failing the build.
15. Tests that run real builds or compiler invocations must be `~size:Large`. Small tests get a 500ms runner deadline that full builds cannot reliably meet under suite load.
