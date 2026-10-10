# riot-build AGENTS

`riot-build` is no longer a daemon boundary. It is the in-process build session and orchestration layer used by `riot-cli`.

## Rules

1. Keep this package one-shot and local. Do not reintroduce background service, RPC, or socket assumptions.
2. Session orchestration belongs here; user-facing argument parsing belongs in `riot-cli`.
3. If behavior starts looking transport-shaped, first ask whether it should be deleted instead.
4. Changes here commonly affect `riot-cli` and `riot-planner`.
5. Artifact lookup must respect the active build lane (`profile` + `target`) instead of assuming host/default output directories.
6. Public command entrypoints should expose typed request/event/error APIs from this package; `riot-cli` should only translate `matches` into those requests and render the resulting events.
7. Normal command entrypoints should assume `riot-cli` already validated workspace load errors; do not reintroduce `load_errors` plumbing into public request types unless a command truly needs partial-workspace behavior.
8. Requested profiles must propagate end-to-end through the local runtime/worker path. Do not recompute `debug` inside the runtime or build worker once a request already carries `profile`.
9. Test-suite discovery narrowing belongs here. When the CLI parses `package:suite`, carry the suite filter through typed test requests and apply it before building/running binaries instead of forwarding the whole selector as a per-test query string.
10. `build` returns a typed `Build_result.t`. Downstream command packages should consume that result directly instead of round-tripping through the runtime/store to rediscover artifacts by name.
11. `Build_result` is package-name keyed, not executor-key keyed. When executor results contain the same package in multiple scopes, merge them and prefer `dev` artifacts/exports over `runtime`, and `runtime` over `build`.
12. Public build entrypoints consume `Riot_model.Workspace.t`, the build-ready workspace shape produced by `riot-deps.ensure_workspace`. Do not reintroduce scanned `Workspace_manifest.t` or separate “prepared workspace” wrapper types into the public `riot-build` request surface.
13. Successful builds now record workspace cache generations through `riot-store`. Keep that hook lightweight and best-effort: build success should not turn into command failure just because cache bookkeeping had a problem. Recorded generations must include the full reachable cache closure needed to keep a warm rebuild warm, including exported action-artifact hashes referenced by cached package artifacts. `riot-build` should always hand successful closures to `riot-store`; `riot-store` owns the cheap state-based decision about whether a cached build can skip generation bookkeeping entirely.
14. `install` promotion failures are fatal. Do not downgrade failed project-root or `~/.riot/bin` promotion into warnings or synthetic success.
15. `run`, `install`, `test`, and `bench` belong in their owning packages on top of `riot-build`. Do not move command-specific orchestration back into `riot-build`.
16. Keep `riot-build` focused on prepared-workspace build resolution and execution. External source or registry loading belongs in `riot-run` / `riot-install`, not in the core build facade.
17. Suite execution and aggregation belong outside `riot-build`. This package should expose build-domain events, not test- or bench-runner policy.
20. Build locks and any other runtime-owned workspace paths must respect `workspace.target_dir_root`. Do not derive `_build`-style locations directly from `workspace.root`.
21. Internal build-session payloads should carry typed target triples, not reparsed target strings. If a worker/runtime boundary needs a target, pass `Riot_model.Target.t` through and stringify only where the filesystem or toolchain process actually needs it.
19. Keep the root `Riot_build` facade as build-focused as possible. Build entrypoints, typed build requests, `Build_result`, build events, build errors, and the build lock surface belong at the top level. Do not reintroduce command-runtime modules or other transport-shaped internals through `Riot_build`.
20. Package/workspace scaffolding does not belong here. `riot-build` must not expose package creation helpers; `riot-init` owns that API.
22. `Build_spec.make` ends in `unit` so its optional `requested_parallelism:int` argument can be omitted. The stored value is `int option`; do not declare the optional argument itself as `int option` and accidentally introduce a second option layer.

23. Cached-action materialization failures must return `Failed (ExecutionFailed ...)` and emit failure telemetry, not raise. Workers must still deliver a completion result to the coordinator when filesystem promotion fails.

24. Public build runtime execution holds a shared `riot-store` build-root guard through execution, promotion, generation bookkeeping, and result preparation. `BuildLock.acquire` also guards its callback, including reentrant calls; independent builds/targets retain shared access. Bare `BuildLock.wait` is only a low-level lane lock, not a complete build-root lifetime guard. Cleanup requires the corresponding exclusive sibling lock and must not delete active work.

## Validate

`timeout 30 riot build riot-build`
