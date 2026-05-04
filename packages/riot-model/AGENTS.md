# riot-model AGENTS

`riot-model` defines the shared types for the build system: workspaces, packages, modules, actions, events, targets, and errors.

## Rules

1. Keep this package free of execution policy. It is the shared vocabulary for the rest of riot.
2. Prefer structured variants and records over loosely typed payloads.
3. Scoped package phases (`Build`, `Runtime`, `Dev`) live here; changes to that shape usually require follow-up in planner, executor, server, and CLI code.
4. Be conservative about breaking public type shapes.
5. Workspace build-path configuration lives in the root `riot.toml` under `[riot].target_dir`; treat that as the source of truth for `_build`-style paths.
6. Formatter ignore configuration lives under `[riot.fmt]` (`ignore = ["substring", ...]`) on both workspace and package manifests. Bare `[fmt]` is only a compatibility fallback.
7. The default `debug` profile is the debugger-friendly baseline: native code with debug symbols and minimal optimization (currently `-inline 0` plus `-g`).
7a. The default `fuzz` profile is the fuzzing baseline: native debug code plus AFL-compatible OCaml instrumentation. Keep it suitable for `riot fuzz`, not release benchmarking.
8. `Ocaml_compiler` owns the shared OCaml warning/flag vocabulary and its string codec for planner and toolchain packages.
9. `User_config` is the shared `~/.riot/config.toml` vocabulary. Registry entries carry `api_url`, `cdn_url`, and `api_token`; preserve those fields across parse/save/update paths.
10. Test support trees under `tests/fixtures/`, `tests/generated/`, and `tests/diagnostics/` are non-compilable inputs. Keep them out of `Package.sources.tests` and test-binary autodiscovery.
11. Lockfile dependency entries should stay flat and exact for registry packages: render `name`, `version`, and `sha256` directly in the dependency table.
12. `Lockfile.t` carries a required `dependency_hash` derived from the raw `[dependencies]`, `[build-dependencies]`, and `[dev-dependencies]` sections of workspace manifests. Treat it as the staleness contract for `riot.lock`, not as optional metadata. Empty lockfiles should still serialize a concrete `packages = []` field so bootstrapped workspaces round-trip cleanly.
13. `Workspace_manager.scan` returns `Workspace_manifest.t`, the scanned declaration form of a workspace. Keep `Workspace.t` as the build-ready workspace shape produced later by `riot-deps.ensure_workspace`.
14. Member package manifest decode failures are workspace load errors. `Workspace_manager.scan` should preserve those failures in `load_error list` so CLI commands can fail honestly.
15. `Package.t` is a private record. Read fields freely, but construct synthetic packages through `Package.synthetic` and keep new package creation logic inside `Package`.
16. Keep `Package` values canonicalized at construction time. Hash-relevant lists such as dependencies, binaries, source buckets, fix providers, and override tables should be sorted or deduped when a `Package.t` is created or scope-adjusted, not re-sorted inside `Package.hash`.
17. Package-management progress belongs on the shared `Event.kind` surface. When `riot add` / `riot rm` / `riot update` need new lifecycle reporting, add structured PM events here.
18. `Workspace_manager.scan` should surface real member and local-package manifest failures while deferring missing external `path` dependencies that also carry a publishable fallback (`version` or `source`) to `riot-deps`.
19. Repository-local operational policy belongs in `.riot/config.toml`, modeled by `Workspace_operational_config`. Keep it separate from `riot.toml` build/package semantics and `~/.riot/config.toml` user config. Test-runner policy such as `[riot.test].small_test_timeout` and `[riot.test].flaky_max_retries` belongs here too.
20. `Workspace_manager.scan` should resolve the build root in one upward pass: prefer the nearest enclosing workspace manifest with `[workspace]`, but if none exists, fall back to the first package manifest with `[package]` and synthesize a one-package workspace so detached package roots still build.
21. `Package.from_toml` should autodiscover a default runtime binary named after the package when `src/main.ml` exists and no explicit runtime `[[bin]]` entries are declared. Apply the same suppression rule per bucket for test/example/bench autodiscovery: declared binaries make that bucket explicit.
22. Workspace manifests may carry an optional `[workspace].name`. Preserve it across parse/load/make paths without turning it into execution policy.
23. Any helper that derives build/cache/sandbox/output paths from a `Workspace.t` must use `workspace.target_dir_root` as the base, not `workspace.root`. Synthetic or cloned workspaces rely on that override being respected end-to-end.
24. `Target.t` is the shared target-triple identity and should stay an alias of `Std.System.TargetTriple.t`. Raw triple parsing belongs in `std`; `riot-model.Target` should only add build-level request parsing, set operations, and configured-target resolution.
25. Package-name validation belongs in `Package_name`. New typed boundaries should prefer `Package_name.t` over raw strings for parsed package identities, and `Package.validate_name` is only a compatibility wrapper.
26. Scoped package projections for `Runtime` and `Dev` must preserve `build_dependencies` as hash-relevant metadata. Those scopes still exclude build-only outputs, but changing a manifest build-dependency path/version/source must invalidate the package cache.
27. `Package.for_scope` may further narrow `Dev` projections by artifact kind. When callers select only tests, examples, or benches, keep the filtered binaries and source buckets aligned so planner/build consumers see one coherent scoped package.
28. Typed parse boundaries belong here. Modules such as `Package_name` and `Target` should return structured error variants plus `error_message` so the rest of the build stack can branch on failure shape.
