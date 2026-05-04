# riot-cli AGENTS

`riot-cli` owns command parsing, user-facing output, and top-level command flow. Domain behavior belongs in the package that owns the command.

## Rules

1. Keep the CLI thin. Parse arguments, resolve workspace context, call package libraries, and render returned events.
2. Reserve stdout for command payloads: JSON, completion scripts, binary/test output, and other requested results. Send progress, status lines, and human errors to stderr.
3. Commands that touch build artifacts must resolve the workspace root and honor `[riot].target_dir`.
4. Built-in commands with package owners should delegate: `riot-check`, `riot-fmt`, `riot-fix`, `riot-publish`, `riot-deps`, `riot-init`, `riot-run`, `riot-install`, `riot-store`, `riot-toolchain`, `riot-lsp`, and `riot-bench`.
5. Keep command request construction explicit. CLI modules should parse `matches`, build typed request values, call the owning package, and render typed events.
6. Keep human and JSON rendering behaviorally aligned. JSON mode should emit JSONL on stdout and include terminal success/error records where the command owns a lifecycle.
7. Build locks must be scoped to the effective build lane. Surface lock waits through structured events, not ad hoc output.
8. Workspace-required commands should render shared neutral workspace guidance when no workspace is found. Workspace-free commands such as `search`, `upgrade`, and `toolchain list-available` should start without workspace scans.
9. Keep workspace and `~/.riot` setup lazy. Built-ins should initialize only the state they need.
10. `riot lsp` must stay a thin delegate into `riot-lsp`, with stdout reserved for the LSP protocol.
11. `riot check` should delegate typechecking flow to `riot-check`; `Check_cmd` stays a wrapper over `Riot_check`.
12. `riot fmt --explain <id>` and parse diagnostics should delegate directly to `syn` / `krasny` surfaces.
13. `riot fix` should keep rule-oriented and diagnostic-oriented surfaces distinct: `--list-rules` describes rules, `--list-diagnostics` describes diagnostic codes.
14. `riot snapshots` owns repository-level snapshot review. Discover pending `.expected.new` files, show diffs, and approve/reject candidates without folding approval into `riot test`.
15. Package-management commands (`add`, `rm`, `update`, `search`, `yank`) should parse flags, delegate to `riot-deps`, and reuse the shared PM event renderer.
16. Outside a workspace, `riot add` may bootstrap a minimal root workspace. `riot rm` and `riot update` should provide targeted workspace guidance.
17. `riot init` should delegate scaffolding to `riot-init` and render structured init events.
18. `riot run` and `riot install` resolve local workspace binaries before external sources. `riot install` may then fall back to registry packages; `riot run` stays workspace-local unless the caller selects a remote source form.
19. Keep `--local` scoped to workspace-binary installs.
20. Remote source runs/installs should reuse cached checkouts by default and refresh only when the caller opts into update behavior.
21. `riot test` and `riot bench` should build once, then delegate to suite binaries. Use repeated `-p/--package` for package narrowing and `-f/--filter` for forwarded substring filtering.
22. Suite discovery/listing should stream JSONL progress and preserve partial results when some suites fail to build or list.
23. `riot test --json` and `riot bench --json` should expose parent-side progress and heartbeat events so long quiet suite runs remain diagnosable.
24. Suite binaries that emit JSONL lifecycle events should have non-summary progress forwarded as suite-scoped events, with forwarded child progress removed from captured stdout.
25. `riot test` owns the explicit `Std.Test` suite-context handoff through `--ctx <json>`, including workspace root, package name, binary path, source file, and reachable runtime binaries.
26. `riot bench` should delegate benchmark-history persistence and comparison loading to `riot-bench`. Plain `riot bench` and `--compare` are read-only; `--record` persists history.
27. `riot build` should exit when the build is done and keep runtime builds as the default. Tests, examples, benches, and `--all` are explicit artifact selectors.
28. `riot build` human output should render structured planner failures as targeted detail lines.
29. `riot info workspace --json` is the canonical editor-facing workspace introspection surface.
30. User-facing aliases such as `docs` -> `doc`, `toolchains` -> `toolchain`, and top-level `help` -> `--help` should normalize before parsing into the canonical command.
31. `riot --version` and `riot version` should prefer installed release metadata, include release id and build sha, and keep dev-build fallback explicit.
32. `riot upgrade` should stay workspace-free and use published Riot release metadata and archives.
33. `riot fuzz` should stay a thin CLI over `riot-fuzz`: parse flags, render events, and leave fuzz case discovery, selector handling, campaign scheduling, and corpus/crash state under `.riot/fuzzing` to `riot-fuzz`/`riot-test`.
