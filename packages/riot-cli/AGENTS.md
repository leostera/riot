# riot-cli AGENTS

`riot-cli` owns command parsing, user-facing output, and the top-level build workflow.

## Rules

1. Keep the CLI thin. It should orchestrate, not duplicate planner or executor logic.
2. Prefer direct local calls into riot libraries over protocol-shaped wrappers.
3. User-facing messages should stay concise and actionable.
4. When adding commands, update completions and help output in the same change.
5. Built-in commands that own domain logic elsewhere should delegate into their package library.
6. Commands that touch build artifacts must resolve the workspace root and honor `[riot].target_dir` instead of assuming `_build` or `./target`.
7. Keep rule-oriented and diagnostic-oriented fix surfaces distinct: `--list-rules` should describe rules, while `--list-diagnostics` should describe diagnostic codes.
8. Keep `riot test` and `riot bench` on the build-once flow: build the workspace once, then delegate `run-tests [query]` / `run-benchmarks [query]` into every suite binary. Use `-p/--package` for package narrowing rather than CLI-side suite prefiltering.
9. Reserve stdout for command payloads (JSON, completion scripts, binary/test output that is the command result). Send CLI control output such as progress, status lines, and user-facing errors to stderr.
10. Build locks must be scoped to the effective build lane, not the whole workspace. If artifacts are split by profile and target, the lock should be too.
11. Package-scoped warnings and failures should be labeled with the package name exactly once; when replaying multiline compiler payloads, preserve the payload text after the first prefixed line instead of reindenting it.
12. `riot install` must reuse the normal streamed build path. Do not keep a silent private build loop in `install.ml`.
13. CLI workspace commands should reject workspace load errors at the boundary instead of threading partial-workspace state through downstream request APIs.
14. `fix_cmd.ml` should parse `matches`, build a `Riot_fix.fix_request`, call `Riot_fix.fix`, and render the returned output/events. Do not delegate raw `matches` into `Riot_fix.Cli.run`.
15. `publish.ml` should parse `matches`, build a `Riot_publish.publish_request`, call `Riot_publish.publish`, and render publish events. Keep the combined publish command surface in `riot-publish`, not in `riot-deps`.
16. `login` and `logout` should stay thin auth-config commands. They manage `~/.riot/config.toml` through `Riot_model.User_config`; do not turn them into registry-discovery or profile-management commands while the main registry stays hardcoded.
17. `riot snapshots` owns repository-level snapshot review commands. Keep it focused on discovering pending `.expected.new` files, showing review diffs, and promoting or rejecting candidates; do not fold snapshot approval into `riot test`. When a TTY is attached, `riot snapshots review` should be interactive and prompt `[a]pprove / [r]eject / [i]gnore / [q]uit`; keep a non-interactive diff dump fallback for non-TTY use.
18. PM human output should only show `Fetching <pkg> <version>` for real download-start events. Cache-hit materialization events and `PackageResolvedForBuild` stay structured in JSON but should be silent in human mode.
19. `add`, `rm`, and `update` are thin package-management commands. Parse flags into `Riot_deps` request types, delegate, and reuse the normal PM event renderer instead of inventing a second lock/progress surface.
20. `riot add` should accept named registry specs, local path specs, and GitHub source specs. Keep the CLI help text and errors honest about the accepted forms, but keep package-name discovery and Git materialization inside `riot-deps`.
21. Outside a workspace, `riot add` may bootstrap a minimal root workspace (`riot.toml` + `riot.lock`) and then route the first add through the workspace root manifest. `riot rm` and `riot update` should stay no-op/user-guidance commands in that case rather than failing with the generic workspace error.
22. `riot test` selectors should treat `package:suite` as suite discovery narrowing, not as a raw per-test substring. Only the remaining query text should be forwarded into `run-tests`.
23. `riot search` should stay workspace-independent. Parse flags in the CLI, delegate to `Riot_deps.search`, and keep stdout reserved for the search results themselves.
24. Keep `Riot_cli.Cli.run` reusable for in-process benches and tools. One-time runtime bootstrapping belongs in `initialize_runtime`, not in every embedded caller loop.
25. Keep workspace scans and `~/.riot` setup lazy. Built-in commands that do not need workspace state or riot-home state should not pay for them during startup.
26. `add` / `rm` / `update` should consume the same shared `Riot_model.Event` PM stream that builds use. Do not duplicate JSON or human rendering logic for package-management-only wrapper events in the command modules.
27. `riot upgrade` stays workspace-free and should reuse the published Riot release archive path plus release metadata JSON. Keep the UX concise, compare the downloaded binary with the installed one before replacing it, write `~/.riot/release.json` for installed metadata, and avoid delegating user-visible control flow to `install.sh`.
28. `riot --version` and `riot version` should prefer installed release metadata when available and render both the release id and build sha. Keep fallback output explicit for dev builds without installed metadata.
29. `riot lsp` must stay a thin delegate into `riot-lsp`, and it must keep stdout protocol-only. Do not run the normal stdout logger/runtime bootstrap for that command.

## Validate

`timeout 30 riot build riot-cli`
`timeout 30 riot test riot-cli:test_selection_tests`
