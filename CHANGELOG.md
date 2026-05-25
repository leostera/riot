# Changelog

## 0.0.43 - 2026-05-15

### riot
- `riot init --bin` starter binaries now print their greeting directly while still loading config and starting logging, so generated projects produce deterministic stdout on Linux smoke tests.
- The Riot binary release script can build from an isolated target directory via `RIOT_RELEASE_TARGET_DIR`, preventing stale `_build/release` outputs from being packaged during forced or repeated releases.

## 0.0.42 - 2026-05-15

### riot
- `riot build --deps` now rejects unsupported package, watch, and dev-artifact flags before workspace resolution, so Docker dependency-cache stages fail with clear argument errors instead of ambiguous build planning behavior.
- CLI commands now surface workspace resolution and command parsing failures through the normal human and JSON error paths across build, plan, test, bench, fuzz, clean, fmt, doc, publish, and toolchain flows.

### blink
- Streaming client and SSE errors are now preserved and reported instead of being silently dropped, making failed or malformed response streams visible to callers.

### kernel
- Linux timer polling now reports timer-drain read failures through the selector result instead of ignoring them, so unexpected kernel I/O failures stop the poller visibly.

### std
- `Fs.ReadDir.next_result` is available for callers that need to distinguish end-of-directory from read failures, and recursive filesystem helpers now propagate directory read, close, and temporary-directory cleanup errors instead of treating them as successful completion.

### suri
- Socket pool read and write failures now preserve their underlying TCP errors, and HTTP/WebSocket handlers log or return those connection failures instead of collapsing them into generic closed-connection cases.

## 0.0.41 - 2026-05-14

### riot
- `riot build --deps` now warms dependency cache layers from `riot.lock` without requiring workspace member manifests. It fetches and builds third-party registry/source libraries, skips workspace/path packages, and is intended for Dockerfile stages that copy only `riot.toml` and `riot.lock` before the full application source.

## 0.0.40 - 2026-05-13

### riot
- Package-provided commands are runnable again from workspaces after their provider package is built. Riot now resolves the generated command binary from the build output, and package commands remain visible through command completions.

### blink
- Recorded request and response body metadata now uses real SHA-256 digests. Recordings created by `Blink.Testing.Recorder` should be regenerated if consumers compare the `sha256` fields from older recordings.

## 0.0.39 - 2026-05-13

### riot
- Watch mode now waits for a fresh source change after a failed build, test run, or watched binary restart instead of immediately replaying queued filesystem noise. This prevents typo-driven compile loops, especially on macOS where FSEvents can emit many metadata and directory events for one save.

## 0.0.38 - 2026-05-13

### riot
- `riot run --watch` now watches the selected binary's workspace dependency cone and restarts the child process when relevant sources change. The `-w/--watch` flag works consistently with `-p/--package`, binary names, and forwarded child arguments.
- Commands that forward child arguments now use `--` as the only parsing boundary. Riot flags can appear before or after positionals before the separator, unknown Riot flags before the separator are rejected, and every argument after `--` is passed through unchanged.

## 0.0.37 - 2026-05-12

### riot
- `riot build --watch` and `riot test --watch` now rerun automatically when files change in the selected workspace package dependency cone. Generated build output, `.riot`, `.git`, `riot.lock`, and pending snapshot candidates are ignored so watch loops do not retrigger on their own artifacts.
- macOS filesystem watching now keeps FSEvents callback state in stable native memory and retains watched paths correctly, preventing watch-mode startup crashes after the initial build.

## 0.0.36 - 2026-05-11

### riot
- `riot test` suite binaries can now share typed setup state between suite hooks and test cases. This keeps the existing case definition API while letting expensive fixtures be created once for a selected suite run.

## 0.0.35 - 2026-05-11

### riot
- `riot test` and `riot fuzz` keep fuzz cases discovered automatically from suite metadata, so packages can run campaigns without hand-maintained target declarations.
- `riot-planner` exposes resolved dependency analysis, and `riot-store` can save prehashed action artifacts. Build tooling can reuse normalized dependency information and avoid hashing artifacts twice when hashes are already known.
- `riot-toolchain` can disable OCaml binary annotation emission for build modes that do not need `.cmt` or `.cmti` files.
- `riot-deps` rejects invalid source subdirectories before they enter manifest or lockfile flows.

## 0.0.34 - 2026-05-10

### riot
- CLI build output now flows through a centralized UI event pipeline. Commands that execute builds, including build, test, run, bench, fuzz, install, and related flows, share the same TUI, line-by-line, and JSON event rendering path.
- Build, dependency, cache, planner, and test events now use a shared structured event envelope from `riot-model`, so JSON output is more regular and easier for tools to consume.
- `riot trace` introduces typed telemetry spans and trace path handling for profiling Riot commands and build-system work.
- Build planning and cache paths continue to move toward graph-native execution, with reused source summaries, better artifact metadata, and faster warm package cache checks.

## 0.0.32 - 2026-05-04

### riot
- Added `riot fuzz`, a coverage-guided fuzzing command for `Std.Test.fuzz` cases. Riot can now discover fuzz cases from test binaries, run campaigns with corpus/crash persistence, replay saved inputs, minimize coverage-redundant corpuses, emit JSON events, and serialize fuzzing through a workspace fuzz lock.
- `riot test` and generated test binaries now understand fuzz cases as first-class test cases. Seed inputs still replay through normal test runs, while `riot fuzz` can drive the same case with generated inputs and mutator/corpus metadata.
- `riot doc` now reuses a generated manifest to skip unchanged documentation builds, shares generated CSS and JavaScript assets across package docs, restores syntax highlighting for fenced code blocks, distinguishes values from functions, renders package overview metadata, and lists variant constructors without extra pipe markers.
- The installer now supports explicit version and install-directory selection. Use `curl -sSL https://get.riot.ml | sh -s -- -v 0.0.32` to install a specific Riot version, or pass `--riot-dir <dir>` to install outside `$HOME/.riot`.
- Release guidance now documents the normal dirty-worktree flow: releases may proceed with unrelated dirty files as long as `./packages`, real `riot.toml` manifests, and release inputs are committed.

## 0.0.31 - 2026-05-03

### riot
- Riot's build cache now keys package actions by the output hashes of dependency artifacts, not only by the inputs used to plan the action. This prevents stale library artifacts from being reused after an upstream dependency rebuilt to a different `.cmi`/`.cmx` shape, fixing inconsistent-assumption failures such as mismatched generated alias modules.
- `riot-store` now fails loudly when an action declares outputs that were not produced. Incomplete or broken action sandboxes are no longer saved as if they were valid cache entries, which makes cache corruption and cross-target output bugs visible at the point they happen.
- `riot-planner` rejects empty library plan bundles and bumps the planner artifact version, forcing old build-cache entries through the corrected dependency-output hashing path.
- Module dependency analysis now treats a reference from `Config.ml` to `Config` as an external or opened dependency when one is available, instead of always interpreting it as a circular self-reference. This lets modules such as application `Config.ml` use `Std.Config` after `open Std`.

## 0.0.30 - 2026-05-02

### riot
- `riot doc` now renders root modules as real detail pages, excludes executable entrypoints from documentation packages, and improves module-page structure so top-level items have working links, signatures, summaries, and detail sections.
- `riot doc` now extracts record field docstrings without duplicating raw comment syntax in rendered signatures, and fenced Markdown code blocks preserve their relative indentation after only the shared doc-comment padding is stripped. This keeps rendered examples readable, including nested `match` branches and indented error handling.
- The installer now tags Riot binary downloads with `X-Riot-Agent: riot-install@1`, so CDN download metrics can distinguish install-script traffic from CLI or other pipeline downloads. The 0.0.30 binary release re-uploads `install.sh` with this behavior.

## 0.0.29 - 2026-05-01

### riot
- `riot build --all` now builds test, bench, and example artifacts only for workspace packages. Downstream dependencies are still built as normal libraries, but their development artifacts are no longer pulled into unrelated workspace builds.

## 0.0.28 - 2026-05-01

### riot
- Fixed `riot-planner` dependency wiring for nested local modules in published packages. Downstream workspaces can now build `kernel` and `std` from the registry again when modules refer to sibling nested modules such as `Regex_stubs` or generated child roots such as `Algo` after an `open`.
- Fixed `riot build --all` in downstream workspaces that depend on packages with published `riot-fix` providers. Riot now builds fix provider runners only for workspace-member packages, so dependency-provided rules no longer create a synthetic `fixme-runner -> riot-fix` edge that the downstream workspace cannot satisfy.
- `riot publish` now accepts `--skip-fmt` for release operators that need to publish a known-good build while intentionally bypassing the `riot fmt --check` preflight. This mirrors `--skip-check` for the fix preflight while keeping the skipped stage explicit in the command line.

## 0.0.27 - 2026-05-01

### riot
- `riot-lsp` now exposes typed hover, inlay hints, semantic completion, and editor-facing diagnostic handling backed by the new Typ inference work. Editors can show richer type information and completion results without depending on the old syntax-view path.
- `riot.nvim` now wires completion and LSP logging through the Riot LSP integration, making editor debugging and completion behavior easier to inspect.
- `riot-planner` now suggests similar available module names when dependency graph verification finds a missing module such as a casing or underscore mismatch. The error path does a little extra work to make module-name mistakes actionable.
- `riot-fix` now reports nested match depth only after the third nested match, so shallow two- and three-level matches no longer trigger the lint.
- `riot-store` preserves cache generation recency more accurately, avoiding cache bookkeeping that can make recently used entries look stale.
- The workspace release set no longer includes the obsolete `parquet` and `pretext` packages. They were removed from the active workspace so release builds and package planning only cover maintained packages.

## 0.0.26 - 2026-04-28

### riot
- `riot build --all` now builds package-provided `riot-fix` rule runners as part of the all-artifacts graph. Packages that ship custom lint rules now fail during the normal workspace build when their generated runner no longer compiles, instead of surprising users later during `riot fix`.
- Generated fix-rule runners now use the same binary entrypoint shape as regular Riot executables: `let main ~args = ...` plus `Runtime.run ~main ~args:Env.args`. This keeps provider binaries compatible with Riot's entrypoint validation.
- `riot fix --check` works with the new `Syn.Ast`-based rule pipeline, including generated package providers. Parse diagnostics and lint diagnostics stay separate, which makes fix output easier to interpret.
- Debug builds now treat OCaml warning 6, omitted labels in function application, as an error. This catches calls that accidentally drop required labels during development instead of letting them pass as warnings.
- `riot test` now honors repeated `-p` filters. Commands such as `riot test -p syn -p krasny` now run both selected packages instead of silently using only the last package flag.
- Human test output now shows per-test timings. Normal timings are subdued, slow small tests are highlighted, and failures render in bold red while JSON output remains machine-readable.
- Snapshot commands start reporting work as pending snapshots are found instead of waiting for a full repository scan. `snapshot accept`, `snapshot reject`, and `snapshot review` only scan supported snapshot locations, so large workspaces get interactive feedback much sooner.
- `snapshot review` now exits cleanly without printing action prompts when there are no pending snapshots.
- `riot publish` now emits an availability check event before querying the registry for an already-published version. Long registry lookups no longer leave human or JSON output completely silent before format/build checks begin.
- Obsolete checked-in Riot binary artifacts were removed from the repository so releases are built from the current release pipeline instead of stale local binaries.

## 0.0.25 - 2026-04-27

### riot
- `riot publish` now supports `--json`, making publish flows easier to script and inspect in automation.
- `riot add` and `riot rm` now accept multiple package names in one command, so dependency edits can be batched without repeated solver runs.
- `riot update` now accepts one or more package names, allowing targeted dependency updates instead of always refreshing the whole dependency set.
- Riot commands run outside a workspace now print guidance that explains the missing workspace context and points users toward initialization, instead of silently doing nothing or failing without direction.
- CLI behavior is covered by additional `riot-e2e` tests for generated workspaces, package commands, publish flags, and command parsing.
- Generated Riot contributor skill files were refreshed with current CLI flags, module-system notes, testing guidance, and benchmark references.

### planner-build
- The planner now rejects direct use of modules from transitive dependencies. Package code may depend on its own modules and direct dependency roots, which keeps package manifests honest and avoids hidden dependency edges.
- Executable, example, bench, and test entry files now need a top-level `let main ~args = ...` entry point, giving binaries one consistent runtime shape before code generation grows macro support.
- Planning and build error rendering was tightened so innermost diagnostics can explain missing entry points and module-graph violations without extra wrapper noise.
- Workspace package labels now show artifact kind and target context for tests, examples, benches, and multi-architecture builds.

## 0.0.24 - 2026-04-24

### riot
- `riot build` now renders structured planning failures as targeted detail lines and keeps package status labels readable for versions, dev artifacts, and multi-target builds.
- Build output now distinguishes workspace dev artifacts such as tests and benches, including labels like `serde-json (test, aarch64-apple-darwin)` when multiple targets are active.
- `riot init` scaffolds workspace defaults for agents, development config, git hooks, Riot GC config, and starter `Std.Log` setup.

### package-management
- Reworked package, workspace, registry, lockfile, and publish/install/run error paths to carry structured typed errors through Riot internals and render strings only at the CLI edge.
- Improved lock refresh and registry-cache failure reporting so package-management flows preserve actionable error context.

## 0.0.23 - 2026-04-23

### riot
- Defaulted Riot to `OCaml 5.5.0-riot.4` across workspace/toolchain defaults, including generated workspaces, toolchain resolution, and bootstrap constants.
- Updated toolchain tests and release scripts to target `5.5.0-riot.4`.

### windows-toolchain
- Updated vendored OCaml to support MinGW cross toolchains reliably.
- Fixed Windows cross-compilation issues by:
  - setting the Windows API floor to `0x0600` for MinGW targets
  - fixing Win32 runtime/header guards
  - adding missing `errno.h` includes in Win32 Unix shims
  - fixing the `yacc/wstr.c` Windows build path
- This enabled shipping the full `5.5.0-riot.4` toolchain matrix, including the Linux-hosted MinGW targets.

## 0.0.22 - 2026-04-23

### riot
- Fixed generated `riot init --bin` workspaces so the starter package builds, runs, and tests correctly when the package has no library archive.
- Dev-scope planning for no-library packages now carries reachable `src/` helper modules into tests/examples/benches instead of linking a missing package `.cmxa`.
- Added Docker smoke fixtures for mounting the current locally built Riot binary into Arch Linux and Ubuntu containers and validating `riot init`, `riot build`, `riot run`, and `riot test --small`.

## 0.0.21 - 2026-04-23

### riot
- `riot init` now lowercases generated starter file stems, so dotted workspace names with normalized package names build and run consistently on case-sensitive systems.
- `Std.Command.output` no longer hangs when the direct child exits while another process inherited the captured stdout/stderr pipe, preserving idle callbacks and streamed stdout line callbacks for long-running commands.
- Release automation now supports force-republishing explicit Riot binary targets and strips release binaries before upload.

### toolchain
- Updated Linux sysroot and OCaml cross-toolchain packaging so Riot-built Linux binaries run on common glibc distributions such as Ubuntu and Arch instead of relying on Ubuntu-specific assumptions.
- Published and validated the `5.5.0-riot.3` toolchains with Riot project smoke tests in Linux containers.

### release
- Published Riot 0.0.20 binaries for `aarch64-apple-darwin`, `aarch64-unknown-linux-gnu`, and `x86_64-unknown-linux-gnu` after validating the generated release artifacts and CDN metadata.

## 0.0.20 - 2026-04-22

### riot
- `riot new --lib` and `riot new --bin` now work both inside a workspace and as standalone package scaffolds.
- `riot new` now keeps `[workspace].members` in sync when adding packages into an existing workspace, and repeated `riot new` flows no longer leave generated packages unbuildable or unrunnable.
- Generated `--bin` scaffolds now use the correct runtime entrypoint shape, so newly created binaries run immediately after scaffolding.
- Test suites now receive structured suite context that includes the binaries built for the owning package, so package tests can execute the just-built artifact instead of relying on a globally installed `riot`.
- Riot now has initial `riot-e2e` generated-workspace coverage for:
  - `riot init`
  - `riot new --lib`
  - `riot new --bin`
  - repeated `riot new` flows inside a workspace

### planner-build
- Hardened package-layout validation so target code is rejected during planning, not later during compilation, when it reaches:
  - library-internal modules directly
  - namespaced internal modules like `Pkg__A`
  - another target's private root module
- Refreshed stale plan handling and planner artifact versioning so old cached plans are rebuilt instead of leaking invalid graph state forward.
- Tightened target ownership between `riot-model` and `riot-planner`: declared binaries in a source bucket now suppress autodiscovery in that same bucket, avoiding fake extra target roots during planning.
- Added real-package kernel planner oracles that pin public-root dependency retention before action planning, including:
  - `Kernel__Net__Addr__Unix` keeping `Result`, `System_error`, and `Socket_addr`
  - `Kernel__Process` depending on `Fs` through the public child root instead of leaking down to `Fs__File`
- Expanded planner regression coverage across:
  - `syn` dependency analysis for alias-open and public-root cases
  - planner package layout validation
  - real-kernel module graph and action graph behavior

## 0.0.19 - 2026-04-22

### riot
- `riot test` now streams human output per test case, passes structured suite context through `--ctx`, exposes built runtime binaries to tests, and treats small tests as a tighter fast path with clearer small/large summaries.
- `riot bench` now records and compares benchmark history, streams benchmark progress/heartbeats, surfaces GC counters and variance, and supports top-level `--warmup`, `--iterations`, and `--compare` controls.
- `riot build` now uses explicit `-p/--package` selection and can build dev artifacts with `--tests`, `--benches`, `--examples`, and `--all`.
- `riot info` now reports workspace/package details more clearly, `riot clean` locks active build lanes before cleaning and reports lock waits, and `riot fmt` stays scoped to workspace sources.
- `riot init` now preserves dotted workspace names while still normalizing starter package names correctly.

### planner-build
- Riot planning now trims unreachable library modules and computes target-private module closures for binaries, tests, examples, and benches, so helper modules can stay target-local instead of being forced into the package library.
- Build caching is more reliable after dependency and manifest changes, including cache invalidation for build dependency path updates and stronger runtime/build dependency hashing.
- Build and test execution gained more stability through lane-lock coordination, generated-root dependency fixes, and tighter small/large test classification.

### docs
- Added an RFD for target-specific module reachability and clarified how that planner model composes with future conditional compilation.

## 0.0.18 - 2026-04-15

### riot
- Expanded Riot build orchestration and toolchain domains (`riot-build`, `riot-planner`, `riot-executor`) with typed command/request surfaces and stronger plan execution contracts.
- Refactored package-orchestration surfaces across `riot-model`, `riot-install`, `riot-run`, and CLI command paths for clearer package and runtime boundaries.
- Fixed planner/build regressions including dependency ordering, explicit-root planning, lazy realization, stale work-plan cache state, and workspace target overrides.
- Hardened artifact and runtime flows in `riot-store` with content-store work, warm-generation dedupe/indexing, and new artifact-store and planner benchmarks.
- Added/updated Riot-side tests and fixtures for planner, build, toolchain, publish, init, and runtime suites.

### tooling
- Added/updated docs and RFD material for scheduler/executor redesign and parallelism behavior.
- Added the new user-facing Riot skill (`riot-ml`) and its associated references/signature guidance.
- Delivered major package/domain updates for supporting ecosystems (`parquet`, `pretext`, `tty`, `contentstore`, `typ`, `suri`, `propane`, `ignore`) with migration, validation, and hardening work.

### performance
- Added and stabilized benchmark coverage across store/build/planner, serde-bin, and compiler/runtime paths.
- Improved internal performance through cache pruning, planning overhead reduction, module-typing caching, and targeted serialization micro-optimizations.

## 0.0.17 - 2026-04-10

### Fixed

- `riot init` now defaults to the current directory when no target path is provided.
- `riot` now gives clearer guidance when commands are run outside a workspace.
- `riot` surfaces a better hint when a package does not define a runnable binary.
- Repaired the `miniriot` bootstrap dependency graph.

## 0.0.16 - 2026-04-10

### Added

- Added hover request support across the LSP stack and editor integrations.
- Added more detailed `riot check` progress events in the CLI.
- Added a large new slice of experimental multicore runtime and `kernel-new` groundwork.

### Changed

- Workspace package-root handling is more consistent across `riot check`, docs generation, and editor flows.
- A broad round of interface and formatter refreshes landed across core packages.

### Fixed

- Fixed package-root scanning in `riot-check` so per-package runs stay scoped correctly.
- Fixed docs planning so package documentation resolves source roots correctly.
- Fixed several parser and formatter edge cases, including keyword-operator handling in `syn`.

## 0.0.15 - 2026-04-06

### Added

- Added cache-first external source workflows for `riot run` and `riot install`, with explicit `--update` refreshes and repo-name default binaries for remote sources.
- Added detached single-package workspace synthesis so Riot commands can build, run, and install from a package root without an enclosing workspace manifest.
- Added `riot yank` plus exact-version yank support in the `pkgs-ml` registry client.
- Added structured test and benchmark suite timing output in microseconds, including per-test durations, suite lifecycle timing, and escaped JSON payloads that pipe cleanly into tools like `jq`.
- Added aggregated case-level summaries in `riot test` and `riot bench`, including measured test time, slowest tests, and aggregated failed test lists in both human and JSON output.
- Added package admin views in the `pkgs.ml` services and migrated the Riot docs site to the new Astro/Starlight content layout.

### Changed

- `riot test` now rebuilds the familiar human suite output from structured runner events, suppresses zero-match suites for filtered runs, and emits richer machine-readable summary events.
- `riot bench --json` and `riot test --json` now keep build events in the JSON stream instead of dropping compile progress when suites run in structured mode.
- Workspace and package resolution for `riot build`, `riot run`, and `riot install` now prefer the nearest enclosing workspace but can fall back to a single package manifest discovered during the same upward scan.
- RFD metadata and the docs content tree were cleaned up so implemented proposals and user-facing docs stay in sync with the current repo state.

### Fixed

- Fixed stale cached package archives and source materializations so corrupted archives are retried and warm cache flows stay usable.
- Fixed bootstrap/miniriot wrapper drift that was breaking `./bootstrap.py && ./miniriot`.
- Fixed invalid JSON serialization for control characters in suite stdout/stderr.
- Fixed multiple docs and web regressions, including sandbox reruns, mobile navigation, accessibility, and builtin release docs rendering.

## 0.0.12 - 2026-04-06

### Added

- Added `Actors.spawn_pinned`, `Actors.spawn_blocked`, and scheduler support for pinned and blocking actor placement.
- Added detached single-package build support so `riot build` now works from a standalone package root with only a package-level `riot.toml`.
- Added `./scripts/release.sh` to automate version bumps, changelog updates, tagging, and Riot release orchestration.

### Changed

- `riot` CLI workspace resolution now scans upward once, preferring an enclosing workspace manifest and otherwise synthesizing a one-package workspace from the nearest package manifest.
- Riot release automation now supports manifest-aware all-target releases from `./scripts/release/riot.sh all`.

### Docs

- Marked implemented RFDs as `implemented`, including the pinned/blocking actor runtime work.

## 0.0.10 - 2026-04-06

### Added

- Added the new `riot-doc` package for documentation generation, HTML rendering, doctree/source transforms, and workspace interface docs generation.
- Added manual cache GC and generation receipts in `riot-store`, plus workspace operational cache config via `.riot/config.toml`.
- Added published toolchain manifests and `riot toolchain list-available` / `riot toolchains list-available`.
- Added JSON event output for `riot doc`.
- Added rooted snapshot preparation and broader session-driven improvements in `typ`.
- Added richer analytics and registry dashboards across the `pkgs.ml` services.

### Changed

- `riot clean` now performs policy-aware cache cleanup, while successful builds record generation metadata without automatically reclaiming cache entries.
- `riot install` now fails when promotion fails instead of reporting a warning and continuing.
- `riot-doc`, `riot-cli`, and related command surfaces gained structured JSON output improvements.
- Package detail, activity, stats, and mobile layouts on `pkgs.ml` were substantially refined.
- The repository now carries a default `.riot/config.toml`.

### Fixed

- Fixed stale exported build artifacts after cache cleanup so warm rebuilds stay warm after `riot clean`.
- Fixed `pkgs.ml` desktop readme/package section behavior and removed the mobile theme toggle.
- Fixed `riot` agent propagation for access analytics and process stamping in the package services.
- Fixed a dead tail in Suri liveview process handling.
- Improved Neovim Riot diagnostic float rendering.

### Docs

- Reworked the `typ` RFD/spec stack with expanded algorithm notes, examples, diagnostics, and semantic slices.
- Split the cache and operational config RFDs into:
  - `RFD0032 - Riot Cache and GC`
  - `RFD0038 - Riot Workspace Operational Config`
- Refreshed workspace documentation around toolchains and typing internals.
