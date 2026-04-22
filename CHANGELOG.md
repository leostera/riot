# Changelog

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

### std-kernel
- Added `Std.Collections.Proplist` for duplicate-friendly property-list workflows.
- Continued IO/runtime hardening and performance work across `std`, `kernel`, `http`, and `serde-json`, including vectored TLS fallback support and lower-overhead buffer/reader paths.

### docs
- Added an RFD for target-specific module reachability and clarified how that planner model composes with future conditional compilation.

## 0.0.18 - 2026-04-15

### riot
- Expanded Riot build orchestration and toolchain domains (`riot-build`, `riot-planner`, `riot-executor`) with typed command/request surfaces and stronger plan execution contracts.
- Refactored package-orchestration surfaces across `riot-model`, `riot-install`, `riot-run`, and CLI command paths for clearer package and runtime boundaries.
- Fixed planner/build regressions including dependency ordering, explicit-root planning, lazy realization, stale work-plan cache state, and workspace target overrides.
- Hardened artifact and runtime flows in `riot-store` with content-store work, warm-generation dedupe/indexing, and new artifact-store and planner benchmarks.
- Added/updated Riot-side tests and fixtures for planner, build, toolchain, publish, init, and runtime suites.

### raml
- Added and split the compiler stack into dedicated packages (`asm`, `raml-core`, `raml-native`, `raml-js`, `raml-wasm`, `raml-cli`).
- Added new MIR/LIR/WIR/native/JS back-end features: scheduling, legalization, cse/liveness/passes, stack/home allocation, entity/purity/jir simplifications, and runtime import/module support.
- Expanded JS backend lowering and passes (JIR/JST tooling, property/intrinsic/object/records/modules flows) and improved backend-specific test coverage and docs.
- Added artifact-store and codegen improvements for Wasm/native toolchains, including layout and decoding/encoding-path hardening.
- Added many compiler fixtures, snapshots, and regression cases across native, Wasm, and JS tool paths.

### serde
- Added schema-driven codec surface and format packages (`serde-cbor`, `serde-bson`, `serde-yaml`, `serde-urlencoded`) and evolved compact binary codec support.
- Expanded `serde-bin` with new codec variants, native fast paths, and additional benchmarked encode/decode scenarios.
- Reworked serde property/test suites and formatter updates for ongoing API and package migrations.

### kernel-std
- Expanded and standardized the new `std` API surface for common project-wide primitives (`path`, `read_dir`, IO, and core runtime entrypoints), with many call sites moved from legacy kernel usage.
- Refactored runtime ownership: moved tar and gzip engines out of kernel into std, and ported FS facade + core runtime/IO onto the smaller kernel model.
- Added a dedicated `std` events seam (`feat(kernel): add fs events seam for std`) and absorbed actors/runtime pieces into kernel/std ownership.
- Removed legacy runtime debt by dropping `kernel-old` and completing staged migration checkpoints (`kernel,std`, `workspace`, `typ`) across validation and bootstrap.
- Standardized behavior around bootstrap and runtime correctness (`self-host bootstrap`, sandbox path normalization, valid float-literal restoration, and warning/validation cleanups).

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
- Added package admin views in the `pkgs.ml` services and migrated `docs.riot.ml` to the new Astro/Starlight content layout.

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
