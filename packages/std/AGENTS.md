# std AGENTS

`std` is the mandatory standard library surface for the rest of the repo.

## Rules

1. Favor small, composable APIs over one-off helpers.
2. Do not leak `Stdlib`, `Unix`, `Sys`, or `Obj` back through this surface unless the boundary is already intentional.
3. Changes here have wide blast radius. Prefer additive evolution and stable signatures.
4. If a utility is only useful for one package, keep it out of `std`.
5. `std` owns its package-provided `riot-fix` rules under `fix/`; keep those diagnostics aligned with the scheduler and `std` ownership rationale.
6. `Std.Test.Cli` owns the portable test-binary contract (`list-tests`, `run-tests`, `--json`, query filtering, and shared selectors such as `--small`, `--large`, and `--flaky`). Keep individual test binaries on that runner instead of inventing local CLIs.
7. Archive and compression APIs should compose with `IO.Reader` and `IO.Writer`. Keep path-based helpers as thin wrappers around the streaming APIs rather than making them the only surface.
8. Binary/text codecs belong under `Std.Encoding`. Keep `Std.Data` focused on structured data formats like JSON, TOML, CSV, XML, and S-expressions.
9. `Std.Test` owns the shared test-binary contract. Per-test callbacks now receive a `Std.Test.ctx`; future snapshot and fixture helpers should extend that context instead of inventing parallel identity plumbing.
10. `Std.Test` case metadata lives on `size` and `reliability`. Keep timeout, retry, and per-case actor isolation behavior inside the shared runner so packages do not grow their own flaky-test or slow-test harnesses.
11. Keep `Std.Test.FixtureRunner` path-typed. Fixture roots and discovery filters should use `Std.Path.t`, and mixed fixture directories should narrow discovery through the shared `~filter` hook instead of package-local file scanning.
12. Keep snapshot approval routing explicit in shared test helpers. Use fixture-provided `snapshot_path` when a suite already has a package-specific approved filename convention, render JSON snapshots through `Std.Data.Json.to_string_pretty` so approved files stay reviewable, and treat `.expected.new` files as visible review artifacts rather than ignored scratch output.
13. Keep `Std.Crypto.Hasher.Intf` string-first. Callers that need to mix existing digests into a hash state should use `write_hash` rather than passing mutable raw bytes through the public API.
14. Keep recursive filesystem walking in `Std.Fs.Walker`, with an iterator-first surface built around `Iterator.t`. Package-level ignore policy belongs in higher layers, not in `std`.
15. Keep `Std.Fs.Walker` on the cheap `ReadDir` path on the common case. Use directory-entry kind hints first and fall back to metadata only for unknown kinds or symlink-following semantics.
16. Keep `Std.Fs.Walker.FileItem` opaque. The walker hot path should stay string-first internally and only pay `Path.t` construction when a caller explicitly asks for it through accessors such as `FileItem.path`.
17. Keep `Std.Fs.ReadDir` split between raw and validated entry APIs. Hot recursive walkers should prefer raw names plus cheap kind hints; convenience path-typed helpers can stay layered above that.
18. Keep `Std.Regex` tree-shaped and pure. Rendering and compilation should flow through `Kernel.Regex`; higher-level pattern syntaxes should target the regex AST instead of emitting ad hoc kernel pattern strings directly.
19. Keep `Std.Glob` as a syntax-and-translation layer. Parse user glob strings into a glob AST first, then lower that AST into `Std.Regex` before compiling.
20. Keep `Std.Bench.Cli` aligned with `Std.Test.Cli` for machine output. `run-benchmarks --json` should emit one final JSON object that captures per-benchmark results plus the suite summary so higher layers can aggregate without scraping pretty text.
21. `Std.Test` JSON output should include per-test `duration_us`, `size`, `reliability`, and retry/timeout status, plus suite-level `started_at_us`, `completed_at_us`, and `duration_us`. Use `Std.Time.Instant` for these monotonic offsets and durations rather than wall-clock timestamps.
22. `Std.Bench` JSON output should include suite-level `started_at_us`, `completed_at_us`, and `duration_us` alongside the existing per-benchmark statistics, measured from `Std.Time.Instant`.
23. `Std.Test.Cli list-tests --json` and `Std.Bench.Cli list-benchmarks --json` are editor-facing discovery contracts. Keep them machine-readable, selector-aware, and rich enough to drive external UIs without scraping pretty output.
24. Prefer `format Format.[ ... ]` for small diagnostic strings that stitch together primitives in `std`. Keep larger renderers and structured text generators on their own domain-specific builders instead of forcing everything through `Kernel.Format`.
25. Keep `Std.Date` as the civil-date surface and `Std.DateTime` as the calendar datetime surface. Leave `Std.Calendar` as the lower-level Gregorian math helper rather than turning it into the primary application-facing API.
26. Keep `Std.Range` order-based and comparator-carrying. Interval operations should respect stored `Included` / `Excluded` / `Unbounded` bounds without growing a step or enumeration model into the core range type.
27. Keep UDP support datagram-first. `Std.Net.UdpSocket` is the core surface; any `UdpServer` convenience wrapper should preserve packet boundaries and avoid pretending UDP has accept/listener semantics.
28. `Std.Runtime` owns the actor runtime implementation. Keep scheduler, mailbox, timer, and process internals under `std/src/runtime`, and treat the `actors` package as a compatibility facade during the migration.

## Validate

`timeout 30 riot build std`
