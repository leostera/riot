# std AGENTS

`std` is the mandatory standard library surface for the rest of the repo.

## Rules

1. Favor small, composable APIs over one-off helpers.
2. Do not leak `Stdlib`, `Unix`, `Sys`, or `Obj` back through this surface unless the boundary is already intentional.
3. Changes here have wide blast radius. Prefer additive evolution and stable signatures.
4. If a utility is only useful for one package, keep it out of `std`.
5. `std` owns its package-provided `riot-fix` rules under `fix/`; keep those diagnostics aligned with the scheduler and `std` ownership rationale.
6. `Std.Test.Cli` owns the portable test-binary contract (`list-tests`, `run-tests`, and query filtering). Keep individual test binaries on that runner instead of inventing local CLIs.
7. Archive and compression APIs should compose with `IO.Reader` and `IO.Writer`. Keep path-based helpers as thin wrappers around the streaming APIs rather than making them the only surface.
8. Binary/text codecs belong under `Std.Encoding`. Keep `Std.Data` focused on structured data formats like JSON, TOML, CSV, XML, and S-expressions.
9. `Std.Test` owns the shared test-binary contract. Per-test callbacks now receive a `Std.Test.ctx`; future snapshot and fixture helpers should extend that context instead of inventing parallel identity plumbing.
10. Keep `Std.Test.FixtureRunner` path-typed. Fixture roots and discovery filters should use `Std.Path.t`, and mixed fixture directories should narrow discovery through the shared `~filter` hook instead of package-local file scanning.
11. Keep snapshot approval routing explicit in shared test helpers. Use fixture-provided `snapshot_path` when a suite already has a package-specific approved filename convention, render JSON snapshots through `Std.Data.Json.to_string_pretty` so approved files stay reviewable, and treat `.expected.new` files as visible review artifacts rather than ignored scratch output.
12. Keep `Std.Crypto.Hasher.Intf` string-first. Callers that need to mix existing digests into a hash state should use `write_hash` rather than passing mutable raw bytes through the public API.

## Validate

`timeout 30 riot build std`
