# macro AGENTS

`macro` is the procedural-macro authoring and expansion layer built on top of `syn`.

## Rules

1. Keep the public expansion contract deterministic and parser-backed.
2. Keep macro discovery and source rewriting explicit; do not hide build policy here.
3. Prefer structured spans and syntax-node traversal over token-text guessing when locating invocations.
4. If the current prototype cannot lower a macro faithfully, fail explicitly instead of emitting lossy fallback code.
5. Keep runtime dependencies out of expanded output when a pure source rewrite is enough.
6. Document prototype limitations in tests and API names rather than pretending they are solved.
7. Keep function-like macro bodies parser-backed in expression position as `name! expr`; only fall back to delimiter-specific handling when the parsed body itself carries those delimiters.
8. Reparse expanded source before returning it; invalid rewritten OCaml is a macro error, not a later planner/compiler surprise.
9. Keep the public provider contract token-stream based. Macros may parse OCaml via `syn`, but the core ABI is provider-driven expansion plus diagnostics, not hardcoded AST-only rewrites.
10. Keep built-in macro providers as bootstrap compatibility only. Package-scoped provider discovery and resolution belong outside ad hoc name matching.
11. Generated macro runners must build from a self-contained workspace closure. Do not assume external path dependencies or ambient `cwd` are enough for nested `riot build` invocations.
12. Macro packages must expose an explicit top-level `let provider () = ...` entrypoint. Validate that contract before runner materialization so invalid providers fail in macro planning, not as nested-build surprises.
13. Macro-runner cache keys must include the effective toolchain input and copied dependency closure, not just provider source files, so reused runners stay valid when helper packages change.

## Validate

`timeout 30 riot build macro`
`timeout 180 riot run -p macro expansion_tests -- run-tests`
