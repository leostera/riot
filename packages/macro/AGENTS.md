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

## Validate

`timeout 30 riot build macro`
`timeout 180 riot run -p macro expansion_tests -- run-tests`
