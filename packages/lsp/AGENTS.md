# lsp AGENTS

`lsp` owns Language Server Protocol types, codecs, and typed method
descriptors.

## Rules

1. Keep `lsp` protocol-only. Do not couple it to `syn`, `riot-fix`, editor plugins, or Riot server state.
2. Build on `jsonrpc`; do not duplicate generic JSON-RPC envelope behavior here.
3. Prefer typed request and notification descriptors over stringly JSON plumbing.
4. Start with the subset Riot actually needs. Do not try to model the whole LSP spec up front.
5. Keep UTF-16 and URI helpers explicit in this package so downstream servers do not reimplement them ad hoc.
6. Keep machine-readable request/response contract tests on `Std.Test.Snapshot`.

## Validate

`timeout 30 riot build lsp`
`timeout 180 riot test lsp:protocol_fixture_tests`
`timeout 180 riot test lsp:utf16_tests`
