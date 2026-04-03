# riot-lsp AGENTS

`riot-lsp` owns Riot's actual Language Server Protocol server.

## Rules

1. Keep `riot-lsp` built on `jsonrpc` and `lsp`; do not duplicate protocol types or JSON codecs here.
2. Keep stdout protocol-only. Never mix logs or human status output into the LSP stream.
3. Keep the first slice syntax-first: lifecycle, document sync, syntax diagnostics, then formatting/code actions later.
4. Keep session state explicit and testable. Prefer pure state transitions over hiding behavior in ad hoc I/O loops.
5. Treat request parsing failures as request-scoped failures. One bad request must not poison the rest of the session.
6. Keep request/response behavior covered by snapshot fixtures.

## Validate

`timeout 30 riot build riot-lsp`
`timeout 180 riot test riot-lsp:framing_tests`
`timeout 180 riot test riot-lsp:session_fixture_tests`
