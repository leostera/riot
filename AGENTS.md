# AGENTS Router

This file is the entrypoint for project-specific agent guidance. If you're looking for your scratch pad and todo list, look at ./TODO.md

The `AGENTS.md` files in this repo are maintained alongside the code and should be updated when behavior or contracts change.

Use it as a router: pick the most relevant existing AGENTS file before making changes.

## Routing Table

- `packages/kernel/AGENTS.md`: C FFI, platform shims, file descriptors, event loop primitives
- `packages/actors/AGENTS.md`: actor runtime, scheduler, mailbox, timers, process lifecycle
- `packages/std/AGENTS.md`: shared standard library surface used by the rest of the repo
- `packages/ignore/AGENTS.md`: ignore-aware recursive walking, gitignore-style precedence, and subtree pruning
- `packages/http/AGENTS.md`: HTTP protocol implementation and wire-level behavior
- `packages/blink/AGENTS.md`: streaming HTTP client built on actors
- `packages/suri/AGENTS.md`: web framework, middleware, routing, liveview, server integration
- `packages/jsonrpc/AGENTS.md`: JSON-RPC framing and codec behavior
- `packages/lsp/AGENTS.md`: Language Server Protocol types, codecs, and typed method descriptors
- `packages/mcp/AGENTS.md`: MCP transport and protocol types
- `packages/syn/AGENTS.md`: parser, lexer, CST, diagnostics
- `packages/krasny/AGENTS.md`: OCaml formatter, document layout, syntax-to-text rendering
- `editors/riot.nvim/AGENTS.md`: Neovim plugin, editor-facing Riot command integration
- `editors/vscode-riot-ml/AGENTS.md`: VS Code extension, editor-facing Riot command integration
- `packages/riot-model/AGENTS.md`: shared build-system types and workspace/package model
- `packages/riot-deps/AGENTS.md`: package management, dependency solving, lock refresh, registry cache layout
- `packages/riot-publish/AGENTS.md`: publish command orchestration across fmt, fix, build, and registry upload
- `packages/riot-planner/AGENTS.md`: build planning and dependency graph construction
- `packages/riot-executor/AGENTS.md`: build execution and result aggregation
- `packages/riot-store/AGENTS.md`: artifact store and cache layout
- `packages/riot-toolchain/AGENTS.md`: compiler/toolchain invocation wrappers
- `packages/riot-build/AGENTS.md`: in-process build session/runtime entrypoints
- `packages/riot-cli/AGENTS.md`: CLI commands and user-facing flows
- `packages/riot-lsp/AGENTS.md`: Riot's Language Server Protocol server, session loop, and editor-facing behavior
- `packages/riot-fmt/AGENTS.md`: `riot fmt` wrapper around krasny-based formatting checks
- `packages/riot-init/AGENTS.md`: workspace/package scaffolding
- `packages/riot-eval/AGENTS.md`: OCaml evaluation tooling
- `packages/riot-fix/AGENTS.md`: linting and auto-fix pipeline
- `packages/fixme/AGENTS.md`: shared rule-authoring types used by riot-fix and generated `fixme-runner` providers
- `packages/tty/AGENTS.md`: terminal control and rendering helpers
- `packages/typ/AGENTS.md`: experimental lowered IR, prototype typing, and snapshot-driven type-analysis exploration
- `packages/gooey/AGENTS.md`: TUI primitives
- `packages/minttea/AGENTS.md`: Elm-style TUI framework
- `packages/sqlx/AGENTS.md`: high-level SQL API
- `packages/sqlx-driver/AGENTS.md`: database driver interface
- `packages/sqlite/AGENTS.md`: SQLite adapter
- `packages/postgres/AGENTS.md`: PostgreSQL adapter
- `packages/pkgs-ml/AGENTS.md`: reusable pkgs.ml registry client and cache layout helpers
- `packages/pubgrub/AGENTS.md`: version solver
- `packages/mime/AGENTS.md`: MIME parsing and rendering helpers
- `packages/propane/AGENTS.md`: property-based testing support
- `packages/hello-foreign/AGENTS.md`: OCaml to Rust FFI smoke test
- `native/AGENTS.md`: Rust binding layer overview and crate routing
- `native/riot-core/AGENTS.md`: shared value model and ABI-safe types
- `native/riot-derive/AGENTS.md`: derive macros for the native binding layer
- `native/riot-ffi/AGENTS.md`: Rust-facing FFI facade and prelude
- `native/riot-bindgen/AGENTS.md`: binding code generation tooling
- `native/hello-rust/AGENTS.md`: example native library used by `hello-foreign`

## Fast Start Checklist

1. Identify the domain area.
2. Read the matching AGENTS file if one exists.
3. Implement changes.
4. Run required builds.
5. Update affected AGENTS files if behavior or contracts changed.

When comitting, always use conventional commits.
