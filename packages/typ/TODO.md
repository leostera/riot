# Typ TODO

This is the working task list for `packages/typ`.

Keep this file blunt and operational.

## Work Loop

Every slice should follow this loop:

1. Pick one task.
2. Read the owning docs in `docs/checker/`.
3. Add or update fixtures / diagnostics / session tests first.
4. Implement the smallest semantic slice that satisfies the docs.
5. Verify it in `typ`, `riot check`, and `riot-lsp`.
6. Commit the slice with a conventional commit.

Do not batch unrelated work into one big change.

## Read First

Always start here:

- [docs/index.md](./docs/index.md)
- [docs/checker/index.md](./docs/checker/index.md)
- [docs/checker/checker.md](./docs/checker/checker.md)
- [docs/checker/solver.md](./docs/checker/solver.md)
- [docs/checker/lowering.md](./docs/checker/lowering.md)
- [docs/checker/engine.md](./docs/checker/engine.md)

Then read the owning feature doc for the task.

## Reading Map

Use this map before touching code.

### Core typing

- [docs/checker/checker.md](./docs/checker/checker.md)
- [docs/checker/solver.md](./docs/checker/solver.md)
- [docs/checker/generalization.md](./docs/checker/generalization.md)

Use for:

- literals
- variables
- functions
- apply
- let / let rec
- value restriction

### Lowering, origins, diagnostics

- [docs/checker/lowering.md](./docs/checker/lowering.md)
- [docs/checker/diagnostics.md](./docs/checker/diagnostics.md)

Use for:

- semantic normalization
- source spans / origins
- recovery nodes
- structured diagnostics

### Ordinary data and patterns

- [docs/checker/nominal_data.md](./docs/checker/nominal_data.md)
- [docs/checker/extensible_variants.md](./docs/checker/extensible_variants.md)
- [docs/checker/pattern_analysis.md](./docs/checker/pattern_analysis.md)

Use for:

- type declarations
- records
- ordinary variants
- extension constructors / exceptions
- exhaustiveness / redundancy

### Advanced core features

- [docs/checker/labeled_args.md](./docs/checker/labeled_args.md)
- [docs/checker/gadts.md](./docs/checker/gadts.md)
- [docs/checker/polyvariants.md](./docs/checker/polyvariants.md)
- [docs/checker/effects.md](./docs/checker/effects.md)

Use for:

- labeled / optional args
- GADTs
- polymorphic variants
- effects

### Modules

- [docs/checker/modules.md](./docs/checker/modules.md)
- [docs/checker/signatures.md](./docs/checker/signatures.md)
- [docs/checker/first_class_modules.md](./docs/checker/first_class_modules.md)
- [docs/checker/recursive_modules.md](./docs/checker/recursive_modules.md)

Use for:

- structures
- signatures
- functors
- first-class modules
- recursive modules

### Engine and host integration

- [docs/checker/engine.md](./docs/checker/engine.md)
- [docs/checker/diagnostics.md](./docs/checker/diagnostics.md)

Use for:

- sessions
- rooted snapshots
- dependency discovery
- `MissingRequirements`
- `ModuleTypings`
- compiler/LSP query behavior

## Current Tasks

### Engine

- [x] Collapse summary state around one canonical `ModuleTypings`.
  The host-facing reusable artifact now lives in `ModuleTypings`; `FileSummary`
  stays as the per-source query result.
- [ ] Make rooted snapshot preparation real.
  `Session.prepare_snapshot` should do dependency discovery, hydrate required
  module typings, and return `MissingRequirements` early when needed.
- [ ] Stop relying on ambient fake knowledge where real loaded module typings
  should be used instead.

### Interfaces and exports

- [ ] Implement real `.mli` / signature checking.
  Exported module views should come from the interface when present.
- [ ] Make persisted module typings carry the real export facts needed by
  downstream checking and editor queries.
- [ ] Tighten export trust / errored export / no export behavior and cover it
  with tests.

### Queries and LSP support

- [ ] Keep `diagnostics` stable over rooted snapshots.
- [ ] Make `type_at` and related index-backed queries line up with the manual.
- [ ] Add `definition_at` support built on origins and exported symbol data.
- [ ] Keep `riot-lsp` aligned with rooted snapshots instead of relying forever
  on the compatibility `Session.snapshot` path.

### Language support

- [ ] Keep pushing real package baselines upward:
  `colors` -> `tty` -> `mime` -> `kernel` -> `std`.
- [ ] Implement one missing feature cluster at a time, based on real package
  diagnostics.
- [ ] Prefer small slices like:
  - one type-declaration rule
  - one pattern rule
  - one labeled-argument behavior
  - one module/signature behavior

### Tests and diagnostics

- [ ] Keep adding fixture snapshots under `tests/fixtures/`.
- [ ] Keep adding structured diagnostic snapshots under `tests/diagnostics/`.
- [ ] Add session / summary / snapshot tests whenever engine behavior changes.
- [ ] Keep diagnostics structured first, human rendering second.

## Code Map

Use this as the rough implementation map.

### Lowering and semantic tree

- `src/lower.ml`
- `src/SemanticTree*.ml`
- `src/Origin*.ml`
- `src/Diagnostic*.ml`

### Solver and inference

- `src/infer.ml`
- `src/TypeRepr*.ml`
- `src/TypeScheme*.ml`
- `src/TypePrinter*.ml`

### Analysis, summaries, queries

- `src/SourceAnalysis.ml`
- `src/ModuleTypings*.ml`
- `src/Query*.ml`
- `src/TypeIndex*.ml`

### Engine

- `src/Session*.ml`
- `src/Snapshot*.ml`
- `src/MissingRequirements*.ml`

If the code boundaries drift too far from the manual, stop and refactor before
adding more feature work.

## Verification Loop

Run these in order for each real slice.

### 1. Local `typ`

Minimum checks:

```ocaml
timeout 180 riot build typ
timeout 180 riot test -p typ
```

If the change is docs-only:

```ocaml
git diff --check -- packages/typ
```

### 2. Compiler-style verification

`typ` must work through `riot check`, not just through package-local tests.

Run:

```ocaml
timeout 30 riot build riot-cli
timeout 30 riot test riot-cli:check_tests
riot run riot -- check -p colors
riot run riot -- check -p tty
riot run riot -- check -p colors --json
```

Check:

- streaming diagnostics still work
- human diagnostics still look right
- JSON still emits per-file events, including clean-file events
- sibling modules still resolve correctly at package scope

Main integration point:

- [check_cmd.ml](../riot-cli/src/check_cmd.ml)

### 3. LSP-style verification

`typ` must also still help `riot-lsp`.

Run:

```ocaml
timeout 30 riot build riot-lsp
timeout 180 riot test riot-lsp:framing_tests
timeout 180 riot test riot-lsp:session_fixture_tests
```

Check:

- type diagnostics still publish cleanly
- spans still map to correct UTF-16 ranges
- package-scoped typing still matches `riot check`
- no JSON-RPC protocol noise leaks into stdout

Main integration point:

- [session.ml](../riot-lsp/src/session.ml)

### 4. Real package loop

Use real packages to decide the next feature slice.

Start with:

```ocaml
riot run riot -- check -p colors
riot run riot -- check -p tty
```

Then move upward:

```ocaml
riot run riot -- check -p mime
riot run riot -- check -p kernel
riot run riot -- check -p std
```

Treat the remaining diagnostics as the feature inventory for the next task.

## Acceptance Checklist

Do not call a feature done until all of these are true:

- [ ] It is covered by fixtures or diagnostics snapshots.
- [ ] Its diagnostics are structured and span-backed.
- [ ] Its origins point back to the right source region.
- [ ] It behaves correctly through `Typ.Query`, not only one-shot checking.
- [ ] It works through `riot check`.
- [ ] It still helps `riot-lsp`.
- [ ] If it affects exports, it is covered by summary / snapshot tests.

## Commit Rule

Commit often.

Good commit shape:

1. one semantic slice
2. matching tests
3. matching docs update if the contract changed

Use conventional commits.
