# Typ Engine

This document specifies the engine-level contract for `typ`.

The point here is simple: the checker docs describe the type system, but Riot
still needs a library engine that can actually use that type system in three
different worlds:

- compiler and build
- LSP and editor tooling
- later macro and lint passes

That engine is where `typ` decides:

- what a session is
- what a snapshot is
- when dependencies must be loaded
- what gets stored and reused between runs
- what queries are allowed to ask

If this layer is vague, then even a perfect type-system spec still leaves us
with a bad library.

## 1. Scope

This document covers:

- sessions
- sources and stable source identities
- rooted snapshots
- missing requirements
- module-summary hydration
- the boundary between `typ` and the host store
- snapshot queries
- one-shot and incremental usage models

This document does not cover:

- the core typing rules
- lowering rules
- the internal representation of the solver

Those are all specified elsewhere.

## 2. What This Layer Is For

The engine layer exists to let one checker serve multiple hosts without
re-implementing the world three times.

That means:

- build wants reusable exported facts
- LSP wants lenient incremental querying over changing text
- later macro or lint passes want typed semantic values in-process

The engine contract must support all three without centering the design on only
one of them.

## 3. Sessions

A session is the long-lived mutable container owned by the host.

It contains at least:

- stable logical sources
- one host-supplied base configuration
- a monotonically increasing revision

The session is where hosts:

- create a source
- update source text
- remove a source
- swap host-provided ambient inputs

The important rule is:

the session is mutable, host-owned state.

It is not itself an analysis result.

## 4. Source Identity

Every source in a session must have a stable `SourceId`.

That `SourceId` must survive text updates to the same logical source.

This matters for:

- incremental editor updates
- origin stability
- diagnostic replacement
- query caching
- summary association

Paths are not enough here.

The host may map paths, buffers, generated fragments, or temporary overlays to
`SourceId`s, but the semantic core should prefer stable source identities over
filesystem names.

## 5. Revisions

Each session update produces a new logical revision.

That means:

- updating source text does not mutate old snapshots
- removing a source does not invalidate the meaning of old snapshots
- new queries should target a newly prepared snapshot for the new revision

The important rule is:

snapshots are revision-bound views, not live windows into mutable session
state.

## 6. Snapshots

A snapshot is the immutable typed world the query layer operates on.

The important rule is:

a snapshot is not just “freeze whatever the session currently has”.

A snapshot is only valid once the required dependency summaries for its roots
have been discovered and hydrated.

That means a snapshot is:

- immutable
- revision-bound
- root-bound
- hydration-complete for those roots

This is the semantic force of the snapshot model, even if one concrete API
still spells it as `Session.snapshot`.

## 7. Rooted Preparation

Snapshots should be rooted.

That means the host prepares a snapshot for:

- one source
- or a small set of sources

not necessarily for the entire session every time.

This matters because:

- LSP usually wants one edited file
- build may want one package or one planned module set
- very large workspaces should not force one giant hydrated snapshot for every
  small edit

So the target behavior is:

```text
prepare_snapshot(session, roots) -> Snapshot or MissingRequirements
```

The exact public API name can change. The behavior should not.

## 8. Dependency Discovery

Preparing a snapshot has a front half and a back half.

The front half is dependency discovery.

That phase must:

1. read the current source text for the roots
2. parse to source syntax
3. discover referenced modules cheaply, before full inference
4. compute the transitive module-summary requirements for the roots

The important point is:

dependency discovery should happen early enough that the engine can bail out
before expensive typing if required summaries are missing.

This is also where the existing `Syn.Deps`-style module-graph intuition fits.

## 9. Missing Requirements

If preparing a snapshot discovers required module summaries that are not
available, the engine should bail out early with a structured
`MissingRequirements` result.

That result should identify, at minimum:

- the missing module names
- enough provenance to know why they were needed
- the requesting roots or source ids

The important rule is:

queries over a valid snapshot should not themselves discover missing
dependencies.

Missing requirements are a preparation-time failure, not a query-time surprise.

## 10. ModuleTypings

`ModuleTypings` is the canonical reusable artifact for one module.

That is the core persistence seam.

A `ModuleTypings` must carry enough information for later sessions to reuse the
module without reopening source or compiler artifacts.

At a minimum that means:

- module identity
- source hash or equivalent input fingerprint
- dependency provenance and fingerprints
- exported values and their schemes
- exported type declarations
- constructors and labels
- exported modules and module types
- exact definition origins and spans for exported symbols
- export trust state

This is the `.cmi`-like boundary for `typ`.

Later richer typed-analysis artifacts may play the role of `.cmti`, but the
core reusable module-typing boundary is `ModuleTypings`.

## 11. Store Boundary

The store is a host concern, not a `typ` concern.

That means:

- `typ` should consume `ModuleTypings` values
- the host should decide how summaries are serialized, cached, keyed, and
  fetched
- a content-addressable store such as `riot-store` is a great host mechanism,
  but it should stay outside the semantic core

The engine contract with the host is:

```text
host loads or computes ModuleTypings values
host prepares a session/config with those summaries
typ consumes them during snapshot preparation and queries
```

That keeps `typ` library-first and testable.

## 12. Fetch-Or-Compute Loop

The host-side algorithm should look like this:

1. build or inspect the package/module graph
2. choose the root source or root set to check
3. parse and discover required modules for those roots
4. for each required module:
   - try to load its `ModuleTypings` from the store by hash or equivalent
   - if it is missing, recursively compute it from that module's package/module
     graph
5. once all required summaries are available, prepare the snapshot
6. run snapshot queries
7. persist newly computed `ModuleTypings` values back to the store

This is intentionally close to the planner/executor loop Riot already uses for
builds.

That is not an accident. It is the right shape.

## 13. Query Semantics

Queries operate over an immutable snapshot.

That means query results are:

- consistent within one snapshot
- unaffected by later session mutations
- safe to compute lazily inside the snapshot implementation

Lazy forcing is allowed internally.

But semantically, a query is just a pure read over one prepared snapshot.

## 14. Core Queries

The engine should support, at minimum, queries like:

- `diagnostics`
- `module_typings_of`
- `export_of`
- `semantic_tree_of_source`
- `type_at`
- `definition_at`
- `scope_at`

Not every one of those needs to exist in the current prototype API today.

The point is that the public shape should be query-first, not tree-first.

That means:

- typed trees or semantic wrappers may exist
- but consumers should not be forced to couple themselves to one internal tree
  shape just to ask a focused question

## 15. definition_at

`definition_at` deserves one explicit rule.

If the queried symbol resolves to an exported symbol from another module, the
engine should be able to answer from `ModuleTypings` data directly.

That means `ModuleTypings` must carry exact definition origin data, not just
types and names.

Otherwise cross-module jumps would still depend on reopening source or external
artifacts, which is exactly what this engine is trying to avoid.

## 16. One-Shot Use

Compiler-style one-shot checking is just a thin host workflow on top of the
same session/snapshot model.

Conceptually:

1. host creates a session
2. host adds the source or package sources
3. host loads dependency summaries
4. host prepares a rooted snapshot
5. host asks for diagnostics and module summaries

That means one-shot use does not need a separate checker architecture.

It just has different lifetime and orchestration around the same core engine.

## 17. Incremental LSP Use

LSP use is the same architecture with different lifetime.

Conceptually:

1. keep one session alive for the workspace or project slice
2. map open buffers to stable `SourceId`s
3. update source text as the user edits
4. prepare a new rooted snapshot for the edited source
5. answer `diagnostics`, `type_at`, `definition_at`, and friends from that
   snapshot

The important point is:

LSP does not need a second typechecker.

It needs the same engine with longer-lived sessions and more frequent snapshot
preparation.

## 18. Parallelism

This engine should be parallel-friendly.

That means:

- snapshot queries should be safe to run in parallel when they only read a
  prepared snapshot
- host-side fetch-or-compute of independent dependency summaries should be
  parallelizable
- query-local mutation inside analysis is fine, as long as it does not escape
  the query boundary

No global mutable checker state should sit in the middle of this and ruin it.

## 19. Diagnostics

The engine layer must preserve diagnostics as structured data all the way
through.

That means:

- parser diagnostics may come from `syn`
- lowering diagnostics come from the lowering layer
- typing diagnostics come from inference and the solver

But the query surface should expose them as structured values, not as rendered
strings.

Rendering belongs to callers like `riot check`, LSP, or later tooling.

## 20. Mapping To `typ`

This document implies a few architectural constraints for `typ`.

1. `Session` should stay host-owned and mutable.

2. `Snapshot` should be the immutable query world.

3. `Config` should carry host-loaded ambient summaries, not grow a giant fake
prelude.

4. `ModuleTypings` should become the one true reusable artifact boundary.

5. `Query` should stay centered on focused semantic questions, not on one giant
returned typed tree blob.

## 21. Relationship To Upstream OCaml

Upstream OCaml gives us excellent typing behavior, but not the library-shaped
engine we want here.

Today Riot still has to do a lot of work around the compiler:

- file accounting
- process orchestration
- `.cmi` placement
- artifact reopening
- string diagnostic parsing

The whole point of this engine contract is to move that burden out of the host
and into a reusable in-process semantic library.

That is the engine contract this layer needs to deliver.
