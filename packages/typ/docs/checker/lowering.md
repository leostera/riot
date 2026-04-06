# Typ Lowering

This document specifies the lowering contract for `typ`.

The point here is simple: `typ` does not infer directly on `Syn.Cst`.

`Syn.Cst` is the source layer. It is where we keep exact syntax, trivia, and
surface spelling.

The checker itself runs on semantic forms.

That means lowering is not an implementation detail. It is the seam that
decides:

- what survives semantically
- what gets normalized away
- what stays only in origin data
- how recovery works when syntax is unsupported or incomplete

If we get this wrong, we either lose too much source information for tools, or
we smuggle too much CST into the semantic layer and end up with a worse CST in
disguise.

## 1. Scope

This document covers:

- semantic equivalence classes
- the split between `ItemTree`, `BodyArena`, and `OriginMap`
- stable semantic identities
- source origins and spans
- unsupported-syntax recovery during lowering
- the contract between lowering and inference

This document does not cover:

- the details of the solver
- the details of query execution
- the exact serialized shape of stored module summaries

Those are all separate concerns. They should be specified separately.

## 2. What Lowering Is For

Lowering has three jobs.

1. Normalize surface syntax into a smaller semantic language.

2. Preserve source linkage through origins instead of keeping CST nodes alive in
every semantic object.

3. Keep the pipeline moving even when syntax is unsupported or partially broken,
by lowering into recovery forms plus structured diagnostics.

That means the lowered layer is not:

- a pretty-printed CST
- a typed tree
- a serialization format

It is the semantic input to inference and later queries.

## 3. Long-Lived Semantic State

For one source file, the semantic lowering result should be split into three
pieces:

- `ItemTree`
- `BodyArena`
- `OriginMap`

wrapped together for convenience by `SemanticTree.file`.

That split is important.

### ItemTree

`ItemTree` is the body-stable top-level skeleton.

This is where we keep:

- type declarations
- exception declarations
- value-item shells
- open items
- unsupported top-level items

The point of `ItemTree` is not to preserve every token. The point is to keep
the top-level structure stable across many body edits.

### BodyArena

`BodyArena` is the normalized local semantic structure.

This is where we keep:

- expressions
- patterns
- bindings
- match cases
- function parameters
- labeled arguments

The point of `BodyArena` is to hold the semantic forms the inferencer will walk.

### OriginMap

`OriginMap` is where source fidelity lives.

It maps stable semantic identities to:

- source id
- source revision
- syntax kind
- span
- lowering label

That is how we preserve exact source attachment without retaining raw CST nodes
inside every long-lived semantic object.

## 4. Stable Semantic Identities

Lowering must assign semantic identities to the things later phases need to
talk about.

At a minimum:

- `ItemId`
- `BindingId`
- `ExprId`
- `PatId`
- `OriginId`

These ids are the real semantic handles.

The important rule is:

queries, diagnostics, and summaries should prefer semantic ids plus origins over
direct pointers into the CST.

That keeps the semantic layer position-independent while still allowing tools to
get back to exact source spans.

## 5. Lower By Semantic Equivalence Classes

This is the main rule.

Lowering happens by semantic equivalence classes.

That means:

- if two surface forms mean the same thing for typing and name resolution, they
  should lower to the same semantic shape
- if the original spelling still matters for tools, keep it in `OriginMap`, not
  in the semantic tree

Examples:

- `let f x = x`
- `let f = fun x -> x`

lower to the same semantic value binding plus function body.

- `function | A -> x | B -> y`
- `fun __arg -> match __arg with | A -> x | B -> y`

lower to the same semantic function-plus-match shape.

- `f a b c`
- nested left-associated applications

lower to one canonical application form in `BodyArena`.

- extra parentheses
- exact punctuation choices
- comments

do not survive semantically; they survive only through origins.

## 6. What Must Survive Semantically

Not everything should normalize away.

Surface distinctions must survive lowering when they change:

- typing behavior
- binding structure
- control flow
- query behavior

Examples of distinctions that should survive:

- `let` versus `match`
- pattern shape
- or-pattern alternatives
- explicit type ascriptions
- recursive versus non-recursive bindings
- labeled versus positional arguments
- module opens
- recovery holes and unsupported nodes

If a distinction changes how the checker reasons, it belongs in the semantic
layer.

If it only changes surface spelling, it belongs in origin data.

## 7. What Stays Only In Origins

Origins are where we keep source fidelity that the checker should not carry
around semantically.

That includes things like:

- exact source span
- syntax kind
- which CST family produced the node
- lowering label for debugging or tooling

This is also where the checker gets the ability to say:

“a-ha, here it is, das bug”

without forcing the semantic layer to store the full CST.

That is the whole point of `OriginMap`.

## 8. ItemTree And BodyArena Contracts

This split is not arbitrary.

Lowering should keep these invariants:

1. Editing inside a body should not, by itself, require unrelated top-level
item shells to be rebuilt in a totally different shape.

2. Top-level declaration facts needed for export summaries should be available
from `ItemTree` plus declaration elaboration, without forcing every body.

3. Local expression and pattern semantics should live in `BodyArena`, not get
stuffed back into item records.

That is the architectural move that keeps incremental behavior sane later on.

## 9. Unsupported Syntax And Recovery

Lowering must be lenient.

That means unsupported syntax should not immediately blow up the whole file.

Instead, lowering should produce:

- one or more structured diagnostics
- a recovery semantic form such as:
  - `Unsupported` top-level item
  - `PUnsupported`
  - `EUnsupported`
  - `EHole`

The recovery form must preserve enough shape that later phases can:

- keep walking
- avoid diagnostic cascades when possible
- still attach errors to useful source spans

This is the right place for the generic `UnsupportedSyntax` family while the
checker grows.

The semantic layer should make unsupportedness explicit, not hide it behind
mysterious missing nodes.

## 10. Lowering Diagnostics

Lowering diagnostics are part of the semantic result.

They should be:

- structured
- origin-backed
- machine-readable

They should talk about the semantic situation lowering encountered, not just
emit flattened strings.

That means a lowering diagnostic may need to carry things like:

- unsupported syntax kind
- the source span
- the surrounding module path
- which recovery form was produced

The consumer can always render a friendly string later.

## 11. Lowering And Inference

Inference runs on the lowered semantic layer, not on `Syn.Cst`.

So the lowering contract with inference is:

1. produce semantic forms the solver can walk directly
2. preserve enough origin data for diagnostics and queries
3. make unsupportedness explicit through recovery nodes
4. do not make the inferencer reconstruct surface syntax distinctions that
   lowering already knew about

If inference needs a distinction and lowering erased it, that is a lowering bug.

If lowering preserves a distinction that inference never needs and no query ever
uses, that is semantic bloat.

## 12. Lowering And Queries

Queries should treat the lowered semantic layer as primary and the CST as an
origin anchor.

That means:

- `type_at` should be driven by semantic ids and indexes, then mapped back to
  source origins
- `definition_at` should resolve to semantic objects or summary data, then use
  origins for spans
- diagnostics should point at origins, not try to re-find nodes in the CST from
  scratch every time

This is the same general reason we want summaries and queries to prefer stable
ids plus origins over raw syntax nodes.

## 13. Mapping To `typ`

This document implies a few architectural constraints for `typ`.

1. `Lower` should build `ItemTree`, `BodyArena`, and `OriginMap` together.

2. `SemanticTree.file` is a convenience wrapper, not the one true internal
store.

3. `OriginMap` should be the main source-backlink mechanism for semantic ids.

4. `SourceAnalysis` should keep both:

- the retained source snapshot or CST when useful
- the semantic layers inference and queries actually run on

That lets `typ` stay tool-friendly without centering the checker on the CST.

## 14. Relationship To Upstream OCaml

Upstream OCaml does not expose this split in the same clean way. Its checker is
much more tightly coupled to parsetree-shaped traversal and typed-tree
production.

That is exactly why this document matters for `typ`.

The design rule we want is:

- source fidelity belongs to the source layer and origins
- semantic normalization belongs to lowering
- typing belongs to the solver and inference layers

That is how `typ` avoids becoming a big CST-shaped typechecker with prettier
names.
