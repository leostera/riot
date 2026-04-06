# Typ Checker Manual

This directory is the checker manual for `typ`.

Right now, this is the main normative spec stack for the package.

The point of these docs is not to narrate the current prototype. The point is
to say what checker and engine we are actually trying to build.

If the implementation drifts from these docs, that should be visible and
discussable.

## How To Read This Manual

Start here:

- [checker.md](./checker.md)
  the first core HM-ish checker slice
- [solver.md](./solver.md)
  the shared algebra underneath the checker slices
- [lowering.md](./lowering.md)
  the contract between source syntax and semantic typing input
- [engine.md](./engine.md)
  the library/runtime contract: sessions, snapshots, summaries, queries

Then read the feature slices:

- [nominal_data.md](./nominal_data.md)
  ordinary type declarations, records, ordinary variants
- [generalization.md](./generalization.md)
  generalization boundaries, nonexpansiveness, and value restriction
- [labeled_args.md](./labeled_args.md)
  labeled and optional parameters, application matching, and defaults
- [modules.md](./modules.md)
  the module calculus: structures, signatures, functors, inclusion,
  strengthening
- [signatures.md](./signatures.md)
  signature elaboration, `with`-constraints, and implementation checking
- [first_class_modules.md](./first_class_modules.md)
  the bridge between the core calculus and the module calculus
- [recursive_modules.md](./recursive_modules.md)
  recursive-module approximation, explicit signatures, and inclusion checks
- [gadts.md](./gadts.md)
  constructor result annotations, refinement, existentials, local equalities
- [extensible_variants.md](./extensible_variants.md)
  open variants, extension constructors, rebinding, and exceptions
- [polyvariants.md](./polyvariants.md)
  row-typed variants, row unification, pressure, and finalization
- [pattern_analysis.md](./pattern_analysis.md)
  exhaustiveness, redundancy, refutation, and fragile matches
- [effects.md](./effects.md)
  effect handlers, effect cases, and continuation typing
- [diagnostics.md](./diagnostics.md)
  structured diagnostics as a first-class machine-facing contract

## What Each Doc Owns

These docs are meant to have clear ownership boundaries.

- `checker.md`
  owns the first core calculus and its typing rules
- `solver.md`
  owns shared inference and solving operations used by the checker docs
- `lowering.md`
  owns semantic normalization, origins, and recovery lowering
- `engine.md`
  owns sessions, rooted snapshots, `ModuleTypings`, store hydration, and query
  semantics
- `generalization.md`
  owns value restriction, nonexpansiveness, and generalization boundaries
- `labeled_args.md`
  owns labeled arrows, optional defaults, and application matching rules
- `signatures.md`
  owns interfaces, signature elaboration, `with`-constraints, and
  implementation checking
- `recursive_modules.md`
  owns recursive-module approximation and acceptance rules
- `extensible_variants.md`
  owns open variants, extension constructors, and exceptions
- `pattern_analysis.md`
  owns exhaustiveness, redundancy, refutation, and fragile-pattern behavior
- `effects.md`
  owns effect-case typing and the core/effect bridge
- feature docs
  own the extra semantic rules for each language fragment
- `diagnostics.md`
  owns the shape and lifecycle of diagnostics across parse, lowering, typing,
  and query boundaries

If two docs start trying to own the same thing, one of them is probably too
wide.

## How This Maps To `typ`

At a high level, the implementation wants these seams too:

- source + parse layer
- lowering layer
- semantic storage layer
- solver + inference layer
- summary/persistence layer
- session/snapshot/query engine layer
- diagnostics layer

That is the architectural point of writing these separately instead of
collecting everything into one huge “design.md”.

## How To Use These Docs During Implementation

The intended loop is:

1. pick one implementation slice
2. find the spec docs that own that slice
3. write fixtures and snapshots against the promised behavior
4. implement the smallest missing piece
5. update the spec only if we learned the contract itself was wrong or missing

That keeps the docs as specs instead of post-hoc changelogs.

## Writing Style

These docs should stay implementation-agnostic, but they should not be vague.

That means a good spec doc should usually include:

- the semantic rule in plain language
- at least one small concrete example when the rule is user-visible
- pseudocode when the control flow matters more than the exact data
- a graph or diagram when the flow is easier to understand visually than in
  paragraphs

Not every doc needs every one of those, but "all prose, no examples, no
algorithm sketch" is usually a sign the contract is still too soft.

## Validation

These docs should be treated like code.

That means a good docs change should at least pass:

- clean internal links
- terminology consistency with the public `typ` interfaces where names already
  exist
- `git diff --check`

And when the implementation exists for the relevant slice:

- snapshots
- diagnostics fixtures
- behavioral tests against the stated contract

## Current Big Picture

The spec stack says `typ` is:

- a library-first checker
- incremental through sessions and rooted snapshots
- semantically centered on lowered forms plus origins, not on raw CST
- query-first at the public boundary
- driven by canonical reusable `ModuleTypings` artifacts
- structured-diagnostic-first from parse through typing

That is the checker and engine we are building.

## Coverage Matrix

This is the current status of the spec set.

### Specified

- core HM-ish calculus
- solver and unification layer
- lowering and origin contract
- generalization and value restriction
- labeled and optional arguments
- nominal type declarations
- ordinary records
- ordinary variants
- signatures and interfaces
- module calculus and functors
- recursive modules
- first-class modules
- extensible variants and exceptions
- GADTs
- polymorphic variants
- pattern analysis
- effect handlers
- diagnostics
- session/snapshot/store/query engine contract

### Intentionally Out

- objects
- classes
- the object system's row calculus
- a user-visible full effect-row system on ordinary function types

### Still Meant To Evolve

- the exact canonical payload shape of `ModuleTypings`
- the exact public query surface once more implementation slices land
- any future experimental extensions Riot may want to add on top of the
  functional OCaml subset
