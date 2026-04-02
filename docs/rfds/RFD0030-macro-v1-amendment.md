# RFD0030 - Macro V1 Amendment to RFD0008

- Feature Name: `macro_v1_amendment`
- Start Date: `2026-04-02`
- Status: `presented`
- Amends: `RFD0008 - Macro`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD amends `RFD0008 - Macro` by narrowing Riot's first macro release to a
smaller, more explicit v1:

- v1 supports **procedural function-like macros only**
- v1 requires **separate macro packages**
- macro packages are declared as `[lib] kind = "macro"`
- macro invocation syntax is a **path-qualified function-like call** such as
  `Sqlx_macro.query! "select * from users"`
- macro execution remains **token-stream based**
- macro expansion becomes a **first-class compilation pipeline stage** in
  `riot`, not an ad hoc preprocessor hook

This amendment does not replace `RFD0008`. It makes concrete choices for the
first implementation where `RFD0008` intentionally left the surface syntax,
package model, and build integration strategy open.

## Motivation
[motivation]: #motivation

`RFD0008` got the architectural center right:

- macro input and output should be token-stream based
- macro authors should be able to opt into `syn`
- diagnostics should be first-class
- macro execution should be build-integrated

What it left deliberately open is now the main source of ambiguity:

- what exact macro forms should v1 support?
- how are macro packages declared?
- how are macro providers loaded?
- how should macro expansion fit into `riot` planning?

That ambiguity is useful at ideation time, but it is not useful for the first
real implementation. The first prototype already showed the failure mode: a
centralized expander inside `packages/macro` is enough to prove parser and
planner plumbing, but it is the wrong ownership model for a real macro system.

The v1 design should force explicit ownership and explicit dependency edges.
`sqlx` should not "casually" grow macros inside its ordinary runtime package.
If `sqlx` wants macros, it should ship `sqlx-macro`. The package graph should
say so plainly, and `riot` should plan those artifacts plainly.

The other motivation is build-system clarity. Macro expansion is not generic
preprocessing. It is a syntax-aware compilation stage with parser inputs,
diagnostics, generated artifacts, and cache invalidation rules. Treating it as
a first-class pipeline stage will make the current macro work easier to reason
about and will also help future stages such as earlier linting, later linting,
or macro-aware typechecking.

Finally, this amendment narrows scope on purpose. Declarative macros,
attribute-style macros, and derive-style macros are valuable, but they should
not complicate the first runtime contract. Riot should ship one clean macro ABI
and one clean build model first.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Contributors should think about macro v1 as:

- one explicit macro package kind
- one explicit macro invocation form
- one explicit token-stream ABI
- one explicit macro expansion stage in the compilation pipeline

### What macro authors write

A package that wants to provide macros should be a dedicated macro package:

```toml
[package]
name = "sqlx-macro"

[lib]
kind = "macro"
path = "src/sqlx_macro.ml"

[dependencies]
macro = { workspace = true }
syn = { workspace = true }
```

That package should export a macro provider module. The exact API names may
change, but the shape should be close to:

```ocaml
let query (input : Macro.TokenStream.t) : Macro.Result.t =
  match Macro.Parse.expr input with
  | Ok expr -> Sqlx_query.expand expr
  | Error diagnostic ->
      Macro.Result.error diagnostic

let provider =
  Macro.Provider.v [
    Macro.Provider.fn "query" query;
  ]
```

The important part is the contract:

- macro input is a token stream
- the macro may choose to parse with `syn`
- the macro returns generated tokens plus diagnostics
- the package owns the macro implementation

### What macro users write

If another package wants to use that macro, it depends on the macro package:

```toml
[dependencies]
sqlx-macro = { workspace = true }
```

Then it invokes the macro through a normal path-qualified name:

```ocaml
let users =
  Sqlx_macro.query! "select * from users"
```

Macro calls should read like ordinary path-qualified function calls, except for
the `!`.

### What syntax v1 supports

V1 supports function-like macros in expression position only.

The parser should recognize a path-qualified macro callee followed by `!` and
then one expression body:

- `Format.format! "hello {}" name`
- `Sqlx_macro.query! "select * from users"`
- `My_pkg.Tools.expand! x + y`

The body boundary should match `parse_expr`. So:

```ocaml
let value = My_pkg.Tools.expand! x + y in
...
```

means the macro body is `x + y`, not just `x`.

The macro runtime should receive the token stream corresponding to that body.
Macros may then parse the body as an OCaml expression if they want, but that is
an author choice, not the ABI.

This means v1 is still constrained by expression syntax. That is acceptable for
the first release. Truly raw token-capture forms can be designed later if Riot
needs them.

### What `riot` does

For one compilation unit, the mental model should be:

```ocaml
Build_pipeline.(
  from_source source
  |> and_then Stage.syn_parse
  |> and_then Stage.macro_expand
  |> and_then Stage.compile
  |> to_action_graph)
```

The exact API shape is not the point. The important part is that macro
expansion is a named stage with named inputs and outputs.

At a high level:

1. `syn` parses the source file.
2. `riot` finds macro invocations.
3. `riot` resolves reachable macro packages from the package dependency graph.
4. `riot` builds or loads the macro runner artifact for those macro providers.
5. `riot` invokes macros with token streams.
6. `riot` reparses the expanded output with `syn`.
7. `riot` continues the normal compilation flow.

That is not string preprocessing. It is planned, validated expansion.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Relationship to `RFD0008`

`RFD0008` remains the base macro RFD. This amendment narrows and concretizes
the first deliverable.

The following parts of `RFD0008` remain unchanged:

- token-stream macro ABI
- optional `syn` parsing inside macro implementations
- reparsing generated output with `syn`
- first-class macro diagnostics
- explicit build integration

This amendment changes the first implementation plan in four main ways:

1. v1 supports only procedural function-like macros
2. v1 requires separate macro packages
3. v1 picks a concrete invocation syntax
4. v1 requires a first-class compilation pipeline stage for macro expansion

## 2. Scope of macro v1

Macro v1 should support exactly this:

- procedural macros
- function-like invocation
- expression-position payloads
- token-stream input and output
- diagnostics emitted by macro implementations

Macro v1 should explicitly not support:

- declarative `macro foo! = ...` syntax
- derive-style macros
- attribute-style macros
- raw non-expression token capture forms
- any claim of full hygiene

Future macro forms should reuse the same runtime and provider model where
possible, rather than creating a second macro system.

## 3. Package model

Macro providers should live in separate packages.

The manifest shape should be:

```toml
[lib]
kind = "macro"
path = "src/sqlx_macro.ml"
```

This amendment intentionally rejects mixed runtime-and-macro packages for v1.
If a package wants to provide runtime APIs and macros, it should publish two
packages, for example:

- `sqlx`
- `sqlx-macro`

The benefits are:

- explicit dependency edges
- simpler planning
- clearer ownership
- simpler invalidation and caching
- less ambiguity around whether a package's normal library build is also a
  macro provider

Macro packages should be treated as a distinct dependency class in `riot` and
should build before packages that invoke them.

## 4. Provider registration model

Each macro package should compile to a provider artifact with a clear
entrypoint. The entrypoint should register the macros exported by that package.

A plausible shape is:

```ocaml
type macro_fn = Macro.TokenStream.t -> Macro.Result.t

type exported_macro = {
  name : string;
  expand : macro_fn;
}

type provider = {
  package_name : string;
  macros : exported_macro list;
}
```

The exact names can change, but the semantics should be:

- one macro package provides one provider entrypoint
- that provider entrypoint declares the macros exported by the package
- `riot` discovers providers from reachable macro dependencies
- `riot` builds a macro-runner artifact from those providers

This should follow the same broad ownership model as package-provided fix
rules, even if the runtime mechanics differ.

## 5. Invocation syntax and parser model

The function-like macro syntax for v1 should be:

- `Pkg.macro_name! expr`
- `Pkg.Subpkg.macro_name! expr`

The parser should treat the callee path plus `!` as a dedicated macro
invocation form in expression position. This should produce a dedicated CST
node, not a generic apply expression and not a late CST rewrite.

The invocation body should be bounded by `parse_expr`.

That means:

- `Macro_pkg.foo! x + y` captures `x + y`
- `Macro_pkg.foo! (x, y)` captures `(x, y)`
- `Macro_pkg.foo! let x = 1 in x + 1` captures that full expression

The runtime input should still be the token stream corresponding to that body,
not the parsed body AST as the public ABI.

## 6. Macro ABI

The macro ABI should stay token-stream based:

```ocaml
type result = {
  output : Macro.TokenStream.t;
  diagnostics : Macro.Diagnostic.t list;
}

val expand : Macro.TokenStream.t -> result
```

The token stream is the only universal boundary that works across:

- macros that want to use `syn`
- macros that want to parse their own DSL
- macros that only do structural token rewrites

`macro` should provide ergonomic adapters such as:

- `Macro.Parse.expr`
- `Macro.Parse.item`
- `Macro.Parse.type_expr`
- `Macro.Parse.pattern`

Those helpers are convenience layers, not the ABI.

## 7. Build pipeline integration

Macro expansion should become a first-class stage in a compilation-unit
pipeline. `riot-planner` should stop treating it as a planner-local helper
hidden inside one compile path.

A likely starting point is a small `Compilation_pipeline.t` or
`Build_pipeline.t` with explicit stages and artifacts. The first concrete use
can stay narrow:

- `syn_parse`
- `macro_expand`
- `compile`

Later stages can be introduced without reworking the planner model from
scratch.

The macro expansion stage should explicitly depend on:

- input source hash
- resolved macro dependency set
- macro package artifact hashes
- macro configuration, if any

Its output should be:

- expanded source or token artifact
- diagnostics
- a validated reparsed syntax tree or equivalent validated artifact

The action graph should then be derived from this compilation pipeline.

## 8. Execution model

The v1 execution model should be explicit and artifact-based.

`riot` should not special-case macros as a compiler side table. Macro packages
should build into real artifacts with real entrypoints. `riot` may execute them
in-process or out-of-process as an implementation detail, but the planner model
should treat them as ordinary build artifacts with deterministic inputs.

The important properties are:

- deterministic expansion
- no hidden network or filesystem behavior
- debuggable execution
- explicit dependency tracking

## 9. Diagnostics

Macro diagnostics should be structured and phase-aware.

Users should be able to tell whether a failure came from:

- parsing the original source
- resolving or loading the macro provider
- macro expansion itself
- reparsing generated output

The macro ABI should therefore allow diagnostics to carry:

- a primary span
- a message
- optional notes or suggestions
- a phase tag or equivalent origin marker

## Drawbacks
[drawbacks]: #drawbacks

- Requiring separate macro packages introduces more package count and more
  dependency edges.
- Restricting v1 to expression-position function-like macros leaves some DSL
  shapes out of scope initially.
- A compilation pipeline abstraction adds planner structure before every future
  stage is fully designed.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

This design is the best amendment to `RFD0008` because it narrows the first
macro release without discarding the original architecture.

Alternatives considered:

- **Allow mixed runtime and macro packages**
  Simpler in the short term, but it makes dependency ownership and planner
  behavior less explicit.

- **Keep the syntax fully open for longer**
  That preserves optionality, but it blocks real implementation decisions in
  `syn`, `riot-model`, and `riot-planner`.

- **Support declarative macros in v1**
  Valuable, but it adds a second authoring surface before the core runtime is
  settled. Declarative macros can be layered on top of the same provider ABI
  later.

- **Keep macro expansion as a planner-local rewrite hook**
  Good enough for a prototype, but the wrong shape for dependency tracking,
  caching, and future build stages.

## Prior art
[prior-art]: #prior-art

The strongest prior art remains Rust:

- token-stream procedural macros
- package-managed macro crates
- derive, attribute, and function-like macro families

This amendment intentionally narrows Riot's first step to just one of those
families while preserving the same core procedural boundary.

There is also direct Riot prior art in `riot-fix` package-provided rule
discovery and generated runner construction. The macro provider model should
take strong cues from that work, even though macro execution happens in the
normal build pipeline rather than the fix pipeline.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- What exact manifest fields should accompany `[lib] kind = "macro"`?
- Should `riot` generate one macro-runner per workspace, per package graph, or
  per compilation context?
- Should v1 store expanded artifacts as rewritten source files, token artifacts,
  or both?
- What minimum hygiene helpers should `macro` expose in v1?
- How should path-qualified macro names map to package names and module names in
  a way that feels unsurprising?

## Future possibilities
[future-possibilities]: #future-possibilities

Once this narrower v1 exists, Riot can add richer macro forms without changing
the core ownership or execution model:

- declarative macros that compile down to the same provider ABI
- derive-style macros
- attribute-style macros
- raw token-capture forms outside expression position
- editor tooling for expansion inspection and macro diagnostics
- stronger hygiene helpers and generated-name support
