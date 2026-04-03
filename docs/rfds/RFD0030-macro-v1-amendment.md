# RFD0030 - Macro V1 Amendment to RFD0008

- Feature Name: `macro_v1_amendment`
- Start Date: `2026-04-03`
- Status: `presented`
- Amends: `RFD0008 - Macro`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD amends `RFD0008 - Macro` by locking Riot's first macro release to a
smaller, build-integrated v1:

- v1 supports **procedural function-like macros only**
- macros are discovered from explicit package metadata under
  **`[riot.macro.provider]`**
- **any package may also provide macros**, just like any package may also
  provide fix rules
- macro invocation syntax is a **path-qualified function-like call** such as
  `Sqlx.query! "select * from users"`
- macro execution remains **token-stream based**
- macro expansion becomes a **first-class compilation-pipeline stage** in
  `riot`

This amendment does not replace `RFD0008`. It narrows the v1 surface and makes
concrete decisions where `RFD0008` intentionally left the package model,
surface syntax, and planner integration open.

This amendment also supersedes earlier drafts of `RFD0030` that required
separate `*-macro` packages or `[lib] kind = "macro"`. V1 should not need a
special library kind just to advertise macro capability.

## Motivation
[motivation]: #motivation

`RFD0008` already got the architectural center right:

- macros consume tokens and produce tokens
- macro implementations may opt into `syn`
- diagnostics are first-class
- macro expansion is build-integrated, not editor-only sugar

What remained open was the package model. The first prototype proved that
parser and planner plumbing can work, but it also showed the wrong direction:
a centralized expander or a special "macro package" kind forces macro support
into an awkward side channel.

Riot already has a better precedent: fix-rule providers. Any package may expose
them through explicit manifest metadata, and planning can discover them without
guessing from naming conventions. Macro providers should work the same way.

This keeps the first release explicit without being needlessly restrictive:

- packages can expose runtime APIs and macro APIs together
- planning can discover macro providers statically from manifests
- planning can reject unknown `Pkg.macro!` uses before runner execution
- the compiler pipeline stays honest about macro expansion as a real stage

At the same time, v1 stays deliberately narrow. Declarative macros, attribute
macros, derive macros, and typed macro passes are all valuable, but they should
layer on top of one clean procedural runtime and one clean planner model.

## Guide-Level Explanation
[guide-level-explanation]: #guide-level-explanation

### What Macro Authors Write

A package that wants to expose macros keeps an ordinary library declaration and
adds explicit macro-provider metadata:

```toml
[package]
name = "sqlx"
version = "0.1.0"

[lib]
path = "src/sqlx.ml"

[riot.macro.provider]
path = "src/sqlx_macro.ml"
module_path = "Sqlx"
macros = ["query", "query_as"]

[dependencies]
macro = { workspace = true }
syn = { workspace = true }
```

The provider source must expose a top-level thunk:

```ocaml
let query (input : Macro.Token_stream.t) : Macro.Result.t =
  match Macro.Parse.expr input with
  | Ok expr -> Sqlx_query.expand expr
  | Error diagnostic ->
      { Macro.Result.output = input; diagnostics = [ diagnostic ] }

let query_as (input : Macro.Token_stream.t) : Macro.Result.t =
  Sqlx_query_as.expand input

let provider () =
  Macro.Provider.v
    ~module_path:[ "Sqlx" ]
    [
      Macro.Provider.fn "query" query;
      Macro.Provider.fn "query_as" query_as;
    ]
```

The important part is the contract:

- macro input is a token stream
- the macro may choose to parse those tokens with `syn`
- the macro returns generated tokens plus diagnostics
- the manifest declares the qualified module path and exported macro names

### What Macro Users Write

If another package depends on `sqlx`, it can invoke declared macros through the
provider's qualified module path:

```toml
[dependencies]
sqlx = { workspace = true }
```

```ocaml
let users =
  Sqlx.query! "select * from users"
```

Macro invocations should read like ordinary qualified function calls with a
bang.

### What Syntax V1 Supports

V1 supports function-like macros in expression position only.

The parser recognizes a path-qualified macro callee followed by `!` and one
expression body:

- `Format.format! "hello {}" name`
- `Sqlx.query! "select * from users"`
- `My_pkg.Tools.expand! x + y`

The body boundary follows `parse_expr`. So:

```ocaml
let value = My_pkg.Tools.expand! x + y in
...
```

means the macro body is `x + y`, not just `x`.

The macro runtime receives the token stream corresponding to that parsed body.
Macros may choose to parse those tokens as OCaml, but that is an author choice,
not something imposed by the ABI.

### What `riot` Does

For one compilation unit, the mental model should be:

```ocaml
Compilation_pipeline.(
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
3. `riot` resolves reachable macro providers from package metadata.
4. `riot` validates the invocation against declared `module_path` and `macros`
   before runner execution.
5. `riot` builds or reuses the macro runner artifact for those providers.
6. `riot` invokes macros with token streams.
7. `riot` reparses the expanded output with `syn`.
8. `riot` continues the normal compilation flow.

That is not string preprocessing. It is planned, validated expansion.

## Reference-Level Explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Relationship to `RFD0008`

`RFD0008` remains the base macro RFD. This amendment narrows the first
deliverable.

The following parts of `RFD0008` remain unchanged:

- token-stream macro ABI
- optional `syn` parsing inside macro implementations
- reparsing generated output with `syn`
- first-class macro diagnostics
- explicit build integration

This amendment makes four concrete choices for v1:

1. only procedural function-like macros ship in v1
2. macro providers are declared in package metadata, not via a special library
   kind
3. invocation syntax is path-qualified and function-like
4. macro expansion is a first-class compilation-pipeline stage

## 2. Package Model

Macro capability is package metadata:

```toml
[riot.macro.provider]
path = "src/provider.ml"
module_path = "Sqlx"
macros = ["query"]
```

This means:

- any package may provide macros
- packages may expose runtime APIs and macro APIs together
- separate `sqlx-macro` style packages remain allowed as an organizational
  choice, but Riot does not require them
- `[lib] kind = "macro"` is not part of v1 and should be rejected

The provider metadata must at least declare:

- `path`: the provider implementation source file
- `module_path`: the qualified module path used at call sites
- `macros`: the exported function-like macro names

Planning should treat that metadata as the source of truth for discovery.

## 3. Provider Contract

The runtime provider ABI remains procedural and token-stream based:

```ocaml
Macro.Token_stream.t -> Macro.Result.t
```

The provider source file must expose:

```ocaml
let provider () = Macro.Provider.v ...
```

Provider validation should happen before runner materialization so that invalid
providers fail as planner diagnostics, not as nested-build surprises.

## 4. Invocation Resolution

Macro invocations must be qualified in v1.

Given:

```toml
[riot.macro.provider]
module_path = "Sqlx"
macros = ["query"]
```

valid use sites look like:

```ocaml
Sqlx.query! "select * from users"
```

and not:

```ocaml
query! "select * from users"
```

Planning should reject:

- unqualified invocations
- unknown provider paths
- unknown macro names for a known provider path
- ambiguous provider-path declarations

Those errors should list reachable provider paths or exported qualified macro
names when possible.

## 5. Pipeline Integration

Macro expansion is planner-owned compilation work. At minimum, the pipeline for
a concrete source file must be able to:

1. parse with `syn`
2. discover invocations
3. resolve reachable providers from package metadata
4. validate declared exports against use sites
5. invoke expansion through a runner artifact
6. reparse expanded output
7. lower to normal compile actions

The action graph should make the source rewrite explicit, for example through a
`WriteFile` or `RunMacroExpansion` action before compile.

## 6. Scope of V1

V1 should support exactly this:

- procedural macros
- function-like macro invocation
- expression-position macro bodies
- package metadata discovery
- runner-based expansion
- diagnostics from macro execution

V1 explicitly does not include:

- declarative macros
- attribute macros
- derive macros
- typed macro passes
- raw non-expression token capture forms

Those can come later once the procedural runtime and planner contract are
stable.

## Rationale And Alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

## Why Not Separate Macro Packages Only?

That is stricter than necessary and does not fit Riot's existing provider
patterns. Fix rules are not forced into separate packages, and macros do not
need to be either.

Separate packages are still a perfectly valid organizational choice when they
help keep dependencies clean. They just should not be mandated by the platform.

## Why Not Keep `[lib] kind = "macro"`?

Because macro capability is not a different kind of library artifact. It is a
provider capability declared by package metadata.

Using a special library kind creates the wrong mental model:

- it suggests packages must choose between runtime code and macro code
- it forces discovery through library classification instead of explicit
  provider metadata
- it drifts away from how Riot already discovers fix providers

## Drawbacks
[drawbacks]: #drawbacks

- Package manifests become slightly more verbose because macro exports are
  declared explicitly.
- Planning has to validate provider metadata and invocation resolution before
  runner execution.
- Provider metadata can drift from implementation unless Riot validates both
  sides consistently.

These are acceptable tradeoffs for a first macro release because they keep
discovery deterministic and planner diagnostics clear.

## Future Possibilities
[future-possibilities]: #future-possibilities

- declarative macros that compile down to the same procedural provider ABI
- attribute and derive macro forms on top of the same runner model
- typed follow-up passes for macros like `format!` that want post-typecheck
  dispatch
- richer token-capture forms for non-OCaml syntaxes

## Unresolved Questions
[unresolved-questions]: #unresolved-questions

- Should one package eventually be allowed to declare multiple macro providers,
  or is one provider per package enough for v1?
- How strict should Riot be about validating that declared `macros = [...]`
  exactly match the provider's runtime export list?
- What is the best long-term cache and materialization strategy for generated
  macro runners?
