# RFD0042 - Script Files for `riot run`

- Feature Name: `riot_run_script_files`
- Start Date: `2026-04-09`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD extends `riot run` with a script-file mode for existing `.ml` files.
When the target resolves to a real file, Riot should treat it as a single-file
Riot program, synthesize the equivalent of a small workspace and package for
it, and then build and run it through the normal Riot planner, dependency, and
runtime flow. This is a workflow change for quick tools and shared scripts, not
a new package format and not a separate evaluator. Script-specific logic should
live in a dedicated `riot-script` package rather than being spread across
`riot-cli` and `riot-build`. The synthetic workspace itself should exist only
in memory rather than as a staged fake project on disk.

- Riot ignores one leading shebang line, then reads a top-of-file prelude of
  `#use <registry-spec>;;` directives.
- `std` is implicitly available by default, but an explicit `#use std@...;;`
  overrides that default.
- Riot wraps the script body in an actor-backed runnable and returns `Ok ()`
  after the body runs.
- A dedicated `riot-script` package owns script parsing, directive
  interpretation, synthetic workspace staging, and wrapper generation.
- The synthetic workspace and package model live in memory only. Riot may still
  reuse Riot's normal global cache or use a `/tmp` cache for resolved
  dependencies and build artifacts.
- Inside a workspace, the script inherits the whole workspace environment,
  including local packages and workspace configuration, while script-local
  external dependencies override workspace external requirements for that
  script.
- Phase 1 only supports registry-style `#use` directives. Path, file, and URL
  forms, plus source-path fidelity in diagnostics, are out of scope.

## Motivation
[motivation]: #motivation

Riot currently makes quick tools and throwaway scripts pay package and workspace
ceremony that is appropriate for maintained binaries but disproportionate for
"just run this file" workflows. 

- **Authoring cost** the author has to create or choose a workspace, create a
package, add dependencies, write `src/main.ml`, and then build and install the
binary tool before the first useful run.

- **Sharing cost:** the interesting artifact stops being the file itself and
becomes the surrounding scaffolding, which makes gists, pasted snippets, and
one-file helpers awkward to share and execute.

We should make a single file easy to run, easy to share, and still feel like a
real Riot program. 

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

This proposal solves the problem by teaching `riot run` a script-file mode for
real `.ml` files. When the target is a file, Riot should treat it as a
single-file Riot program, read a small script prelude, synthesize the
equivalent of an in-memory workspace and package for it, and then run it
through the normal Riot build and runtime flow.

### Standalone script

Suppose a contributor writes this file:

```ocaml
#!/bin/bash riot run

#use blink;;

let _ :: url :: _ = Std.Env.args

Blink.get url
```

and runs:

```sh
riot run ./curl.ml -- leostera.com
```

With this proposal, Riot should:

1. detect that `./curl.ml` is an existing `.ml` file
2. ignore the leading shebang line
3. read the top-of-file `#use ...;;` prelude
4. treat `std` as implicitly available, then add `blink` as an ordinary Riot
   package dependency
5. synthesize an in-memory workspace/package for the script
6. generate a runnable wrapper that starts `Actors.run`
7. build and run the script through the normal Riot pipeline

The script body itself remains ordinary top-level OCaml. Riot is not asking the
author to define a package, a binary manifest, or a manual `main` wrapper for a
disposable tool.

The workflow should feel morally equivalent to:

1. `riot init`
2. `riot add blink`
3. place the script body into `src/main.ml`
4. `riot run`

except that Riot performs those steps synthetically and caches the result.
except that Riot performs those steps synthetically in memory while still
reusing Riot's normal dependency and build caches.

### Inside an existing workspace

Now suppose the same script lives inside a workspace and wants to use local
workspace packages too.

In that case, `riot run ./tools/curl.ml -- leostera.com` should still enter
script mode, but the script should inherit the whole workspace environment:

- local workspace packages remain available
- workspace toolchain and profile configuration remain in effect
- workspace operational config remains in effect
- workspace external dependencies remain visible unless the script explicitly
  overrides them with `#use`

This means the script behaves like an extra synthetic package attached to the
current workspace, not like a separate detached evaluator.

### Prelude boundary

Script directives are intentionally limited to the top of the file.

This is valid:

```ocaml
#use blink;;
#use suri@0.1;;

let value = 1
```

This is not:

```ocaml
let value = 1
#use blink;;
```

The goal is to keep the model simple: Riot reads a script prelude first, then
the rest of the file is just OCaml source.

### Implicit `std`

Scripts get `std` by default. A script that does not mention `std` can still
refer to qualified `Std` names such as `Std.Env.args`.

If the prelude contains:

```ocaml
#use std@0.1;;
```

that explicit requirement replaces the implicit default. This lets a script pin
`std` when needed instead of always floating to Riot's default choice.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Target resolution

`riot run` should gain a script-file branch in its target resolution.

The rule is:

1. if the target argument resolves, from the process current directory, to an
   existing `.ml` file, enter script mode
2. otherwise keep the existing `riot run` resolution behavior

This preserves the current local-binary and remote-source flows for non-file
targets while making real script paths work anywhere.

### Package boundary

This feature should introduce a dedicated `riot-script` package.

`riot-script` should own:

- shebang stripping
- `#use ...;;` prelude parsing and validation
- script dependency interpretation
- synthetic workspace and package construction
- generated wrapper and virtual source layout

`riot-cli` should stay thin:

- detect when `riot run <target>` resolves to a real `.ml` file
- delegate script loading/preparation into `riot-script`
- continue rendering user-facing output

`riot-build` should keep owning the normal build and run path:

- consume the prepared synthetic workspace/package shape
- plan, build, and run it like any other Riot package

This keeps script-specific behavior in one package instead of distributing it
across CLI parsing, dependency loading, and build orchestration modules.

### Script parsing

Phase 1 script parsing is intentionally small and line-oriented.

Riot should:

1. read the file contents
2. if the first line starts with `#!`, drop that line from script preprocessing
3. read a contiguous leading block of `#use <spec>;;` directives
4. stop directive parsing at the first non-empty, non-directive OCaml line
5. treat any later `#use` line as an error

In phase 1, `<spec>` is limited to registry-style package requirements such as:

- `blink`
- `blink@0.1`
- `std@0.1`

Path, file, and URL forms are reserved for later expansion.

### Effective dependencies

The script's effective dependency set is:

1. implicit `std`, unless replaced by an explicit `#use std@...;;`
2. every explicit `#use <spec>;;` directive from the prelude
3. when inside a workspace, the inherited workspace environment and local
   packages

The important override rule is:

- script-local external requirements override inherited workspace external
  requirements for that synthetic script package

That override is intentionally allowed to fail. If the resulting dependency
graph is unsatisfiable or otherwise incompatible, Riot should fail through the
normal dependency/build error path rather than rejecting the override up front.

### Synthetic workspace model

The script should be lowered to ordinary Riot workspace/package vocabulary
instead of inventing a second build path.

Outside a workspace, Riot should construct a detached synthetic workspace in
memory.

Inside a workspace, Riot should reuse the prepared workspace environment and
append a synthetic package for the script in memory.

In both cases:

- the caller's directory is not mutated
- no synthetic project root is written to disk
- no local `riot.toml` or `riot.lock` is written beside the script
- the planner, dependency solver, and builder still see an ordinary workspace
  and package graph
- Riot may still reuse the normal global cache or use a `/tmp` cache for
  dependency materialization and built artifacts

### Wrapper generation

The script body remains plain top-level OCaml source.

Riot should stage that body and generate a wrapper equivalent to:

```ocaml
let main ~args:_ =
  let module Script = struct
    (* stripped script body *)
  end in
  Ok ()

let () = Actors.run ~main ~args:Std.Env.args ()
```

The exact generated source may differ, but the semantics should be:

- Riot owns runtime startup
- the script body executes within an actor-backed `main`
- the wrapper returns `Ok ()`
- scripts do not get an implicit `open Std`
- script arguments are accessed only through `Std.Env.args`

### Cache and reuse

Synthetic script workspaces should be cache-first.

The cache identity should be derived from:

- the effective script body after removing the shebang and directive prelude
- the directive prelude itself
- the effective inherited workspace environment when run inside a workspace

This allows Riot to reuse resolved dependencies and normal build artifacts
across repeated runs of the same script without persisting a synthetic script
workspace on disk. The cache backing that reuse may be Riot's normal global
cache or a `/tmp` cache owned by `riot-script`. Phase 1 does not need an
explicit refresh flag yet; it should reuse cached dependency and build state
until a later proposal adds explicit update controls.

### Diagnostics

Phase 1 may surface generated cached paths in compiler and build diagnostics.
That is an acceptable trade-off for the first version. Source-path fidelity can
be improved later without changing the core script model.

## Drawbacks
[drawbacks]: #drawbacks

- `riot run` becomes more magical. It now hides synthetic workspace/package
  creation behind what looks like a simple file run.
- Generated cached paths in diagnostics are less friendly than the original
  script path.
- Reusing `#use` overloads a directive name that already has OCaml toplevel
  expectations, even though this RFD keeps the Riot meaning scoped to script
  mode.
- Script-local dependency overrides can create confusing failure modes when they
  intentionally diverge from a workspace's existing external dependency graph.
- Phase 1 still leaves out important future forms such as local paths, file
  includes, and remote file/source imports.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

The central design choice in this RFD is to keep script execution on Riot's
existing package/build/runtime path instead of creating a special evaluator.
The second important design choice is to keep script-specific logic in a
dedicated `riot-script` package.

That is the best fit for Riot because the desired experience is not "execute
arbitrary OCaml text somehow." The desired experience is "treat this file like a
real Riot program without forcing me to package it by hand."

`riot-script` is the best home for that behavior because the logic is cohesive
and script-shaped: parse the prelude, interpret script dependencies, stage the
synthesized sources, and prepare an ordinary Riot workspace/package to hand off
to the existing build path. Putting that behavior directly in `riot-cli` would
make the CLI own domain logic it should only orchestrate. Putting it directly in
`riot-build` would blur the line between script preparation and the normal
session/build runtime.

Alternatives considered:

### Do nothing

Riot could keep requiring scripts to become real workspaces or packages.

That preserves conceptual purity, but it keeps the ceremony problem exactly
where it is. It also makes future script-import work harder because Riot still
would not have a single-file program surface.

### Add a helper command or generator

Riot could offer something like a `riot init script` helper or a temporary
workspace generator.

That reduces some typing, but it does not remove the structural mismatch. The
user still has to manage generated scaffolding when the real unit of work is the
file itself.

### Build a separate evaluator or interpreter path

Riot could parse the file, interpret directives, and run it through a custom
execution path.

That would likely drift away from Riot's normal dependency, planning, and build
semantics. It would also duplicate behavior that Riot already has for ordinary
packages.

### Use `#dep` or `#require` instead of `#use`

`#dep` is explicit and avoids OCaml baggage.

`#require` is closer to OCaml package-loading expectations.

This RFD chooses `#use` anyway because the desired script experience is terse
and file-first, and because Riot is not changing OCaml globally. It is defining
a Riot-owned script prelude for `riot run <file>.ml`. That said, the overload is
real, and it is part of the cost of this choice.

### Keep scripts outside the actor runtime

Riot could make script mode a simple top-level file runner and leave
`Actors.run` to explicitly packaged programs.

That would make script mode feel smaller, but it would give one-file scripts a
different runtime model than normal Riot binaries. This RFD prefers to keep
script execution actor-backed by default so quick tools and packaged tools share
the same runtime expectations.

## Prior art
[prior-art]: #prior-art

There are a few relevant precedents:

- `uv run` for running Python code with dependency-aware environment setup
- `bun run` for lightweight file- and package-oriented JavaScript workflows
- `deno run` for direct execution of shareable scripts with a strong
  script-first model
- `go run` for treating source files and small programs as directly runnable
  units
- OCaml toplevel `#use` for loading source files into an interactive session
- OCaml `#require` in `findlib`/`topfind`-style environments for package loading
- tools in other ecosystems that let a single file declare dependencies and then
  execute through the normal package/build machinery rather than through a
  separate interpreter-only path

The lesson worth taking from that prior art is not "copy every script syntax."
The useful lesson is that a one-file workflow becomes much more valuable when it
uses the system's normal dependency and execution model instead of creating a
permanently second-class fast path. `uv run`, `deno run`, and `go run` are all
useful reminders that "run this file directly" becomes much more compelling
once the command is treated as a normal part of the toolchain rather than as an
awkward side workflow.

Riot intentionally diverges from normal OCaml expectations here by giving
`#use` a Riot-specific meaning in script mode. The justification is not
compatibility with the OCaml toplevel. The justification is that Riot script
mode is a separate, explicit command surface owned by `riot run`.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- What exact generated file layout should Riot use under the synthetic script
  source model if the synthetic workspace is not persisted on disk?
- Should phase 1 expose a user-visible way to inspect the synthetic workspace
  Riot generated for a script?
- What is the cleanest way to surface "late `#use` directive" errors so the
  prelude boundary is obvious?
- Should a future refresh flag update only external dependencies, or also force
  regeneration of the synthetic workspace layout?
- When future `#use` forms add file and URL imports, should all of them keep the
  same "top-of-file prelude only" rule?

## Future possibilities
[future-possibilities]: #future-possibilities

This RFD intentionally leaves room for a broader Riot script import model.

Natural extensions include:

- using this script model to power a better browser-side Riot playground in
  `sites/play.riot.ml`, where user scripts can run more directly from the
  editor without first being expanded into full workspace scaffolding
- a Riot eval or REPL experience that can load and run package-aware scripts,
  including remote package imports
- a package-aware `use` surface in the Riot language/tooling story so script
  imports are not limited to directive-style preludes forever
- a repository or marketplace of small one-file Riot tools that can be shared
  and executed with minimal ceremony
- `#use ./path/to/pkg;;`
- `#use ./path/to/file.ml;;`
- `#use github.com/owner/pkg;;`
- `#use https://example.com/file.ml;;`
- an explicit update or refresh flag for script dependencies
- rewriting diagnostics back to the original script path
- `riot install <script-file>` or related promotion flows for scripts that
  graduate from disposable tools to reusable commands
- editor and LSP support that understands the synthetic script workspace model

If that future arrives, phase 1 should still stand as the simple base case:
single-file Riot programs should feel real, not second-class, and they should
run through Riot's ordinary semantics rather than through a separate script
subsystem.
