# RFD0048 - Ephemeral Evaluation Report for `riot eval`, `riot run`, and `riot repl`

- Feature Name: `riot_ephemeral_evaluation_report`
- Start Date: `2026-05-07`
- Status: `draft`

## Summary
[summary]: #summary

This RFD is a report on the `riot eval`, `riot run <file>`, and `riot repl`
prototype work. It records the goals, constraints, implementation shape,
workarounds, and blockers found while trying to execute Riot code
ephemerally inside a workspace or outside any workspace.

- `riot-eval` should become the shared substrate for one-shot eval, script
  execution, and eventually REPL phrase evaluation
- the prototype proved that Riot can build synthetic code through `riot-build`
  and execute it in project context, including `#use` package requests
- the hard parts are native OCaml code loading, global module identity, `.cmi`
  digest consistency, duplicate runtime packages, and long-lived session state
- temporary source packages can be synthetic, but their workspace model should
  be in memory; persistent package artifacts should reuse Riot's normal caches
- a future Riot runtime/compiler should make phrase compilation, code loading,
  versioned modules, and long-lived actors first-class rather than accidental

## Motivation
[motivation]: #motivation

Riot wants a small family of ephemeral execution commands:

```sh
riot eval 'println "hello"'
riot run hello.ml -- arg1 arg2
riot repl
```

They should feel like ordinary Riot code execution, not a separate language or
a special testing harness. In particular:

- `Std` should be available by default
- `#use pkg;;` should fetch, build, and load a package for the current script
  or session
- package code should be resolved using the current workspace when there is one
- the same commands should work outside a workspace by resolving packages
  through `pkgs.ml`
- build progress and package-manager progress should flow through the existing
  Riot build UI renderer
- one-shot eval should run the code block and exit
- script files should be executable without creating a package on disk
- a REPL session should remain alive after loading packages and spawning actors
- actors spawned from the REPL should live until they terminate naturally or are
  explicitly terminated

The first prototype was useful precisely because it pushed against several
current system boundaries. It showed that `riot-build` can be embedded and used
as the source of truth for package closures and artifacts. It also showed that
native OCaml's compiler and runtime model fights a dynamic, Elixir-like REPL in
several places.

This report exists so the next implementation pass does not rediscover those
failures by accident. It is also useful input for Riot's longer-term runtime
and compiler work: if Riot is writing a new runtime for OCaml-shaped code, the
runtime should make these workflows easy by construction.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The desired mental model is one evaluation substrate with three front doors.

`riot eval <string>` runs one expression or structure block, in Riot context,
then exits. It is intentionally simple. It builds `std` by default, accepts
explicit package selections, and can parse `#use pkg;;` prelude directives. It
does not preserve state after the command exits.

`riot run <file>` runs a script file through the same substrate. The script file
can contain a prelude with `#use` directives, receives forwarded arguments, and
executes as Riot code without requiring the user to create a package. RFD0042
describes this user-facing script-file direction.

`riot repl` keeps an evaluation session alive. It should support loading
packages, evaluating later phrases against earlier phrase bindings, and keeping
spawned actors alive while the REPL remains alive. The REPL does not need a
sophisticated code server. It does need a stable runtime process and a stable
view of loaded package code.

### What the prototype tried

The prototype moved the evaluator toward this shape:

1. Create a `riot-eval` package that owns ephemeral execution.
2. Let `riot-cli` commands call into `riot-eval`.
3. Let `riot-run` delegate script files into `riot-eval`.
4. Let `riot-repl` use `riot-eval` for stateful phrase evaluation.
5. Use `riot-build` and `riot-planner` directly instead of rebuilding package
   planning logic inside the evaluator.
6. Generate synthetic runner packages for one-shot eval and scripts.
7. Use detached, generated workspaces outside a user workspace.
8. Resolve detached packages through `pkgs.ml`.
9. Build requested package closures through normal Riot build requests.
10. Use package artifacts from build results to find archives and interface
    directories.

This path is still the right broad direction. The important correction is that
the synthetic workspace should be an in-memory model. Generated source files
may still need scratch paths because the current compiler consumes files, but
Riot should not create a persistent fake workspace such as `.riot/eval/detached`
per invocation. Persistent storage should be package registry materialization
and build artifacts in the normal Riot cache.

### What worked

Several pieces were validated by the prototype:

- one-shot eval can build a synthetic package and run it
- script files can be lowered into synthetic runner packages
- `#use http;;` can resolve and build a package closure outside a workspace
- `riot run http_parse.ml -- <request>` can parse HTTP using `http`
- package-manager and build progress can be surfaced through event callbacks
- REPL phrases can compile into native shared objects and be loaded
- later REPL phrases can see earlier phrase modules by generating opens
- package roots can be exposed through local aliases when compiled package
  roots are versioned/hash-qualified

The prototype also proved that the evaluator should not duplicate build logic.
When it tried to guess archive names or scan target directories, it drifted
from the build graph. Using `riot-build` results and package artifact exports
was the correct boundary.

### What broke

The prototype hit several hard failures:

- phrases failed with `.cmi` digest mismatches when `Std.cmi` and `Http.cmi`
  came from different builds
- loading a package compiled against one `Std__Time__Duration.cmi` while the
  phrase compiler saw another produced "inconsistent assumptions"
- native `Dynlink` could not load two modules with the same compilation-unit
  name, which forced package-root versioning
- loading another copy of runtime packages such as `kernel` and `std` into the
  same process disturbed runtime assumptions
- the REPL prompt initially used actor-backed `Std.IO.Stdin`, which required a
  current runtime process; after package loading, that current process context
  was no longer reliable
- top-level phrase bindings were not naturally available to later phrases
  because each phrase was compiled as a module
- the same package loaded a second time cannot replace the old code under the
  same module name
- actors spawned before a package reload would continue running old code even
  if a later package version was loaded under a new name

These are not just implementation bugs. They reveal the mismatch between a
native OCaml process and a long-lived dynamic Riot session.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Current prototype shape

The prototype introduced a package boundary roughly like this:

- `riot-cli` owns command parsing and UI rendering
- `riot-repl` owns prompt behavior, line input, and REPL directives
- `riot-run` delegates script files to `riot-eval`
- `riot-eval` owns prelude parsing, synthetic package generation, detached
  workspace construction, one-shot eval execution, script execution, stateful
  REPL phrase evaluation, package loading, and compiler/linker invocation
- `riot-build` owns package closure building
- `riot-planner` owns package/module/action planning
- `riot-store` owns package artifacts and action artifacts
- `riot-deps` and `pkgs-ml` own detached package resolution and materialization

The one-shot path is:

```text
riot eval <source>
  -> parse #use prelude
  -> build synthetic eval-runner package
  -> link executable through riot-build
  -> run executable and exit
```

The script path is:

```text
riot run file.ml -- args
  -> parse script prelude
  -> generate script module plus runner
  -> build synthetic runner package
  -> run executable with forwarded args
```

The REPL path is:

```text
riot repl
  -> create stateful eval session
  -> build base runtime package view
  -> read phrase
  -> handle #use by building/loading package closure
  -> compile phrase to a unique module
  -> link phrase to .cmxs
  -> Dynlink.loadfile
  -> remember phrase module for later phrases
```

### Artifact handling

The evaluator must treat `riot-build` output as authoritative. It should not
derive package artifacts by scanning `_build/out`, and it should not guess names
such as `Http.cmxa` from the package name.

The relevant artifact contract is:

- `riot-build` returns a `Build_result.t`
- each package result exposes package artifacts
- package artifacts contain export entries
- archive loading should find `.cmxa` exports from those entries
- include paths for phrase compilation should come from the same package
  artifacts

This matters because package-root relocation changes archive names. A source
package named `http` might compile to a root such as:

```text
Http_v0_0_32_b53c5d218a
```

The public package root remains `Http`, but the compiled unit root is versioned.
That distinction must be reflected in both the compile-time `.cmi` world and
the runtime-loaded archive world.

### Interface digest consistency

OCaml `.cmi` files carry interface digests. If a module was compiled against
one digest and a later compile sees another digest for the same interface name,
the compiler rejects the program.

The prototype saw this directly:

```text
Error: The files Std.cmi and Http.cmi make inconsistent assumptions
       over interface Std__Time__Duration
```

This happened when the phrase compiler staged `Std.cmi` from one build and
`Http.cmi` from another. It also happened when host binary artifacts, detached
debug artifacts, old global-cache artifacts, and package artifacts were mixed.

The workaround was:

- prefer artifacts from the current build result
- keep host runtime interfaces separate from loaded package interfaces
- stage a fresh per-phrase `.cmi` directory
- avoid letting stale `.cmi` files win by basename collision
- expose loaded package public roots through local aliases to compiled roots

This is fragile but informative. A production evaluator needs an explicit
interface environment object. It should know exactly which package artifact
provided each interface and should reject mixed worlds before invoking the
compiler.

### Native dynlink and global module names

Native OCaml `Dynlink` can load new native shared objects, but it does not
provide a normal dynamic-language module loader:

- it cannot unload a compilation unit
- it cannot replace a compilation unit with the same name
- loaded code remains in the process for the life of the process
- module identity is tied to compilation-unit names

This forces versioned module roots if a REPL wants to load a new copy of a
package:

```text
Std__*       -> Std_v003_hash__*
Http__*      -> Http_v003_hash__*
My_pkg__*    -> My_pkg_vdev_hash__*
```

The user-facing public root can still be `Http`, but internally the phrase
source must generate:

```ocaml
module Http = Http_v0_0_32_b53c5d218a;;
```

This lets two package versions coexist. It does not provide live upgrade. Any
old values, closures, modules, or actors still refer to the old code. A REPL can
warn when a package root is rebound to a newer compiled root, but it cannot
magically update old actors.

### Runtime packages are special

The largest warning from the prototype is that `kernel` and `std` are not
ordinary user libraries inside this process.

They contain process-wide and domain-local runtime state:

- runtime scheduler state
- actor process state
- domain-local storage keys
- actor-backed IO services
- top-level initialization
- global effects and exception machinery

Loading another compiled copy of runtime packages into the same native process
can break assumptions that are normally safe for a statically linked program.
The REPL observed this when package loading was followed by a prompt read
through `Std.IO.Stdin`. That path required `Runtime.self ()` to send an actor
message to the stdin service. After loading the package closure, the thread's
current runtime-process context was no longer reliable, so the prompt failed
with:

```text
Failure("No process running")
```

The immediate workaround was to keep `tty` for terminal setup/size/restore but
read REPL prompt input through direct `Kernel.IO.Stdin` chunk reads instead of
actor-backed `Std.IO.Stdin`.

That workaround is acceptable for a prototype prompt. It is not the deeper
solution. The deeper rule is:

- the evaluator process should have one runtime
- loaded user packages should link against that runtime
- `kernel` and `std` should be treated as host-provided runtime packages, not
  dynamically loaded package closure members
- phrase compilation should see runtime interfaces matching the host runtime
- package closures should not dynlink another runtime copy into the REPL process

### Phrase evaluation

OCaml's toplevel has phrase-specific machinery. The prototype deliberately did
not depend on the shipped OCaml toplevel. Instead, it compiled each phrase as a
fresh module:

```text
eval_<session>_phrase_1.ml -> Eval_<session>_phrase_1.cmxs
eval_<session>_phrase_2.ml -> Eval_<session>_phrase_2.cmxs
```

This works, but it means ordinary top-level bindings are module members:

```ocaml
let hello () = println "hello";;
```

becomes effectively:

```ocaml
Eval_session_phrase_2.hello
```

Later phrases only see `hello` if the evaluator opens previous phrase modules
or generates explicit aliases. That is manageable, but it means the REPL is not
just "compile this source." It needs a session environment:

- loaded package aliases
- persistent opens
- previous phrase modules
- argument overrides for script-like sessions
- interface include roots
- loaded library archives
- source paths and unique module names

A future compiler surface should expose phrase parsing and phrase lowering as
first-class operations. For Riot's own frontend, `syn` should be able to parse
phrase-level input without pretending it is a full file. For the current OCaml
backend, `riot-eval` still needs to lower phrases into valid compilation units.

### Detached execution

Outside a workspace, the evaluator has to create a package universe. The
prototype used generated package manifests and scratch source roots, then let
`riot-deps` resolve dependencies through `pkgs.ml`.

The important constraints are:

- detached execution must not write a persistent fake workspace per invocation
- multiple concurrent evals must not share generated source paths
- generated source roots can be scratch directories
- package artifacts should go through the normal global Riot cache
- package registry materialization should reuse normal package caches
- package resolution should not refetch the index or packages unnecessarily

The missing piece is an in-memory workspace API strong enough for build
planning. Riot should be able to construct:

```ocaml
Workspace.t {
  root = scratch_root;
  target_dir_root = global_eval_target_root;
  packages = generated_and_resolved_manifests;
}
```

without needing a durable `.riot/eval/detached` source workspace. Source files
for generated eval packages may still be written to a unique scratch root
because the current compiler consumes file paths, but the workspace model itself
should not be a persistent user-visible project.

### Caching and avoiding repeated work

The prototype still does too much repeated work:

- detached commands update or check the `pkgs.ml` index frequently
- synthetic runner packages are rebuilt unless their generated inputs hash the
  same way
- REPL package loads build closures on demand but do not have a strong
  long-lived build session cache
- phrase compilation has to restage interfaces

The intended cache model should be:

- registry index cache is shared and freshness-controlled
- materialized packages are reused by package identity
- package artifacts are reused through `riot-store`
- synthetic runner package names are content-addressed
- generated source roots are unique per command/session
- REPL sessions keep loaded package aliases and include roots in memory
- build sessions expose package artifacts directly to `riot-eval`
- phrase compilation uses a session interface environment rather than
  reconstructing it from directories each time

### Build UI and package-manager events

The prototype confirmed that `riot-eval` should not render progress itself.
It should accept an event callback and forward:

- package-manager events from detached dependency resolution
- build events from `riot-build`
- load events for packages and phrases, if added later

`riot-cli` should decide how these events render in human, quiet, or JSON modes.
This keeps `riot-eval` useful as a library for `riot-run`, `riot-repl`, tests,
and future tools.

### What a new Riot runtime should make easy

If Riot is writing a new runtime for OCaml-shaped code, this experiment gives a
concrete list of runtime features that should not be accidental:

- one stable runtime identity per process
- explicit code identity separate from public module names
- safe loading of new code versions without corrupting runtime singletons
- package/module namespaces that can include version and content identity
- public aliases that can rebind in a session without replacing old code
- actor metadata that records the code version used at spawn time
- warnings when a session loads a new package version while old actors still
  run old code
- first-class REPL sessions with explicit environment objects
- direct non-actor console IO for tooling paths that must survive scheduler
  context changes
- actor-aware IO for user code that runs inside a runtime process
- explicit lifecycle for loaded code, even if unloading is unsupported
- clear boundaries between host runtime packages and user packages

For the compiler/frontend side, Riot should make these easy:

- parse a phrase without requiring a whole module file
- lower a phrase into a compilation unit with stable generated names
- expose the exact interface environment used for compilation
- surface package artifact metadata directly to the evaluator
- compile against public-root aliases while linking compiled-root archives
- detect mixed interface worlds before invoking the backend compiler
- support in-memory package/workspace models even when the backend needs
  scratch source files

## Drawbacks
[drawbacks]: #drawbacks

The report points toward a larger evaluator architecture than a tiny wrapper
around `ocamlopt` and `Dynlink`.

The main costs are:

- `riot-eval` becomes a real subsystem, not just a command helper
- the build system needs stable library APIs for in-memory workspaces and
  package artifact environments
- the planner needs package-root relocation to be robust for libraries,
  binaries, tests, and synthetic packages
- the runtime needs a clear "host runtime versus loaded package" boundary
- REPL sessions need state and policy around loaded versions
- diagnostics need to explain dynamic loading constraints without exposing too
  much backend trivia

There is also a semantic cost. If multiple package versions coexist in one REPL
session, users can observe old and new code at the same time. That is useful for
experimentation, but it can be confusing:

```text
# #use my_pkg;;
# let pid = spawn_old_actor ();;
# #use my_pkg;;  # after editing the package
# spawn_new_actor ();;
```

The old actor still runs old code. Riot can warn about this, but not silently
change it without a live-upgrade system. Live upgrade is out of scope for this
work.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Do nothing

Riot can keep one-shot command execution limited to real packages and binaries.
That avoids the complexity in this report, but leaves several user-facing
workflows awkward:

- quick project-context experiments require creating files or packages
- script files from RFD0042 remain unimplemented or special-cased
- a Riot REPL cannot load packages and keep actors alive
- detached usage outside a workspace remains weak
- users cannot write `riot eval 'println "hello"'` as a simple tool

Doing nothing also means the runtime/compiler lessons stay implicit. Future
attempts would likely rediscover the same `.cmi`, dynlink, and runtime-context
failures.

### Keep `riot eval`, `riot run`, and `riot repl` separate

Each command could own its own build and loading behavior. That looks simpler
locally, but it duplicates the hardest parts:

- prelude parsing
- detached package resolution
- synthetic package generation
- build event forwarding
- package artifact lookup
- interface environment construction
- package-root aliasing

The prototype showed that duplication here quickly becomes wrong. The shared
substrate should be `riot-eval`.

### Use the OCaml toplevel implementation

OCaml ships toplevel machinery, and it is useful prior art. The prototype
initially explored it, but Riot's goals are not the same as `ocaml`'s stock
toplevel:

- Riot wants project/package context through `riot-build`
- Riot wants `Std.Runtime.main` behavior so actors work naturally
- Riot wants detached package resolution through `pkgs.ml`
- Riot wants script-file execution and one-shot eval through the same substrate
- Riot wants package artifact caching and build UI events
- Riot is moving toward its own frontend and runtime

Depending on OCaml's toplevel internals would make Riot's evaluator inherit a
large surface that is not designed around Riot packages, actors, or future
runtime work. The better use of the OCaml toplevel is as a source of concepts:
phrase parsing, phrase typing, environment extension, and directive handling.

### Interpret instead of compiling

An interpreter would avoid native dynlink issues and could make phrase state
easy. It would not run the same code as the compiled package, and it would need
an interpreter for the language and runtime semantics Riot actually supports.

That is too much divergence for the near-term goal. `riot eval` and `riot run`
should execute code through the same backend and package artifacts users rely
on elsewhere.

### Compile a new executable for every phrase

The evaluator could avoid dynlink by compiling and running a fresh executable
for every phrase. That would make process state simple and avoid native module
replacement. It would fail the REPL goal:

- actors would not stay alive between phrases
- values would not persist naturally
- package loading would repeat work
- the UX would feel like repeated script execution, not a REPL

This is acceptable for `riot eval` and `riot run`; it is not acceptable for the
long-lived REPL.

### Run the REPL evaluation in a child process

A child process could isolate runtime copies and allow restart-based cleanup.
The parent prompt could stay stable, and the child could be restarted when the
loaded world becomes inconsistent.

This is a plausible fallback, but it changes the actor story. Actors spawned in
the child live only in that child. Parent/child communication becomes a protocol
problem, and debugging moves across a process boundary. It may still be useful
for crash isolation, but it should not be the first model for an actor-native
Riot REPL.

### Proposed direction

The proposed direction is:

1. Keep `riot-eval` as the shared substrate.
2. Use `riot-build` as the only package build/link source of truth.
3. Use in-memory detached workspace models with scratch source roots.
4. Treat `kernel` and `std` as host runtime packages in the REPL process.
5. Do not dynlink duplicate runtime packages into a REPL session.
6. Namespace user package compiled roots by package identity and content.
7. Expose public package roots through session-local aliases.
8. Keep old package versions loaded and warn when a public root is rebound.
9. Give REPL sessions explicit environment state.
10. Make phrase parsing/lowering a first-class `syn`/compiler capability.

## Prior art
[prior-art]: #prior-art

### OCaml toplevel

The OCaml toplevel has phrase parsing, typing, environment extension, and
directive handling. It is the clearest prior art for the shape of a phrase
evaluator. Its internals are not a direct fit for Riot because Riot needs
package-aware builds, actor runtime behavior, detached package resolution, and
future runtime/compiler ownership.

### Native OCaml dynlink

Native `Dynlink` is enough to load compiled plugins, but not enough to provide
dynamic-language module replacement. It is additive, process-global, and tied
to compilation-unit names. The prototype's package-root relocation is a direct
response to this limitation.

### Elixir shell

The desired REPL feel is closer to an actor runtime shell than to a pure
functional evaluator. Spawned processes should keep running while the shell
continues. Reloading code should not pretend to rewrite already-running
processes. If Riot supports loading a new package version, it should make the
old-version actor behavior visible.

### Deno and Bun eval/script workflows

The desired `riot eval` and `riot run <file>` workflows are closer to modern
tooling eval and script execution than to a package-only build system. A user
should be able to run a small expression or file in project context without
scaffolding a package.

### RFD0042 script files

RFD0042 describes script-file behavior for `riot run`. This report confirms
that `riot-eval` is the right substrate for that work: scripts, one-shot eval,
and REPL phrases all need the same prelude parsing, package selection, detached
resolution, synthetic source generation, and build artifact handling.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should the REPL ever load a new `std` or `kernel`, or should those always be
  pinned to the host runtime?
- How should a REPL session display that `Http` has been rebound from one
  compiled root to another?
- How much of phrase parsing belongs in `syn` versus `riot-eval`?
- Should phrase typing happen before backend compilation, or should the first
  implementation continue relying on backend compiler diagnostics?
- What is the right public representation of a session interface environment?
- Can Riot avoid restaging `.cmi` files for each phrase by using a stable
  compiler environment directory per session?
- Should detached package resolution refresh `pkgs.ml` automatically, or should
  it use a freshness policy shared with package install/add commands?
- How should build UI rendering behave inside an interactive REPL without
  corrupting the prompt?
- Should `riot repl` support a reset command that starts a new child session
  while keeping the parent prompt alive?
- What is the minimum test matrix for native dynlink behavior across macOS,
  Linux, and future toolchains?

## Future possibilities
[future-possibilities]: #future-possibilities

- Add a durable `riot-eval` session API with explicit package aliases, loaded
  libraries, phrase modules, interface roots, and actor metadata.
- Add `syn` phrase parsing so REPL input does not depend on file-level parsing
  assumptions.
- Add a package artifact environment API in `riot-build` or `riot-store` that
  returns the exact compile/link world for a package closure.
- Add a "runtime-provided package" concept so REPL loads can exclude host
  runtime packages while still compiling against their interfaces.
- Add version-change warnings when a REPL loads a new package root while old
  actors or phrase values still refer to an older compiled root.
- Add a child-process fallback mode for isolation, crash recovery, or runtime
  reload experiments.
- Add JSON event streams for eval/repl package loading so editors can show
  progress and loaded package state.
- Add a direct script cache keyed by source hash, dependency prelude, package
  versions, profile, target, and toolchain identity.
- Add runtime support for code identity on actors so inspection can show which
  package version a process is running.
- Use this report as input to Riot's new runtime design so dynamic code loading,
  phrase evaluation, package namespaces, and long-lived actors are explicit
  requirements rather than afterthoughts.
