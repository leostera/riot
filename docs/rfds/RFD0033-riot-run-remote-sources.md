# RFD0033 - Remote Source Install and Run

- Feature Name: `riot_remote_sources`
- Start Date: `2026-04-03`
- Status: `presented`

## Summary
[summary]: #summary

This RFD extends `riot install` and `riot run` so they can work directly from
remote source packages, without first creating a local workspace or editing a
manifest. It also extends `riot install` so a non-URL, non-local target can be
resolved from the package registry and installed as a package binary.

Examples:

```sh
riot install suri
riot install owner/tool
riot install https://github.com/owner/tool@tool
riot run owner/tool
riot run https://github.com/owner/tool
riot run https://github.com/owner/tool@tool
riot run https://example.com/releases/tool.tar.gz@main -- --help
```

These commands should:

1. resolve the target:
   - for `riot install`, local binary first, then remote source, then registry
     package
   - for `riot run`, local binary first, then remote source
2. for remote or registry installs, download or materialize sources into
   Riot's global cache
3. build them in an isolated cached project root
4. select the binary:
   - explicit `@<bin>`
   - otherwise `main`
5. for `riot install`, promote the selected binary into `~/.riot/bin`
6. for `riot run`, execute the selected binary without promoting it

GitHub shorthand:

```text
owner/tool
```

is treated as:

```text
https://github.com/owner/tool
```

These commands are intentionally workspace-free.
They do not edit the caller's `riot.toml`, do not write a local `riot.lock`,
and do not treat the current directory as the build root for the downloaded
package.

## Motivation
[motivation]: #motivation

Riot already has a strong source-dependency and materialization story:

- GitHub and source locators are first-class dependency inputs
- materialized sources live in a shared global cache
- the builder can compile packages from those materialized roots
- registry packages already resolve to exact package identities and versions

But today there are still awkward gaps in the user experience:

- if a user wants to install a Riot CLI directly from source, they must first
  clone or otherwise fetch it manually
- if a user wants to quickly try a Riot tool or app from source, they must
  first clone or otherwise fetch it manually
- then they must enter the repo or wire it into a workspace
- then build, install, or run it explicitly

That is too much ceremony for tools that are naturally source-addressed.

The desired experience is closer to:

```sh
riot install owner/tool
riot run owner/tool
```

That should feel like:

- "fetch this runnable Riot package"
- "build it with Riot's normal toolchain and cache model"
- "either install or run its selected binary"

without:

- mutating the caller's workspace
- inventing a second source-materialization story outside `riot-deps`

This feature is especially useful for:

- bootstrap tools and project generators
- CLIs published as Riot packages but discovered via source URLs
- installing a tool from GitHub without cloning it manually
- installing a tool from the registry without first adding it to a workspace
- trying a package from GitHub before deciding to add it as a dependency
- running a source-addressed tool pinned to a branch, ref, or tarball URL

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

### Basic install

If a user points `riot install` at a remote source package:

```sh
riot install owner/tool
```

Riot should:

1. normalize the shorthand to a canonical source locator
2. materialize the package into Riot's shared source cache
3. build it in an isolated Riot-managed project cache
4. install its selected binary into `~/.riot/bin`

If the package defines a binary named `main`, that binary is the default
install target.

`riot install` should also support a package-name fallback:

```sh
riot install suri
```

If `suri` is not a local binary target and is not recognized as a remote source
specifier, Riot should:

1. search the registry for package `suri`
2. resolve it to an exact package version
3. materialize and build that package
4. install its selected binary into `~/.riot/bin`

This should feel like "install this package's CLI from the registry" rather
than "modify my current workspace to depend on it."

### Basic run

If a user points `riot run` at a remote source package:

```sh
riot run owner/tool
```

Riot should reuse the exact same source and build machinery, but execute the
selected binary directly instead of promoting it to `~/.riot/bin`.

### Explicit binary selection

If the package does not expose a `main` binary, the user should specify one:

```sh
riot install https://github.com/owner/repo@tool
riot install owner/tool@tool
riot run https://github.com/owner/repo@tool
riot run owner/tool@tool
```

This `@<bin>` suffix is part of the `riot install` / `riot run` CLI syntax, not
part of the remote locator itself.

Arguments after `--` are forwarded to the selected binary:

```sh
riot install owner/tool -- --help
riot run owner/tool -- --help
riot run https://example.com/releases/tool.tar.gz@main -- --verbose
```

### How targets are resolved

`riot install` should resolve its first argument in this order:

1. local install target
2. remote source target
3. registry package name

`riot run` should resolve its first argument in this order:

1. local run target
2. remote source target

This keeps `riot install foo` ergonomic while avoiding ambiguity for `riot run`
where executing an arbitrary registry package by name would be more surprising.

### What counts as a remote source target?

`riot install` and `riot run` should enter remote-source mode only for
unmistakably remote-looking specs, such as:

- `owner/repo`
- `github.com/owner/repo`
- `https://...`
- `http://...`

Ordinary local behavior should remain unchanged for normal local
package/workspace flows.

### Workspace-free behavior

These commands should not mutate the current repository.

In particular:

- no `riot.toml` edits
- no local `riot.lock` writes
- no use of the caller's `_build` as the primary build root for the remote
  package

Instead, Riot should use:

- the shared global source cache for fetched materialized sources
- a Riot-managed detached global build root for the built artifacts

That makes reruns fast without polluting the caller's workspace.

### Trust model

This feature is explicitly source-addressed and therefore trust-sensitive.

Running:

```sh
riot install https://github.com/owner/repo
riot run https://github.com/owner/repo
```

means "download, build, and install or execute code from that source."

Riot should keep that trust boundary honest:

- the source locator should be shown clearly in progress or error output
- the command should not silently reinterpret remote source locators as package
  registry installs
- the command should stay explicit and source-oriented

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### CLI syntax

The proposed forms are:

```text
riot install <remote-spec>
riot install <remote-spec>@<binary>
riot install <package-name>
riot install <package-name>@<binary>
riot run <remote-spec>
riot run <remote-spec>@<binary>
riot run <remote-spec> -- <args...>
riot run <remote-spec>@<binary> -- <args...>
```

Where `<remote-spec>` may be:

- `owner/repo`
- `github.com/owner/repo`
- `https://github.com/owner/repo`
- `https://github.com/owner/repo/path/to/pkg`
- `https://example.com/releases/tool.tar.gz`

For Git-based locators, Riot may also support source refs in the same style as
source dependencies, for example:

```text
https://github.com/owner/repo#main
```

If both a source ref and binary selection are present, the split should be:

- `#...` belongs to the source locator
- the rightmost `@<bin>` belongs to `riot install` / `riot run`

Example:

```text
https://github.com/owner/repo#main@tool
```

means:

- source locator: `https://github.com/owner/repo#main`
- binary: `tool`

Where `<package-name>` is a registry package identifier such as:

```text
suri
```

resolved through Riot's package registry machinery.

### Install target resolution

`riot install <x>` should resolve `x` in this order:

1. if `x` names a local installable target in the current workspace, use the
   current local install flow
2. else if `x` parses as a remote source spec, use the remote-source flow from
   this RFD
3. else treat `x` as a registry package name and resolve/install it from the
   registry

That means:

```text
riot install riot
```

continues to prefer the local package/binary when invoked inside the Riot repo,
while:

```text
riot install suri
```

can fall through to the registry package flow when no local target matches.

### Registry-backed install flow

Registry-backed install should reuse the same detached build model as remote
source install after resolution succeeds:

1. resolve the package name to an exact package version
2. materialize that exact package into Riot's global source cache
3. build it in the detached global build root
4. install the selected binary into `~/.riot/bin`

This keeps source-addressed and registry-addressed installs on one build path
after target resolution.

### Source materialization

Remote install/run and registry-backed install should reuse Riot's existing
source-materialization
infrastructure instead of creating a separate downloader.

#### Git / GitHub locators

These should reuse the existing source dependency machinery in `riot-deps`,
including:

- GitHub shorthand normalization
- source materialization into Riot's shared cache
- reuse across multiple workspaces on the same machine

#### Tarball URLs

Direct tarball URLs should be materialized into a Riot-managed archive/source
cache with the same end result:

- a normalized package root on disk containing `riot.toml`

The materialization contract should be:

- if the archive already extracts to a package root, use it directly
- if it extracts to a synthetic repo snapshot root, normalize to the actual
  package root when that can be determined safely
- otherwise fail honestly

### Detached build root

Remote install/run should not build into the caller's workspace.

Instead, Riot should create a detached remote build identity derived from the
remote package selection, for example from:

- canonical source locator
- selected ref, if any
- selected package subdir, if any

That identity can then own a Riot-managed build root under:

```text
~/.riot/build/<remote-id>/...
```

Inside that root, Riot should reuse the same general layout as a normal local
`_build` tree, for example:

```text
~/.riot/build/<remote-id>/<profile>/<target>/cache/...
~/.riot/build/<remote-id>/<profile>/<target>/out/...
~/.riot/build/<remote-id>/<profile>/<target>/sandbox/...
```

This gives remote install/run:

- isolated build outputs
- persistent warm-build reuse across invocations
- no pollution of the caller's `_build`

and it avoids introducing a second detached-build abstraction that differs from
the normal Riot build layout.

#### Remote id examples

`<remote-id>` should be derived from the normalized remote source identity, not
from the exact way the user invoked the binary.

Examples:

```text
owner/tool
=> canonical identity: github:https://github.com/owner/tool#default
=> remote-id: 7f/2c/7f2c9e...
=> build root: ~/.riot/build/7f/2c/7f2c9e.../debug/aarch64-apple-darwin/...
```

```text
https://github.com/owner/tool#main
=> canonical identity: github:https://github.com/owner/tool#main
=> remote-id: 3a/91/3a91b4...
=> build root: ~/.riot/build/3a/91/3a91b4.../debug/aarch64-apple-darwin/...
```

```text
https://github.com/owner/tool#3f0e2af
=> canonical identity: github:https://github.com/owner/tool#3f0e2af
=> remote-id: ab/44/ab44de...
=> build root: ~/.riot/build/ab/44/ab44de.../debug/aarch64-apple-darwin/...
```

```text
https://github.com/owner/repo/path/to/pkg#v1.2.0
=> canonical identity: github:https://github.com/owner/repo#v1.2.0:path=/path/to/pkg
=> remote-id: 91/c0/91c0aa...
=> build root: ~/.riot/build/91/c0/91c0aa.../debug/aarch64-apple-darwin/...
```

```text
https://example.com/releases/tool.tar.gz
=> canonical identity: tarball:https://example.com/releases/tool.tar.gz
=> remote-id: c8/17/c81752...
=> build root: ~/.riot/build/c8/17/c81752.../debug/aarch64-apple-darwin/...
```

Two rules should keep this predictable:

- the selected binary should not be part of `<remote-id>`
- mutable refs and pinned refs should produce different ids

That means:

- `riot install owner/tool@main` and
  `riot run owner/tool@tool` should share the same
  detached build root if they build the same source tree
- `#main` and `#3f0e2af` should not share the same detached build root

### Package root discovery

The simplest first model is:

- the resolved remote source must point to a package root containing `riot.toml`
- or to a repo/path that can be normalized to a single package root

If Riot cannot determine the package root unambiguously, the command should
fail and ask the user to provide a more specific locator, typically including a
package subdirectory.

If the remote source resolves to a Riot workspace without a package at the
selected root, Riot should fail rather than guessing which member package to
use.

### Binary selection

Binary selection should be deterministic:

1. if `@<bin>` is present, use that exact binary
2. else if the package exposes a binary named `main`, use `main`
3. else fail with a message listing the available binaries

The first rollout should not guess based on:

- "only one binary exists"
- package name matching
- arbitrary first binary in manifest order

The point is to keep remote execution predictable.

### Interaction with current `riot install` and `riot run`

Current local behavior should remain unchanged unless the first argument is
clearly a remote source spec, or in the case of `riot install`, unless it falls
through to registry package lookup.

That means:

- `riot install riot` remains the normal local behavior
- `riot install suri` can fall through to the registry package behavior
- `riot run my-binary` remains the normal local behavior
- `riot install owner/tool` enters remote-source mode
- `riot run owner/tool` enters remote-source mode

This keeps the command surface compact without stealing ordinary local binary
names.

### Build and install/run flow

The intended execution flow is:

1. parse the target as a remote source spec
2. canonicalize it
3. materialize it into Riot's shared source cache
4. load the remote package/workspace metadata
5. select the package root
6. select the binary:
   - explicit `@bin`
   - otherwise `main`
7. build the remote package through Riot's normal build runtime
8. either:
   - install the resulting binary into `~/.riot/bin`
   - or execute it with any forwarded args

This should reuse existing build/runtime code where possible rather than
introducing a private second build loop.

For registry-backed install, the same flow applies after the initial package
name resolution step.

### Relationship between install and run

`riot install <remote-spec>` should be the first rollout slice.

It needs almost all of the machinery that `riot run <remote-spec>` needs:

- remote source parsing
- source materialization
- detached build roots
- package root discovery
- binary selection
- remote package builds

`riot run <remote-spec>` can then be built on top of the same pipeline, with
the only behavioral difference being:

- `install` promotes the selected binary into `~/.riot/bin`
- `run` executes the selected binary directly without promotion

## Drawbacks
[drawbacks]: #drawbacks

- This expands `riot install` and `riot run` into more polymorphic command
  surfaces.
- Remote execution is trust-sensitive and easy to misuse.
- Tarball handling adds another source-materialization path to maintain.
- Detached global build roots add more Riot-managed state under `~/.riot/build`.

It also introduces a small parsing burden:

- `@` means binary selection here
- `#` may mean source ref

That is compact and ergonomic, but it is also more syntax to teach.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why put this on `riot install` and `riot run` instead of a new command?

Because the mental model is still:

- "install this Riot package binary"
- "run this Riot package binary"

The difference is only where the package comes from:

- local workspace package
- remote source package
- registry package

Keeping this on the existing commands is more discoverable than inventing
separate verbs whose only job is "like install/run, but from a URL."

### Why support install first?

Because `riot install <remote-spec>` gives us the highest-value slice first.

It requires almost all of the same machinery as remote run, but it produces a
durable, immediately useful result:

- a promoted binary under `~/.riot/bin`

That makes it a better first implementation target than remote run alone.

### Why still support run?

Because execute-on-demand is still useful and fits naturally on top of the same
pipeline.

Once remote install exists, remote run is almost the same command except for
the final promotion step.

### Why default only to `main`?

Because it is the least surprising convention.

It lets Riot support the common case ergonomically while avoiding implicit
guesses when the package exposes multiple binaries or uses a nonstandard name.

### Why not mutate the caller's workspace and lockfile?

Because that would make remote install/run much heavier and much more
surprising.

The feature should be disposable and side-effect-light from the user's point of
view. The only persistent state should be Riot-managed cache state.

### Why not support registry package names too?

Because that is a different product shape.

`riot run std` should not suddenly mean "fetch a registry package and execute
it" when it may also mean "run a local binary named std."

By contrast, `riot install std` can reasonably fall through to "install the
registry package binary" once local install targets and remote source parsing
have both failed.

This RFD is intentionally source-addressed:

- explicit URLs
- GitHub shorthand
- direct tarball locators

with one deliberate extension:

- `riot install <package-name>` as a registry install fallback

Registry-driven remote execution for `riot run` can be considered later if the
UX is still worth it.

## Prior art
[prior-art]: #prior-art

- `go run`
  - Go demonstrates the ergonomics of source-addressed execution for quick
    tools and examples.
- `cargo install --git`
  - Cargo shows the usefulness of source-addressed CLI acquisition.
- `cargo install ripgrep`
  - Cargo also shows that registry-backed install by package name is a natural
    complement to source-addressed install.
- `npx`
  - `npx` demonstrates the value of "execute without setting up a project
    first," even though its package and trust model differ from Riot's.

Riot would intentionally combine:

- source-addressed ergonomics
- Riot's existing source materialization cache
- detached build caching
- durable install behavior
- registry-backed install by package name
- explicit binary selection rules

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should tarball support be part of the first rollout, or should the first
  slice only support Git/GitHub locators?
- Should the first implementation ship only `riot install <remote-spec>` and
  defer `riot run <remote-spec>` to a follow-up, even if the RFD defines both?
- Should registry-backed install ship in the same first rollout as remote
  install, or should it follow immediately after?
- Should Riot support an explicit `--bin` flag alongside `@<bin>` for cases
  where shell quoting or URL syntax becomes awkward?
- Should remote run expose a `--refresh` flag to force refetching mutable refs
  such as `main`?
- Should Riot allow remote install/run against a workspace root and infer a
  package when exactly one runnable package exists, or should it stay fully
  explicit?

## Future possibilities
[future-possibilities]: #future-possibilities

- add remote registry-backed run/install flows if the UX is worth the extra
  ambiguity
- add lockfile-like pinning or printed provenance summaries for repeated remote
  runs
- add cache inspection and cleanup surfaces for detached remote build roots
