open Std

type file = { relative_path: string; content: string; executable: bool }

let riot_skill_files = [
  {
    relative_path = ".agents/skills/riot/SKILL.md";
    content = {|---
name: riot
description: Build, configure, run, test, benchmark, format, and troubleshoot projects that use the Riot toolchain. Use when Codex needs to work with `riot.toml`, `.riot/config.toml`, `ocaml-toolchain.toml`, `riot init`, `riot add`, `riot build`, `riot run`, `riot test`, `riot bench`, `riot fmt`, or `riot fix` in a Riot workspace or detached Riot package.
---

# Riot

Use this skill when the project uses Riot as the package manager, build system,
and app workflow.

Prefer Riot-native commands and Riot-native project layout. Do not default to
`dune`, `opam`, or raw `ocamlc` workflows unless the project explicitly says it
is doing something outside the normal Riot flow.

If the repository ships its own local contributor guidance, treat that as
higher priority than this generic user skill.

## Quick start

1. Find the nearest `riot.toml`.
2. Decide whether you are in a workspace root or a detached single-package root.
3. Use the narrowest Riot command that answers the task.
4. Read the matching reference file only when needed.

## Workflow map

### Scaffold or inspect a project

- Use `riot init [path]` to create a new workspace.
- Use `riot new packages/<name>` to add another package to an existing
  workspace.
- Read [references/workspaces.md](references/workspaces.md) for the workspace,
  package, and config-file mental model.
- Read [references/modules.md](references/modules.md) when a task touches module
  visibility, public APIs, or dependency boundaries.

### Add or update dependencies

- Use `riot add <package>`.
- Use `riot rm <package>`.
- Use `riot update` to re-resolve the workspace and refresh `riot.lock`.
- Prefer changing Riot manifests over introducing a second dependency workflow.

### Build or typecheck

- Use `riot build` for the full workspace.
- Use `riot build -p <package>` when the task is package-local.
- Use `riot check` when the task is primarily about typing rather than a full
  build.
- Read [references/commands.md](references/commands.md) for command patterns.

### Run a binary

- Use `riot run <binary>` when the workspace has a single obvious binary or the
  target name is unambiguous.
- Use `riot run -p <package> <binary>` when you need package disambiguation.
- Do not guess binary paths under `_build`; let Riot resolve the runnable.

### Run tests or benchmarks

- Use `riot test` for the default test pass.
- Use `riot test -p <package>` to narrow by package.
- Use `riot test -f <query>` to filter suites and cases by substring.
- Use `riot bench` for benchmarks.
- Use `riot bench -p <package> -f <query>` for focused benchmarks.
- Read [references/testing.md](references/testing.md) for test authoring and
  selector details.
- Read [references/benchmarking.md](references/benchmarking.md) for benchmark
  authoring, `--warmup`, `--record`, and `--compare`.

### Troubleshoot

- First check root detection, selector shape, and build-lane assumptions.
- Then check config file placement and target-dir assumptions.
- For module availability errors, read [references/modules.md](references/modules.md)
  before changing manifests or imports.
- Read [references/troubleshooting.md](references/troubleshooting.md).

## Core rules

- Treat Riot as the source of truth for build, run, test, bench, formatting,
  and dependency management.
- Do not assume `_build`; respect `[riot].target_dir` when the workspace sets
  one.
- Default to the `debug` profile unless the user explicitly asks for another
  profile such as `--release`.
- A package-local `riot.toml` with `[package]` can still be a valid build root.
  Riot does not require every task to start at a workspace root.
- Keep project semantics in `riot.toml`.
- Keep repository-local operational behavior in `.riot/config.toml`.
- Keep user-local settings such as registry auth in `~/.riot/config.toml`.

## Command selection rules

- If the user asks whether one package builds, start with `riot build -p <package>`
  before `riot build`.
- If the user asks about one binary, prefer `riot run -p <package> <binary>`
  over guessing an artifact path.
- If the user asks about one failing suite, prefer `riot test -p <package> -f <suite>`
  before a workspace-wide `riot test`.
- If the user asks for machine-readable output, prefer Riot commands with
  `--json` instead of scraping pretty text.
- If the task is formatting or linting, use `riot fmt` and `riot fix` before
  reaching for unrelated external tools.

## Testing and selection rules

- Use `-p` or `--package` for package narrowing when the command supports it.
- For `riot test` and `riot bench`, use `-f` or `--filter` to filter suites and
  cases by substring.
- Without `-p`, a filter shaped like `package:suite` narrows suite discovery.
- `riot test --small`, `riot test --large`, and `riot test --flaky` partition
  the matched case set.
- Repository-shared test policy lives in `.riot/config.toml` under
  `[riot.test]`, not in `riot.toml`.

## References

- Read [references/workspaces.md](references/workspaces.md) for workspace roots,
  detached packages, manifests, and output directories.
- Read [references/modules.md](references/modules.md) for Riot's module graph,
  direct dependency visibility, and target privacy rules.
- Read [references/commands.md](references/commands.md) for practical command
  patterns.
- Read [references/testing.md](references/testing.md) for test authoring and
  selection workflows.
- Read [references/benchmarking.md](references/benchmarking.md) for benchmark
  authoring, selection, recording, and comparison workflows.
- Read [references/troubleshooting.md](references/troubleshooting.md) when Riot
  behavior looks surprising.
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/agents/openai.yaml";
    content = {|interface:
  display_name: "Riot"
  short_description: "Build, run, and test Riot projects"
  default_prompt: "Use $riot to create, build, run, test, or troubleshoot a Riot workspace."
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/commands.md";
    content = {|# Command patterns

Use this reference when you know the task and need the right Riot command
shape.

## Scaffold

Create a new workspace:

```sh
riot init
```

Create a new package inside an existing workspace:

```sh
riot new packages/my-package
```

## Dependencies

Add a runtime dependency to the current package:

```sh
riot add std
```

Add to a specific manifest or scope:

```sh
riot add -p my-package serde-json
riot add --workspace std
riot add --dev propane
riot add --build codegen-tool
```

Remove a dependency from the current package:

```sh
riot rm std
```

Remove from a specific manifest or scope:

```sh
riot rm -p my-package serde-json
riot rm --workspace std
riot rm --dev propane
```

Refresh dependencies:

```sh
riot update
```

## Build and typecheck

Build the whole workspace:

```sh
riot build
```

Build one package:

```sh
riot build -p my-package
```

Build several packages:

```sh
riot build -p app -p worker
```

Compile development artifacts:

```sh
riot build --tests
riot build --examples
riot build --benches
riot build --all
```

Build for another target or all configured targets:

```sh
riot build -x linux
riot build -x aarch64-apple-darwin
riot build --all-targets
```

Use release profile or limit parallelism:

```sh
riot build --release
riot build -j 4
```

Typecheck the workspace or a package:

```sh
riot check
riot check -p my-package
riot check --json
riot check --explain TYP2001
```

When the task is narrow, prefer package-scoped commands before workspace-wide
ones.

## Run binaries

Run a local binary:

```sh
riot run my-binary
```

Disambiguate by package:

```sh
riot run -p my-package my-binary
```

List runnable binaries:

```sh
riot run --list
riot run --list --json
```

Forward args after `--`:

```sh
riot run -p my-package my-binary -- --port 8080
```

Riot can also run remote sources:

```sh
riot run https://github.com/owner/tool
riot run --update https://github.com/owner/tool
```

## Tests and benchmarks

Run all tests:

```sh
riot test
```

Filter by test-case name:

```sh
riot test -f parser
```

Narrow by package or repeat the package flag:

```sh
riot test -p my-package
riot test -p app -p worker
```

Filter within a package:

```sh
riot test -p my-package -f parser
```

Narrow by suite without a package flag:

```sh
riot test -f my-package:parser_tests
```

List matched suites and cases without running them:

```sh
riot test --list
riot test -p my-package -f parser --list
```

Run only small, large, or flaky cases:

```sh
riot test --small
riot test --large
riot test --flaky
```

Run benchmarks:

```sh
riot bench
riot bench -p my-package
riot bench -p my-package -f hashmap
riot bench --list
```

Benchmark run controls:

```sh
riot bench --iterations 100 --warmup 10
riot bench --record
riot bench --compare 5
```

Read [benchmarking.md](benchmarking.md) before recording or comparing benchmark
history.

## Formatting and fixes

Check formatting:

```sh
riot fmt --check
riot fmt --check --json
riot fmt --verify
```

Apply or inspect fixes:

```sh
riot fix --check .
riot fix --apply .
riot fix --list-rules
riot fix --list-diagnostics
```

## Docs, cleanup, and publishing

Generate docs:

```sh
riot doc -p my-package
riot doc --all
riot doc --release
riot doc --output docs
```

Clean build outputs:

```sh
riot clean
riot clean --force
```

Publish packages:

```sh
riot publish --dry-run
riot publish -p my-package
riot publish --workspace
riot publish --skip-check
```

## Machine-readable output

Use `--json` when a machine-readable stream is better than scraping human
output:

```sh
riot build --json
riot test --json
riot bench --json
riot fmt --check --json
riot fix --check --json .
riot doc --json
riot clean --json
riot info --json
```
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/testing.md";
    content = {|# Testing

Use this reference when the task involves writing tests, running `riot test`,
reviewing snapshots, suite selection, or repository-shared test policy.

## Mental model

At the top level, `riot test` is a Riot command that:

1. build the needed packages once
2. discover suite binaries
3. run those suite binaries through their machine-readable contracts
4. aggregate the results

That means the right user workflow is usually:

- narrow by package or suite first
- then narrow by query if needed
- use `--json` when tooling needs structured results

## Writing unit tests

Use `Std.Test` for normal package tests. Keep unit tests focused on one behavior
and return `Ok ()` or `Error msg`.

```ocaml
open Std

let test_adds_numbers = fun _ctx ->
  Test.assert_equal ~expected:4 ~actual:(2 + 2);
  Ok ()

let tests = Test.[
  case "adds numbers" test_adds_numbers;
]

let main ~args =
  Test.Cli.main ~name:"math_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
```

Use `Test.case ~size:Test.Large` for slow or integration-heavy tests. Use
`Test.case ~reliability:Test.(Flaky { retry_attempts = 2 })` only when the
behavior is intentionally flaky and retry policy is part of the contract.

## Writing e2e tests

Use e2e tests for black-box behavior that needs real files, commands, or a
temporary workspace. Mark them large by default:

```ocaml
let test_cli_smoke = fun _ctx ->
  (* Create a temp workspace, run the command, then assert on status, files, and
     key output fragments. *)
  Ok ()

let tests = Test.[
  case ~size:Large "cli smoke" test_cli_smoke;
]
```

Prefer semantic assertions over whole-output snapshots: check exit status,
important output fragments, created files, and follow-up command behavior.

## Snapshot tests

Use snapshots when the output is intentionally large or text-shaped, such as
formatted code, diagnostics, generated files, or serialized JSON.

```ocaml
let test_rendered_output = fun ctx ->
  Test.Snapshot.assert_text ~ctx ~actual:(render ())
```

Use inline snapshots for short expected values that should stay in the test
file:

```ocaml
let test_inline_output = fun ctx ->
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual:"hello\n"
    ~expected:"hello\n"
```

External snapshots write pending `*.expected.new` files when output changes.
Review them before approving:

```sh
riot snapshots review
riot snapshots approve
riot snapshots reject
```

## Test selection

These are different selectors:

- `riot test`
  Runs the default test set.
- `riot test -p <package>`
  Runs tests from one package. Repeat `-p` to select multiple packages.
- `riot test -f <query>`
  Filters suites and cases by substring.
- `riot test -p <package> -f <query>`
  Filters within the selected package or packages.
- `riot test -f <package:suite>`
  Narrows suite discovery when no `-p` filter is present.
- `riot test --list`
  Lists matched suites and cases without running them.
- `riot test --small`
  Runs only cases marked small.
- `riot test --large`
  Runs only cases marked large.
- `riot test --flaky`
  Runs only cases marked flaky.
- `riot test --release`
  Builds and runs tests with the release profile.

Do not confuse package or suite selection with case-name selection.

## Shared test policy

Repository-shared test policy lives in `.riot/config.toml`:

```toml
[riot.test]
small_test_timeout = "500ms"
flaky_max_retries = 3
```

Use that file for repository-local policy. Do not put this policy in
`riot.toml`, and do not assume every user should store it in
`~/.riot/config.toml`.

## Advanced note: suite binaries

If you are debugging the generated suite binary directly, Riot test suites
typically expose subcommands such as:

- `list-tests`
- `run-tests [query]`

Most user tasks should still go through `riot test` first.

## When to use `--json`

Use `--json` when:

- you need to feed results into tooling
- you need reliable machine-readable timing or status output
- scraping human output would be fragile

Examples:

```sh
riot test --json
riot test -p my-package -f parser --json
```
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/benchmarking.md";
    content = {|# Benchmarking

Use this reference when the task involves writing benchmarks, running
`riot bench`, recording benchmark history, or comparing performance over time.

## Writing benchmarks

Use `Std.Bench` for benchmark suites. Keep each benchmark body focused on the
operation being measured, and prepare large fixtures outside the measured
function when possible.

```ocaml
open Std

let bench_push () =
  let values = Collections.Vector.create () in
  Collections.Vector.push values 42

let benchmarks = Bench.[
  case "vector push" bench_push;
  with_config
    ~config:{ iterations = 1_000; warmup = 50 }
    "vector push configured"
    bench_push;
]

let main ~args =
  Bench.Cli.main ~name:"vector_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
```

Use comparison benchmarks when several implementations answer the same question:

```ocaml
let benchmarks = Bench.[
  compare_with_config
    ~config:{ iterations = 100; warmup = 10 }
    "lookup"
    [
      make_case "HashMap" bench_hashmap_lookup;
      make_case "Vector scan" bench_vector_scan;
    ];
]
```

## Running benchmarks

Start narrow and use package filters when possible:

```sh
riot bench
riot bench -p my-package
riot bench -p my-package -f lookup
riot bench --list
```

Without `-p`, a filter shaped like `package:suite` narrows suite discovery:

```sh
riot bench -f my-package:vector_bench
```

## Iterations and warmup

Benchmark cases can set their own `iterations` and `warmup` counts. CLI flags
override those counts for the matched run:

```sh
riot bench -p my-package -f lookup --iterations 200 --warmup 20
```

Use warmup to let caches, JIT-like runtime paths, and one-time setup noise settle
before measurement. Increase iterations when measurements are too noisy or when
comparing small operations.

## Recording and comparing history

Benchmark history is opt-in. Use `--record` when a run should be persisted under
`.riot/bench` for future comparison:

```sh
riot bench -p my-package -f lookup --record
```

Use `--compare <n>` to show up to `n` previous comparable suite runs alongside
the current result:

```sh
riot bench -p my-package -f lookup --compare 5
```

Comparable runs match by package, suite, profile, target, and benchmark case
name. Use stable benchmark names so history can line up across runs.

## JSON and direct suite debugging

Use `--json` for tooling:

```sh
riot bench -p my-package -f lookup --json
```

If you are debugging the generated benchmark binary directly, benchmark suites
typically expose:

- `list-benchmarks`
- `run-benchmarks [query]`

Most user tasks should still go through `riot bench` first so Riot can build the
workspace once and pass the right context to each suite.
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/troubleshooting.md";
    content = {|# Troubleshooting

Use this reference when Riot behavior seems surprising or a command that should
be simple does not behave as expected.

## Root detection problems

Symptoms:

- Riot says it cannot find a workspace
- the wrong package set is being built
- commands behave differently from a parent directory and a package directory

Checks:

- find the nearest `riot.toml`
- decide whether it is a `[workspace]` root or a `[package]` root
- remember that a detached package root is still valid

## Build-path problems

Symptoms:

- a guessed binary path does not exist
- a script assumes `_build/...` but the project uses another directory
- artifacts appear to be missing after a profile or target change

Checks:

- read `[riot].target_dir` from `riot.toml`
- remember that artifacts are lane-scoped by `profile + target`
- prefer `riot run` or `riot build` over path guessing

## Selector problems

Symptoms:

- `riot test` appears to run the wrong set of tests
- a query filters nothing
- package narrowing is confused with case-name filtering

Checks:

- use `-p` or `--package` for package narrowing when supported
- use `-f` or `--filter` for suite and case-name filtering
- without `-p`, use `-f package:suite` to narrow suite discovery
- use `--small`, `--large`, and `--flaky` only for case policy filtering

## Config-placement problems

Symptoms:

- a setting does not seem to affect the repository
- one user sees different behavior from another
- test policy is applied inconsistently

Checks:

- project semantics belong in `riot.toml`
- repository-local operational policy belongs in `.riot/config.toml`
- user-local settings belong in `~/.riot/config.toml`

## Toolchain problems

Symptoms:

- behavior differs across machines
- builds break after a toolchain change
- the wrong OCaml version appears to be in use

Checks:

- read `ocaml-toolchain.toml`
- confirm the user is actually using Riot's toolchain flow
- avoid mixing unrelated external setup steps into a normal Riot workflow
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/workspaces.md";
    content = {|# Workspaces and config

Use this reference when you need to decide what kind of Riot project you are in
and which config file owns a behavior.

## Project roots

Riot supports two common roots:

- a workspace root with a `riot.toml` that contains `[workspace]`
- a detached package root with a `riot.toml` that contains `[package]`

Do not assume every Riot project has a multi-package workspace. A single
package can be a valid build root on its own.

## File roles

### `riot.toml`

This is the project semantics file.

Use it for things like:

- workspace membership
- package metadata
- dependencies
- binaries and libraries
- build profiles
- build-path settings such as `[riot].target_dir`

Dependency declarations also define module visibility. Source files may use the
current package modules and the public modules of direct dependencies only. Read
[modules.md](modules.md) before adding a dependency just to quiet a module
availability error.

### `.riot/config.toml`

This is repository-local operational policy.

Use it for behavior that should apply to everyone working in the repository but
is not part of the package or workspace semantics.

Current examples include test policy:

```toml
[riot.test]
small_test_timeout = "500ms"
flaky_max_retries = 3
```

### `~/.riot/config.toml`

This is user-local Riot configuration.

Use it for machine- or user-specific state such as registry auth or personal
settings. Do not put repository behavior here unless the user explicitly asks
for a local override.

### `ocaml-toolchain.toml`

This pins the OCaml toolchain Riot should use for the project.

If the user reports toolchain drift, build failures after version changes, or
cross-machine mismatches, read this file.

## Build output layout

By default Riot writes build artifacts under `_build`, but that is only the
default.

If the workspace sets `[riot].target_dir`, use that instead.

The build root is lane-scoped:

```text
<target_dir>/<profile>/<target>/...
```

Important consequences:

- do not hardcode `_build`
- do not assume host-default paths if the workspace or user requested a target
- do not guess executable paths when `riot run` can resolve them for you

## Package layout

The common workspace shape is:

```text
riot.toml
ocaml-toolchain.toml
packages/
  my-package/
    riot.toml
    src/
    tests/
```

`riot init` scaffolds a workspace with:

- a root `riot.toml`
- `ocaml-toolchain.toml`
- a starter package under `packages/<name>/`
- a root `Dockerfile`
- a GitHub Actions workflow at `.github/workflows/ci.yml`

## Default runtime binary

If a package has `src/main.ml` and does not declare explicit `[[bin]]` entries,
Riot can treat that as the default runtime binary for the package.

That means `riot run` may work without extra manifest boilerplate when the
package layout is conventional.
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/modules.md";
    content = {|# Module system and dependency boundaries

Use this reference when a build fails because a module is unavailable, when you
are changing package dependencies, or when source files cross package or target
boundaries.

## Mental model

Riot plans builds from a module graph. Each source file may depend on:

- modules from the same package
- public top-level modules from direct dependencies
- regular dependencies plus dev-dependencies when planning test or benchmark
  targets

Riot does not expose transitive dependencies. If package `app` depends on
`std`, and `std` depends on `kernel`, `app` may use `Std` but may not use
`Kernel` unless `app` declares `kernel` as a direct dependency. Prefer the
public API of the direct dependency when that is the intended abstraction.

## Direct dependency examples

Allowed:

```ocaml
let path = Std.Path.v "riot.toml"
```

Not allowed when the package only depends on `std`:

```ocaml
let path = Kernel.Path.v "riot.toml"
```

Allowed for tests when `std` is in `[dependencies]`:

```ocaml
let test_path = Std.Path.v "fixtures/input.txt"
```

Dev-dependencies add extra modules for test and benchmark planning, but they do
not replace regular dependencies. Tests inherit normal package dependencies and
can also use modules from `[dev-dependencies]`.

## Public package modules

Use the top-level module exposed by the dependency package. For example, a
package that depends on `serde-json` should normally reach JSON APIs through
`Serde_json`. It should not import modules from packages that `serde-json`
happens to depend on unless those packages are also direct dependencies.

If a package should expose a helper from one of its dependencies, add that
helper to the package's public module instead of having downstream packages
reach through to the transitive dependency.

## Package-local modules

Modules in the same package can depend on each other directly. Keep reusable
code in package library modules instead of in binary, test, or benchmark
entrypoints.

When a target needs shared code from the package, import it through the package
library's public module. If the shared code only lives in another target
entrypoint, move it into a library or helper module first.

## Fixing module availability errors

When Riot reports that a module is not available:

1. Check whether the source is importing a transitive dependency directly.
2. Prefer using the direct dependency's public module if it exports the API.
3. Add a direct dependency only when the package really owns that dependency.
4. For tests or benchmarks, put test-only packages in `[dev-dependencies]`.
5. Move target-shared code into the package library rather than importing one
   target entrypoint from another.

Do not work around these errors by guessing build paths or manually invoking the
compiler. The module graph is Riot's source of truth for what the package is
allowed to use.
|};
    executable = false;
  };
]

let dev_config_toml = fun ~workspace_name ->
  {|[app]
name = "|}
  ^ workspace_name
  ^ {|"
log_level = "info"

[[log.handler]]
type = "stdout"
format = "full"
|}

let workspace_riot_config_toml = {|
[riot.cache]
keep_generations = 5
max_size = "700 MiB"
|}

let pre_commit_hook = {|
#!/bin/sh
set -eu

riot fmt
riot fix
riot build
riot test --small
|}
