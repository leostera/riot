<h1 align="center">
  <img alt="riot logo" src="https://github.com/leostera/riot-new/blob/main/assets/logo.png?raw=true" width="300"/>
</h1>

<p align="center">
There are many OCaml stacks, this one is mine.
</p>

<p align="center">
  <a href="#quick-start">Quick Start</a> |
  <a href="#what-riot-is">What Riot Is</a> |
  <a href="#command-surface">Command Surface</a> |
  <a href="#what-you-can-build">What You Can Build</a> |
  <a href="#non-goals">Non-goals</a>
</p>

Riot is a tech-demo of an opinionated OCaml stack for building applications as
one piece: tools, services, databases, packages, experiments, and systems that
need to stay understandable while they grow.

It is centered around one tool, `riot`, plus a modern package registry,
prebuilt OCaml toolchains, a multi-core-ready actor-model runtime, a new
standard library, and first-class support for agentic work.

## Quick Start

Install Riot:

```sh
curl -sSL https://get.riot.ml | sh
riot --help
```

Start a workspace:

```sh
riot init app
cd app
riot build
riot test
```

## What Riot Is

Riot is a stack, but it is also a tool: `riot`. The goal is that this is the
only tool you need inside the stack.

Riot includes:

- a package-aware build system using plain `riot.toml` manifests
- package management through `pkgs.ml`, including add, remove, update, search,
  publish, yank, login, and logout flows
- managed OCaml toolchains declared in `ocaml-toolchain.toml`, including
  prebuilt host and cross-target toolchains
- `riot fmt`, a strict zero-knobs formatter optimized for readability and
  stable diffs
- `riot fix`, an extensible linting and codemod surface with package-provided
  rules and automated fixes
- one test runner for unit tests, property tests, snapshot tests, and replayed
  fuzz cases
- `riot fuzz` for coverage-guided fuzzing campaigns against parsers, codecs,
  protocol handlers, and CLI boundaries
- `riot snapshots` for reviewing generated expected-output changes
- `riot bench` for benchmark runs, recording, and comparison
- `riot doc` for local and release package documentation
- `riot run` for local executables, GitHub repositories, URLs, and one-off
  workflow tools
- package provider commands like `riot sqlx:migrate`, so dependencies can
  extend the local workflow without becoming separate global tools
- `.agents/skills/riot-ml` in generated projects, plus `--json` output across
  important command paths so agents can inspect and repair workflows as data
- an actor-model runtime and standard library surface for building real OCaml
  applications

The main public surfaces are:

- Landing page: <https://riot.ml>
- Installer: <https://get.riot.ml>
- Documentation: <https://docs.riot.ml>
- Package registry: <https://pkgs.ml>
- Source repository: <https://github.com/leostera/riot>
- Agent discovery: <https://riot.ml/llms.txt>

## Command Surface

Riot keeps the normal software-development loop behind one command family:

| Command | Purpose |
| --- | --- |
| `.agents/skills/riot-ml` | local agent instructions for Riot projects |
| `riot build` | build packages and workspaces |
| `riot fmt` | format OCaml with one house style |
| `riot fix` | lint, explain rules, and apply safe fixes |
| `riot test` | run unit, property, snapshot, and replayed fuzz tests |
| `riot fuzz` | run and replay coverage-guided fuzzing campaigns |
| `riot snapshots` | approve or reject pending snapshot candidates |
| `riot bench` | run, record, and compare benchmarks |
| `riot add`, `riot rm`, `riot update` | manage dependencies and `riot.lock` |
| `riot publish`, `riot yank` | publish and manage registry releases |
| `riot search`, `riot login`, `riot logout` | work with `pkgs.ml` |
| `riot init`, `riot new` | create workspaces and packages on the blessed path |
| `riot toolchain` | install, list, and validate OCaml toolchains |
| `riot doc` | generate package documentation |
| `riot run` | run workspace binaries and remote sources |
| `riot <pkg>:<cmd>` | run package-owned workflow commands |

Most automation-oriented commands either support `--json` today or are designed
around structured output, so scripts, editors, CI jobs, and agents do not need
to scrape prose.

## What You Can Build

Riot is intended for:

- multi-core applications using actors, supervision, and message passing
- command line interfaces with clear errors and structured output
- cloud and networked services that do real IO
- developer tooling such as formatters, linters, code generators, release tools,
  migration scripts, and project automation
- TUI applications with packages like `minttea` and `gooey`
- web applications with `suri`, including LiveView-style flows
- database-backed systems with `sqlx`, `postgres`, and `sqlite`
- agentic workflows that can be run, inspected, repaired, and repeated

## Non-goals

Riot is not a full port of the Erlang VM. It does not try to support Erlang or
Elixir bytecode, hot-code reloading in live applications, function-call-level
tracing in live applications, or ad-hoc distribution.

Riot is also not trying to preserve compatibility with the traditional OCaml
toolchain or experience. This is my own vision of what writing OCaml could look
like.

## Acknowledgments

Riot continues work I started with
[Caramel](https://github.com/leostera/caramel), an Erlang backend for the OCaml
compiler.

If you are looking for the old Riot library that used to be published in opam,
you can find it at
<https://github.com/leostera/riot/commit/310a4868edaa4f97304ca5398f23d843b8b26eae>.
