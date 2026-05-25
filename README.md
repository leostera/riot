<h1 align="center">
  <img alt="riot logo" src="https://github.com/leostera/riot-new/blob/main/assets/logo.png?raw=true" width="300"/>
</h1>

<p align="center">
There are many OCaml stacks, this one is mine.
</p>

<p align="center">
  <a href="#quick-start">Quick Start</a> |
  <a href="#non-goals">Non-goals</a> |
  <a href="#acknowledgments">Acknowledgments</a>
</p>

Riot is an [actor-model][actors], multi-core-ready ecosystem and tooling for
OCaml 5, that I designed from the ground up to help me ship _great software_.

It includes:

* An actor-based multi-core scheduler, so going parallel is as easy as calling `spawn`!

* `std` -- my take on a modern batteries-included standard library with 98% of
what you need to build apps, including supervision trees, application
abstractions, layered config management, logging and tracing, common data
formats (json, toml, etc), lazy iterators and collections, datetime and time
and timers, file system and networking, an arg parser, unicode support,
synchronization primitives, external commands, cryptography, and more.

* `riot` -- a new extensible build system and package manager that becomes the
only tool you need to install and use to do _everything_ in this stack. 

* `ocaml-toolchain.toml` -- a managed toolchain story, including support for
cross-compilation

* a familiar package management experience -- with commands like `riot add
agents@0.1.2` to add, remove, and update your dependencies

* an extensible command system, where packages can provide custom commands to
support your workflows better, and they are all surfaced via riot. Think `riot
minttea:gen component`

* `riot fix` -- a new extensible linter, where packages can provide custom
linting rules and automated fixes

* `riot fmt` -- a strict, zero-knobs formatter, optimized for readability and
small diffs

* A new procedural macro system, allowing packages to provide macros using
`syn`, a lossless concrete syntax tree for OCaml.

* ...and a whole lot of features and libraries I've had to build to get this
thing up and running!

## Quick Start

To get a feel for Riot quickly:

```sh
curl -sSL https://get.riot.ml | sh
riot --help
```

To start an empty workspace run `riot init` and follow the instructions. The
generated workspace includes a starter package, test, Dockerfile, and GitHub
Actions workflow.

Or you can scaffold a starter Riot application by running:

```sh
riot run leostera/create-riot-app
```

## Non-goals

There's a lot of things Riot aims to be, but here's a few that Riot does _not_ try to be:

1. Riot is not a full port of the Erlang VM and it won't support several of its
   use-cases, like:

   * supporting Erlang or Elixir bytecode
   * hot-code reloading in live applications
   * function-call level tracing in live applications
   * ad-hoc distribution

2. Riot is also not trying to preserve compatibiilty with the traditional OCaml
   toolchain or experience. This is my own vision of what writing OCaml could
   look like.

## Acknowledgments

Riot is the continuation of the work I started with
[Caramel](https://github.com/leostera/caramel), an Erlang-backend for the OCaml
compiler.

If you're looking for the old Riot library that used to be published in opam you can find it here: https://github.com/leostera/riot/commit/310a4868edaa4f97304ca5398f23d843b8b26eae
