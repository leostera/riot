---
name: riot-ml
description: Use when helping users build, test, benchmark, run, and maintain OCaml projects with riot. This skill routes to the best Riot workflow, prefers machine-readable `--json` output, and applies package/dependency conventions.
---

# Riot ML

## Use this skill
Use this skill when the user wants to:
- create/build/test/bench/run a project with `riot`
- understand how to add dependencies
- use community package documentation
- troubleshoot common Riot project workflows

## User workflow
1. Confirm the project type and intent (`build`, `test`, `bench`, `run`, or maintenance).
2. Use the default commands first:
   - `riot build`
   - `riot test`
   - `riot bench`
   - `riot run`
3. Prefer machine-readable flow whenever available by adding `--json`.
4. If dependency or package behavior is unclear, check the official docs:
   - `docs.riot.ml` for command and ecosystem guidance
   - `docs.pkg.ml/p/<pkg>/<version>/` for package docs

## Practical Riot style guidance for users
1. Keep APIs safe by default:
   - Use `Result`/`Option` for fallible operations.
   - Prefer explicit `match` error handling over ad hoc exceptions.
2. Use the conversion naming pattern:
   - Prefer `from_string`/`to_string` instead of `of_string` when both forms exist.
3. Keep unsafe APIs explicit:
   - Prefix externals with `unsafe_`.
   - Use `_unchecked` only for explicit exceptional paths.
4. Use structured errors for control flow:
   - Avoid adding new custom exceptions for normal flow.
   - Prefer typed `Result` payloads and explicit variants.
5. Use `Std.panic` only for intentional hard-fail boundaries.
6. Equality rules:
   - `=` is structural equality.
   - `!=` is structural disequality (operator `< >` is not available in this ecosystem).
   - Use `Std.Ptr.equal` for explicit pointer checks.
7. Prefer `Std` APIs over custom one-offs in project code:
   - `Std.Path`, `Std.IO`, `Std.Data.Json` and related utilities.
8. Use `riot` commands with machine-readable output:
   - Add `--json` for `build`, `test`, `bench`, `run`, `check`, `fmt`, `fix`, and `info`.
9. Narrow runs and checks by package early:
   - `-p` / `--package` selectors for iterative work.
10. Read local signatures from `~/.riot/registry` before adding wrapper code:
   - quick `.mli` discovery helps avoid API confusion.
11. Prefer direct iterator module use in code:
    - `open Std.Iter`, then call `Iterator.map`, `Iterator.to_list`, etc.

## Testing
For test authoring, selectors, fixture runners, and snapshot workflow, read:

- [Riot testing workflow](references/testing.md)

## Fuzzing
For fuzz case authoring, campaign runs, replay, and corpus handling, read:

- [Riot fuzzing workflow](references/fuzzing.md)

## Benchmarking
For benchmark grouping, regression checks, and comparison workflow, read:

- [Riot benchmarking workflow](references/benchmarking.md)

## Common Riot commands
- `riot build --json` : compile packages and dependencies.
- `riot test --json` : run project tests.
- `riot fuzz --list --json` : list fuzz cases.
- `riot fuzz -p <package> -f <filter> --duration 10m --json` : run a focused fuzz campaign.
- `riot fuzz minimize-corpus -p <package> -f <filter> --json` : delete coverage-redundant local corpus inputs.
- `riot bench --json` : run benchmarks.
- `riot run --json` : run a target package/binary.
- `riot upgrade` : upgrade the Riot binary.
- `riot fmt [--check] --json` : check or apply formatting.
- `riot fix [--check] --json` : run linting/fixes.
- `riot check --json` : perform consistency checks.
- `riot info --json` : inspect workspace/package metadata.

## Ecosystem anchors
- `riot.ml`: website and high-level entrypoint.
- `docs.riot.ml`: documentation portal.
- `pkgs.ml`: package registry used by dependency workflows.

## Finding installed library signatures (`.mli`) in `~/.riot/registry`
- `~/.riot/registry` stores installed package artifacts.
- To inspect local package signatures:
  - `find ~/.riot/registry -name '*.mli' | head`
  - narrow by package:  
    `find ~/.riot/registry -path '*<pkg>*' -name '*.mli'`
  - inspect package docs quickly:  
    `find ~/.riot/registry -path '*<pkg>*' | head`
- Useful for reading APIs before opening external docs.
