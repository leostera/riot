# Validation

Use workspace Riot for repository validation:

```sh
riot run riot -- <command>
```

This avoids accidentally running a globally installed `riot` that does not include the branch changes.

## Build

```sh
riot run riot -- build --all
riot run riot -- build -p <package>
```

Use `build --all` after changes that touch shared packages, CLI plumbing, compiler flags, generated runners, or public APIs used downstream.

## Tests

```sh
riot run riot -- test -p <package> --json
riot run riot -- test -p <package> -f "<filter>" --json
riot run riot -- test --json
```

Prefer package and filter selectors for iteration, then broaden once the focused slice is green. For snapshot failures, inspect the approved `.expected` and pending `.expected.new` before deciding whether the implementation or the snapshot is wrong.

## Format

```sh
riot run riot -- fmt --check
riot run riot -- fmt
```

Use `fmt --check` when you only need status, timing, or syntax-support feedback. Run `fmt` only when in-place formatting is intended.

## Fix

```sh
riot run riot -- fix --check
```

It may produce many warnings while rules are being migrated, but the command itself should run. Rule behavior should be validated with package tests and focused snapshots where possible.

## Benchmarks

```sh
riot run riot -- bench -p <package> --warmup 10 --compare 5 --json
riot run riot -- bench -p <package> -f "<filter>" --warmup 10 --compare 5 --json
```

`--compare` compares current results to previous recorded comparable runs. It is not for comparing unrelated benchmark functions inside a single current run. When benchmarking stages, keep one comparison group per stage:

- parse implementation A vs parse implementation B
- source representation A vs source representation B
- lower implementation A vs lower implementation B
- solve implementation A vs solve implementation B

Compare benchmark functions only when they do the same work.

## Fuzzing

```sh
riot run riot -- fuzz --list --json
riot run riot -- fuzz -p <package> -f "<filter>" --duration 10m --json
riot run riot -- fuzz -p <package> -f "<filter>" --replay <input-path> --json
riot run riot -- fuzz minimize-corpus -p <package> -f "<filter>" --json
```

Keep generated `.riot/fuzzing/**/corpus/` state local and commit only small, intentional seeds or minimized crash examples.

## Bootstrap

Use bootstrap validation only when the first-build path is touched:

```sh
./bootstrap.py
./miniriot
```

The normal workspace Riot binary may not exist or may be irrelevant for these failures. Inspect `_build/bootstrap/sandbox/miniriot` for stage-1 compile failures and `_build/bootstrap/out/<pkg>/graph.dot` for miniriot dependency graph issues.

## Common Package Checks

- `syn`: `riot run riot -- test -p syn --json`; use filters for parser, AST, and dependency fixtures.
- `krasny`: `riot run riot -- test -p krasny --json`; policy changes need focused fixtures or inline snapshots.
- `std`: `riot run riot -- test -p std --json`; shared test, bench, snapshot, IO, and collection changes have high blast radius.
- `riot-fix`: run package tests plus `riot run riot -- fix --check` when rule execution or generated runner code changes.
- Release work: use the `riot-release` skill.
