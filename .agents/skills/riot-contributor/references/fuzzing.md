# Repository Fuzzing

Use this reference when changing fuzzing support or using fuzzing inside the Riot monorepo.

## Ownership

- `packages/std` owns `Std.Test.fuzz`, fuzz metadata, and test-binary replay behavior.
- `packages/riot-test` owns shared test discovery, selectors, suite listing, and test-binary interaction.
- `packages/riot-fuzz` owns campaign scheduling, AFL-style coverage/forkserver execution, mutation, corpus/crash layout, locking, replay, and minimization.
- `packages/riot-cli` stays thin: parse `riot fuzz` flags, render events, and delegate to `riot-fuzz`.

Update the relevant package `AGENTS.md` when changing public contracts, artifact layout, CLI behavior, or ownership boundaries.

## Commands

Always use the workspace Riot binary:

```sh
riot run riot -- fuzz --list --json
riot run riot -- fuzz -p syn -f "parser" --duration 10m --json
riot run riot -- fuzz -p typ -f "infer" --runs 1000 --json
riot run riot -- fuzz -p <package> -f "<filter>" --replay <input-path> --json
riot run riot -- fuzz minimize-corpus -p <package> -f "<filter>" --json
```

- Use `-p` and `-f` to select one package, suite, or case while iterating.
- Use `--duration` for campaigns and `--runs` for short deterministic checks.
- Use `--timeout-ms` to keep one generated input from hanging a campaign.
- Use `--concurrency` to run several selected fuzz cases in parallel.
- `riot fuzz` serializes workspace fuzz commands with `_build/fuzz.lock`.
- Fuzz targets run with `RIOT_SCHEDULERS=1` by default; parallelism should happen at campaign level, not inside one fuzz-case process.

## Artifact Policy

Generated fuzz state lives under:

```text
.riot/fuzzing/<package>/<suite>/<case>/
```

Repository policy:

- Keep generated `corpus/`, `redundant/`, and `findings/` directories out of git.
- Keep only small, intentional seeds and minimized crash examples.
- Commit crash examples only after replaying them and confirming they describe a real regression.
- Prefer minimizing or reducing a crash before adding it to the durable artifact set.
- Keep captured crash output only when it materially helps triage or review.

The root `.gitignore` should ignore generated corpus-like state:

```gitignore
.riot/fuzzing/**/corpus/
.riot/fuzzing/**/redundant/
.riot/fuzzing/**/findings/
```

Do not reintroduce large generated corpuses into commits. They make PRs, rebases, and clone/fetch operations expensive while adding little reviewable signal.

## Replay In Tests

- `riot test` replays declared `Test.fuzz` seeds, curated fixture corpuses, local `.riot/fuzzing/**/corpus/` inputs, and saved `.riot/fuzzing/**/crashes/` inputs.
- A bug found by fuzzing should end as a focused regression: parser recovery case, typechecker fixture, inline seed, or tracked crash input.
- When a fuzz crash exposes an ordinary parser/typechecker bug, fix the package behavior and keep the smallest reproducer.
- Do not treat ignored local corpus growth as test coverage. Durable coverage comes from seeds, fixtures, and minimized crash regressions.

## Good Initial Targets

- `syn`: parser recovery, lexer/parser boundary behavior, AST view construction, diagnostic robustness.
- `typ`: parsing plus lowering/type analysis paths that should reject bad input with structured errors, not crashes.
- `std`: byte/text parsers, argument parsing, codecs, and shared test harness protocol.

Avoid fuzzing the whole Riot CLI or whole build graph as a first target. Prefer small test-binary entrypoints that execute one well-defined boundary with one input payload.
