# Riot Fuzzing Workflow

Use this reference when authoring or running fuzz tests in a Riot project.

## When To Fuzz

- Use property tests for broad algebraic invariants with generated structured values.
- Use fuzz tests for byte or text inputs that drive parsers, decoders, protocol handlers, CLI parsers, and other input-sensitive code.
- Keep fuzz targets narrow. A fuzz case should exercise one boundary with one input payload, not a whole application workflow.
- Treat crashes, panics, assertion failures, timeouts, and unexpected non-zero exits as findings to replay and reduce.

## Authoring Cases

Fuzz cases are ordinary `Std.Test` cases declared with `Test.fuzz`:

```ocaml
Test.fuzz
  "parser accepts arbitrary input"
  ~seeds:[""; "let x = 1"; "module M = struct end"]
  ~mutator:Test.Fuzz.Mutator.text
  (fun _ctx input ->
    match Parser.parse input with
    | Ok _
    | Error _ -> Ok ())
```

- Use `~seeds` for small inline examples that should always replay under `riot test`.
- Use `~corpus:Test.Fuzz.Corpus.files [...]` or `dir` for curated fixture files.
- Use `~mutator` to give the fuzzer domain hints such as dictionaries, maximum input length, and splicing behavior.
- Return `Error <message>` for a bug and `Ok ()` for expected accept/reject behavior.

## Commands

- Prefer JSON output for automation: `riot fuzz --json`.
- List cases before guessing selectors: `riot fuzz --list --json`.
- Narrow by package: `riot fuzz -p <package> --json`.
- Filter by substring or `package:suite:case`: `riot fuzz -p <package> -f "<filter>" --json`.
- Run a bounded campaign: `riot fuzz -p <package> -f "<filter>" --duration 10m --json`.
- Bound generated inputs with `--max-len <bytes>`.
- Bound each generated input execution with `--timeout-ms <ms>`.
- Use `--runs <n>` for deterministic short campaigns and `--seed <value>` when reproducing campaign behavior.
- Use `--concurrency <n>` to run multiple selected fuzz cases in parallel.

## Replay And Minimization

- Replay a saved input against exactly one selected fuzz case:

```sh
riot fuzz -p <package> -f "<filter>" --replay .riot/fuzzing/<pkg>/<suite>/<case>/crashes/<file> --json
```

- Minimize a local coverage corpus:

```sh
riot fuzz minimize-corpus -p <package> -f "<filter>" --json
```

- A replay failure should become a small durable regression input, either as an inline seed, a curated fixture, or a tracked crash example.

## Artifacts

Riot stores fuzz state under:

```text
.riot/fuzzing/<package>/<suite>/<case>/
```

- `corpus/` contains generated coverage-increasing inputs. Treat it as local state, not source.
- `crashes/` contains inputs that made the fuzz case fail. Minimize and keep only intentional regression examples.
- `crash-artifacts/` contains captured stdout, stderr, and status for crash triage.
- Declared `~seeds`, curated fixture corpuses, local `corpus/` inputs, and saved `crashes/` replay under `riot test`.

Generated corpuses can grow quickly. Do not commit large generated `corpus/` directories unless a project explicitly documents a different policy.
