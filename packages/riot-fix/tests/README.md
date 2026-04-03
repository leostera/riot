# riot-fix test suite

This directory contains fixture-backed snapshot tests for `riot fix` plus OCaml
unit tests.

## Structure

- `000*_*.ml`: fixture inputs
- `*.ml.expected`: approved JSON snapshots for the aggregated `riot fix --check`
  result on that fixture
- `fixture_tests.ml`: native `Std.Test.FixtureRunner` suite for the fixture corpus
- `runner_tests.ml`: unit and integration coverage for the library/runtime pieces
- `test_runner.py`: legacy helper kept only for codebase-oriented experiments

## Running fixture snapshots

```bash
riot test riot-fix:fixture_tests
```

## Reviewing snapshot changes

```bash
riot snapshots review packages/riot-fix/tests
riot snapshots approve packages/riot-fix/tests
riot snapshots reject packages/riot-fix/tests
```

## Codebase audit

```bash
python3 packages/riot-fix/tests/test_runner.py codebase
```
