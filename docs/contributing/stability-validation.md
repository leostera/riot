# Local stability validation

The first v0.1 validation lane is a focused, existing-cache smoke check. It does
not certify a clean bootstrap, all packages, or all platforms. LSP is not required
for the initial daily-driver milestone; use compiler diagnostics through build.

## Run

From the repo root, choose a specific executable rather than relying on PATH:

```sh
python3 scripts/validate-stability.py \
  --driver ./_build/debug/aarch64-apple-darwin/out/riot-cli/riot \
  --timeout 30
```

Adjust the target path for your host. The script requires Python 3 and a Unix host.
It builds `riot-cli`, then requires these suites to report nonempty passing results:

- `kernel:kernel_new_foundation_tests`
- `std:std_test_cli_tests`
- `riot-build:build_core_tests`
- `riot-cli:test_selection_tests`
- `riot-planner:action_hash_tests`
- `riot-planner:package_planning_tests`

For targeted work, repeated `--suite package:suite` arguments replace the default
suite list. `--timeout` is a per-command external deadline, not a per-case timeout.
The default deadline is 120 seconds; choose a realistic explicit value for cold
compilation rather than treating the 30-second example as a universal budget.

Each invocation creates a unique directory under `_build/stability/`, or under
`--log-dir`. It retains the selected executable as `riot-driver`, so rebuilding
the original output path cannot silently change the driver between checks.

## Evidence and failures

The printed `report.json` path contains source SHA, working-tree status, driver
path and digest, host, toolchain configuration, command arguments, exits, durations,
timeouts, and required-suite counts. Every command has separate stdout/stderr logs.
Do not infer source identity solely from `riot --version`: installed release
metadata may affect that output.

The first failure stops the run with a nonzero exit. Successful process exit is
not enough for a required suite: missing or duplicate terminal summaries, incorrect
suite identity, zero tests, failed/skipped cases, and inconsistent counts all fail.
The supervisor terminates the command's process group on timeout or interruption,
escalating to SIGKILL if necessary. This is process supervision, not a sandbox;
children deliberately starting a separate session are outside that group.

No cache cleanup, snapshot approval, install, publish, or automatic retry occurs.
Retain a failed run's directory before running cleanup commands. If a later retry
passes, report both observations rather than erasing the first failure.

The script snapshots the driver but builds and tests the current working tree.
It is not a clean-checkout self-hosting proof. That remains separate work under
[RFD0045](../rfds/RFD0045-repeatable-stability-validation.md).

## Reproduce cold builds without clearing real caches

Use Python 3.12 or newer for the committed-export lane:

```sh
python3 scripts/validate-cold-build.py \
  --driver ./_build/debug/aarch64-apple-darwin/out/riot-cli/riot \
  --toolchain "$HOME/.riot/toolchains/5.5.0-riot.2/aarch64-apple-darwin" \
  --toolchain-version 5.5.0-riot.2 \
  --host-target aarch64-apple-darwin \
  --package kernel \
  --timeout 120
```

The script exports a committed revision (default `HEAD`, configurable with
`--revision`) into a unique `_build/stability/cold-*` directory. Uncommitted source
edits are not included. It creates a private HOME and copies only the explicitly
selected compiler; the compiler manifest and exported source configuration must
match the requested version/host. Relative internal compiler symlinks are preserved;
absolute or escaping links are rejected.

The first build starts with no `_build` or user registry/project cache. A warm
control runs only after the cold command succeeds. Each command requires a
nonempty terminal build result with no partial failure; a zero exit with missing
JSON evidence is not accepted. Failure stops immediately, retains the command
logs/report, and does not substitute a successful retry. Use `--package riot-cli`
for the CLI closure. These diagnostic deadlines are not yet release performance
budgets.

This is isolation from existing build and user caches, not a fully hermetic build:
PATH, host native libraries, and other host environment settings remain inherited.
The selected compiler is copied, not downloaded through a pinned bootstrap. The
report identifies that limitation explicitly. The script does not delete its
retained workspace or alter your installed Riot.

If process-group cleanup is denied, validation fails and preserves a
`cleanup_error` alongside the command result. A separate bounded leader kill is
attempted; this does not prove all children were cleaned up. Inspect remaining
processes before continuing, rather than treating cleanup failure as a pass.

## Test the supervisor

```sh
python3 -m unittest discover -s scripts/tests -p 'test_validate*.py' -v
```

These tests cover JSON-result validation, log preservation, spawn failures,
zero-test false positives, and process-group timeout escalation.

## Commits while the full workspace is not green

The existing pre-commit hook runs workspace-wide formatting, builds, and tests.
At the initial stabilization baseline it fails on unrelated deferred packages.
For a narrowly scoped, independently validated change, a one-command
`git -c core.hooksPath=/dev/null commit ...` bypass can keep commits small. Record
the bypass and the checks actually run; do not change the hook configuration or
claim that the full workspace passed. Prefer restoring a useful shared validation
policy over making this exception permanent.
