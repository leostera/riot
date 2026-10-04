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

## Test the supervisor

```sh
python3 -m unittest discover -s scripts/tests -p 'test_validate_stability.py' -v
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
