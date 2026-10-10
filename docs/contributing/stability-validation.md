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
- `kernel:kernel_new_process_tests`
- `std:std_test_cli_tests`
- `std:std_command_tests`
- `riot-build:build_core_tests`
- `riot-cli:test_selection_tests`
- `riot-planner:action_hash_tests`
- `riot-planner:package_planning_tests`
- `riot-build:action_executor_tests`
- `riot-build:build_lock_tests`
- `riot-build:cache_tests`
- `riot-build:concurrent_tests`
- `riot-build:executor_behavior_tests`
- `riot-build:coordinator_tests`
- `riot-store:store_tests`

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

## Active-build cleanup exclusion

```sh
python3 scripts/validate-build-cleanup.py \
  --driver <explicit-candidate-cli> \
  --toolchain <installed-host-compiler-directory> \
  --toolchain-version 5.5.0-riot.2 \
  --host-target aarch64-apple-darwin --timeout 120
```

This creates a standalone library fixture outside the monorepo (so ancestor
workspace discovery cannot select Riot itself), with a private HOME/compiler.
A deliberate wrapper around the **copied** `ocamlopt.opt` gates its first actual
compilation through a FIFO. While that compilation is active, `clean --force`
must promptly exit 1 and emit the specific build-root-in-use failure without
entering cleanup. The sentinel must survive, and compilation must subsequently
complete successfully with strict terminal build evidence. The raw negative
command result is retained alongside its separately validated expectation.
Fixture paths, wrapper text/digests, compiler manifest, driver snapshot/digest,
commands, logs, and external deadlines are retained. No real installation/cache
is cleared; fixtures are not automatically deleted. The controlled compiler is
not bootstrap certification, and this library is not the representative service.

The same command with `--scenario cancellation` delivers SIGINT to the build's
supervised process group while compilation is paused. The compiler must belong
to that group; the CLI must terminate with failure within ten seconds and leave
no group members after a bounded three-second observation. A subsequent build
must succeed without clearing caches. The interrupted command remains a raw
failure with a separately recorded expected outcome; recovery is not substituted
for a failed cancellation gate. The first build has a total external `--timeout`
deadline, including the barrier interaction; recovery has its own deadline.

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
for the CLI closure. Add `--edit-probe` to measure a third build after changing
its help description in the disposable export. The mutation preserves file size
and mtime; the produced CLI's help must expose the original text before the edit
and the new text afterwards. Reports retain both source and executable digests.
The final edited executable is a test artifact, **not** the unmodified candidate;
use the separate bootstrap-produced CLI for candidate validation.

The 120-second command deadline gives substantial headroom over the retained
Apple Silicon CLI controls (approximately 45 seconds cold, 0.7 seconds warm,
and 2.2 seconds for the description edit). These diagnostic deadlines are not
release performance budgets, and do not establish Linux timings.

This is isolation from existing build and user caches, not a fully hermetic build:
PATH, host native libraries, and other host environment settings remain inherited.
The selected compiler is copied, not downloaded through a pinned bootstrap. The
report identifies that limitation explicitly. The script does not delete its
retained workspace or alter your installed Riot.

If process-group cleanup is denied, validation fails and preserves a
`cleanup_error` alongside the command result. A separate bounded leader kill is
attempted; this does not prove all children were cleaned up. Inspect remaining
processes before continuing, rather than treating cleanup failure as a pass.

## Pinned bootstrap and fresh self-rebuild

On Apple Silicon, with Python 3.12+, curl, Git, Xcode command-line tools, and
Homebrew PCRE2/OpenSSL development libraries available:

```sh
python3 scripts/validate-bootstrap.py \
  --host-target aarch64-apple-darwin \
  --timeout 120
```

`scripts/bootstrap-pins.json` records exact-version HTTPS archive URLs and
reviewed SHA256 values for Riot `0.0.17` and compiler `5.5.0-riot.2`. Each run
redownloads both archives to a unique `bootstrap-*` evidence directory; it does
not select `latest`, copy the installed compiler, or use a repo `_build` driver.
Curl uses a private HOME and ignores user curl configuration. Hash verification
precedes extraction. Extraction uses Python's data filter, rejects duplicate or
unsafe paths, and accepts only regular `riot`/`release.json` bootstrap payloads.
Older archives' AppleDouble metadata is ignored.

The downloaded bootstrap builds `riot-cli` from an empty committed export with
private HOME/cache state. The produced CLI is snapshotted and builds a *second*
empty export, followed by its warm control. Reports tie both exports to the same
source SHA and tie the second-stage driver to the first-stage executable digest.
The first failure stops the entire lane without retry or cache substitution.
The resulting executable paths/digests and all archive/command logs are retained.
Use `--revision` for a specific committed source; uncommitted changes are excluded.

The pinned bootstrap has an older `BuildCompleted` JSON result dialect. Its
separate validator requires nonempty, unique, successful package results,
matching statistics, and the requested runtime CLI. The current driver's
`returning_results` validator remains unchanged. A successful exit alone is not
accepted for either dialect.

These pins currently cover Apple Silicon only. An absent Linux pin fails rather
than skipping its gate. This lane is not hermetic: host PATH, C compiler/SDK, and
native libraries are prerequisites. Checksums pin exact reviewed bytes; they are
not a release signature or independent provenance attestation. The diagnostic
120-second command deadline is recorded, not presented as a release speed SLA.

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
