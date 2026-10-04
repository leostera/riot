# RFD0045 - Repeatable Stability Validation

- Feature Name: `repeatable_stability_validation`
- Start Date: `2026-10-04`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD proposes a shared local and CI validation workflow that turns Riot's daily-driver requirements into reproducible evidence. It starts with self-hosting and focused correctness regressions rather than a new testing framework or an immediate rewrite of the build scheduler.

- The same checked-in commands run locally and in CI.
- Clean bootstrap, warm-cache behavior, and failure recovery are distinct checks.
- Build/cache invariants and runtime races have explicit regression coverage.
- Required suites cannot disappear behind successful zero-test runs.
- Full workspace certification and arbitrary performance targets are out of scope.

## Motivation
[motivation]: #motivation

A successful invocation of an old Riot binary against an existing build directory
does not show that the current source can build itself on a fresh machine.
Likewise, hundreds of passing library cases do not establish correctness of a
cache key or a rare scheduler transition.

The survey at `df8b93642` found passing local kernel/std suites alongside a CLI
compilation failure, with every GitHub workflow disabled. The old CI referred to
Tusk and obsolete command flags. This is evidence that the repo lacks an active
GitHub enforcement path, not evidence about any external CI that may exist.

There is also a concrete correctness lead: foreign dependency actions sort their
`build_cmd` list before hashing in `packages/riot-planner/src/action.ml`. Reordering
commands can change behavior while retaining the hash. The existing spec inventory
records this concern. Other entries are historical: current package planning now
includes the toolchain hash, despite an inventory entry claiming otherwise.

Maintainers currently pay both a detection cost and a triage cost. They must
reproduce integration failures manually and decide whether old bug inventories
still apply. Simply adding more tests without defining which must run leaves
that structural gap intact.

The opportunity is a small trusted loop: a contributor can validate a change;
a reviewer can see exactly what ran; and a candidate can demonstrate that cold,
warm, and failed runs all preserve the required contracts.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Suppose you change package planning. Before merging, you run the same validation
entrypoint as CI. It builds a candidate Riot, invokes that candidate on a fixture,
changes an input, rebuilds, and checks the observable output. A cache hit is not
accepted merely because a log line says it was cached.

Suppose instead you change actor shutdown. Focused tests coordinate actors through
messages or barriers to reproduce exit and wakeup ordering. A separate bounded
stress job repeats the scenario. A timeout reports the command, seed where
applicable, and captured output; it is a failure, not a retry-until-green policy.

The workflow has three lanes:

1. **Change validation:** focused required suites and a CLI build.
2. **Clean integration:** isolated bootstrap, self-hosting, and an external-project
   smoke test on supported hosts.
3. **Candidate validation:** the above plus target execution, failure injection,
   repeated runtime tests, and recorded resource measurements.

These are proposed workflow lanes, not new `riot` subcommands. Initial
implementation should use small scripts and the existing runner.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Bootstrap and isolation

The clean lane uses a fresh checkout or disposable workspace and a dedicated Riot
home, without deleting the contributor's real `_build` or `~/.riot`. It records
the source SHA, host/target, compiler version, bootstrap binary/source identity,
and commands. A cache may accelerate change validation, but clean integration
must remain available and periodically required.

The bootstrap path must be chosen and documented before enabling the clean lane:
use a pinned published driver with verified bytes, or repair and pin the
`bootstrap.py`/miniriot path. Do not silently fall back between the two. The
first-stage driver builds the candidate; the candidate then builds the current
CLI source and a standalone application. Behavioral self-hosting is required;
bit-for-bit identical binaries are not an initial requirement.

Generated scaffolding is tested as generated, including its dependency resolution.
Any fixture-specific edits are explicit. Tests must not substitute monorepo path
dependencies for the registry dependencies that users would receive.

### Build and cache regression matrix

Tests live with the owning planner, build, store, and dependency packages. Before
adding a test, reconcile existing coverage and spec inventory entries against
current code. Each closed bug gets an implementation-level reproduction and an
updated inventory status; historical snapshot RFDs are not rewritten.

Required scenarios include:

| Change or failure | Required observation |
| --- | --- |
| Reorder foreign build commands | Different action identity and correct output |
| Change source, dependency path, flags, or toolchain | Affected output rebuilt; no stale success |
| Repeat unchanged build | Correct output reused without unnecessary compilation |
| Remove or corrupt a cached export | Rebuild or explicit failure, never incomplete success |
| Fail upstream action | Dependents terminally accounted for; command exits nonzero |
| Interrupt build | No false success; subsequent build can recover |
| Concurrent build or clean | Lock policy honored; no partial artifacts accepted |
| Promote/install a binary | Executable result or reported failure |

Tests vary one input at a time and check both action identity where relevant and
observable artifacts. Cache-schema changes or hash fixes must invalidate old
entries rather than retaining suspect artifacts for compatibility.

### Runtime regression matrix

First locate existing tests and restore coverage displaced by the move into
`Std.Runtime`; absence of a runtime-named file alone is not proof of no coverage.
The required behavior inventory covers:

- Message delivery, selective receive, and wakeups under concurrent senders.
- Links, monitors, process exit, and supervision restart/cleanup behavior.
- Receive timeout versus message delivery and cancellation versus timer firing.
- Pinned and blocking placement, work stealing, and scheduler shutdown.
- I/O readiness and close/exit races, including cleanup of owned descriptors.

Prefer deterministic synchronization over timing sleeps. Tests that genuinely
need stress repetition have a bounded iteration count and external timeout.
Run on the supported macOS and Linux hosts; a cross-compiled binary that never
executes is not runtime evidence. The test harness should also exercise child
process failures so a runtime defect cannot silently kill the reporting path.

### Result and failure contract

Each validation run records command, exit status, duration, platform, suite/case
counts, skips, and artifact/log locations. Existing JSON test events are preferred
over parsing human output. Required suite identities and nonempty discovery are
checked explicitly. Test counts may evolve; hardcoded historical totals are not
the acceptance mechanism.

An external deadline supervises every potentially hanging process tree. On timeout,
terminate children and report failure. Preserve stdout/stderr and pending snapshot
diffs; never auto-approve snapshots or turn a required failure into a passing retry.
Flaky/large policies remain those of RFD0039 and `Std.Test`.

The local entrypoint should accept the driver path explicitly. Candidate tests use
the freshly produced binary, not whichever `riot` happens to be first on PATH.
GitHub workflows call this entrypoint instead of duplicating its logic in YAML.

### Rollout

Start by restoring CLI compilation and running targeted existing tests. Next land
the local smoke entrypoint, then active host CI, then isolated self-hosting and
external-project validation. Add correctness reproductions in small owner-scoped
changes. Resource budgets follow measured baselines and RFD0044 scope decisions.

## Drawbacks
[drawbacks]: #drawbacks

Fresh environments and cross-host runners cost time and bandwidth. Networked
scaffolding tests can expose registry outages unrelated to a code change, so logs
must distinguish download failures from compiler failures without treating either
as success. Deterministic race tests require careful design and cannot enumerate
all interleavings. Maintaining suite discovery checks is additional work.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

- **Do nothing:** retain quick manual checks, but integration failures continue to
  reach the daily workflow without a known last-green baseline.
- **Re-enable old YAML unchanged:** smaller diff, but stale flags and unpinned
  environments do not establish current behavior.
- **Only run all unit tests:** useful coverage, but misses bootstrap, candidate
  identity, external consumers, and destructive recovery scenarios.
- **Rewrite the scheduler first:** may improve structure but increases simultaneous
  change before a regression baseline exists. RFD0043 should use these gates.
- **Shared scripts over existing tests:** adds the missing integration layer without
  inventing another test framework or duplicating package ownership.

## Prior art
[prior-art]: #prior-art

RFD0002 records Riot's bootstrap approach; its existence is not proof that the
current path works. RFD0012 and the TLA+ specs supply cache/scheduler invariants
that need executable regression counterparts. RFD0039 supplies test policy, and
`Std.Test` already provides structured results. The lesson is to connect existing
mechanisms into a repeatable validation contract, not replace them.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Discussion: which pinned bootstrap is the maintained entrypoint?
- Discussion: which required host runners are available and what CI budget is practical?
- Implementation: which runtime invariants already have current executable coverage?
- Implementation: how should process-tree supervision and resource measurements be
  implemented portably on the supported hosts?
- Later: coverage reporting and additional targets can follow independently.

## Future possibilities
[future-possibilities]: #future-possibilities

Add coverage reports, longer randomized concurrency runs, hermetic dependency
mirrors, reproducible-binary checks, and trend reporting. These extend the evidence
base without changing the initial requirement that the same candidate must survive
both ordinary development and recovery from failures.
