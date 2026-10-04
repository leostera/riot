# RFD0044 - v0.1 Daily-Driver Stability

- Feature Name: `v0_1_daily_driver_stability`
- Start Date: `2026-10-04`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD proposes a bounded release contract for a reasonably stable Riot v0.1 that its maintainer can use every day, before broad public adoption. It prioritizes predictable application development and recoverability over completing the entire ecosystem.

- One supported install-to-application workflow, exercised outside the monorepo.
- Explicit support tiers for platforms, commands, and experimental packages.
- Evidence-based readiness gates and a short daily-use candidate period.
- Public registry operations and broad launch readiness are a separate milestone.
- New compiler backends, macro completeness, and general API redesign are not prerequisites.

## Motivation
[motivation]: #motivation

Riot has a build system, package manager, managed toolchains, runtime, standard
library, application libraries, and developer tooling. The problem is no longer
whether enough capabilities exist. It is whether their composition is reliable
enough to stop development work from turning into toolchain maintenance.

A local survey at commit `df8b93642` found:

- `./riot test -p kernel` passed 319 cases and `./riot test -p std` passed 560,
  with no reported failures or skips. These were local runs with existing caches,
  not a clean-machine or cross-platform certification.
- `./riot build riot-cli` failed at the `Build_spec.make` interface. Its optional
  parallelism parameter disagreed with the implementation and lacked an erasing
  positional argument. A narrow fix can unblock this without a scheduler rewrite.
- All four GitHub workflow files were disabled.
- The existing driver reported version `0.0.15`, while the CLI manifest said
  `0.0.17`. This is an observed metadata discrepancy, not proof of binary provenance.
- The CLI did not register `check`, and `lsp stdio` explicitly reported unavailable.
- `NEXT.md` mixed correctness problems with optional API changes and new features.

These observations establish integration and verification gaps, not a claim that
every untested component is broken. The recurring cost is manual rediscovery:
the maintainer must remember which binary, package, command, or cache state works.
That cost remains even after any single compilation error is fixed.

Daily work needs three ordinary scenarios to succeed: creating an application,
editing and testing it repeatedly, and updating Riot without losing a working
environment. If we keep treating every roadmap item as a release prerequisite,
the finish line moves while those scenarios remain weakly verified.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Suppose you want to start a small service. You install a pinned Riot candidate,
initialize a workspace outside this repository, add a dependency, run the program,
change a module, run its tests, format it, and build a release executable. You
should not need a second Riot checkout or an undocumented cache deletion.

For an existing application, you keep the previous working Riot version available
while trying the candidate. If the candidate fails, you can reinstall the known
version without rebuilding Riot from source. A regression is recorded with a
reproduction and promoted into a test, rather than becoming a permanent workaround.

The release has two milestones:

1. **Daily-driver candidate:** the supported local development loop works, rollback
   works, and known limitations are explicit.
2. **Public v0.1 announcement:** the candidate also has a verified distribution
   matrix, registry operational checks, and user documentation consistent with it.

A package being present in the workspace does not automatically make it part of
the supported v0.1 surface. Experimental work can continue without blocking the
candidate unless it breaks the supported dependency closure.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Proposed scope

The initial required developer host is `aarch64-apple-darwin`; the initial Linux
execution target is `x86_64-unknown-linux-gnu`. This is a proposal, not an observed
support guarantee. `aarch64-unknown-linux-gnu` is a candidate for the required
matrix if it is needed by the first daily-use application. Other targets remain
experimental until they have execution evidence.

The supported workflow includes:

- `init`, dependency addition and lockfile reuse, `build`, `run`, and `test`.
- `fmt`, the existing lint workflow, useful failure messages, and meaningful exits.
- Installation of a pinned candidate and recovery to a known-good version.
- Runtime and std behavior required by the selected application.
- A documented editing path using compiler diagnostics through build. LSP is
  explicitly deferred for the initial daily-driver milestone; restoring `check`
  or LSP is not a prerequisite. Neither should be advertised as supported until
  its own validation passes.

Raml backend completeness, a new macro system, script-file execution, full typing
parity, new collection/numeric packages, and general naming cleanups are excluded.
RFD0043 may improve the build implementation, but this release does not require
its full architectural scope if the existing design satisfies correctness gates.

### Acceptance evidence

RFD0045 defines repeatable validation; RFD0046 defines safe candidate delivery.
The daily-driver candidate must have:

1. A source revision, toolchain identity, bootstrap identity, and exact commands
   recorded alongside results. No unexplained local cache dependence.
2. A newly built Riot that can rebuild Riot and build a fresh external workspace.
3. Passing required kernel/std/runtime and build/cache regression suites, with
   zero silently skipped required suites or unexpected snapshot candidates.
4. A representative application completing install, dependency resolution,
   development builds, tests, formatting, and release execution.
5. A bounded failure path: compiler errors and cancellation terminate, report a
   nonzero result where appropriate, and do not leave later builds unusable.
6. Verified candidate installation and rollback, with no destructive replacement
   on download, verification, or extraction failure.
7. Measured cold build, warm build, edit-build, and peak-memory baselines. Numeric
   budgets are set from those measurements, not invented in this RFD.

The proposed soak is five actual development days on the same candidate. Record
host, application, commands used, failures, workarounds, and memory/performance
surprises. Toolchain-caused data loss, incorrect artifacts, hangs, or a required
workflow that cannot complete block promotion. Fixes affecting required workflows
produce a new candidate and restart the soak; unrelated documentation changes do
not. A five-day soak is a practical policy proposal, not a statistical guarantee.

### Work ordering and ownership

`TODO.md` is the live execution ledger, linked to these RFDs. It distinguishes
observed failures, unverified risks, completed checks, and deferred scope. RFDs
remain design records rather than accumulating per-run logs.

Work proceeds in this order: restore the local build; establish repeatable
validation; close reproduced correctness failures; exercise an external app;
verify candidate delivery; soak; decide whether public-launch gates are met.
Small fixes need not wait for all policy details to be settled. Broader changes
remain separate reviewable slices.

## Drawbacks
[drawbacks]: #drawbacks

A deliberately small support matrix disappoints users on other hosts. Maintaining
an external application and candidate evidence costs ongoing time. A feature can
exist without being release-supported, which requires careful documentation.
The soak delays promotion and cannot prove concurrency correctness by itself.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

- **Do nothing:** keep using whichever checkout works. This avoids release work
  but preserves manual recovery and makes regressions hard to localize.
- **Only fix today's build:** necessary and cheap, but it does not verify installs,
  new workspaces, runtime races, or tomorrow's integration breakage.
- **Finish the entire roadmap:** offers a larger announcement but continuously
  expands the verification surface and delays daily use.
- **Bounded daily-driver release:** fixes the recurring integration cost while
  allowing experimental packages to evolve. Most enforcement can be scripts and
  existing tests; no new release-management service is needed.

## Prior art
[prior-art]: #prior-art

Riot's `NEXT.md` captures useful maintainer friction but lacks a stable acceptance
boundary. RFD0039 supplies test-size and flaky-test vocabulary; use that vocabulary
rather than inventing a second test policy. RFD0043 motivates build scheduling
improvements, but architecture work and release acceptance have different finish
lines. Release candidates and dogfooding provide workflow evidence; they should
complement, not replace, deterministic regression tests.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Discussion: which real application is the first daily-use consumer, and does it
  require ARM Linux? LSP is not required for the initial milestone.
- Discussion: is five development days the right candidate soak?
- Implementation: what cold/warm build and memory budgets reflect that application?
- Before public announcement: which APIs receive a compatibility commitment and
  which registry/security operational checks must be independently exercised?

## Future possibilities
[future-possibilities]: #future-possibilities

Expand the supported matrix as execution evidence exists, add more real consumer
applications, and publish per-release compatibility and performance reports.
None of these extensions is needed to start using the bounded candidate daily.
