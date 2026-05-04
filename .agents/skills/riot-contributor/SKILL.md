---
name: riot-contributor
description: "Use when changing, reviewing, debugging, testing, benchmarking, documenting, or releasing code inside the Riot monorepo. This skill covers repository-specific OCaml/Riot ML conventions, workspace Riot commands, AGENTS.md routing, snapshots, validation, and contributor workflow. Use riot-ml for ordinary projects that consume the riot CLI."
---

# Riot Contributor

## Use This Skill

Use this skill for work inside this repository: packages, compiler code, editors, specs, docs, release tooling, repo-local skills, and AGENTS guidance.

For a user project outside this repo, use `riot-ml` instead.

## Contributor Loop

1. Read the root `AGENTS.md`, then the most specific `AGENTS.md` for every touched path.
2. Check the dirty worktree and preserve unrelated user changes.
3. Keep changes package-scoped and aligned with the existing local contracts.
4. Validate with the workspace Riot binary: `riot run riot -- ...`.
5. Update affected `AGENTS.md` files when package behavior, public contracts, or workflow expectations change.
6. Commit with conventional commits when the user asks to commit or when the slice is ready.

## Dirty Worktree Rule

- Never stash, drop, reset, restore, or otherwise move code you did not touch.
- If unrelated dirty files block a command, stop and report the blocker. Let the
  owner decide whether to commit, stash, move, or discard their own work.
- If a clean tree is required for validation or release work, ask for one or use
  a separate worktree that does not disturb the shared workspace.

## References

- Read [workflow](references/workflow.md) for the day-to-day contributor process.
- Read [routing](references/routing.md) when choosing which package guidance to load.
- Read [validation](references/validation.md) for build, test, format, fix, snapshot, and benchmark commands.
- Read [conventions](references/conventions.md) for repository-wide Riot ML and API conventions.
- Read [fuzzing](references/fuzzing.md) when adding fuzz cases, running campaigns, triaging crashes, or touching `.riot/fuzzing` artifacts.
- Read [bootstrap and miniriot](references/bootstrap.md) when touching first-build, toolchain bootstrap, or `packages/miniriot`.
- Read [package boundaries](references/package-boundaries.md) when deciding whether behavior belongs in `kernel`, `std` runtime, or a higher package.
