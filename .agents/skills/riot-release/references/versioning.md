# Versioning

## Riot semver

The Riot release version is the package version carried by the real workspace
and service manifests, especially `packages/riot-cli/riot.toml`.

Use search, not memory.

Prefer starting with the helper:

```bash
.agents/skills/riot-release/scripts/release_inventory.py
.agents/skills/riot-release/scripts/release_inventory.py --json
```

### Inspect current versions

```bash
.agents/skills/riot-release/scripts/release_inventory.py
sed -n '1,40p' packages/riot-cli/riot.toml
sed -n '1,40p' ocaml-toolchain.toml
```

### Find real release manifests

Exclude fixtures under `tests/`.

```bash
.agents/skills/riot-release/scripts/release_inventory.py --list-manifests
find packages sites -path '*/tests/*' -prune -o -name riot.toml -print | sort
```

### Bump the Riot semver

- Update `[package].version` in every real `riot.toml` under `packages/` and
  `sites/`.
- Do not bump fixture manifests under test workspaces.
- Keep the versions aligned across the workspace release set.
- The helper reports any real release manifests whose version has drifted from
  `packages/riot-cli/riot.toml`.

### Update the changelog

`CHANGELOG.md` is the release summary. Add a new top entry:

```md
## 0.0.24 - 2026-04-24
```

Write release notes for users, not for the commit log:

- Always put `### riot` first.
- Bundle all `riot-*` package changes under `### riot`.
- Do not bundle other packages together. Each non-`riot-*` package that needs
  release notes gets its own `### <package>` subsection.
- Each bullet should explain what changed, why it matters, and any behavior or
  migration impact users should know about.
- Avoid filler such as "tests were expanded", "code was formatted", or
  "internals were refreshed" unless the user-facing behavior is also stated.
- Do not dump raw commit subjects. Rewrite them into concise, descriptive
  capability, bugfix, compatibility, or performance notes.

Use the last reachable semver tag as the diff anchor:

```bash
git describe --tags --abbrev=0 --match '[0-9]*.[0-9]*.[0-9]*'
```

Manual release work does not require a globally clean tree. Dirty files outside
the release scope may remain as long as all changes under `./packages`, every
real `riot.toml` release manifest, and the release inputs being published are
committed. If the higher-level `./scripts/release.sh` wrapper has a stricter
clean-tree preflight, use the lower-level publish/release commands from the
release commit instead of moving unrelated work.

### Commit and tag

The conventional release prep commit used in this repo is:

```bash
git commit --no-verify -m "chore(release): prepare <version>"
git tag -a <version> -m "<version>"
```

Use `--no-verify` only when the hook is failing because of unrelated worktree
state or known non-release failures.

## Toolchain epoch

The OCaml toolchain epoch is separate from Riot semver.

Examples:
- `5.5.0-riot.3`
- `5.5.0-riot.4`

When the default toolchain changes, search for the old epoch across the repo and
update every baked-in default and test expectation:

```bash
rg -n '5\\.5\\.0-riot\\.[0-9]+' .
```

At minimum, expect to touch:
- `ocaml-toolchain.toml`
- bootstrap/default-toolchain code
- generated-template defaults
- toolchain tests
- release and smoke scripts if they pin an epoch in examples or expectations

Treat the toolchain epoch as part of the release story. If Riot defaults to a
new toolchain, that usually justifies a new Riot/package release too.

## Publishing from the correct point

If Riot binaries have already shipped for version `X.Y.Z`, and `HEAD` moved on,
publish the package set from tag `X.Y.Z`, not from newer `HEAD`.

That keeps:
- the Git tag
- the published packages
- the Riot binaries

on the same release point.
