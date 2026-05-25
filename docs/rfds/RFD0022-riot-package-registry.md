# RFD0022 - Riot Package Registry

- Feature Name: `riot_package_registry`
- Start Date: `2026-03-27`
- Status: `implemented`

## Summary
[summary]: #summary

This RFD documents Riot's package registry as a current-system snapshot. The
registry lives in `sites/api.pkgs.ml` and acts as the control-plane service
for publication, name claims, and immutable release records, while install hot
paths are deliberately handled elsewhere.

- `riot publish` is the only way to claim and publish a public package name
- the registry owns authentication, publish validation, immutable release
  records, and lifecycle events
- successful publish updates the sparse index, search data, and derived web
  views synchronously
- `riot add <package-name>` installs from the sparse index and artifacts, not
  by querying the registry as an install-time dependency solver
- the registry does not own solving, lockfiles, workspace resolution, or build
  execution

## Motivation
[motivation]: #motivation

Riot needs a stable answer to a few ecosystem questions:

- who is allowed to publish `package-name`
- what makes a published release valid
- how does a named package become installable by others
- where does provenance and publication history live

Without a registry control plane, Riot would keep paying for ad hoc or
implicit publication behavior:

- package names would not have a single ownership boundary
- publication rules would be harder to audit and enforce consistently
- install flows would have to blur publication authority with install-time
  resolution
- contributors would have a weaker answer to provenance, release immutability,
  and lifecycle history

At the same time, Riot does not want the registry to become a monolith. Named
installs should feel fast, but the install hot path should not require the full
control-plane service for every operation.

That is why the current registry model is intentionally narrow:

- explicit authenticated publication
- stable named package claims
- immutable release records
- synchronous downstream read-model updates

while still leaving solving, lockfiles, workspace resolution, and build
execution in Riot itself.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

There are two workflows to keep separate in your head: publish and install.

### Named publishes

`riot publish`

This asks the registry to:

1. accept a package-root `tar.gz` artifact upload
2. validate that the package is publishable
3. authenticate the publisher
4. claim the package name if allowed
5. create the immutable published release record
6. store the immutable manifest and source artifact in R2
7. synchronously update the sparse index
8. synchronously update search and derived web views
9. emit lifecycle events

After a successful publish, the package is immediately available through:

- `GET /v1/search`
- the sparse index under `cdn.pkgs.ml/index/v1/...`
- artifact downloads under `cdn.pkgs.ml/...`
- `pkgs.ml`

### Named installs

`riot add minttea`

This is not a GitHub fetch and it is not a registry-side dependency solve.
The user-facing mental model is:

- the registry is the publish authority
- the sparse index is the install read model

So a named install should use:

1. `GET /v1/search` only for discovery UX, if needed
2. the sparse index config at `cdn.pkgs.ml/index/v1/config.json`
3. the package shard document at `cdn.pkgs.ml/index/v1/...`
4. the immutable artifact URL referenced by the chosen release

That separation is the important guide-level point. The registry makes a
package publishable and installable, but normal installs do not depend on the
registry worker staying in the hot path.

## Authentication model

The current implemented auth model is:

- GitHub OAuth for user identity
- session cookies for `pkgs.ml`
- API tokens for `riot publish`
- temporary `ROOT_AUTH_TOKEN` support for operator and e2e flows

Users authenticate through:

- `GET /v1/auth/github/start`
- `GET /v1/auth/github/callback`

Users can then create publish tokens through:

- `GET /v1/me/tokens`
- `POST /v1/me/tokens`
- `DELETE /v1/me/tokens/<token-id>`

Tokens are currently scoped to:

- `publish`

## Package-name and release rules

The implemented publish rules are:

1. package names are globally unique
2. a package name may only be claimed by one account owner
3. versions must be valid semver
4. versions are immutable

The version immutability rule is important:

- same package name + same version + same artifact digest: publish short-circuits as
  an idempotent success
- same package name + same version + different artifact digest: publish fails with
  conflict

This means Riot does not support overwriting an already-published version.

## Publish validation rules

The registry currently enforces:

- `package.public = true`
- `package.name` exists
- `package.version` is semver-valid
- `package.description` exists
- `package.license` exists and is SPDX-compatible
- semver dependencies must already exist in the registry
- git dependencies are allowed if the git reference parses

The current exception is compiler-shipped OCaml libraries.
These are treated as built-in dependencies and do not require publication:

- `stdlib`
- `unix`
- `dynlink`

Workspace publish ordering is intentionally not solved in the registry.
That orchestration belongs in `riot publish`.

## API surface
[reference-level-explanation]: #reference-level-explanation

The main registry endpoints are:

- `GET /`
- `POST /v1/publish`
- `GET /v1/search?q=<query>`
- `GET /v1/events?limit=<count>&after=<event-id>`
- `GET /v1/packages/<package-name>/events?version=<version>&limit=<count>`
- `GET /v1/views/packages/<package-name>/overview`
- `GET /v1/views/packages/<package-name>/relations`
- `GET /v1/views/recent/packages`
- `GET /v1/views/popular/packages`
- `GET /v1/views/categories`
- `GET /v1/views/owners/<github-login>/packages`

The stable API surface is `api.pkgs.ml/v1/...` for control-plane routes, while
the sparse index and immutable artifacts are served from `cdn.pkgs.ml`.

## Storage model

The implementation is database-first for control-plane data and R2-first for
heavy immutable artifacts.

### D1

Cloudflare D1 stores:

- users
- sessions
- oauth state
- api tokens
- package claims
- published releases
- request-driven registry events
- search rows and FTS tables
- derived web-view documents

### R2

Cloudflare R2 stores:

- immutable published source tarballs
- immutable publication manifests
- sparse package-index files
- cached user avatars
- exported D1 backups

## Events

The registry records lifecycle events in D1 and exposes them through
`/v1/events`.

The currently emitted package lifecycle events are:

- `package.submitted`
- `package.verified`
- `package.published`
- `package.searchable`
- `package.indexed`

These events drive:

- the public activity page
- debugging of publish pipelines
- future downstream systems such as docs, security checks, or analytics

## Relationship to `riot add` and `riot publish`

This RFD captures the registry contract that `riot` should build against.

`riot add` needs two modes:

- source mode via `resolve`
- named-package mode via the sparse index

`riot publish` needs:

- Git-aware package locator detection
- auth token support
- workspace publish ordering
- clear handling of immutable version conflicts

Those client-side concerns are intentionally deferred to a later Riot package
management RFD.
