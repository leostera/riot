# RFD0024 - Riot Package Search

- Feature Name: `riot_package_search`
- Start Date: `2026-03-27`
- Status: `implemented`

## Summary
[summary]: #summary

This RFD documents Riot package search as a current-system snapshot. Search is
a discovery read model exposed from `sites/api.pkgs.ml` at
`GET https://api.pkgs.ml/v1/search?q=<query>` and keeps one searchable row per
published package.

- search is for discovery, not for installs or publication authority
- it returns only packages that are already published, indexed, and
  installable
- the current implementation uses D1 plus SQLite FTS5 and stores one row per
  package, not per release
- ranking is intentionally simple and package-name biased
- the search API is allowed to evolve independently from the sparse index as
  long as the install path remains index-driven

## Motivation
[motivation]: #motivation

The sparse index is optimized for exact installs, not for discovery.
Without search, users have to know package names already or reach outside Riot
to find them.

That creates a gap in the package experience:

- package discovery has no first-party home
- users cannot search by description, repository, or other package metadata
- the install path would be pressured to solve a discovery problem it was not
  designed to solve

At the same time, Riot does not want search to become the authority on whether
a package exists. That authority should stay in the publish and index data.

So the current search service exists to answer the discovery question only:

- "what packages should I look at?"

while leaving the install question to the sparse index:

- "which published package artifact should I actually install?"

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Suppose a user does not know the exact package name yet and searches for
"json rpc" or a repository owner.

The current mental model is:

- search is how users discover candidate packages
- the sparse index is how Riot installs a chosen package

So search is intentionally package-oriented and latest-state oriented. It is
not trying to be a second install index or a second publication database.

### Service boundary

Search is no longer a separate worker.
It is a read path inside `sites/api.pkgs.ml`.

That means:

- one API origin: `api.pkgs.ml`
- one D1 database for control-plane and search data
- one publish flow that can update publish records, index data, search data,
  and web views together

### Search model

Search keeps one row per package, representing the latest indexed state of the
package.

The search row is derived from the package's sparse index document and latest
release metadata.
It is intentionally package-oriented, not release-oriented.

That means a package with ten releases still returns one result.

### Indexed fields

The current search corpus includes:

- package name
- normalized package name
- latest version
- description
- license
- homepage
- repository
- root module
- canonical source locator
- repo owner
- repo name
- source subdirectory
- release count
- updated timestamp

This is enough for fuzzy matching on:

- exact package names
- partial package names
- owner or repo names
- source provenance
- natural-language description text

### Query semantics

The API is:

```text
GET /v1/search?q=<query>
```

Behavior:

- empty or missing query returns route metadata
- a match returns one entry per package
- a miss returns an empty `results` array, not an error

The ranking model is intentionally simple:

1. exact normalized package-name match
2. package-name prefix match
3. package-name substring match
4. FTS rank fallback

That keeps obvious package-name queries fast and intuitive without overbuilding
ranking logic too early.

## Update model
[reference-level-explanation]: #reference-level-explanation

Search updates happen synchronously during publish-time indexing.

The flow is:

1. publish succeeds
2. sparse index document is updated
3. search row is upserted in D1
4. `package.searchable` is emitted

This means the search invariant is:

- if search returns a package, that package is already installable through the
  sparse index

## Why D1 + FTS5

The current implementation uses D1 because it is already the registry's
control-plane database and FTS5 is sufficient for the initial search problem.

That gives Riot:

- low operational overhead
- one row per package
- transactional updates alongside other publish-time metadata work
- acceptable fuzzy search for the current corpus size

If search requirements later exceed what D1 and FTS5 provide, the external API
shape can stay stable while the implementation changes.

## Relationship to `riot add`

Search is not on the install critical path.
It exists for discovery.

Future `riot add` should use:

- search for package discovery UX
- the sparse index for actual install resolution

That distinction is important:

- `riot add kernel` should not require search to work
- search should help users find `kernel`
- the sparse index should remain the authoritative install fast path
