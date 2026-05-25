---
title: What lives here
description: Scope of docs.pkgs.ml and how it differs from pkgs.ml.
---

`docs.pkgs.ml` is reserved for generated package documentation.

That means this site should eventually host:

- rendered package API docs
- module and type documentation
- versioned package docs
- static package-doc sites under `/p/<package>/<version>/`

This site should **not** be the main human registry UI. For that, use:

- `pkgs.ml` for search, package pages, stats, activity, and account management
- `api.pkgs.ml` for control-plane API access
- `cdn.pkgs.ml` for sparse index and immutable package artifact downloads

## Route shape

Package docs are expected to be served from:

```text
docs.pkgs.ml/p/<package>/<version>/
```

and backed by generated static files stored in the package CDN bucket at keys
like:

```text
docs/<package>/<version>/index.html
docs/<package>/<version>/...
```

## Current status

The domain now acts as a worker-backed forwarding surface for those generated
docs paths.

When a package is published, the docs worker first records that release in a D1
table of releases to process.

A timer-driven worker then claims queued releases and stages two
container-oriented pipeline requests:

- a docs build request that will eventually run `riot doc`
- a build verification request that will install Riot in a clean sandbox and run
  `riot build`

Those requests are stored in R2 and indexed in D1 so a future container-backed
runner can claim them without guessing the contract. The actual runner is still
the next step.
