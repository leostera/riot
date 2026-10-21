---
title: How docs are built
description: Routes, versioning, and the pipeline behind docs.pkgs.ml.
---

`docs.pkgs.ml` serves generated API documentation for packages published on
[pkgs.ml](https://pkgs.ml). Search, package pages, stats, and accounts stay on
pkgs.ml.

## Routes

Every published version gets its own docs:

```text
https://docs.pkgs.ml/p/<package>/<version>/
```

Module pages live below that root, for example
`/p/std/<version>/Std/List/index.html`.

## Publishing pipeline

1. You publish a release with `riot publish`.
2. The docs worker records the release and queues it for processing.
3. A sandboxed runner installs Riot, runs `riot build` to verify the package,
   and runs `riot doc` to generate the HTML.
4. The generated files are stored in the package CDN and served from this
   domain.

If you open a version before its docs are ready, the page reports the current
pipeline status instead.

## Related services

- [pkgs.ml](https://pkgs.ml): search, package pages, stats, and accounts.
- `api.pkgs.ml`: the registry API used by the Riot CLI.
- `cdn.pkgs.ml`: the sparse index and package artifacts.
