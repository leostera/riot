# riot-store AGENTS

`riot-store` owns the artifact cache and its on-disk layout.

## Rules

1. Treat manifest layout and hash addressing as compatibility-sensitive.
2. Keep writes atomic where practical. Partial cache entries are worse than misses.
3. Store logic should not know about CLI or session behavior.
4. Cache roots and export manifests are scoped by build lane (`profile` + `target`); do not assume host-default cache paths when serving cross-build artifacts.
5. Artifact manifests must preserve cached `ocamlc_warnings` so package warnings can be replayed without rebuilding.
6. Package exports now live in the hash-addressed artifact `manifest.json`. Do not reintroduce a second payload manifest for package exports.
7. Do not add package-name export lookup scans over the cache. Callers should hold the build result or hash they need and derive export paths from that explicit data.
8. Cache GC and generation receipts live here. Keep the fast path cheap: deciding whether GC runs should rely on small workspace-level metadata first, and only read generation receipt bodies when a GC pass is actually needed.
9. The first cache-GC rollout is workspace-wide, not lane-local. Receipt retention and size accounting span all lanes under the workspace build root together.
10. Keep `riot-store` Riot-specific. Generic content-addressable directory and bundle primitives belong in `contentstore`; `riot-store` should layer package artifacts, manifests, and lane policy on top.
11. Post-build generation recording must stay cheap on repeated warm builds. `state.json` is the authoritative generation-recency index; do not make the build path discover recency by scanning or parsing generation payload files.
12. Inside `riot-store`, lane targets should stay typed as `Riot_model.Target.t`. Only stringify them when encoding JSON receipts/state or deriving on-disk lane paths.

13. Cache hits verify the manifest's build hash and each declared payload's regular-file kind, size, and SHA512 digest. Referenced exports must be declared and verified against their action manifests. Verify all files before promotion or export materialization touches working outputs.
14. Same-hash publication reuses immutable directories. Refuse a damaged existing payload or unreadable manifest with a typed, actionable error; do not report new outputs as stored while retaining damaged bytes, or silently remove corruption evidence. Verification is not a signature/authentication guarantee.

## Validate

`timeout 30 riot build riot-store`
