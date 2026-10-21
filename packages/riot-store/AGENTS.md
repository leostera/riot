# riot-store AGENTS

`riot-store` owns the artifact cache and its on-disk layout.

## Rules

1. Treat manifest layout and hash addressing as compatibility-sensitive.
2. Keep writes atomic where practical. Partial cache entries are worse than misses.
3. Store logic should stay focused on artifacts, cache layout, and persistence; CLI and session behavior belong above it.
4. Cache roots and export manifests are scoped by build lane (`profile` + `target`); derive cross-build artifact paths from the lane.
5. Artifact manifests must preserve cached `ocamlc_warnings` so package warnings can be replayed without rebuilding.
6. Package exports live in the hash-addressed artifact `manifest.bin`.
7. Callers should hold the build result or hash they need and derive export paths from that explicit data.
8. Cache GC and generation receipts live here. Keep the fast path cheap: deciding whether GC runs should rely on small workspace-level metadata first, and only read generation receipt bodies when a GC pass is actually needed.
9. The first cache-GC rollout is workspace-wide, not lane-local. Receipt retention and size accounting span all lanes under the workspace build root together.
10. Keep `riot-store` Riot-specific. Generic content-addressable directory and bundle primitives belong in `contentstore`; `riot-store` should layer package artifacts, manifests, and lane policy on top.
11. Post-build generation recording must stay cheap on repeated warm builds. `state.bin` is the authoritative generation-recency index.
12. Inside `riot-store`, lane targets should stay typed as `Riot_model.Target.t`. Only stringify them when encoding receipts/state or deriving on-disk lane paths.
13. Artifact manifests record both `input_hash` and `output_hash`: use `input_hash` as the cache lookup/materialization key, and `output_hash` as the produced-content fingerprint that downstream planners consume.
14. Incremental graph node payload storage is generic and opaque here. Planner/build packages own payload codecs and invalidation semantics; `riot-store` owns namespaced hash-addressed persistence.
15. Cache hits are verified, not trusted. Every lookup and promotion checks each manifest entry for a safe relative path (no absolute paths, empty, `.` or `..` components), a regular non-symlink file, the recorded size, and the recorded SHA512, and fails with `ArtifactIntegrityFailed`. The `verified.bin` stat-fingerprint sidecar (size, mtime ns, inode) may skip rehashing only outside a 1s racy-clean window; it must never hide a later mutation, including after the in-memory artifact cache is warm. Re-saving an artifact replaces a corrupt one.
