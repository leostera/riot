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

15. `Build_root_lock` protects build-root lifetime across processes: builds hold shared guards, policy and force cleanup require an exclusive guard and fail promptly on contention. The persistent guard is a hidden sibling (`.<root-name>.riot-build.lock`), never inside the removable root and never unlinked on release. Preserve the inode across cleanup. Guard callbacks release on both ordinary and exceptional exits.

16. Artifact temporary-name allocation must use an atomic read-modify-write nonce, not separate cell reads/writes. Clock resolution and PID alone cannot distinguish simultaneous same-hash writers. Keep contention coverage bounded, require every writer to finish successfully, and verify promoted bytes.

## Validate

`timeout 30 riot build riot-store`
