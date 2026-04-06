# contentstore AGENTS

`contentstore` owns generic content-addressable storage primitives.

## Rules

1. Keep this package generic. Riot-specific artifact manifests, package exports, and build-lane policy belong in higher layers such as `riot-store`.
2. Treat on-disk layout and hash-addressed paths as compatibility-sensitive.
3. Keep writes atomic where practical. Partial cache entries are worse than misses.
4. Prefer small storage primitives over domain-specific helpers. Higher layers should compose these primitives into their own typed caches.
5. Generic blob and JSON persistence belong here. Typed decoding and host-specific lookup policy do not.

## Validate

`timeout 30 riot build contentstore`
`timeout 180 riot test contentstore:store_tests`
