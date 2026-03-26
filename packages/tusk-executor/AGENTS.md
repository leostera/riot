# tusk-executor AGENTS

`tusk-executor` runs planned build actions and aggregates results.

## Rules

1. Execution order must respect planner dependencies.
2. Package scheduling is scoped and keyed by `Package.key`; keep `Build`, `Runtime`, and `Dev` completion state distinct.
3. Keep caching, worker coordination, and result reporting explicit.
4. Avoid hiding tool invocation failures. Surface structured errors with enough context to debug builds.
5. There is one package coordinator path and one action executor path; do not reintroduce duplicate schedulers without a clearly separate role.
6. When changing concurrency behavior, re-check interactions with `tusk-store` and `tusk-toolchain`.
7. Treat `Build_ctx.available_parallelism` as the only execution concurrency budget and thread it into action execution; avoid package-level worker pools competing for parallelism ownership.

## Validate

`timeout 30 tusk build tusk-executor`
