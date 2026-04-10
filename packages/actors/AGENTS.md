# actors AGENTS

`actors` is a compatibility facade over `Std.Runtime` during the runtime absorption migration.

## Rules

1. Preserve facade compatibility first. Public `Actors.*` names should stay wired to `Std.Runtime` without regressing behavior.
2. Runtime semantics now live in `packages/std/src/runtime`; make scheduler or mailbox changes there, not by re-growing local implementation code in `actors`.
3. Keep `Actor` as the honest primary name and `Process` as a compatibility alias until the repo no longer needs it.
4. `actors` still owns its package-provided `riot-fix` rules under `fix/`; keep those diagnostics aligned with the compatibility surface that remains public here.

## Validate

`timeout 30 riot build actors`
