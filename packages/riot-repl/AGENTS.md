# riot-repl AGENTS

`riot-repl` owns the interactive shell around phrase evaluation.

## Rules

1. Keep prompts, line input, terminal setup, and REPL-only directives here.
2. Use `tty` for terminal control and fall back to plain stdin when needed.
3. Delegate package builds, loading, and phrase execution to `riot-eval`.
4. Do not duplicate compile, link, dynlink, or build-planning behavior here.
