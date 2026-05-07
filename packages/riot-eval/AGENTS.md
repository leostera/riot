# riot-eval AGENTS

`riot-eval` owns ephemeral source execution for `riot eval`, script-file
execution, and REPL phrase evaluation.

## Rules

1. Keep terminal UI, prompts, and line editing out of this package.
2. Use `riot-build` to build package closures; do not duplicate build planning
   or artifact discovery logic.
3. One-shot eval and script execution should materialize workspace-attached
   synthetic runner packages and let `riot-build` compile/link them.
4. Keep `#use` prelude parsing here so strings, files, and REPL phrases share
   one package-loading contract.
5. Keep the session API stateful. REPL callers rely on loaded packages and
   previous phrase modules remaining available for later phrases.
6. Keep one-shot APIs explicitly one-shot. They run the generated code block and
   then return to the caller; they do not keep the process alive.
7. Detached eval/repl/run contexts are not attached to a source workspace. Build
   an in-memory workspace from generated eval packages, resolve `std` and
   `#use` packages through pkgs.ml, and let registry materialization reuse the
   normal package cache. Use a stable Riot-managed build target for detached
   package artifacts; keep generated sources in per-request scratch roots.
8. Script files should not eagerly depend on every package in an attached
   workspace. The synthetic runner should build `std`, explicit `#use`
   directives, and explicit CLI package selections; that keeps simple scripts
   isolated from unrelated workspace package failures.
9. Build and package-manager progress should flow through the public event
   callback. Keep rendering in callers such as `riot-cli`; do not print progress
   directly from `riot-eval`.
10. Script files receive forwarded `riot run <file> -- ...` arguments through a
    top-level `args` binding inside the generated script module.
11. REPL package loading must use package artifact exports from `riot-build`
    results. Do not guess archive names or scan output directories for the
    requested package closure; phrase compilation should expose loaded package
    roots through local aliases to the compiled archive root.
