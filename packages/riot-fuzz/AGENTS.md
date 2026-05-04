# riot-fuzz AGENTS

`riot-fuzz` owns Riot's coverage-guided fuzzing engine and the native AFL-compatible
forkserver boundary.

## Rules

1. Keep CLI concerns out of this package. Callers should pass target binaries, arguments,
   declared corpus/mutator metadata, and event callbacks; `riot-fuzz` owns mutation,
   coverage, corpus loading, and persistence.
2. Keep native code under `native/` and OCaml externals under `src/`, matching the
   package-local native layout used by `packages/kernel`.
3. The durable fuzz state is `.riot/fuzzing/<package>/<suite>/<case>/corpus`,
   `.riot/fuzzing/<package>/<suite>/<case>/crashes`, and crash triage artifacts
   under `.riot/fuzzing/<package>/<suite>/<case>/crash-artifacts`. Do not hide it
   with ignore rules.
4. Prefer the AFL forkserver protocol for instrumented OCaml binaries. A slower spawn-per-input
   fallback may exist only as an explicit fallback path, not as the primary engine.
5. Treat the 64KiB AFL coverage map as package-owned state. Reset it before each run, read it
   after each run, and save inputs only when they crash or expand observed coverage.
6. Reuse `riot-test` for test selection, suite discovery, and `Std.Test.Cli`
   context/argument contracts, including fuzz corpus and mutator metadata parsed from
   `list-tests --json`. Do not duplicate selector parsing in `riot-cli`.
7. Campaign-level parallelism belongs here. Use an owner-managed worker pool so events
   from concurrently running fuzz binaries are serialized before reaching CLI renderers.
8. Corpus minimization deletes coverage-redundant generated inputs. Do not move them
   into a durable redundant directory.
9. Keep OS process capture on `Std.Command`; `riot-fuzz` should not reach into
   `Kernel` just to spawn, time out, or capture replay output.
