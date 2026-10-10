# kernel-new AGENTS

`kernel-new` is Riot's new platform abstraction layer. It should stay narrow: portable public contracts over platform-specific implementations, with just enough foundational types to let `std` build on top of it.

## Rules

1. Keep public APIs portable. Platform-specific modules should live underneath public handles such as `Fs.File` or `Async`.
2. Do not expose `Unix.file_descr` or similar platform-native handles in the public surface.
3. Keep `Reader` and `Writer` out of `kernel-new`; those stay in `std`.
4. Put all native code in `native/`, and keep it Riot-authored.
5. Do not depend on `stdlib` or `unix` in `kernel-new` implementation code. If a compiler-owned type such as `string` or `option` must be referenced, keep that dependency explicit and minimal.
6. Prefer explicit error variants over exception-driven APIs. Each public module should own a small typed `error` type, and `Kernel_new.Error` should wrap those typed errors at package boundaries. Native stubs should return `Result.t` with canonical `SystemError.t` codes instead of surfacing platform exceptions into OCaml.
7. Keep numeric system-error code bridges internal. `Kernel_new.SystemError` is the public symbolic contract; raw code values belong only in package-internal native plumbing.
8. Build new native stubs mechanically and narrowly. Avoid monolithic helpers that smuggle policy into C.
9. If a capability has a real async or readiness-driven path, do not add a blocking helper for it in `kernel-new`. Fast metadata/sysinfo calls are fine when they are inherently synchronous.
10. Tests belong in `tests/` and benchmarks in `bench/`, using `std` as a dev-dependency.
11. Start with the Unix backend, but keep the directory structure ready for additional backends under each public module.
12. Source-layout and code-hygiene rules do not belong in unit tests. Encode them in docs, review guidance, or separate tooling instead.
13. Do not add `Backend.ml` shim modules. Prefer local backend files such as `env/unix.ml`; if the current planner cannot support a deeper nested split yet, keep the implementation in the public module rather than introducing a backend shim.
14. Keep `Kernel.Random.Source` entropy-only. OS randomness belongs here; PRNG policy, distributions, and sampling combinators belong in `std`.
15. Keep the tracked IO implementation paths lowercase: `src/io/io.ml`, `src/io/io.mli`, and `src/io/iovec/`. Internal code refers to `Io`; the public facade exposes `Kernel.IO`. Case-insensitive worktrees can conceal incorrect tracked spelling, so validate path changes from a fresh Git export.

16. A successful process-source registration must retain either a native exit watch or a pending priority-readiness hint. kqueue `ESRCH` can mean exit won the registration race; never silently accept it and leave the caller parked. Pending hints are token-preserving, consumed once, replaced on reregister, removed on deregister/close, and bounded by `Poll.poll ~max_events`. Native process changes retry `EINTR` and capture errno before reentering the OCaml runtime.

17. `Fs.File.try_lock_shared` is a nonblocking advisory shared lock, compatible with other shared holders and excluding exclusive holders. The Unix shim uses `flock(LOCK_SH | LOCK_NB)`, retries `EINTR`, and distinguishes contention (`Ok false`) from system errors.

## Validate

`timeout 30 riot build kernel-new`
`timeout 180 riot test -p kernel-new`
`timeout 180 riot bench -p kernel-new --json`
