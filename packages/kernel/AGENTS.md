# kernel AGENTS

`kernel` is Riot's platform abstraction layer. It should stay narrow: portable public contracts over platform-specific implementations, with just enough foundational types to let `std` build on top of it.

## Rules

1. Keep public APIs portable. Platform-specific modules should live underneath public handles such as `Fs.File` or `Async`.
2. Expose portable handles in the public surface; keep `Unix.file_descr` and similar platform-native values inside backend implementation code.
3. Keep `Reader` and `Writer` out of `kernel`; those stay in `std`.
4. Put kernel native code in `packages/kernel/native/`, and keep it Riot-authored.
5. Keep `kernel` implementation dependencies minimal and explicit. If a compiler-owned type such as `string` or `option` must be referenced, isolate that dependency and keep `stdlib` / `unix` out of the portable implementation surface.
6. Prefer explicit error variants over exception-driven APIs. Each public module should own a small typed `error` type, and `Kernel.Error` should wrap those typed errors at package boundaries. Native stubs should return `Result.t` with canonical `SystemError.t` codes.
7. Keep numeric system-error code bridges internal. `Kernel.SystemError` is the public symbolic contract; raw code values belong only in package-internal native plumbing.
8. Build new native stubs mechanically and narrowly. Keep policy in OCaml and C helpers focused on the exact syscall or platform primitive they wrap.
9. Treat OCaml heap pointers as unstable across `caml_enter_blocking_section()`. Blocking native calls should copy `String_val`, `Bytes_val`, or similar heap-backed pointers first, or move the kernel-facing buffer type to owned off-heap storage.
10. Keep `Kernel.IO.IoVec` self-contained and off-heap. Segments are owned byte slices for syscall-facing `readv`/`writev`; `from_string`/`from_bytes` copy into that storage.
11. Keep `Kernel.IO.IoSlice`, `IoVec`, and `Buffer` checked-by-default. Range-sensitive operations should return `Result.t`; reserve `_unchecked` helpers for hot paths that have already established bounds once.
12. Make zero-copy slicing the default within kernel I/O. `IoSlice.sub`, `shift`, and `split_at` should produce shared off-heap views, while `from_*` / `to_*` names remain the explicit copy boundaries into or out of OCaml heap `string` / `bytes`.
13. Prefer async or readiness-driven APIs for capabilities that have a real nonblocking path. Fast metadata/sysinfo calls are fine when they are inherently synchronous.
14. Tests belong in `tests/` and benchmarks in `bench/`, using `std` as a dev-dependency.
15. Start with the Unix backend, but keep the directory structure ready for additional backends under each public module.
16. Encode source-layout and code-hygiene rules in docs, review guidance, or separate tooling.
17. Prefer local backend files such as `env/unix.ml`. If the current planner cannot support a deeper nested split yet, keep the implementation in the public module until the direct backend layout is available.
18. Keep `Kernel.Random.Source` entropy-only. OS randomness belongs here; PRNG policy, distributions, and sampling combinators belong in `std`.
19. Keep runtime GC control narrow. Expose low-level collection primitives and sampled counters through `Kernel.Gc`, but keep benchmarking or tuning policy in `std` and higher layers.
20. A successful process-source registration must retain either a native exit watch or a pending priority-readiness hint. kqueue `ESRCH` can mean exit won the registration race; never silently accept it and leave the caller parked. Pending hints are token-preserving, consumed once, replaced on reregister, removed on deregister/close, and bounded by `Poll.poll ~max_events`. Native process changes retry `EINTR` and capture errno before reentering the OCaml runtime.
