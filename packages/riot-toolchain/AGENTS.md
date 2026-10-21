# riot-toolchain AGENTS

`riot-toolchain` wraps compiler and toolchain discovery and invocation.

## Rules

1. Keep external tool invocation centralized here so compiler knowledge has one owner.
2. Treat path resolution and target-platform handling as core behavior.
3. When adding flags, verify whether they belong to host, target, or package profile configuration.
4. For cross builds, prefer the installed toolchain’s bundled compiler/sysroot or explicit env overrides before falling back to host PATH discovery.
5. Treat cross toolchains as incomplete until the bundled C toolchain and sysroot are present, not just the OCaml binaries.
6. Toolchain downloads must download and extract into a per-process staging directory beside the final path, verify the manifest there, and only then replace the existing install with a rename. Never extract directly into the final toolchain path; an interrupted install must not leave a partial toolchain behind. Use `curl -f` so HTTP errors fail instead of producing a bogus tarball.
7. Toolchain cache fingerprints must change when installed compiler artifacts or bundled sysroot markers change, even if the install path stays the same.
8. Toolchains shipped in release archives should include `manifest.json` with a stable `toolchain_fingerprint`, and toolchain cache hashing should prefer this manifest over probing files on disk.
9. Prepared compiler invocations should stay opaque to callers; keep execution semantics in `riot-toolchain`.
10. OCaml compiler invocations should set an explicit warning baseline; keep warning 49 (`no-cmi-file`) disabled by default unless a caller intentionally opts into it.
11. Dev-only source trees (`tests/`, `examples/`, `bench/`) may use numbered filenames for fixtures and demos. Keep warning 24 (`bad-module-name`) disabled for those source paths while normal library/runtime compilation keeps the standard warning set.
12. Parse OCaml compiler stderr into structured diagnostics here; higher layers may rewrite diagnostic paths, but coarse compiler-output parsing belongs in `Ocamlc`.
13. Reuse `Riot_model.Ocaml_compiler` for shared warning/flag vocabulary and string parsing/rendering.
14. Published toolchain discovery should come from the shared CDN `ocaml/manifest.json`; keep availability parsing in `riot-toolchain`, not in CLI code.
15. Inside riot packages, target identities should stay typed as `Riot_model.Target.t` / `Std.System.TargetTriple.t`. Only stringify targets at external boundaries such as URLs, filesystem layout, shell commands, or published manifest rows.
16. `Ocamlc.run` prepends the absolute compiler's directory to `PATH`, because cross toolchains ship their binutils (for example `x86_64-linux-gnu-as`) beside the compiler and it resolves them by bare name. Keep `PATH` out of `Ocamlc.to_string` and the invocation identity so cache keys do not depend on the caller's environment.
