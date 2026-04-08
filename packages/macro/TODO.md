# Macro Development TODO

## Local Build/Test Loop

When iterating on macros, use this loop:

1. Build the macro-related packages:
   `riot build macro riot-planner riot-executor`
2. Run the macro package unit tests:
   `riot test -p macro`
3. Run planner macro tests:
   `riot test -p riot-planner macro-bearing`
4. Run executor macro integration tests:
   - `riot run -p riot-executor package_builder_tests -- run-tests 'build expands format! in binary end-to-end'`
   - `riot run -p riot-executor package_builder_tests -- run-tests 'build rejects macro provider export drift'`
5. If anything fails, fix and repeat from step 1.

## Next Work

- Add richer parser-driven macro ideas from RFC follow-up: `include_string!`, `include_bytes!`, `env!`, `panic!`, `todo!`, logging shims.
- Add explicit dependency metadata for macros that read files/env so runner caching and invalidation are trustworthy.
- Add `Format_parser` regression cases for additional placeholder forms only after `format!` behavior is intentionally frozen.
- Introduce a small changelog for each macro-capability addition in the manifest contract.
- Keep docs and interface exposure (`macro.mli`) aligned with runtime behavior for every macro package change.

## Bootstrap Evaluation

If macros become syntax-surface features, bootstrap must support macro expansion before normal `riot` is available.

- `bootstrap.py` currently builds `miniriot` and then runs it; `miniriot` currently compiles packages directly.
- `packages/miniriot` currently does not provide the full `macro` execution pipeline.
- That means a full macro-enabled syntax is impossible unless `miniriot` ships a bootstrap-stage macro pass.

Bootstrap work items:

1. Add a tiny function-like macro expander in `miniriot` that can run in bootstrap-only mode.
2. Add minimal manifest discovery for `[riot.macro.provider]` during bootstrap.
3. Expand macro-bearing source in `miniriot` before compilation, with manifest/runtime drift checks based on that bootstrap provider set.
4. Move baseline macro provider surface (`panic!`, `todo!`, basic `format!`, etc.) into the earliest-seeded package (`kernel`/seed package), so `std` and higher layers can depend on it.
5. Keep `miniriot` as a constrained subset; once `riot` exists, hand off to full macro pipeline (`macro` package + generated runner).

### Macro Evaluation Efficiency

- Add time-to-bootstrap baselines:
  - `time ./bootstrap.py`
  - `time ./miniriot`
  - `time riot build macro riot-planner riot-executor`
- Track file/runner cache behavior during repeated builds:
  - clean run
  - second run with no changes
  - touched-provider run (`packages/macro` signature/source change only)
- On macOS, capture macro expansion hot spots with `xctrace`:
  - `xctrace record --template 'Time Profiler' --launch ./miniriot --output /tmp/miniriot.trace`
  - `xctrace record --template 'Time Profiler' --launch riot --output /tmp/riot.trace`
- Compare runs with and without macro-bearing fixtures (empty macro package vs format!/env!-style providers).
- Capture and keep:
  - total wall time (s)
  - number of macro expansions
  - generated-runner materialization count
  - cache hit ratio (provider hash / runner workspace reuse)
- If `xctrace` is unavailable, use a conservative fallback:
  - `time`, command logs, and a small benchmark harness around `macro` package build phases.
