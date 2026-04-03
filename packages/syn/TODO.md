# syn TODO

## CST Builder Perf

The pathological slowdown is in `Syn.build_cst`, specifically in
`packages/syn/src/cst_builder.ml`.

### Known Baseline

- Slow repro fixture:
  - `packages/krasny/tests/fixtures/0988_sparse_index_release_decoder_tuple_match.ml`
- Last trustworthy old baseline:
  - `syn print-cst 0988`: about `53s`
  - `build_cst` inside that: about `46s`
- Current optimized baseline:
  - `syn print-cst 0988`: `43.346s`

So the latest optimization slice improved the bad repro by about:
- `9.7s`
- roughly `18%`

That is real progress, but it is still far too slow.

### Correctness / Workflow Rules

1. Only run one `riot` or `syn` process at a time.
2. On macOS, do not rebuild while a native `syn` binary is running.
   - If the binary changes while running, macOS can get into a bad state and kill or corrupt execution.
3. Prefer this serial loop:
   - `riot clean`
   - `riot build syn`
   - run one timing command
   - run one test command
4. Do not trust timings collected while a build is also happening.

### Measurement Loop

1. Clean and rebuild:
   - `timeout 120 riot clean`
   - `timeout 240 riot build syn`

2. Measure the slow repro:
   - `./_build/debug/aarch64-apple-darwin/out/syn/syn print-cst packages/krasny/tests/fixtures/0988_sparse_index_release_decoder_tuple_match.ml > /dev/null`

3. If a change looks promising, profile it:
   - `xcrun xctrace record --template 'Time Profiler' --output /tmp/syn-print-cst.trace --time-limit 20s --launch -- ./_build/debug/aarch64-apple-darwin/out/syn/syn print-cst packages/krasny/tests/fixtures/0988_sparse_index_release_decoder_tuple_match.ml`

4. Export profiler data:
   - `xcrun xctrace export --input /tmp/syn-print-cst.trace --toc`
   - `xcrun xctrace export --input /tmp/syn-print-cst.trace --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]' --output /tmp/syn-print-cst-time-profile.xml`

5. Validate correctness after each coherent slice:
   - `timeout 240 riot build syn`
   - `timeout 180 riot test syn:fixture_tests`
   - `timeout 600 riot test syn:cst_fixture_tests`

### Current Findings

- `print-ceibo` is fast.
- `cst_json` is fast.
- `build_cst` is the pathological step.
- Native profiling with `xctrace` is useful after setting debug profile to native.

### Latest xctrace Hot Frames

From the current `0988` trace, the top OCaml hotspots are:

- `camlSyn__Cst_builder$expression_from_node_3919`
- `camlCeibo__Red$fun_1031`
- `camlSyn__Cst_builder$apply_argument_from_node_3906`
- `camlCeibo__Red$fold_children_367`
- `camlSyn__Cst_builder$normalize_greedy_labeled_argument_3911`
- `camlSyn__Cst_builder$normalize_greedy_tuple_argument_value_3910`
- `camlSyn__Cst_builder$string_delimiter_and_contents_1833`
- `camlSyn__Cst_builder$token_starts_with_uppercase_2950`
- `camlSyn__Cst_builder$constant_from_parts_2838`
- `camlSyn__Cst_builder$literal_tokens_from_node_2800`
- `camlCeibo__Red$direct_tokens_788`
- `camlSyn__Cst_builder$ident_path_from_node_2908`

Interpretation:

- literals were a real slice, but they are no longer the main bottleneck
- the next biggest target is the apply / greedy-argument normalization path
- repeated `direct_tokens` / `direct_nodes` scans under `expression_from_node` are still expensive

### Latest Uncommitted Optimization Slice

Currently uncommitted changes in `packages/syn/src/cst_builder.ml` do this:

1. Literal lifting:
   - replace old `literal_token_from_node`
   - add `literal_tokens_from_node`
   - avoid the old repeated literal-sign rescans

2. Constructor patterns:
   - reuse one direct-child scan for constructor pattern existentials and arguments

3. Match cases:
   - cache direct tokens once in `match_case_from_node`
   - stop rescanning the same case node repeatedly for `|`, `when`, and `->`

These changes build cleanly and produced the current `43.346s` timing.

### Next Suggested Order

1. Optimize the apply / greedy-argument path:
   - `apply_argument_from_node`
   - `collect_apply_arguments`
   - `rebuild_apply_chain`
   - `normalize_greedy_labeled_argument`
   - `normalize_greedy_tuple_argument_value`

2. Re-measure `0988`

3. Run `riot test syn:fixture_tests`

4. Run `riot test syn:cst_fixture_tests`

5. If still too slow, inspect:
   - `ident_path_from_node`
   - `token_starts_with_uppercase`
   - remaining `Ceibo.Red.direct_tokens` / `fold_children` callsites in hot expression paths
