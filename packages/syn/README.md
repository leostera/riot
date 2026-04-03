# syn

`syn` is Riot's lossless OCaml lexer, parser, and typed CST layer.

It exists for tooling, not compilation. The package keeps the exact parsed
syntax available through Ceibo trees and can also lift successful parses into a
typed `Syn.Cst` for lints, refactors, fixture tests, and future type-directed
tools.

## Current shape

`syn` works in layers:

1. `Lexer` tokenizes source code.
2. `Parser` builds a lossless Ceibo green tree and structured diagnostics.
3. `Ceibo.Red` provides positioned traversal over that lossless tree.
4. `CstBuilder` can lift a successful parse into a typed `Syn.Cst`.

Two invariants matter:

- the parser always returns a Ceibo tree, even for malformed input
- the typed CST only exists when parsing was clean

In other words:

```ocaml
let result = Syn.parse ~filename:(Path.v "file.ml") source in

match result.cst, result.diagnostics with
| Some cst, [] ->
    (* safe to do typed structural analysis *)
    cst
| None, diagnostics ->
    (* stay on diagnostics and the raw lossless tree *)
    ignore diagnostics
```

## Public API

The high-level entrypoints are:

- `Syn.tokenize`
- `Syn.parse`
- `Syn.parse_implementation`
- `Syn.parse_interface`

`Syn.parse` returns a `Parser.parse_result` with:

- `tree`: the lossless Ceibo green tree
- `cst`: `Syn.Cst.source_file option`
- `diagnostics`: structured parse diagnostics

Use `Ceibo.Red.new_root result.tree` when you need direct access to the lossless
syntax tree. Use `result.cst` when you want a typed, grammar-oriented view of a
clean parse.

## CLI helpers

The package also ships CLI surfaces that are used heavily by the fixture suite:

```sh
riot run syn -- print-ceibo path/to/file.ml
riot run syn -- print-cst path/to/file.ml
riot run syn -- parse --json path/to/file.ml
riot run syn -- token-stream --json path/to/file.ml
```

`print-ceibo` always prints the lossless parse result plus diagnostics.
`print-cst` prints a typed CST result when the parse was clean, or a
`parse_error` payload otherwise.

## CST design

`Syn.Cst` is intentionally faithful to the successful Ceibo parse.

- keep exact `syntax_node` and `Token.t` handles when spelling matters
- represent implementation and interface files explicitly
- keep the public tree grammar-oriented rather than rule-oriented
- bail from the lift when a syntax family cannot be reified precisely

That split lets tooling stay ergonomic without losing exact source anchoring for
future refactors and formatting.

## Testing

The main validation surfaces are:

```sh
timeout 30 riot build syn
timeout 180 riot test syn:fixture_tests
timeout 180 riot test syn:diagnostic_tests
timeout 600 riot test syn:cst_fixture_tests
```

The native fixture suites compare:

- `*.expected_lossless.json` for Ceibo output
- `*.expected_cst.json` for typed CST output
- `*.diagnostic` files for diagnostic expectations

The lossless fixture suite snapshots the full parse result, while the CST suite
snapshots the public `print-cst` result contract, including parser failures when
they are the approved behavior for a fixture.

## Contributing

When extending `syn`:

1. preserve the lossless Ceibo tree first
2. add or update typed CST nodes in `cst.ml`
3. lift them in `cst_builder.ml`
4. update `cst_json.ml` if the fixture shape changed
5. refresh or add fixtures in `packages/syn/tests/fixtures`
6. run the fixture runner

If a syntax family cannot be modeled precisely yet, fix the lift or keep working
at the Ceibo layer. Do not reintroduce public placeholder nodes into the CST.
