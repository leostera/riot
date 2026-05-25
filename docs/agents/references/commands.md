# Command patterns

Use this reference when you know the task and need the right Riot command
shape.

## Scaffold

Create a new workspace:

```sh
riot init
```

Create a new package inside an existing workspace:

```sh
riot new packages/my-package
```

## Dependencies

Add a dependency:

```sh
riot add std
```

Remove a dependency:

```sh
riot rm std
```

Refresh dependencies:

```sh
riot update
riot update std
```

## Build and typecheck

Build the whole workspace:

```sh
riot build
```

Build one package:

```sh
riot build my-package
```

Typecheck the workspace or a package:

```sh
riot check
riot check -p my-package
```

When the task is narrow, prefer package-scoped commands before workspace-wide
ones.

## Run binaries

Run a local binary:

```sh
riot run my-binary
```

Disambiguate by package:

```sh
riot run -p my-package my-binary
```

Forward args after `--`:

```sh
riot run -p my-package my-binary -- --port 8080
```

Riot can also run remote sources:

```sh
riot run https://github.com/owner/tool
```

## Tests and benchmarks

Run all tests:

```sh
riot test
```

Filter by test-case name:

```sh
riot test parser
```

Narrow by suite:

```sh
riot test my-package:parser_tests
```

Run only small, large, or flaky cases:

```sh
riot test --small
riot test --large
riot test --flaky
```

Run benchmarks:

```sh
riot bench
riot bench hashmap
```

## Formatting and fixes

Check formatting:

```sh
riot fmt --check
```

Apply or inspect fixes:

```sh
riot fix --check .
riot fix --apply .
```

## Machine-readable output

Use `--json` when a machine-readable stream is better than scraping human
output:

```sh
riot build --json
riot test --json
riot bench --json
riot fmt --check --json
```
