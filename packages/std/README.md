# std

Riot's standard library for building real applications.

`std` is the package most Riot users should start with. It is not just a bag of
helpers. It is the application-facing layer that brings together Riot's actor
runtime, supervision model, filesystem and process I/O, networking, data
formats, testing, benchmarking, configuration, logging, and the everyday
utility modules you end up reaching for in real programs.

If you are building an application, a service, a CLI, a tool, or a test suite
in Riot, this is usually the right package to depend on first.

## Install

```sh
riot add std
```

## When to reach for `std`

Use `std` when you want one cohesive surface for:

- actor-oriented concurrency and supervision;
- files, directories, paths, and generic I/O;
- JSON, TOML, CSV, XML, S-expressions, and text encoding;
- TCP, TLS, HTTP data types, URIs, and addresses;
- structured logging, telemetry, configuration, and environment handling;
- testing, benchmarking, worker pools, and background jobs;
- Unicode-aware strings, iterators, collections, graphs, and utilities.

Use a lower-level package like `kernel` only when you specifically need its
lower-level primitives or you are implementing infrastructure that sits under
`std`.

## Quick start

```ocaml
open Std

let load_config = fun path ->
  Fs.read (Path.v path)
  |> Result.and_then Data.Toml.parse_string

let main = fun ~args:_ ->
  let config =
    load_config "riot.toml"
    |> Result.expect ~msg:"failed to load config"
  in
  ignore config;
  Log.info "loaded config successfully";
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
```

## What `std` includes

`std` is broad. The easiest way to approach it is by domain.

### Application and process model

- `Application` for ordered startup and shutdown of multi-part systems.
- `Agent` for simple shared state built on actors.
- `Supervisor` for fault-tolerant process trees.
- `Process`, `Pid`, `Message`, `Task`, `Timer`, and `WorkerPool` for everyday
  process lifecycle, async work, time-based work, and parallel execution.
- `Global` and `System` for runtime-wide state and system queries.

### Filesystem, paths, and I/O

- `Fs` for high-level filesystem work.
- `Fs.File`, `Fs.Fd`, `Fs.Metadata`, `Fs.Permissions`, and `Fs.ReadDir` for
  more focused file and directory control.
- `Fs.FileWatcher` and `Fs.Walker` for watching and traversing directory trees.
- `IO` for generic readers, writers, and streaming-style APIs.
- `Path` for typed, composable filesystem paths.
- `Archive` and `Compress` for tar and gzip operations.

### Networking and protocols

- `Net` as the umbrella networking namespace.
- `Net.Addr`, `Net.Uri`, `Net.TcpClient`, `Net.TcpListener`,
  `Net.TcpServer`, `Net.TcpStream`, and `Net.TlsStream`.
- `Net.Http` and its submodules:
  `Header`, `Method`, `Request`, `Response`, `Status`, and `Version`.

### Data formats and encoding

- `Data.Json`, `Data.Toml`, `Data.Csv`, `Data.Xml`, and `Data.Sexp`.
- `Encoding.Base16`, `Encoding.Base32`, `Encoding.Base64`,
  `Encoding.Base85`, and `Encoding.Octal`.
- `Regex` and `Glob` for matching and pattern compilation.

### Logging, configuration, and observability

- `Log` and its related modules for structured logging.
- `Telemetry` for emitting measurements and events.
- `Config`, `Config.Loader`, `Config.Provider`, `Config.Server`,
  `Config.Spec`, and `Config.Validator`.
- `Env` and `Command` for environment access and external process execution.

### Collections and iteration

- `Collections` and its core structures, including `Deque` and `Heap`.
- `Iter` and iterator/cursor tooling for functional and parsing-oriented
  iteration.
- `Graph`, `Graph.SimpleGraph`, `Graph.Dot`, and `Graph.Mermaid`.

### Time, dates, randomness, and identifiers

- `Time`, `Time.Duration`, `Time.Instant`, and `Time.SystemTime`.
- `Datetime` and `Calendar`.
- `Random`, `UUID`, and `Version`.

### Text, Unicode, and language-facing utilities

- `String`, `Char`, `Uchar`, and `Unicode`.
- Unicode-aware helpers for graphemes, segmentation, width, and UTF-8/UTF-16.
- `Diff`, `ArgParser`, `Sync`, `Type`, `Ref`, `Ptr`, and `Exception`.

### Cryptography and hashing

- `Crypto`, `Crypto.Digest`, and `Crypto.Hasher`.
- Concrete algorithms including `Md5`, `Sha1`, `Sha256`, and `Sha512`.

### Testing and benchmarking

- `Test` and its supporting modules:
  `Assertions`, `Cli`, `FixtureRunner`, `Reporter`, `Snapshot`,
  `TestContext`, and more.
- `Bench` and its runner/reporting surface for repeatable benchmarks.

## Module map

If you like scanning names before digging deeper, this is the condensed map of
the package surface:

- Runtime and application:
  `Application`, `Agent`, `Supervisor`, `Process`, `Pid`, `Message`, `Task`,
  `Timer`, `WorkerPool`, `Global`, `System`
- Filesystem and I/O:
  `Fs`, `Fs.File`, `Fs.Fd`, `Fs.Metadata`, `Fs.Permissions`, `Fs.ReadDir`,
  `Fs.FileWatcher`, `Fs.Walker`, `IO`, `Path`, `Archive`, `Compress`
- Networking:
  `Net`, `Net.Addr`, `Net.Uri`, `Net.TcpClient`, `Net.TcpListener`,
  `Net.TcpServer`, `Net.TcpStream`, `Net.TlsStream`, `Net.Http`
- Data and codecs:
  `Data.Json`, `Data.Toml`, `Data.Csv`, `Data.Xml`, `Data.Sexp`,
  `Encoding`, `Regex`, `Glob`
- Ops and runtime support:
  `Config`, `Log`, `Telemetry`, `Env`, `Command`
- Core utilities:
  `Result`, `Option`, `String`, `Bool`, `Char`, `Int`, `Int32`, `Int64`,
  `Float`, `UUID`, `Version`, `Random`, `Ref`, `Ptr`, `Type`
- Iteration and structure:
  `Collections`, `Iter`, `Graph`, `Diff`, `Sync`
- Time:
  `Time`, `Datetime`, `Calendar`
- Unicode:
  `Unicode`, `Uchar`
- Crypto:
  `Crypto`
- Validation and tooling:
  `Test`, `Bench`, `ArgParser`, `GenStage`

## Examples

`std` already ships with runnable examples. Two good entry points are:

```sh
riot run -p std hello_world
riot run -p std walker_find -- --count-only packages/std/examples
```

The examples directory currently includes:

- `hello_world.ml` for the smallest possible `Std` + `Runtime.run` program.
- `walker_find.ml` for recursive filesystem walking and glob-based filtering.
- `test_uri.ml` for URI parsing and manipulation.
- `unicode_example.ml`, `test_unicode_tables.ml`, and
  `word_navigation_example.ml` for Unicode and text segmentation work.
- `uuid_test.ml` for UUID generation and formatting.
- `bench_demo.ml`, `vector_bench.ml`, and `hashmap_bench.ml` for the benchmark
  surface.
- `debug_query.ml` and `line_wrapping_example.ml` for focused text and tooling
  helpers.

## Where to read next

- `src/std.mli` is the authoritative top-level module guide.
- `examples/` is the fastest way to see package pieces working together.
- `tests/` is useful when you want executable examples of specific edge cases
  and expected behavior.

## Related packages

- `kernel` is the lower-level substrate beneath `std`.
- `actors` is a compatibility facade over `Std.Runtime` while the repo
  finishes migrating off the old package boundary.
- `blink` builds an HTTP client on top of the same model.
- `suri` is the web framework layer when you need HTTP routing and server-side
  application structure.
