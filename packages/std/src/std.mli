(** # Std - Riot's Comprehensive Standard Library

    A complete standard library providing modern primitives for building robust,
    concurrent, fault-tolerant applications in OCaml.

    ## Table of Contents

    ### Quick Navigation
    
    - [Quick Start](#quick-start) - Get started quickly
    - [Browse by Category](#browse-by-category) - Organized by domain
    - [Find by Use Case](#find-by-use-case) - "I want to..." index
    - [Alphabetical Index](#alphabetical-index) - All modules A-Z
    - [Module Hierarchy](#module-hierarchy) - Complete tree view

    # Quick Start

    Common patterns to get you started:

    **Reading and writing files:**
    ```ocaml
    open Std

    (* Read file *)
    let content = Fs.read (Path.v "config.toml")
      |> Result.expect ~msg:"Config file required"

    (* Write file *)
    Fs.write "Hello, World!" (Path.v "output.txt")
      |> Result.expect ~msg:"Failed to write"

    (* List directory *)
    match Fs.read_dir (Path.v "src") with
    | Ok iter -> Iterator.iter (fun path -> 
        println (Path.to_string path)) iter
    | Error e -> eprintln "Error reading directory"
    ```

    **Error handling:**
    ```ocaml
    (* Option for missing values *)
    let maybe_user = find_user id in
    let name = maybe_user 
      |> Option.map (fun u -> u.name)
      |> Option.unwrap_or ~default:"Guest"

    (* Result for recoverable errors *)
    let result = 
      parse_config path
      |> Result.and_then validate_config
      |> Result.map_err (fun e -> format "Config error: %s" e)
    ```

    **Working with collections:**
    ```ocaml
    (* Create collections using helpers *)
    let numbers = vec [1; 2; 3; 4; 5] in
    let words = set ["hello"; "world"] in
    let config = map [("host", "localhost"); ("port", "8080")] in

    (* Or use modules directly *)
    let v = Collections.Vector.of_list [1; 2; 3] in
    Collections.Vector.push v 4;
    Collections.Vector.get v 0 (* Some 1 *)
    ```

    **Actor concurrency:**
    ```ocaml
    (* Simple state server *)
    let counter = Agent.start (fun () -> 0) in
    Agent.update counter (fun n -> n + 1);
    let value = Agent.get counter (fun n -> n) in

    (* Process supervision *)
    let worker_spec = Supervisor.child_spec
      ~id:"worker_1"
      ~start:(fun () -> spawn my_worker)
      () in
    
    let sup = Supervisor.start_link
      ~strategy:OneForOne
      ~children:[worker_spec]
      ()
    ```

    **Parsing data formats:**
    ```ocaml
    (* JSON *)
    let json = Data.Json.of_string {|{"name": "Alice", "age": 30}|}
      |> Result.expect ~msg:"Invalid JSON" in
    let name = Data.Json.get_field "name" json
      |> Option.and_then Data.Json.get_string in

    (* TOML config files *)
    let config = Data.Toml.parse "config.toml"
      |> Result.expect ~msg:"Bad config" in
    let table = Data.Toml.get_table config
    ```

    # Browse by Category

    ## Core Types & Error Handling

    - {!Result} - **When:** You need explicit error handling with typed errors.
      For operations that can fail in expected ways (file I/O, parsing, network).
      *Example:* `Fs.read path |> Result.map parse`
    
    - {!Option} - **When:** A value might not exist and that's normal.
      For optional fields, lookups that may not find anything, nullable values.
      *Example:* `HashMap.get map key |> Option.unwrap_or ~default:0`
    
    - {!Path} - **When:** Working with filesystem paths.
      Provides type safety, UTF-8 validation, and cross-platform path operations.
      *Example:* `let config = home / Path.v ".config" / Path.v "app.toml"`
    
    - {!String} - **When:** Processing text with UTF-8 character iteration.
      *Example:* `String.into_iter text |> Iterator.map Unicode.Rune.to_upper`
    
    - {!Int}, {!Int32}, {!Int64}, {!Float}, {!Bool}, {!Char} - 
      **When:** Working with primitive types. Extended with parsing, formatting,
      and utility functions.
    
    - {!UUID} - **When:** You need globally unique identifiers.
      For entity IDs, request tracking, distributed system coordination.
      *Example:* `let id = UUID.v4 () in UUID.to_string id`
    
    - {!Version} - **When:** Parsing or comparing semantic versions.
      For dependency management, feature flags, API versioning.
      *Example:* `Version.compare (Version.parse "1.2.3") (Version.parse "1.3.0")`
    
    - {!Ref} - **When:** You need unique, opaque, type-witnessing references.
      For ensuring type safety across module boundaries.
    
    - {!Ptr} - **When:** You need physical equality checks or pointer operations.
      Rarely needed in normal application code.
    
    - {!Type} - **When:** Working with type-level programming utilities.

    ## Collections & Data Structures

    - {!Collections} - **Parent module** containing all collection types.
    
    - {!Collections.Vector} - **When:** You need a growable array with O(1) indexed access.
      For lists where you need random access, frequent appends, or size changes.
      *Example:* `let v = vec [1; 2; 3] in Vector.push v 4`
    
    - {!Collections.HashMap} - **When:** You need O(1) key-value lookups.
      For caches, indexes, configuration maps, counting occurrences.
      *Example:* `let counts = HashMap.create () in HashMap.insert counts key 1`
    
    - {!Collections.HashSet} - **When:** You need unique values with O(1) membership tests.
      For deduplication, seen-before tracking, set operations.
      *Example:* `let visited = HashSet.create () in HashSet.insert visited node`
    
    - {!Collections.Queue} - **When:** You need FIFO processing.
      For task queues, breadth-first search, event buffers.
      *Example:* `Queue.enqueue q task; Queue.dequeue q`
    
    - {!Collections.Deque} - **When:** You need efficient push/pop at both ends.
      For undo/redo, sliding windows, double-ended queues.
      *Example:* `Deque.push_front dq item; Deque.pop_back dq`
    
    - {!Collections.Heap} - **When:** You need a priority queue.
      For task scheduling, A* search, finding top-k items.
      *Example:* `Heap.push heap (priority, task); Heap.pop heap`
    
    - {!Collections.List} - **When:** You need immutable linked lists.
      For functional programming, pattern matching, recursive algorithms.

    ## Time & Date

    - {!Time} - **Parent module** containing all time-related types.
    
    - {!Time.Duration} - **When:** Representing time spans or intervals.
      For timeouts, delays, measuring elapsed time, scheduling.
      *Example:* `let timeout = Duration.of_sec 30 in sleep timeout`
    
    - {!Time.Instant} - **When:** Measuring elapsed time monotonically.
      For benchmarking, profiling, timeout tracking (not affected by clock changes).
      *Example:* `let start = Instant.now () in (* work *) Instant.elapsed start`
    
    - {!Time.SystemTime} - **When:** Working with wall-clock time.
      For timestamps, logging, scheduling at specific times.
      *Example:* `let now = SystemTime.now () in SystemTime.to_unix now`
    
    - {!Datetime} - **When:** Working with calendar dates and times.
      For date arithmetic, formatting, parsing human-readable dates.
      *Example:* `Datetime.parse "2024-01-15T10:30:00Z"`
    
    - {!Timer} - **When:** Setting up timed events in actor systems.
      For periodic tasks, delayed execution, timeouts.
      *Example:* `Timer.send_after duration pid message`

    ## Filesystem & I/O

    - {!Fs} - **When:** Performing filesystem operations.
      For reading/writing files, directory operations, metadata queries.
      *Example:* `Fs.read path, Fs.create_dir_all dir, Fs.copy ~src ~dst`
    
    - {!Fs.File} - **When:** You need fine-grained file control.
      For streaming large files, append mode, specific permissions.
      *Example:* `File.open_ path |> Result.and_then (File.read_all)`
    
    - {!Fs.Fd} - **When:** Working with low-level file descriptors.
      Rarely needed - use {!Fs} or {!Fs.File} instead.
    
    - {!Fs.Permissions} - **When:** Checking or setting Unix file permissions.
      *Example:* `Fs.set_permissions path Permissions.executable`
    
    - {!Fs.Metadata} - **When:** Querying file information (size, type, times).
      *Example:* `Fs.metadata path |> Result.map Metadata.len`
    
    - {!Fs.ReadDir} - **When:** Iterating over directory contents.
      Usually accessed via `Fs.read_dir` which returns an iterator.
    
    - {!Fs.FileWatcher} - **When:** Watching files for changes.
      For hot-reload, build systems, monitoring configuration files.
      *Example:* `FileWatcher.watch paths ~on_change:(fun p -> reload p)`
    
    - {!IO} - **When:** Working with generic I/O abstractions.
      For Reader/Writer traits, vectored I/O operations.

    ## Networking

    - {!Net} - **Parent module** for all networking functionality.
    
    - {!Net.TcpServer} - **When:** Building a TCP server that accepts connections.
      For HTTP servers, game servers, RPC servers, any service accepting TCP.
      *Example:* `TcpServer.start ~port:8080 ~handler`
    
    - {!Net.TcpListener} - **When:** You need low-level control over accepting connections.
      Usually {!Net.TcpServer} is easier.
    
    - {!Net.TcpStream} - **When:** Making TCP client connections or handling accepted ones.
      For database clients, service-to-service communication, any TCP protocol.
      *Example:* `TcpStream.connect addr |> Result.and_then (send_request)`
    
    - {!Net.TcpClient} - **When:** Building reusable TCP client abstractions.
    
    - {!Net.TlsStream} - **When:** You need TLS/SSL encrypted connections.
      For HTTPS, secure database connections, encrypted RPC.
    
    - {!Net.Addr} - **When:** Parsing or constructing network addresses.
      *Example:* `Addr.of_host_and_port ~host:"example.com" ~port:443`
    
    - {!Net.Uri} - **When:** Parsing or building URIs/URLs.
      *Example:* `Uri.parse "https://example.com/api/v1/users"`
    
    - {!Net.Http} - **Parent module** for HTTP functionality.
    
    - {!Net.Http.Request} - **When:** Building HTTP requests.
      *Example:* `Request.create ~method_:GET ~uri:(Uri.parse url)`
    
    - {!Net.Http.Response} - **When:** Building HTTP responses.
      *Example:* `Response.create ~status:OK ~body:"Hello"`
    
    - {!Net.Http.Header} - **When:** Working with HTTP headers.
      *Example:* `Header.add headers "Content-Type" "application/json"`
    
    - {!Net.Http.Method} - **When:** Representing HTTP methods (GET, POST, etc.).
    
    - {!Net.Http.Status} - **When:** Working with HTTP status codes.
      *Example:* `Status.is_success 200, Status.reason_phrase 404`
    
    - {!Net.Http.Version} - **When:** Handling HTTP protocol versions.

    ## Data Formats & Encoding

    - {!Data} - **Parent module** for all data format operations.
    
    - {!Data.Json} - **When:** Parsing/generating JSON for APIs or config.
      For REST APIs, JSON-RPC, configuration files, data interchange.
      *Example:* `Json.of_string str |> Result.and_then parse_user`
    
    - {!Data.Toml} - **When:** Reading TOML configuration files.
      Simpler and more readable than JSON for application config.
      *Example:* `Toml.parse "config.toml" |> Result.map extract_settings`
    
    - {!Data.Sexp} - **When:** Working with S-expressions.
      For Lisp-like data, configuration, serialization.
    
    - {!Data.Csv} - **When:** Processing CSV data files.
      For spreadsheet data, data exports, bulk imports.
      *Example:* `Csv.parse file |> List.map parse_row`
    
    - {!Data.Xml} - **When:** Parsing or generating XML.
      For SOAP, RSS, SVG, legacy APIs.
    
    - {!Encoding.Base16} / {!Encoding.Hex} - **When:** Hex encoding/decoding.
      For displaying binary data, hash digests, debugging.
      *Example:* `Encoding.Hex.encode bytes |> println`
    
    - {!Encoding.Base32} - **When:** Base32 encoding (URL-safe, case-insensitive).
      For IDs that must be human-readable and case-insensitive.
    
    - {!Encoding.Base64} - **When:** Base64 encoding/decoding.
      For embedding binary in text (emails, JSON), basic auth headers.
      *Example:* `Encoding.Base64.encode image_bytes`
    
    - {!Encoding.Base85} - **When:** Ascii85/Base85 encoding (more compact).
      For embedding binary when space matters (PostScript, PDF).

    - {!Encoding.Octal} - **When:** Octal numeric text for modes and archive fields.
      *Example:* `Encoding.Octal.encode_int 0o755`

    ## Cryptography

    - {!Crypto} - **When:** You need cryptographic hashing.
      For content-addressable storage, integrity checking, checksums.
      *Example:* `Crypto.hash_string data |> Crypto.Digest.hex`
    
    - {!Crypto.Sha256} - **When:** You need SHA-256 specifically.
      Most common secure hash, good for general use.
      *Example:* `let h = Sha256.create () |> Sha256.update data |> Sha256.finish`
    
    - {!Crypto.Sha512} - **When:** You need stronger hashing (512-bit).
    
    - {!Crypto.Md5} - **When:** You need MD5 (LEGACY ONLY).
      NOT cryptographically secure. Use only for checksums, never passwords.
    
    - {!Crypto.Hasher} - **When:** Building hash algorithms or abstractions.
    
    - {!Crypto.Digest} - **When:** Formatting hash digests.
      *Example:* `Digest.hex hash, Digest.base64 hash`

    ## Process Management (Actor/OTP Patterns)

    - {!Process} - **When:** Working with process state and lifecycle.
      For process monitoring, linking, exit handling.
      *Example:* `Process.monitor pid, Process.is_alive pid`
    
    - {!Pid} - **When:** Working with process identifiers.
      *Example:* `send pid message, Pid.to_string pid`
    
    - {!Message} - **When:** Defining extensible message types for actors.
      For building type-safe actor protocols.
    
    - {!Agent} - **When:** You need simple concurrent state access.
      For counters, caches, shared configuration, session storage.
      *Example:* `let cache = Agent.start (fun () -> HashMap.create ())`
    
    - {!Supervisor} - **When:** Building fault-tolerant process trees.
      For applications that must recover from crashes automatically.
      *Example:* `Supervisor.start_link ~strategy:OneForOne ~children`
    
    - {!Supervisor.Dynamic} - **When:** Managing thousands of dynamic children.
      For connection pools, worker pools, per-user sessions.
      *Example:* `Dynamic.start_child sup ~start:(fun () -> spawn worker)`
    
    - {!Task} - **When:** Running async operations.
      For parallel work, background jobs, fire-and-forget tasks.
      *Example:* `Task.async (fun () -> expensive_computation ())`
    
    - {!WorkerPool} - **When:** Distributing work across multiple workers.
      For CPU-bound parallel work, batch processing.
      *Example:* `WorkerPool.map pool ~f:process_item items`
    
    - {!Application} - **When:** Building multi-application systems.
      Handles dependency resolution and ordered startup/shutdown.
      *Example:* `Application.start_applications [db_app; web_app]`

    ## I/O & System

    - {!Command} - **When:** Running external programs.
      For build tools, CLI utilities, shell scripts.
      *Example:* `Command.create "git" ["status"] |> Command.run`
    
    - {!Env} - **When:** Reading environment variables or system info.
      *Example:* `Env.get "DATABASE_URL" |> Result.unwrap_or ~default:local`
    
    - {!System} - **When:** Querying system information.
      For OS detection, resource limits, system paths.
    
    - {!Log} - **When:** Adding structured logging to your application.
      *Example:* `Log.info "User %s logged in" username; Log.set_level Debug`
    
    - {!Exception} - **When:** Working with exceptions programmatically.
      For exception handling utilities, custom exceptions.
    
    - {!Random} - **When:** Generating random values.
      *Example:* `Random.int 100, Random.choice list`

    ## Iteration & Cursors

    - {!Iter} - **Parent module** for iteration utilities.
    
    - {!Iter.Iterator} - **When:** You need immutable, backtrackable iteration.
      For functional pipelines where you might need to retry or branch.
      *Example:* `Iterator.map f iter |> Iterator.filter p |> Iterator.collect`
    
    - {!Iter.MutIterator} - **When:** You need efficient single-pass iteration.
      For streaming large datasets, one-time traversal.
      *Example:* `MutIterator.fold (fun acc x -> acc + x) 0 iter`
    
    - {!Iter.Cursor} - **When:** Parsing strings with backtracking.
      For hand-written parsers, protocol parsing.
      *Example:* `Cursor.take_while cursor Char.is_digit`
    
    - {!Iter.MutCursor} - **When:** Parsing strings efficiently (single-pass).
      For performance-critical parsers.

    ## Graph & Visualization

    - {!Graph} - **Parent module** for graph functionality.
    
    - {!Graph.SimpleGraph} - **When:** Building dependency graphs with topo sort.
      For build systems, module dependencies, task ordering.
      *Example:* `SimpleGraph.topo_sort graph`
    
    - {!Graph.Dot} - **When:** Generating Graphviz DOT diagrams.
      For visualizing graphs in tools like Graphviz.
      *Example:* `Dot.create ~name:"deps" |> Dot.to_string`
    
    - {!Graph.Mermaid} - **When:** Generating Mermaid.js diagrams.
      For documentation, markdown diagrams, web-based visualization.
      *Example:* `Mermaid.create ~direction:LR () |> Mermaid.to_string`

    ## Testing

    - {!Test} - **When:** Writing unit tests.
      *Example:* `Test.case "addition" (fun () -> assert_equal ~expected:4 ~actual:(2+2))`
    
    - {!Test.Assertions} - **When:** Making test assertions.
      *Example:* `assert_true condition; assert_ok result; assert_error bad_result`
    
    - {!Test.Cli} - **When:** Running tests from command line.
    
    - {!Test.Reporter} - **When:** Customizing test output formats.
      Built-in: TAP, JUnit XML, JSON, Pretty.

    ## Unicode & Text

    - {!Unicode} - **When:** Working with Unicode text properly.
      For international text, emoji, terminal width, text segmentation.
      *Example:* `String.grapheme_count "👨‍👩‍👧‍👦", String.width "你好"`
    
    - {!Unicode.Rune} - **When:** Working with individual Unicode code points.
      *Example:* `Rune.is_letter r, Rune.to_upper r`
    
    - {!Unicode.Grapheme} - **When:** Working with user-perceived characters.
      For cursor movement, text editing, character counting.
    
    - {!Unicode.Utf8} - **When:** Encoding/decoding UTF-8.
      *Example:* `Utf8.decode_rune str pos`
    
    - {!Unicode.Segmentation} - **When:** Breaking text into words/sentences/lines.
      For text editors, word wrapping, search.
      *Example:* `Segmentation.wrap_lines ~width:80 text`

    ## Utilities & Misc

    - {!ArgParser} - **When:** Parsing command-line arguments.
      For building CLI tools with flags, options, subcommands.
      *Example:* `ArgParser.parse spec args`
    
    - {!Diff} - **When:** Computing differences between data structures.
      For change detection, version control, auditing.
      *Example:* `Diff.compute old new |> Diff.changes`
    
    - {!Telemetry} - **When:** Adding instrumentation and metrics.
      For monitoring, profiling, observability.
      *Example:* `Telemetry.emit "http.request" metadata`
    
    - {!Sync} - **When:** Using synchronization primitives.
      For mutable cells, mutexes, condition variables.
      *Example:* `let cell = cell 0 in Cell.update cell (fun n -> n + 1)`
    
    - {!GenStage} - **When:** Building back-pressure aware pipelines.
      For streaming data processing with flow control.

    # Find by Use Case

    ## "I Want To..."

    **...Read/Write Files**
    → {!Fs.read}, {!Fs.write}, {!Fs.File}, {!Path}

    **...Parse Configuration Files**
    → {!Data.Toml} for TOML, {!Data.Json} for JSON, {!Env} for env vars

    **...Handle Errors Gracefully**
    → {!Result} for recoverable errors, {!Option} for missing values, {!Exception}

    **...Work with Collections**
    → {!Collections.Vector} for arrays, {!Collections.HashMap} for lookups,
      {!Collections.Queue} for FIFO, {!Collections.HashSet} for uniqueness

    **...Build a TCP Server**
    → {!Net.TcpServer}, {!Net.TcpListener}, {!Net.TcpStream}

    **...Make HTTP Requests**
    → {!Net.Http.Request}, {!Net.Http.Response}, {!Net.TcpStream}

    **...Parse JSON/XML/CSV**
    → {!Data.Json}, {!Data.Xml}, {!Data.Csv}, {!Data.Sexp}

    **...Hash Data**
    → {!Crypto.Sha256}, {!Crypto.hash_string}, {!Crypto.Digest}

    **...Measure Time/Benchmark**
    → {!Time.Instant} for elapsed time, {!Time.Duration} for intervals

    **...Schedule Tasks**
    → {!Timer}, {!Task}, {!WorkerPool}

    **...Build Fault-Tolerant Systems**
    → {!Supervisor}, {!Supervisor.Dynamic}, {!Application}

    **...Manage Shared State**
    → {!Agent} for simple state, {!Sync.Cell} for mutable cells

    **...Process Text/Unicode**
    → {!String}, {!Unicode}, {!Unicode.Segmentation}

    **...Run External Commands**
    → {!Command}

    **...Parse Command-Line Args**
    → {!ArgParser}

    **...Write Tests**
    → {!Test}, {!Test.Assertions}

    **...Log Messages**
    → {!Log}

    **...Generate UUIDs**
    → {!UUID}

    **...Encode/Decode Data**
    → {!Encoding.Base64}, {!Encoding.Hex}, {!Encoding.Base32}, {!Encoding.Octal}

    **...Visualize Graphs**
    → {!Graph.Dot}, {!Graph.Mermaid}, {!Graph.SimpleGraph}

    **...Process Large Datasets**
    → {!Iter.MutIterator}, {!Fs.File} for streaming, {!WorkerPool}

    **...Watch Files for Changes**
    → {!Fs.FileWatcher}

    **...Work with Dates**
    → {!Datetime}, {!Time.SystemTime}

    # Alphabetical Index

    - {!Agent} - Simple concurrent state wrapper
    - {!Application} - Multi-app system with dependency resolution
    - {!ArgParser} - Command-line argument parsing
    - {!Bool} - Boolean operations
    - {!Char} - Character operations
    - {!Collections} - Data structures (Vector, HashMap, Queue, etc.)
    - {!Command} - External program execution
    - {!Crypto} - Cryptographic hashing
    - {!Data} - Data format parsing (JSON, TOML, CSV, etc.)
    - {!Datetime} - Calendar date and time
    - {!Diff} - Compute differences in data structures
    - {!Encoding} - Binary/text and numeric encodings
    - {!Env} - Environment variables
    - {!Exception} - Exception handling utilities
    - {!Float} - Floating-point operations
    - {!Fs} - Filesystem operations
    - {!GenStage} - Back-pressure aware pipelines
    - {!Graph} - Graph data structures and visualization
    - {!Int} - Integer operations
    - {!Int32} - 32-bit integer operations
    - {!Int64} - 64-bit integer operations
    - {!IO} - Generic I/O abstractions
    - {!Iter} - Iteration and cursor utilities
    - {!List} - Immutable linked lists (alias to Collections.List)
    - {!Log} - Structured logging
    - {!Message} - Extensible message type for actors
    - {!Net} - Networking (TCP, HTTP, TLS)
    - {!Option} - Optional values
    - {!Path} - Type-safe filesystem paths
    - {!Pid} - Process identifiers
    - {!Process} - Process state and operations
    - {!Ptr} - Physical equality and pointers
    - {!Random} - Random value generation
    - {!Ref} - Unique opaque references
    - {!Result} - Error handling with Result type
    - {!String} - UTF-8 string operations
    - {!Supervisor} - OTP-style process supervision
    - {!Sync} - Synchronization primitives
    - {!System} - System information
    - {!Task} - Asynchronous task execution
    - {!Telemetry} - Instrumentation and metrics
    - {!Test} - Testing framework
    - {!Time} - Time measurement and duration
    - {!Timer} - Timer operations
    - {!Type} - Type-level utilities
    - {!Unicode} - Unicode text processing
    - {!UUID} - Universally unique identifiers
    - {!Version} - Semantic versioning
    - {!WorkerPool} - Parallel execution with worker pools

    # Module Hierarchy

    ```
    Std
    ├── Core Types
    │   ├── Result - Error handling with typed errors
    │   ├── Option - Optional values (None/Some)
    │   ├── String - UTF-8 strings with iteration
    │   ├── Int, Int32, Int64 - Integer types
    │   ├── Float - Floating-point numbers
    │   ├── Bool - Booleans
    │   ├── Char - Characters
    │   ├── Path - Type-safe filesystem paths
    │   ├── UUID - Unique identifiers
    │   ├── Version - Semantic versions
    │   ├── Ref - Opaque references
    │   ├── Ptr - Pointer operations
    │   └── Type - Type utilities
    │
    ├── Collections
    │   ├── Collections (parent)
    │   ├── Vector - Growable arrays
    │   ├── HashMap - Hash tables
    │   ├── HashSet - Unique value sets
    │   ├── Queue - FIFO queues
    │   ├── Deque - Double-ended queues
    │   ├── Heap - Binary heaps
    │   └── List - Linked lists
    │
    ├── Time & Date
    │   ├── Time (parent)
    │   ├── Duration - Time spans
    │   ├── Instant - Monotonic time
    │   ├── SystemTime - Wall-clock time
    │   ├── Datetime - Calendar operations
    │   └── Timer - Timed events
    │
    ├── Filesystem
    │   ├── Fs (parent) - Main FS operations
    │   ├── File - File operations
    │   ├── Fd - File descriptors
    │   ├── Permissions - Unix permissions
    │   ├── Metadata - File metadata
    │   ├── ReadDir - Directory iteration
    │   └── FileWatcher - File change watching
    │
    ├── Networking
    │   ├── Net (parent)
    │   ├── TcpServer - TCP server
    │   ├── TcpListener - Accept connections
    │   ├── TcpStream - TCP connections
    │   ├── TcpClient - TCP client
    │   ├── TlsStream - TLS/SSL
    │   ├── Addr - Network addresses
    │   ├── Uri - URI/URL parsing
    │   └── Http
    │       ├── Request - HTTP requests
    │       ├── Response - HTTP responses
    │       ├── Header - HTTP headers
    │       ├── Method - HTTP methods
    │       ├── Status - Status codes
    │       └── Version - HTTP versions
    │
    ├── Data Formats
    │   ├── Data (parent)
    │   ├── Json - JSON parsing
    │   ├── Toml - TOML config files
    │   ├── Sexp - S-expressions
    │   ├── Csv - CSV data
    │   └── Xml - XML parsing
    │
    ├── Encodings
    │   ├── Encoding (parent)
    │   ├── Base16 - Hex encoding
    │   ├── Hex - Alias for Base16
    │   ├── Base32 - Base32 encoding
    │   ├── Base64 - Base64 encoding
    │   ├── Base85 - Ascii85 encoding
    │   └── Octal - Octal numeric text encoding
    │
    ├── Cryptography
    │   ├── Crypto (parent)
    │   ├── Sha256 - SHA-256 hashing
    │   ├── Sha512 - SHA-512 hashing
    │   ├── Md5 - MD5 hashing
    │   ├── Hasher - Hash interface
    │   └── Digest - Digest formatting
    │
    ├── Actors/OTP
    │   ├── Process - Process lifecycle
    │   ├── Pid - Process IDs
    │   ├── Message - Actor messages
    │   ├── Agent - Simple state server
    │   ├── Supervisor - Fault tolerance
    │   │   └── Dynamic - Dynamic supervisor
    │   ├── Task - Async operations
    │   ├── WorkerPool - Parallel work
    │   └── Application - Multi-app systems
    │
    ├── I/O & System
    │   ├── IO - I/O abstractions
    │   ├── Command - Run external programs
    │   ├── Env - Environment variables
    │   ├── System - System info
    │   ├── Log - Structured logging
    │   ├── Exception - Exception handling
    │   └── Random - Random generation
    │
    ├── Iteration
    │   ├── Iter (parent)
    │   ├── Iterator - Immutable iteration
    │   ├── MutIterator - Mutable iteration
    │   ├── Cursor - Immutable cursor
    │   └── MutCursor - Mutable cursor
    │
    ├── Graph
    │   ├── Graph (parent)
    │   ├── SimpleGraph - Dependency graphs
    │   ├── Dot - Graphviz format
    │   └── Mermaid - Mermaid.js format
    │
    ├── Testing
    │   ├── Test (parent)
    │   ├── Assertions - Test assertions
    │   ├── Cli - Test runner
    │   └── Reporter - Output formats
    │       ├── TAP - TAP format
    │       ├── JUnit - JUnit XML
    │       ├── JSON - JSON output
    │       └── Pretty - Pretty print
    │
    ├── Unicode
    │   ├── Unicode (parent)
    │   ├── Rune - Code points
    │   ├── Grapheme - User characters
    │   ├── Utf8 - UTF-8 encoding
    │   └── Segmentation - Text breaking
    │
    └── Utilities
        ├── ArgParser - CLI arguments
        ├── Diff - Difference computation
        ├── Telemetry - Instrumentation
        ├── Sync - Synchronization
        └── GenStage - Backpressure pipelines
    ```

    # Common Patterns

    ## Error Handling Strategies

    ```ocaml
    (* Use Result for operations that can fail *)
    let safe_divide x y =
      if y = 0 then Error "Division by zero"
      else Ok (x / y)

    (* Chain operations that might fail *)
    let process_file path =
      Fs.read path
      |> Result.and_then parse_config
      |> Result.and_then validate_config
      |> Result.map apply_config

    (* Provide defaults for missing values *)
    let port = 
      Env.get "PORT"
      |> Result.and_then Int.of_string
      |> Result.unwrap_or ~default:8080

    (* Use Option for lookups *)
    let user_name id =
      HashMap.get users id
      |> Option.map (fun u -> u.name)
      |> Option.unwrap_or ~default:"Guest"
    ```

    ## File I/O Patterns

    ```ocaml
    (* Read entire file *)
    let content = Fs.read (Path.v "file.txt")
      |> Result.expect ~msg:"File required"

    (* Write file atomically *)
    let atomic_write path content =
      let tmp = Path.add_extension path ~ext:"tmp" in
      Fs.write content tmp
      |> Result.and_then (fun () -> Fs.rename ~src:tmp ~dst:path)

    (* Process directory recursively *)
    let rec process_dir dir =
      match Fs.read_dir dir with
      | Ok iter ->
          Iterator.iter (fun path ->
            if Fs.is_dir path |> Result.unwrap_or ~default:false
            then process_dir path
            else process_file path
          ) iter
      | Error _ -> ()

    (* Stream large file *)
    let process_large_file path =
      File.open_ path
      |> Result.and_then (fun file ->
          File.read_lines file
          |> Iterator.map parse_line
          |> Iterator.filter is_valid
          |> Iterator.collect
        )
    ```

    ## Network Programming

    ```ocaml
    (* Simple TCP server *)
    let handler stream =
      let buf = Bytes.create 1024 in
      match TcpStream.read stream buf () with
      | Ok n ->
          let response = process_request (Bytes.sub buf 0 n) in
          TcpStream.write stream (Bytes.of_string response) ()
      | Error _ -> ()

    let server = TcpServer.start ~port:8080 ~handler

    (* HTTP client pattern *)
    let fetch url =
      Uri.parse url
      |> Result.and_then (fun uri ->
          let addr = Addr.of_uri uri in
          TcpStream.connect addr
        )
      |> Result.and_then (fun stream ->
          let request = Http.Request.create ~method_:GET ~uri in
          send_request stream request;
          read_response stream
        )
    ```

    ## Actor Supervision

    ```ocaml
    (* Worker with supervisor *)
    let worker_spec id =
      Supervisor.child_spec
        ~id:(format "worker_%d" id)
        ~start:(fun () -> spawn (worker id))
        ~restart:Permanent
        ()

    let supervisor =
      Supervisor.start_link
        ~strategy:OneForOne
        ~intensity:{ max_restarts = 5; window = Duration.of_sec 10 }
        ~children:[
          worker_spec 1;
          worker_spec 2;
          worker_spec 3;
        ]
        ()

    (* Dynamic worker pool *)
    let pool = Supervisor.Dynamic.start_link () in
    for i = 1 to 10 do
      Supervisor.Dynamic.start_child pool
        ~start:(fun () -> spawn (worker i))
        ()
      |> ignore
    done
    ```

    ## Data Processing Pipelines

    ```ocaml
    (* Functional pipeline *)
    let results =
      items
      |> List.to_seq
      |> Iterator.of_seq
      |> Iterator.map process
      |> Iterator.filter is_valid
      |> Iterator.take 100
      |> Iterator.collect

    (* Parallel processing *)
    let results =
      WorkerPool.map pool ~f:expensive_computation items

    (* Stream processing *)
    let process_stream () =
      Fs.read_dir (Path.v "data")
      |> Result.map (fun iter ->
          MutIterator.filter_map (fun path ->
            Fs.read path
            |> Result.and_then parse
            |> Result.to_option
          ) iter
          |> MutIterator.to_list
        )
    ```
*)

(** Module Declarations *)
module Agent = Agent

module Archive = Archive

(** **When to use:** Simple concurrent state access

    Use Agent when you need a lightweight state server accessed from multiple processes.
    Perfect for counters, caches, shared configuration, or session storage.
    
    **Don't use when:** You need complex request/reply patterns → use GenServer
    
    **Examples:**
    - Shared application configuration
    - In-memory cache
    - Request counter / rate limiter
    - User session storage *)
module ArgParser = Arg_parser

(** **When to use:** Parsing command-line arguments
    
    Use ArgParser for building CLI tools with flags, options, and subcommands.
    
    **Examples:**
    - Build tools (like riot, cargo, npm)
    - Developer utilities
    - System administration scripts *)
module Bench = Bench

(** **When to use:** Microbenchmarking performance
    
    Use Bench for measuring and comparing performance of code.
    Simple, low-level benchmarking framework with basic statistics.
    
    **Examples:**
    - Comparing data structure performance
    - Measuring algorithm speed
    - Performance regression testing
    
    **See also:** {!Test} for unit testing *)
module Bool = Bool

(** **When to use:** Boolean operations
    
    Extended bool operations with parsing, formatting, and utilities. *)
module Char = Char

(** **When to use:** Character classification and conversion
    
    Extended character operations beyond stdlib.
    
    **See also:** {!Unicode.Rune} for Unicode code points *)
module Collections = Collections

(** **When to use:** Working with data structures
    
    Parent module containing Vector, HashMap, HashSet, Queue, Deque, Heap, and List.
    
    Use collections when you need:
    - Fast lookups → HashMap
    - Unique values → HashSet
    - Dynamic arrays → Vector
    - FIFO queues → Queue
    - Double-ended operations → Deque
    - Priority ordering → Heap
    - Functional lists → List *)
module Compress = Compress

(** **When to use:** Streaming compression/decompression

    Use Compress for codec-style I/O transformations such as gzip streams.
    
    **See also:** {!Archive} for tar archives layered on top of readers. *)
module Command = Command

(** **When to use:** Running external programs
    
    Use Command when you need to execute shell commands or external tools.
    
    **Examples:**
    - Running git commands
    - Invoking build tools
    - System administration tasks
    - Integration with external utilities
    
    **See also:** {!System} for system information *)
module Config = Config

(** **When to use:** Application configuration management
    
    Use Config for type-safe, environment-aware configuration loading.
    
    **Examples:**
    - Loading server configuration
    - Database connection settings
    - Environment-specific configs (dev/test/prod)
    - Multi-app configuration files
    
    **Features:**
    - Namespaced TOML sections
    - Type-safe parsing
    - Environment detection
    - Deep merging *)
module Crypto = Crypto

(** **When to use:** Cryptographic hashing
    
    Use Crypto for content-addressable storage, integrity verification, checksums.
    
    **Examples:**
    - Hash-based deduplication
    - Content verification
    - Cache keys
    - Merkle trees
    
    **WARNING:** NOT for password hashing - use proper KDFs like Argon2
    
    **Algorithms:** SHA-256 (recommended), SHA-512, MD5 (legacy only) *)
module Data = Data

(** **When to use:** Parsing/generating data formats
    
    Parent module for structured data formats.
    
    **Use cases:**
    - API communication → Json
    - Configuration files → Toml  
    - Data export/import → Csv
    - Legacy systems → Xml
    - Lisp-like data → Sexp *)
module Encoding = Encoding

(** **When to use:** Binary/text and numeric encodings

    Parent module for Base16/Hex, Base32, Base64, Base85, and Octal.

    **Use cases:**
    - Hex digests and binary inspection → Hex / Base16
    - Human-friendly binary transport → Base32
    - Binary-in-text payloads → Base64
    - Compact text encodings → Base85
    - Octal numeric fields → Octal *)
module Calendar = Calendar

module Datetime = Datetime

(** **When to use:** Calendar dates and times
    
    Use Datetime for human-readable dates, date arithmetic, formatting.
    
    **Examples:**
    - Parsing user-input dates
    - Date calculations (add 3 months)
    - Timezone-aware operations
    - Calendar displays
    
    **See also:** {!Time.SystemTime} for timestamps, {!Time.Instant} for elapsed time *)
module Diff = Diff

(** **When to use:** Computing differences between data structures
    
    Use Diff for change detection, audit logs, version control.
    
    **Examples:**
    - Configuration change tracking
    - Database record diffing
    - API response comparison
    - Undo/redo systems *)
module Env = Env

(** **When to use:** Environment variables and system info
    
    Use Env for reading configuration from environment, detecting OS/arch.
    
    **Examples:**
    - Reading DATABASE_URL, API_KEY
    - Detecting production vs development
    - Platform-specific behavior
    - Container orchestration config *)
module Exception = Exception

(** **When to use:** Exception handling utilities
    
    Use Exception for programmatic exception handling and custom exceptions.
    
    **Prefer:** Result type for expected errors
    **Use exceptions for:** Unexpected/unrecoverable errors *)
module Float = Float

(** **When to use:** Floating-point operations
    
    Extended float operations with parsing and formatting. *)
(** **When to use:** Filesystem operations
    
    Use Fs for all file and directory operations with Result-based error handling.
    
    **Common operations:**
    - Reading/writing files
    - Creating directories
    - Copying/moving files
    - Querying metadata
    - Watching for changes
    
    **See also:** {!Path} for path manipulation, {!Fs.File} for streaming *)
module Fs = Fs

(** **When to use:** Graph data structures and visualization
    
    Use Graph for dependency graphs, build systems, workflow diagrams.
    
    **Modules:**
    - SimpleGraph → dependency tracking with topo sort
    - Dot → Graphviz visualization
    - Mermaid → Markdown-friendly diagrams *)
module Graph = Graph

module IO = IO

(** **When to use:** Generic I/O abstractions
    
    Use IO for Reader/Writer traits and vectored I/O operations.
    
    **Most users should use:** {!Fs} or {!Net} instead *)
module Int = Int

(** **When to use:** Integer operations
    
    Extended int operations with parsing, formatting, and utilities. *)
module Int32 = Int32

(** **When to use:** 32-bit integer operations *)
module Int64 = Int64

(** **When to use:** 64-bit integer operations
    
    Use Int64 for timestamps, large numbers, file sizes. *)
module Iter = Iter

(** **When to use:** Iteration and parsing utilities
    
    Parent module for Iterator, MutIterator, Cursor, and MutCursor.
    
    **Use Iterator when:** You need functional, backtrackable iteration
    **Use MutIterator when:** You need efficient, single-pass iteration
    **Use Cursor when:** You're parsing strings with backtracking
    **Use MutCursor when:** You need fast, single-pass string parsing *)
module List = Collections.List

(** **When to use:** Immutable linked lists
    
    Use List for functional programming, pattern matching, recursive algorithms.
    
    **Prefer Vector when:** You need random access or frequent appends *)
module Log = Log

(** **When to use:** Structured logging
    
    Use Log for application logging with levels (Debug, Info, Warn, Error).
    
    **Examples:**
    - Application events
    - Error tracking
    - Debugging
    - Audit trails *)
module Message = Message

(** **When to use:** Defining extensible message types
    
    Use Message for building type-safe actor communication protocols. *)
module Net = Net

(** **When to use:** Network I/O
    
    Parent module for TCP, HTTP, TLS, URI, and network addresses.
    
    **Common tasks:**
    - TCP servers → TcpServer
    - TCP clients → TcpStream
    - HTTP → Http.Request, Http.Response
    - TLS/SSL → TlsStream
    - URLs → Uri *)
module Option = Option

(** **When to use:** A value might not exist
    
    Use Option when absence is normal and not an error condition.
    
    **Examples:**
    - Optional configuration fields
    - HashMap lookups (key might not exist)
    - User input that can be empty
    - Function arguments that are optional
    
    **Prefer Result when:** Absence indicates an error you want to handle *)
module Path = Path

(** **When to use:** Working with filesystem paths
    
    Use Path for type-safe, UTF-8 validated, cross-platform path operations.
    
    **Always use Path instead of strings for filesystem paths!**
    
    **See also:** {!Fs} for filesystem operations *)
module Pid = Pid

(** **When to use:** Process identifiers
    
    Use Pid for actor/process identification and messaging. *)
module Process = Process

(** **When to use:** Process lifecycle and monitoring
    
    Use Process for monitoring, linking, and managing actor processes.
    
    **Examples:**
    - Monitoring other processes
    - Process linking for crash propagation
    - Checking if process is alive
    - Getting process info *)
(** **When to use:** Physical equality and pointer operations
    
    Rarely needed in normal application code. *)
module Ptr = Ptr

(** **When to use:** Random value generation
    
    Use Random for generating random integers, floats, selecting from lists.
    
    **Examples:**
    - Game mechanics
    - Testing with random data
    - Load balancing
    - Sampling *)
module Random = Random

(** **When to use:** Unique, opaque, type-witnessing references
    
    Use Ref for ensuring type safety across module boundaries. *)
module Ref = Ref
module Result = Result

(** **When to use:** Explicit error handling
    
    Use Result for operations that can fail in expected, recoverable ways.
    
    **Examples:**
    - File I/O (file might not exist)
    - Parsing (input might be invalid)
    - Network operations (connection might fail)
    - Validation (data might not meet requirements)
    
    **Always use Result instead of exceptions for expected errors!**
    
    **See also:** {!Option} for missing values without error context *)
module String = String

(** **When to use:** UTF-8 string processing
    
    Use String for text manipulation with proper UTF-8 iteration support.
    
    **See also:** {!Unicode} for advanced text processing *)
module Supervisor = Supervisor

(** **When to use:** Building fault-tolerant process trees
    
    Use Supervisor for applications that must automatically recover from crashes.
    
    **Strategies:**
    - OneForOne → restart only failed child
    - OneForAll → restart all children when one fails
    - RestForOne → restart failed child and those started after
    - SimpleOneForOne → dynamic children with same spec
    
    **Use Dynamic when:** Managing thousands of dynamic children
    
    **Examples:**
    - Long-running services
    - Connection pools
    - Worker pools
    - Per-user sessions *)
module Sync = Sync

(** **When to use:** Synchronization primitives
    
    Use Sync for mutable cells, mutexes, condition variables.
    
    **Most common:** Cell for mutable values
    
    **See also:** {!Agent} for concurrent state with actor patterns *)
module System = System

(** **When to use:** System information queries
    
    Use System for OS detection, resource limits, system paths. *)
module Task = Task

(** **When to use:** Asynchronous task execution
    
    Use Task for fire-and-forget or awaitable async operations.
    
    **Examples:**
    - Background jobs
    - Parallel computations
    - Async I/O operations
    
    **See also:** {!WorkerPool} for distributing work across workers *)
module Telemetry = Telemetry

(** **When to use:** Instrumentation and metrics
    
    Use Telemetry for adding observability to your application.
    
    **Examples:**
    - Request timing
    - Counter metrics
    - Custom events
    - Performance monitoring *)
module Test = Test

(** **When to use:** Writing unit tests
    
    Use Test for building test suites with assertions and reporters.
    
    **Reporters:** TAP, JUnit XML, JSON, Pretty print *)
module Time = Time

(** **When to use:** Time measurement
    
    Parent module for Duration, Instant, and SystemTime.
    
    **Use Duration for:** Time spans, timeouts, delays
    **Use Instant for:** Elapsed time, benchmarking
    **Use SystemTime for:** Wall-clock time, timestamps *)
module Timer = Timer

(** **When to use:** Timed events in actor systems
    
    Use Timer for scheduling delayed or periodic messages to processes.
    Perfect for timeouts, periodic tasks, scheduled events, and heartbeats.
    
    **Common use cases:**
    - Request timeouts (cancel operation after N seconds)
    - Periodic health checks or heartbeats
    - Scheduled cleanup tasks
    - Rate limiting with time windows
    - Delayed retries
    - Session expiration
    
    **Examples:**
    
    Timeout pattern:
    ```ocaml
    let with_timeout ~duration operation =
      let timer_ref = ref None in
      let operation_pid = spawn (fun () ->
        let result = operation () in
        Option.iter Timer.cancel !timer_ref;
        Ok result
      ) in
      timer_ref := Some (Timer.send_after operation_pid 
        (Message.Timeout) ~after:duration);
      receive_result operation_pid
    ```
    
    Periodic heartbeat:
    ```ocaml
    let start_heartbeat server =
      Timer.send_interval server 
        (Message.Heartbeat)
        ~interval:(Duration.of_sec 30)
    ```
    
    Delayed retry:
    ```ocaml
    let retry_after_delay ~delay pid request =
      Timer.send_after pid 
        (Message.Retry request) 
        ~after:delay
    ```
    
    **See also:** {!Time.Duration} for creating time spans *)
module Type = Type

(** **When to use:** Type-level programming utilities *)
module Unicode = Unicode

(** **When to use:** Unicode text processing
    
    Use Unicode for proper handling of international text, emoji, terminal width.
    
    **Modules:**
    - Rune → Unicode code points
    - Grapheme → User-perceived characters
    - Utf8 → UTF-8 encoding/decoding
    - Utf16 → UTF-16 line/column conversion
    - Segmentation → Word/sentence/line breaking *)
module UUID = Uuid

(** **When to use:** Globally unique identifiers
    
    Use UUID for entity IDs, request tracking, distributed coordination.
    
    **Examples:**
    - Database primary keys
    - Request IDs for tracing
    - Session tokens
    - Distributed system coordination *)
module Version = Version

(** **When to use:** Semantic versioning
    
    Use Version for parsing, comparing, and managing semantic versions.
    
    **Examples:**
    - Dependency management
    - API versioning
    - Feature flags based on version
    - Migration scripts *)
module WorkerPool = Worker_pool

(** **When to use:** Parallel work distribution
    
    Use WorkerPool for CPU-bound parallel processing or batch jobs.
    
    **Examples:**
    - Image processing pipeline
    - Data transformation
    - Batch analytics
    - Parallel map operations *)

(** Re-exported from Global *)
include module type of Global

(** Application Management *)
module Application = Application

(** **When to use:** Multi-application systems with dependencies
    
    Use Application for composing multiple OTP-style applications with
    automatic dependency resolution and ordered startup/shutdown.
    
    **Examples:**
    - Web app with database app dependency
    - Microservices with shared infrastructure
    - Plugin systems *)
val start: apps:Application.t list -> unit

(** Start the runtime with applications.
    
    Applications are started in dependency order.
    Uses `Actors.run` under the hood.
    
    **Example:**
    ```ocaml
    let () = Std.start ~apps:[database_app; web_app]
    ``` *)

(** Helper Functions from Global *)
val panic: string -> 'a

(** Panic with a message - raises an uncatchable exception.
    
    **When to use:** Unrecoverable errors, invariant violations.
    **Don't use for:** Expected errors → use Result instead *)
val cell: 'a -> 'a Sync.Cell.t

(** Create a mutable cell with the given value.
    
    **Example:** `let counter = cell 0 in Cell.update counter (fun n -> n + 1)` *)
val print: string -> unit

(** Print to stdout with immediate flush (no newline) *)
val println: string -> unit

(** Print to stdout with newline and immediate flush *)
val eprint: string -> unit

(** Print to stderr with immediate flush (no newline) *)
val eprintln: string -> unit

(** Print to stderr with newline and immediate flush *)
val todo: string -> 'a

(** Mark code as TODO with a message - panics when called.
    
    **Use for:** Placeholder implementations during development *)
val unimplemented: unit -> 'a

(** Mark code as unimplemented - panics when called *)
(** Collection Type Aliases and Constructors *)
type 'a vec = 'a Collections.Vector.t
(** Vector type alias - dynamically-sized array *)
type 'a queue = 'a Collections.Queue.t
(** Queue type alias - FIFO queue *)
type 'a set = 'a Collections.HashSet.t
(** Set type alias - hash-based set *)
type ('k, 'v) map = ('k, 'v) Collections.HashMap.t

(** Map type alias - hash-based map *)
val vec: 'a list -> 'a vec

(** Create a vector from a list.
    
    **Example:** `let v = vec [1; 2; 3] in Vector.push v 4` *)
val queue: 'a list -> 'a queue

(** Create a queue from a list.
    
    **Example:** `let q = queue [1; 2; 3] in Queue.dequeue q` *)
val set: 'a list -> 'a set

(** Create a set from a list.
    
    **Example:** `let s = set [1; 2; 3; 2; 1] in HashSet.len s (* 3 *)` *)
val map: ('k * 'v) list -> ('k, 'v) map

(** Create a map from a list of key-value pairs.
    
    **Example:** `let m = map [("a", 1); ("b", 2)] in HashMap.get m "a"` *)

(** Process Management *)
exception Receive_timeout

(** Raised when a receive operation times out *)
exception Syscall_timeout

(** Raised when a syscall operation times out *)
type 'msg selector = 'msg Actors.selector

(** Message selector type *)
val self: unit -> Pid.t

(** Get the PID of the currently running process *)
val spawn: (unit -> (unit, Process.exit_reason) Kernel.result) -> Pid.t

(** Spawn a new process *)
val spawn_link: (unit -> (unit, Process.exit_reason) Kernel.result) -> Pid.t

(** Spawn a new process linked to the current process *)
val send: Pid.t -> Message.t -> unit

(** Send a message to a process *)
val receive: selector:'value selector -> ?timeout:Time.Duration.t -> unit -> 'value

(** Receive a message using a selector *)
val receive_any: ?timeout:Time.Duration.t -> unit -> Message.t

(** Receive any message *)
val sleep: Time.Duration.t -> unit

(** Sleeps the current process for at least the specified duration *)
val yield: unit -> unit

(** Yield control to the scheduler *)
val shutdown: status:int -> unit

(** Shutdown the runtime with the given exit status *)
module Dynlink = Kernel.Dynlink

(** Dynamically link libraries *)
module Ops = Kernel.Ops
