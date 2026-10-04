# Now

Daily-driver v0.1 stabilization is tracked in [TODO.md](TODO.md), with scope and
acceptance proposals in [RFD0044](docs/rfds/RFD0044-v0-1-daily-driver-stability.md).
The lists below remain the broader backlog, not an all-items-required release gate.

* riot check -p kernel is fast <100ms

# Before Announcing

* Std.String <- actual zero-copy string views with .sub, could we make this copy-on-write ?

* ~acc -> ~init or ffolds

* option.for_each -> option.if_some ~fn

* riot-deps make workspace_manager a required param instead of an optinoal one

* everywhere we're using Cell + list maybe we just need to use Vector 

* riot fix rule id != string

* turn every `error: string` into a structured error

* clean up the riot-test selector filter flag code

* Bug: riot build doesn't promote binaries with the right permissions?

* Std.Log should be set up by default on the generated template code

* Std.Collections.Proplist : ('key, 'value) list helpers

* riot build takes _ages_ to plan big packages like std: is it syn?

* riot clean should take a build lock to ensure noone is using _build while it tries to clean it

* when installing sources in ~/.riot/registry we should make them read-only! 

* lint: if a function uses raise, it should be called _unchecked

* lint: externals should be called unsafe_* 

* riot init should initialize:
    * .agents/skills/riot/* 
    * config/dev.toml

* riot info workspace
  riot info pkg[@vsn]  should show information about that package (local or remote), including links to the docs, should also include the local ~/.riot/registry path

* riot help == riot --help

* bug: riot init arewedown.dev fails silently -- workspaces can contain dots in names, that's fine, but the package name becomes arewdown-dev

* bug: changing riot.toml dep paths didn't break the cache :( 

* regression: cold riot build takes _ages_ on syn.deps? 

* riot snapshots review sucks: its hella slow, its not very interactive (a+enter? yuk)

* riot bench streams outputs/results

* riot test support multipl `-p` flags

* riot test/bench should support `--filter` flag to be used with `--package` flag

* need a riot SKILLS for agents

* keep workign on making docs.riot.ml look great and writing the docs 

* riot run hello_world.ml should just work:
  * parses files and detects pragmas like
    #dep "std":
  * creates a synthetic workspace, plans and builds and runs the script on the global cache

* build lock should be acquired _after planning_ to allow for fully cached builds to finish immediately

* reacahability analysis during planning -- if a module in a package isn't included/reached by any other module, it shouldn't be part of the build!

* `riot check` should be able to type-check Riot! 

* `riot lsp` doesn't use 200gb of ram


# Next

* little tool for git commit queues?

* riot fmt should rename files for you to be consistent with snake_cased files -- this is the first step and then th next step is translating those automatically into CamelCased modules

* how to make `riot test` more semantically aware? we can definitely build the graph of static dependencies into a test, could we use that to compute the tests that we _know_ need to be rerun?

* riot fmt cache -- hash files after formatting them and save marks on the _build cache, if the file has been hashed-seen before, then it is a formatted file already!

* riot/config.toml support a [target."str".runner] run command like `docker run -ti
ubuntu` that can help us run cross-compiled binaries in a container so can configure
  [target.linux.runner]
  image = "ubuntu"

  and this basically starts the container by mounting the binary and then runs the binary
  in the container, streaming output

* lint rule so modules namespaced with <pkg>_*.ml or <subdir>_*.ml we tell the user they don't have o

* modules referencing themselves (A.ml using A inside) aren't circular dependencies! this allows modules like Suri.Config to call the Std.Config module after an `open`

* enforce examples/binaries have a `val main : ~args:string list -> result` function by autoamtically wrapping/injecting a `let () = Actors.run ~main ~env:Std.Env.args ()` 

* RIOT_LOG=debug should set the log level of Std.Log to debug -- that way we can just put a bunch of Log calls everywhere!

* bug? how do we support creating projects without a .mli file and just generate it at build time for you?

# Later

* registry safeties: rate-limiting!

* enforce my-pkg/src/my_subdir/hello_world.ml becomes MyPkg.SubDir.HelloWorld module

* implement js targets

* riot fix: allow for disabling specific rules like [@fix.disable "rule id"]

* `riot trace` -- instrument and dump traces for tests and programs? is this worth doing?

* `riot fetch` -- download everything that needs downloading

* `riot toolchain` should not crash outside the workspace ; riot toolchain list
[Scheduler] Process pid<0> finished with exception: Panic("Failed to scan workspace")
[Scheduler] Backtrace:
Raised at Stdlib__Hashtbl.find in file "hashtbl.ml", line 584, characters 13-28
Called from Kernel__Collections__Hashmap.get in file "/Users/leostera/Developer/github.com/leostera/riot/_build/release/aarch64-apple-", line 30, characters 11-33

; riot toolchain install
[Scheduler] Process pid<0> finished with exception: Panic("Failed to scan workspace")
[Scheduler] Backtrace:
Raised at Stdlib__Hashtbl.find in file "hashtbl.ml", line 584, characters 13-28
Called from Kernel__Collections__Hashmap.get in file "/Users/leostera/Developer/github.com/leostera/riot/_build/release/aarch64-apple-", line 30, characters 11-33

;

* riot toolchain install has shitty output

```
Installing OCaml 5.5.0-riot.2 toolchains...

  ✓ aarch64-apple-darwin (host) - already installed
  ✓ aarch64-unknown-linux-gnu - already installed
  📥 aarch64-unknown-linux-musl - downloading...
📥 Downloading OCaml 5.5.0-riot.2 for aarch64-unknown-linux-musl (cross-compilation from aarch64-apple-darwin to aarch64-unknown-linux-musl)...
```

* minttea test cases that let you specify inputs and assert outputs in _turns_

* Std.BigInt
* Std.Decimal -- arbitrary precision arithmetic
* Money package

* Std.Rational
* Std.Bitwise 
* Std.Collections.Tuple
* Std.Collections.Bag -- like set, but with duplicates
* Std.Collections.Trees - RB, AVL, ...

* Fs.ls vs Fs.into_iter path

* `ftp` package

* Consider dropping Std.Data.Csv/Xml/Sexp
* Consider `crypto` package with bcrypt, argon, blake, etc

* Std.Task.Supervisor
* Std.Random.(one_of, choose n, take n, between(min,max)) and random primitives (bool, string, char, int, etc)

* Std.Port for long-running external programs to communicate
let cmd = Command.make "echo 'what'" in
let (status, stdout, stderr) = Command.output cmd in
let status = Command.status cmd in
let handle = Command.spawn cmd in

let* { status; _ } = Command.(make "echo .." |> output) in

let python_server () = 
  Command.make 
    ~stdin:`inherit
    "python server.py"
in

let pid = 
Port.spawn ~cmd:python_server in
Port.spawn_executable
Port.fd
Port.spawn_driver

Port.open({:spawn, "..."}, opts)


* Std.Command.run "..." = (make "..." |> output)

* Borow from Elixir.Enum group_by

* Admin panel for pkgs.ml -- admins are listed via env vars at deploy time?

* Introduce versioning by Riot version -- 2026.03 -- this internally uses different compiler versions
