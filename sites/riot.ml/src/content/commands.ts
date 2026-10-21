/**
 * Copy for the "one tool" command explorer on the landing page.
 *
 * Text fields support `backtick` spans, rendered as inline code by
 * `InlineText`. Keep descriptions short: one idea per bullet, no marketing
 * filler, and commands that actually exist in `riot --help`.
 */

export type CommandGroup = "Start" | "Build" | "Quality" | "Ship";

export interface CommandExample {
  title: string;
  description: string;
  code: string;
}

export interface CommandSurface {
  id: string;
  group: CommandGroup;
  /** Short label shown in the command list. */
  label: string;
  /** One-line summary shown under the label. */
  summary: string;
  /** Commands covered by this surface. */
  commands: string[];
  title: string;
  description: string;
  points: string[];
  examples: CommandExample[];
}

export const commandGroups: CommandGroup[] = ["Start", "Build", "Quality", "Ship"];

export const commandSurfaces: CommandSurface[] = [
  {
    id: "new",
    group: "Start",
    label: "riot init",
    summary: "Workspaces and packages",
    commands: ["riot init", "riot new"],
    title: "Start on the golden path",
    description:
      "`riot init` creates a workspace and `riot new` adds packages to it, so you never assemble boilerplate by hand.",
    points: [
      "A new workspace comes with manifests, an OCaml toolchain config, and agent instructions in `.agents`.",
      "`riot new` drops a package into the right place without you remembering the directory layout.",
      "Your first project already has a package, a test loop, and an obvious next command.",
    ],
    examples: [
      {
        title: "Create a workspace",
        description: "Start from the shape Riot expects.",
        code: "$ riot init app\n$ cd app",
      },
      {
        title: "Add a package",
        description: "Keep new packages on the same path.",
        code: "$ riot new packages/api",
      },
      {
        title: "What you get",
        description: "Manifests, sources, tests, and toolchains all have an obvious home.",
        code: "app/\n  ocaml-toolchain.toml\n  riot.toml\n  packages/\n    app/\n      riot.toml\n      src/\n      tests/",
      },
    ],
  },
  {
    id: "toolchain",
    group: "Start",
    label: "riot toolchain",
    summary: "Prebuilt OCaml compilers",
    commands: ["riot toolchain"],
    title: "OCaml toolchains, handled",
    description:
      "Riot ships prebuilt OCaml toolchains for supported platforms and installs exactly the one your workspace asks for.",
    points: [
      "Declare the OCaml version and targets once, in `ocaml-toolchain.toml`.",
      "Riot provisions and validates toolchains under `~/.riot/toolchains`, including cross-compilation targets.",
      "Builds, tests, docs, benchmarks, and fuzzing all use the same toolchain.",
    ],
    examples: [
      {
        title: "Declare the toolchain",
        description: "The workspace records what it needs.",
        code: '# ocaml-toolchain.toml\n[toolchain]\nversion = "5.5.0-riot.1"\ntargets = ["aarch64-apple-darwin", "x86_64-unknown-linux-gnu"]',
      },
      {
        title: "Install what's missing",
        description: "Download and validate the declared toolchains.",
        code: "$ riot toolchain install",
      },
      {
        title: "See what's available",
        description: "List every prebuilt toolchain Riot publishes.",
        code: "$ riot toolchain list-available",
      },
    ],
  },
  {
    id: "build",
    group: "Build",
    label: "riot build",
    summary: "Package-aware builds",
    commands: ["riot build"],
    title: "Fast, package-aware builds",
    description:
      "Packages are the unit of the build, and each one is described by a plain `riot.toml`. No S-expressions, no build folklore.",
    points: [
      "Dependencies, targets, and generated artifacts all live in one package model.",
      "Build one package or the whole workspace, in debug or release, from the same command.",
      "Cross-compile with an exact target triple or a broad pattern like `linux`.",
    ],
    examples: [
      {
        title: "Build everything",
        description: "Every package in the workspace, debug profile.",
        code: "$ riot build",
      },
      {
        title: "Build one package",
        description: "Narrow the build to the slice you're working on.",
        code: "$ riot build -p std",
      },
      {
        title: "Release and cross builds",
        description: "Optimized output, or a Linux binary from your laptop.",
        code: "$ riot build --release\n$ riot build -x linux",
      },
    ],
  },
  {
    id: "run",
    group: "Build",
    label: "riot run",
    summary: "Run local or remote binaries",
    commands: ["riot run"],
    title: "Run anything, from anywhere",
    description:
      "`riot run` builds and runs executables from your workspace, a GitHub repository, or a URL.",
    points: [
      "No hunting for build output paths: name the binary and go.",
      "Pick a package when more than one target is runnable.",
      "Pass runtime arguments after `--`, so it stays scriptable.",
    ],
    examples: [
      {
        title: "Run a workspace binary",
        description: "Build and run in one step.",
        code: "$ riot run app",
      },
      {
        title: "Pick the package",
        description: "Disambiguate and forward arguments.",
        code: "$ riot run -p web server -- --port 8080",
      },
      {
        title: "Run a remote tool",
        description: "One-off tools and generators, straight from a repo.",
        code: "$ riot run https://github.com/owner/tool",
      },
    ],
  },
  {
    id: "package-commands",
    group: "Build",
    label: "riot pkg:cmd",
    summary: "Commands from your dependencies",
    commands: ["riot <pkg>:<cmd>"],
    title: "Packages can ship commands",
    description:
      "Dependencies can register first-class Riot commands, so migrations, codegen, and upgrades feel native instead of needing another global tool.",
    points: [
      "Packages declare commands in `riot.toml`; Riot runs them as `riot package:command`.",
      "Riot builds the provider first, so the command always matches the version you depend on.",
      "Arguments after `--` go straight to the provider.",
    ],
    examples: [
      {
        title: "Add a provider",
        description: "Its commands arrive with the dependency.",
        code: "$ riot add sqlx",
      },
      {
        title: "Run its workflow",
        description: "Feels built in; the package still owns the behavior.",
        code: "$ riot sqlx:migrate\n$ riot sqlx:prepare -- --database app_dev",
      },
    ],
  },
  {
    id: "fmt",
    group: "Quality",
    label: "riot fmt",
    summary: "One house style",
    commands: ["riot fmt"],
    title: "One house style, zero config",
    description:
      "`riot fmt` is a fast formatter tuned for readability and small diffs. There are no options to argue about.",
    points: [
      "Formatting is part of the stack, not a per-project debate.",
      "Output reads well and stays stable, so reviews show the real change.",
      "Check mode reports drift in CI without touching files.",
    ],
    examples: [
      {
        title: "Format the workspace",
        description: "Rewrite files in place.",
        code: "$ riot fmt",
      },
      {
        title: "Check in CI",
        description: "Report drift as JSON, change nothing.",
        code: "$ riot fmt --check --json",
      },
      {
        title: "What a rewrite looks like",
        description: "Clearer intent, boring diff.",
        code: '- let  greeting name=print_endline("hello, "^name)\n+ let greeting = fun name ->\n+   print_endline ("hello, " ^ name)',
      },
    ],
  },
  {
    id: "fix",
    group: "Quality",
    label: "riot fix",
    summary: "Lints, fixes, and codemods",
    commands: ["riot fix"],
    title: "Lints that fix themselves",
    description:
      "`riot fix` is an extensible linter. When Riot knows the fix, it shows the edit and can apply it for you.",
    points: [
      "Packages can ship their own rules and fixes, and workspaces opt in from `riot.toml`.",
      "Upgrades get easier: the package that changed can ship the codemod that moves your code along.",
      "Every rule has a stable id and an explanation.",
    ],
    examples: [
      {
        title: "Check, then apply",
        description: "See diagnostics first, then let Riot make the safe edits.",
        code: "$ riot fix --check\n$ riot fix --apply",
      },
      {
        title: "Explain a rule",
        description: "Understand a warning before acting on it.",
        code: "$ riot fix --explain class-case-module-names",
      },
      {
        title: "Readable warnings",
        description: "The problem, the source, and the next step.",
        code: "[warning] class-case-module-names\n\n  3 | module Fair_value = Quant.Fair_value\n    |        ^^^^^^^^^^\n\n  -> Rename Fair_value to FairValue",
      },
    ],
  },
  {
    id: "test",
    group: "Quality",
    label: "riot test",
    summary: "Unit, property, and snapshot tests",
    commands: ["riot test"],
    title: "Every kind of test, one runner",
    description:
      "Unit tests, property tests, snapshot tests, and fuzz cases all run under `riot test`.",
    points: [
      "Unit tests for exact behavior, properties for invariants, snapshots for output that needs review.",
      "Fuzz cases replay as regular tests, so strange inputs found later stay fixed.",
      "Filter by package or test name to keep the loop tight.",
    ],
    examples: [
      {
        title: "Run a focused slice",
        description: "Select a package, then narrow by name.",
        code: "$ riot test -p blink -f retry",
      },
      {
        title: "List before running",
        description: "Inspect suites and cases as data.",
        code: "$ riot test --list --json",
      },
      {
        title: "Mixed output",
        description: "Units, properties, and fuzz cases in one stream.",
        code: "prop blink::client_property_tests::retry delay is monotonic ... ok (63us)\ntest blink::client_tests::retries retryable statuses ... ok (990us)\nfuzz blink::client_tests::connect budget blocks ... ok (399ms)",
      },
    ],
  },
  {
    id: "fuzz",
    group: "Quality",
    label: "riot fuzz",
    summary: "Coverage-guided fuzzing",
    commands: ["riot fuzz"],
    title: "Coverage-guided fuzzing, built in",
    description:
      "Point `riot fuzz` at parsers, codecs, protocol handlers, and CLIs, and let coverage feedback find the edge cases.",
    points: [
      "A fuzz case is an ordinary test declared with `Test.fuzz`, with seeds and mutator hints.",
      "Corpus and crashes live under `.riot/fuzzing`, with captured output for triage.",
      "Turn a real finding into a small, durable regression test.",
    ],
    examples: [
      {
        title: "Run a bounded campaign",
        description: "Ten minutes of coverage-guided exploration.",
        code: "$ riot fuzz -p blink -f retry --duration 10m",
      },
      {
        title: "Replay a crash",
        description: "Reproduce one saved input against one case.",
        code: "$ riot fuzz -p blink -f retry \\\n    --replay .riot/fuzzing/blink/client/retry/crashes/id-000001",
      },
      {
        title: "Minimize the corpus",
        description: "Drop redundant inputs so the corpus stays useful.",
        code: "$ riot fuzz minimize-corpus -p blink -f retry",
      },
    ],
  },
  {
    id: "snapshots",
    group: "Quality",
    label: "riot snapshots",
    summary: "Review expected output",
    commands: ["riot snapshots"],
    title: "Review snapshots, don't bless them",
    description:
      "When generated output changes, the test writes a pending candidate. `riot snapshots` is where you approve or reject it.",
    points: [
      "Ideal for formatter output, diagnostics, generated docs, and text reports.",
      "Candidates never silently replace the expected file.",
      "Interactive review lets you approve, reject, or skip without hunting for fixture paths.",
    ],
    examples: [
      {
        title: "Review interactively",
        description: "Walk every pending snapshot.",
        code: "$ riot snapshots review",
      },
      {
        title: "Approve or reject a slice",
        description: "Act on one package or test area at a time.",
        code: "$ riot snapshots approve packages/std\n$ riot snapshots reject packages/std/tests/parser",
      },
    ],
  },
  {
    id: "bench",
    group: "Quality",
    label: "riot bench",
    summary: "Benchmarks with history",
    commands: ["riot bench"],
    title: "Benchmarks you can compare",
    description:
      "`riot bench` runs micro and macro benchmarks, records results, and compares them against earlier runs on the same toolchain.",
    points: [
      "Benchmarks use the same packages, profiles, and toolchain as everything else.",
      "`--record` saves a run under `.riot/bench`; `--compare` shows history next to the current result.",
      "Use the release profile when the question is about optimized code.",
    ],
    examples: [
      {
        title: "Record a baseline",
        description: "Save an optimized run for later comparison.",
        code: "$ riot bench -p std -f parser --release --record",
      },
      {
        title: "Compare against history",
        description: "The last five comparable runs, side by side.",
        code: "$ riot bench -p std -f parser --compare 5",
      },
    ],
  },
  {
    id: "packages",
    group: "Ship",
    label: "pkgs.ml",
    summary: "The package registry",
    commands: ["riot add", "riot update", "riot publish", "riot search"],
    title: "A registry that's part of the tool",
    description:
      "pkgs.ml is Riot's package registry: index, search, publishing, generated docs, and public download stats.",
    points: [
      "Add, remove, update, publish, yank, and search without leaving `riot`.",
      "Publishing generates documentation and serves it from docs.pkgs.ml.",
      "Public stats show maintainers which versions people actually use.",
    ],
    examples: [
      {
        title: "Find and add a package",
        description: "Search, then update your manifest and lockfile.",
        code: "$ riot search std\n$ riot add std",
      },
      {
        title: "Refresh dependencies",
        description: "Resolve the newest compatible set.",
        code: "$ riot update",
      },
      {
        title: "Publish and yank",
        description: "Ship a release, or retract a bad one without rewriting history.",
        code: "$ riot publish\n$ riot yank std@0.6.0",
      },
    ],
  },
  {
    id: "doc",
    group: "Ship",
    label: "riot doc",
    summary: "Generated documentation",
    commands: ["riot doc"],
    title: "Docs from your source",
    description:
      "`riot doc` generates documentation for your packages and their dependencies, written in Markdown right in your source comments.",
    points: [
      "Read locally exactly what your users will read once you publish.",
      "Package docs and API references share one readable style.",
      "Published packages get their docs hosted on docs.pkgs.ml.",
    ],
    examples: [
      {
        title: "Generate docs",
        description: "For the workspace, or one package.",
        code: "$ riot doc\n$ riot doc -p std",
      },
      {
        title: "Release docs",
        description: "Preview what will be published.",
        code: "$ riot doc --all --release",
      },
    ],
  },
];
