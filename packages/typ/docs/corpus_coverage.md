# Riot Source Corpus Coverage

This is the inventory and baseline for repository-specific `typ` parity. The
coverage target is deliberately narrower than full OCaml conformance.

## Corpus Definition

A source is in scope when it is a tracked `.ml` or `.mli` file under `packages/`
or `compiler/`, except files under vendored/generated paths or fixture inputs.
Test and example implementation code counts; parser-error samples, fixture
programs, and embedded workspace fixtures do not. Because fixture directory
names vary, the current inventory excludes path components containing
`fixture`, as well as `packages/syn/tests/diagnostics/` (intentional invalid
parser samples).

Initial inventory on 2026-10-11:

- 2,404 tracked source/interface files in scope: 1,576 `.ml` and 828 `.mli`
- 70 packages represented
- 0 `.ml`/`.mli` files under `compiler/` in this checkout; all sources are under
  `packages/`
- 388 source files are under `tests/` directories and are included

The package set is: `blink`, `colors`, `contentstore`, `docker-client`,
`dotenv`, `fixme`, `gooey`, `hello-foreign`, `http`, `human-units`, `ignore`,
`jollyroger`, `jsonrpc`, `kernel`, `krasny`, `lsp`, `markdown`, `mcp`, `mime`,
`miniriot`, `minttea`, `mysql`, `pkgs-ml`, `postgres`, `propane`, `pubgrub`,
`riot-bench`, `riot-build`, `riot-build2`, `riot-check`, `riot-cli`, `riot-cmd`,
`riot-deps`, `riot-doc`, `riot-e2e`, `riot-fix`, `riot-fmt`, `riot-fuzz`,
`riot-init`, `riot-install`, `riot-lsp`, `riot-model`, `riot-planner`,
`riot-publish`, `riot-repl`, `riot-run`, `riot-store`, `riot-test`,
`riot-toolchain`, `riot-trace`, `serde`, `serde-bin`, `serde-bson`,
`serde-cbor`, `serde-json`, `serde-toml`, `serde-urlencoded`, `serde-yaml`,
`sqlite`, `sqlx`, `sqlx-driver`, `std`, `suri`, `suri-jobs`, `suri-mailer`,
`swisstable`, `syn`, `testcontainers`, `tty`, and `typ`.

Recount the current candidate inventory from the repository root with:

```sh
python3 - <<'PY'
import subprocess
paths = subprocess.check_output(["git", "ls-files", "*.ml", "*.mli"], text=True).splitlines()
def excluded(path):
    parts = path.split("/")
    return (parts[0] not in ("packages", "compiler")
        or any("fixture" in part for part in parts)
        or ("tests" in parts and "diagnostics" in parts)
        or any(part in ("vendor", "generated") for part in parts))
print(sum(not excluded(path) for path in paths))
PY
```

Review the exclusion policy if fixture conventions change; never use it to hide
first-party implementation or test code that should be checked.

## First-Pass Feature Signals

A text scan of the in-scope corpus found files containing these likely
constructs. The scan is heuristic, including comments and strings; these are
triage signals, not a parser-derived support matrix.

| Signal | Files |
| --- | ---: |
| First-class module spellings `(module` / `(val` | 141 |
| Polymorphic-variant tag spellings | 227 |
| Local module bindings | 98 |
| Exception/raise/try spellings | 255 |
| Labeled or optional argument markers | 1,617 |
| GADT-like arrows in type declarations | 102 |

`Syn`'s current package contract excludes OCaml classes/objects. Remaining
keyword hits are comments, raw formatter input, or embedded JavaScript rather
than evidence of class/object syntax in first-party OCaml implementation code.

## Baseline And Existing Integration

- The corpus runner's `Syn.parse` pass reports **0 parser diagnostics across
  all 2,404 in-scope files**. This does not imply that `Typ` accepts or
  typechecks those files.
- Baseline `riot test -p syn --json`: **1,197 passed, 0 failed**.
- Baseline `riot test -p typ --json`: **1,942 total; 342 passed and 1,600
  failed**. The failures comprise 1,599 snapshot mismatches (1,000 in
  `infer_diagnostic_tests`, 599 in `infer_tests`) and one 500ms timeout in
  `ast_tests`. The approved snapshots predate the current structured
  `Typ.Ast`/diagnostic output; some inference mismatches also reflect currently
  unimplemented semantics, so these must be reviewed rather than mass-approved.
- The package-aware checker implementation exists under `packages/riot-check`,
  but `riot-check` is absent from the root workspace member list and from
  `riot-cli` dependencies/commands. `riot run riot -- check --help` fails with
  `Unknown argument: check`. `packages/riot-check/src/check/session.ml` also
  references `Typ.Check`, which is not exported by the current `typ` facade.
  Therefore neither a corpus-wide `riot check` run nor Typ checking as part of
  the build is currently wired. This is a concrete integration gap, not a
  successful checker baseline.
- The workspace CLI can be built with the globally installed Riot binary, but
  the check command has not yet been added to this checkout's built CLI. The
  earlier miniriot bootstrap also hit a kernel dependency cycle before reaching
  Syn or Typ; it is not the route used for normal validation.

## Soundness Checkpoint (2026-10-11)

- `Typ` now has explicit synthesis/checking entry points for annotations,
  functions, tuples, lists, conditionals, matches, tries, sequences, local lets,
  and known application arguments. The bidirectional rule contract is in
  [bidirectional_typing.md](./bidirectional_typing.md).
- Let-generalization uses HM levels. Unification performs an occurs check and
  lowers escaping metavariable levels before linking. A conservative
  nonexpansive-value restriction is active; regressions cover environment
  capture and a polymorphic-reference escape.
- Unresolved value, constructor, field, type, type-variable, and module names
  now emit structured source-located errors. Type-constructor arity mismatches are also
  structured errors. Recovery variables may continue traversal but cannot
  erase these diagnostics. Record puns lower to their OCaml value identifiers,
  and an expected nominal record type can disambiguate an unqualified label;
  focused coverage includes qualified/unqualified puns, invalid labels,
  record-update base checking, and source-order precedence across open scopes.
- Common built-in `option`/`result` constructors and basic reference/prelude
  signatures are modeled explicitly. This is not yet the workspace dependency
  environment: imports and full library interfaces still need package-aware
  loading.
- `~/.riot/bin/riot test -p typ --json`: **361 passed, 0 failed** after adding
  character interval pattern checking. This validates Typ's focused/snapshot
  suite only.
- The standalone runner at `packages/typ/examples/corpus_check.ml` enumerated
  **2,404** in-scope tracked files and reported **71 passed, 2,333 failed**:
  **0 parser failures**, **104 AST-lowering failures**, and **2,229 Typ
  failures**. This is not the package-aware gate: it checks files independently
  without dependency interfaces, so unresolved imported modules are expected.
  The runner is a diagnostic aid, not evidence of corpus parity. `riot check`
  coverage remains blocked until unsupported syntax and package-aware loading
  are resolved; no sources may be excluded to make the result green.

## Coverage Tracking

As corpus typechecking becomes runnable, maintain a reviewed map from actual
syntax and typing features to (a) in-scope source examples, (b) focused
`Syn`/`Typ` tests, and (c) diagnostic behavior. Rerun parsing and typing after
each meaningful checker slice. Never make a source pass by silently omitting it
from the corpus or swallowing its diagnostics.
