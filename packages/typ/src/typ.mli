open Std

(** Typ - Experimental library-first type analysis for Riot

    `typ` is the current prototype package for Riot's future OCaml typechecker.
    It is intentionally split into explicit stages instead of one monolithic
    pass:

    - `Check` orchestrates parse, lower, and infer for one source input.
    - `Lower` turns successful `Syn.Cst` files into a `SemanticTree` with
      recovery nodes and structured lowering diagnostics.
    - `Infer` runs the current prototype inference engine over the semantic tree and
      produces export snapshots plus detailed expression/item traces.
    - `Report` renders those results into snapshot-friendly text so prototype
      behavior is easy to review in fixtures.

    The package is still experimental. The API is intentionally explicit and
    library-shaped, but it does not yet represent the final long-lived surface
    of Riot's future typechecker.

    # Current Shape

    The prototype currently supports a narrow functional core:

    - value bindings and simple `let rec ... and ...`
    - variables, literals, tuples, `if`, `match`, `fun`, `function`
    - positional application and infix lowering through ordinary application

    Unsupported syntax is not silently dropped. It is lowered into explicit
    recovery items or hole expressions with span-backed diagnostics so callers
    can still inspect partial results.

    # Suggested Entry Points

    Most callers should start with:

    - `Typ.Check.check_source` to analyze one source string
    - `Typ.Report.render_report` to render the result for tests or debugging

    The lower-level modules are also exported so the prototype can be exercised
    stage-by-stage while the architecture is still being explored.
*)

(** Structured diagnostics produced by the prototype lowering and inference
    stages. These are separate from `Syn.Diagnostic`, which still owns parse
    diagnostics. *)
module Diagnostic : module type of Diagnostic

(** Semantic tree used by the prototype checker.

    This is the current "middle layer" between `Syn.Cst` and inferred types:
    items, expressions, patterns, and stable-ish origin records live here. *)
module SemanticTree : module type of SemanticTree

(** Prototype type graph representation used during inference. *)
module TypeRepr : module type of TypeRepr

(** Quantified schemes exported from the prototype inferencer. *)
module TypeScheme : module type of TypeScheme

(** Pretty-printers for prototype types and schemes. *)
module TypePrinter : module type of TypePrinter

(** Shared output types for one `Check` run, including exports, diagnostics,
    and per-item / per-expression traces. *)
module Check_result : module type of Check_result

(** CST-to-semantic-tree pass.

    This stage only runs on clean `Syn.Cst` inputs. Unsupported syntax is
    preserved through recovery nodes plus lowering diagnostics. *)
module Lower : module type of Lower

(** Semantic-tree type inference pass.

    The current implementation is a small prototype engine with query-local
    mutable state and snapshot-oriented traces. *)
module Infer : module type of Infer

(** One-shot orchestration entrypoint for the prototype typechecker.

    `Check.check_source` is currently the main API for package tests and
    exploratory tooling. *)
module Check : module type of Check

(** Snapshot-oriented rendering helpers for `Check_result.t`.

    This keeps human-readable output separate from the semantic pipeline so the
    checker can stay library-first. *)
module Report : module type of Report
