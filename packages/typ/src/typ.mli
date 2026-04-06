open Std

(** Typ - Experimental library-first type analysis for Riot

    `typ` is the current prototype package for Riot's future OCaml typechecker.
    Its public shape now follows the architecture proposed in the RFD more
    closely:

    - `Session` owns logical sources and stable `SourceId`s
    - `Snapshot` is an immutable analysis revision
    - `Query` exposes query-first semantic access over one snapshot
    - `Batch` and `Check` are convenience wrappers for one-shot callers
    - `Lower` and `Infer` still implement a basic functional-subset pipeline
      underneath that architecture

    The current checker remains intentionally limited, but the plumbing is now
    arranged so future work can focus on richer type rules, better diagnostics,
    and more fixtures instead of repeated structural churn.

    # Current Semantic Layers

    The prototype lowers clean `Syn.Cst` files into explicit semantic storage:

    - `ItemTree`
      top-level item skeletons that are stable over many body edits
    - `BodyArena`
      normalized expressions, patterns, and bindings
    - `OriginMap`
      source-backed origins for semantic IDs in one source revision
    - `FileSummary`
      export-facing result with an explicit trust state
    - `ModuleTypings`
      canonical reusable module-typing artifacts paired with module identity
      and input hashes
    - `TypeIndex`
      a query-oriented expression-type index used by `Query.type_at`

    A convenience `SemanticTree` wrapper keeps those pieces together for the
    current prototype inferencer and report snapshots.

    # Suggested Entry Points

    New callers should prefer:

    - `Typ.Session.empty`
    - `Typ.Session.create_source`
    - `Typ.Session.prepare_snapshot`
    - `Typ.Query.diagnostics`
    - `Typ.Query.type_at`
    - `Typ.Query.module_typings_of`

    Existing tests and batch-oriented tools can still use:

    - `Typ.Batch.check_source`
    - `Typ.Check.check_source`
    - `Typ.Report.render_report`
*)
module SourceId: module type of SourceId

module ItemId: module type of ItemId

module BindingId: module type of BindingId

module ExprId: module type of ExprId

module PatId: module type of PatId

module OriginId: module type of OriginId

module Position: module type of Position

module Source: module type of Source

module MissingRequirements: module type of MissingRequirements

module Diagnostic: module type of Diagnostic

module Explanations: module type of Explanations

module OriginMap: module type of OriginMap

module ItemTree: module type of ItemTree

module BodyArena: module type of BodyArena

module SemanticTree: module type of SemanticTree

module TypeDecl: module type of TypeDecl

module TypeRepr: module type of TypeRepr

module TypeScheme: module type of TypeScheme

module TypePrinter: module type of TypePrinter

module TypeIndex: module type of TypeIndex

module FileSummary: module type of FileSummary

module ModuleTypings: module type of ModuleTypings

module Config: module type of TypConfig

module Check_result: module type of Check_result

module Lower: module type of Lower

module Infer: module type of Infer

module SourceAnalysis: module type of SourceAnalysis

module Snapshot: module type of Snapshot

module Session: module type of Session

module Query: module type of Query

module Batch: module type of Batch

module Check: module type of Check

module Report: module type of Report
