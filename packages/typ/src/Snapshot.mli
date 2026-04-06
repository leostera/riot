open Std

(** Immutable analysis view over one [Session] revision. *)
type t

(** Build a rooted snapshot from the current session sources.

    Analyses are prepared lazily per source and only forced by queries that
    actually need them. Before the final per-source analysis runs, the snapshot
    may synthesize an ambient environment from sibling source exports and
    host-loaded module summaries so implicit file modules can participate in
    name resolution. *)
val make: revision:int -> roots:SourceId.t list -> config:TypConfig.t -> sources:Source.t list -> t

(** Monotonic session revision captured by this snapshot. *)
val revision: t -> int

(** Root sources this snapshot was prepared for. *)
val roots: t -> SourceId.t list

(** Enumerate all per-source analyses in this snapshot.

    Calling this forces every lazy per-root analysis. *)
val analyses: t -> SourceAnalysis.t list

(** Enumerate the in-memory export summaries for every source in this snapshot.

    Calling this forces every lazy per-root analysis. *)
val file_summaries: t -> FileSummary.t list

(** Enumerate the host-facing persisted summaries for every rooted module in
    this snapshot.

    This follows the same canonical per-module boundary as [module_summaries],
    so paired [.ml]/[.mli] roots contribute one persisted summary.

    Calling this forces every lazy per-root analysis. *)
val persisted_summaries: t -> PersistedSummary.t list

(** Enumerate the host-facing module summaries for every rooted module in this
    snapshot.

    Calling this forces every lazy per-root analysis. *)
val module_summaries: t -> ModuleSummary.t list

(** Find the canonical host-facing module summary for one rooted source's
    module, when present.

    This preserves the rooted query boundary while ensuring callers see the
    same canonical [ModuleSummary] that [module_summaries] enumerates for that
    module. *)
val find_module_summary: t -> SourceId.t -> ModuleSummary.t option

(** Find the analysis for one logical source within this snapshot.

    Calling this forces analysis only for the requested root source. *)
val find_analysis: t -> SourceId.t -> SourceAnalysis.t option
