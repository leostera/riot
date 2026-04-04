open Std

(** Immutable analysis view over one [Session] revision. *)
type t

(** Build a snapshot from the current session sources.

    Analyses are prepared lazily per source and only forced by queries that
    actually need them. *)
val make: revision:int -> config:TypConfig.t -> sources:Source.t list -> t

(** Monotonic session revision captured by this snapshot. *)
val revision: t -> int

(** Enumerate all per-source analyses in this snapshot.

    Calling this forces every lazy per-source analysis. *)
val analyses: t -> SourceAnalysis.t list

(** Find the analysis for one logical source within this snapshot.

    Calling this forces analysis only for the requested source. *)
val find_analysis: t -> SourceId.t -> SourceAnalysis.t option
