open Std

(** Mutable host-owned set of logical sources. *)
type t

(** Start a session with one host-supplied configuration. *)
val empty: config:TypConfig.t -> t

(** Inspect the host configuration carried by the session. *)
val config: t -> TypConfig.t

(** Add one logical source and return its stable [SourceId]. *)
val create_source: t -> kind:Source.kind -> origin:Source.origin -> text:string -> t * SourceId.t

(** Replace the text for one existing source while preserving its [SourceId]. *)
val update_source_text: t -> SourceId.t -> text:string -> t

(** Remove one logical source from future snapshots. *)
val remove_source: t -> SourceId.t -> t

(** Prepare one rooted immutable snapshot.

    Preparing a rooted snapshot validates that all requested root sources are
    present in the session, expands each rooted logical module to any sibling
    [.ml]/[.mli] sources already present in the session, discovers module
    dependencies for that rooted closure, and returns a structured
    missing-requirements payload when they are not available. *)
val prepare_snapshot: t -> roots:SourceId.t list -> (Snapshot.t, MissingRequirements.t) result

(** Freeze the current session state into one immutable [Snapshot].

    Per-source analyses are loaded lazily from the resulting snapshot so LSP
    callers can query only the files they touch, while compiler-style callers
    can still force everything through [Snapshot.analyses] or the batch lane.

    This is the compatibility wrapper for preparing a rooted snapshot over all
    current session sources. *)
val snapshot: t -> Snapshot.t
