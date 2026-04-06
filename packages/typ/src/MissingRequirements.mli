open Std

(** Structured requirements that prevented a rooted snapshot from being
    prepared. *)
type requirement =
  | MissingRootSource of { source_id: SourceId.t }
  | MissingModuleSummary of { module_name: string; requested_by: SourceId.t list }
(** Opaque collection of missing requirements. *)
type t

(** Build one missing-requirements payload from prepared requirements. *)
val of_list: requirement list -> t

(** Enumerate requirements in canonical deterministic order.

    Missing root sources are sorted by [SourceId], and missing module-summary
    requirements are sorted by module name with deduplicated, sorted
    [requested_by] source ids. *)
val requirements: t -> requirement list

(** Whether no requirements are missing. *)
val is_empty: t -> bool

(** Encode the payload as structured JSON for tests and tooling. *)
val to_json: t -> Data.Json.t
