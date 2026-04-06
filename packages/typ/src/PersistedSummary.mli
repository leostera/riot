open Std

(** Host-facing persisted representation of one reusable module export summary.

    [typ] computes in-memory {!FileSummary.t} values while analyzing sources.
    Hosts such as the build system, LSP, or future cache layers should cross
    the persistence boundary through this module instead of depending directly
    on one concrete serialization format.

    Today the persistence format is JSON. Later work can add or replace storage
    codecs without changing the semantic boundary exposed here. *)
type t

(** Capture a checked source's reusable export summary for persistence. *)
val of_file_summary: FileSummary.t -> t

(** Recover the in-memory export summary from one persisted value. *)
val to_file_summary: t -> FileSummary.t

(** Inspect the summarized source identity carried by the persisted value. *)
val source_id: t -> SourceId.t

(** Extract the exported environment carried by the persisted value. *)
val exports: t -> FileSummary.exports

(** Extract the exported lowered type declarations carried by the persisted
    value. *)
val type_decls: t -> FileSummary.type_decl list

module Json: sig
  (** Encode a persisted summary as structured JSON.

      This is the current prototype serialization format. *)
  val to_json: t -> Data.Json.t

  (** Decode a persisted summary from structured JSON. *)
  val of_json: Data.Json.t -> (t, string) result
end
