open Std

(** Export-facing summary for one analyzed source. *)
type exports = (string * TypeScheme.t) list

type export_result =
  (** Exports are safe enough for downstream reuse. *)
  | TrustedExport of {
      exports: exports;
    }
  (** Exports were still computed, but the source had type or lowering errors. *)
  | ErroredExport of {
      exports: exports;
    }
  (** No export can be trusted for this source revision. *)
  | NoExport

type t = {
  (** Source summarized by this export result. *)
  source_id: SourceId.t;
  (** Trust-classified export payload. *)
  export_result: export_result;
}

(** Build a trusted export summary. *)
val trusted: source_id:SourceId.t -> exports -> t

(** Build an export summary that carries results despite analysis errors. *)
val errored: source_id:SourceId.t -> exports -> t

(** Build a summary with no reusable export. *)
val missing: source_id:SourceId.t -> t

(** Extract the export environment, or [[]] for [NoExport]. *)
val exports: t -> exports

(** Encode the export summary as structured JSON for snapshot tests and tooling. *)
val to_json: t -> Data.Json.t

(** Render the summary as debug text. *)
val to_string: t -> string
