open Std

(** Canonical reusable module-typing artifact.

    [ModuleTypings] is the single host-facing value that build, LSP, and future
    cache layers persist, reload, merge, and hand back into new [Session]s.
    It carries the exported typing facts for one module together with the
    module identity and source hash a host needs for provenance and reuse. *)
type t

(** Build trusted module typings. *)
val trusted:
  module_name:string ->
  source_hash:Crypto.hash ->
  ?type_decls:FileSummary.type_decl list ->
  FileSummary.exports ->
  t

(** Build module typings that still carry exports despite diagnostics. *)
val errored:
  module_name:string ->
  source_hash:Crypto.hash ->
  ?type_decls:FileSummary.type_decl list ->
  FileSummary.exports ->
  t

(** Build module typings with no reusable export payload. *)
val missing:
  module_name:string ->
  source_hash:Crypto.hash ->
  ?type_decls:FileSummary.type_decl list ->
  unit ->
  t

(** Lift one per-source [FileSummary] into canonical module typings. *)
val of_file_summary:
  module_name:string ->
  source_hash:Crypto.hash ->
  FileSummary.t ->
  t

(** Recover one per-source [FileSummary] from module typings.

    This is mainly useful for tests and compatibility seams that still consume
    source-local summaries. *)
val to_file_summary: source_id:SourceId.t -> t -> FileSummary.t

(** Build a deterministic synthetic source hash for module typings that do not
    come from one real source input, such as bootstrap or merged dependency
    summaries. *)
val synthetic_source_hash:
  module_name:string ->
  export_result:FileSummary.export_result ->
  type_decls:FileSummary.type_decl list ->
  Crypto.hash

(** Recover the module name associated with these typings. *)
val module_name: t -> string

(** Recover the source hash associated with these typings. *)
val source_hash: t -> Crypto.hash

(** Recover the export trust result carried by these typings. *)
val export_result: t -> FileSummary.export_result

(** Extract the exported environment carried by these typings. *)
val exports: t -> FileSummary.exports

(** Extract the exported lowered type declarations carried by these typings. *)
val type_decls: t -> FileSummary.type_decl list

module Json: sig
  (** Encode module typings as structured JSON. *)
  val to_json: t -> Data.Json.t

  (** Decode module typings from structured JSON. *)
  val of_json: Data.Json.t -> (t, string) result
end
