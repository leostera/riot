open Std

(** In-memory export-facing summary for one analyzed source.

    Once a source finishes checking, the reusable type environment it exports
    is captured here. Hosts should persist or reload that boundary through
    {!ModuleTypings} rather than adding serialization concerns to the core
    semantic layers. *)
type type_decl = {
  (** Lexical module path that owns the declaration, empty at top level. *)
  scope_path: string list;
  (** Lowered declaration summary exported by the source. *)
  declaration: TypeDecl.t;
}
type exports = (string * TypeScheme.t) list
type export_result =
  (** Exports are safe enough for downstream reuse. *)
  | TrustedExport of { exports: exports }
  (** Exports were still computed, but the source had type or lowering errors. *)
  | ErroredExport of { exports: exports }
  (** No export can be trusted for this source revision. *)
  | NoExport
type t = {
  (** Source summarized by this export result. *)
  source_id: SourceId.t;
  (** Trust-classified export payload. *)
  export_result: export_result;
  (** Exported lowered type declarations preserved for summary hydration. *)
  type_decls: type_decl list;
}

(** Build a trusted export summary. *)
val trusted: source_id:SourceId.t -> ?type_decls:type_decl list -> exports -> t

(** Build an export summary that carries results despite analysis errors. *)
val errored: source_id:SourceId.t -> ?type_decls:type_decl list -> exports -> t

(** Build a summary with no reusable export. *)
val missing: source_id:SourceId.t -> ?type_decls:type_decl list -> unit -> t

(** Extract the export environment, or [[]] for [NoExport]. *)
val exports: t -> exports

(** Extract the lowered exported type declarations. *)
val type_decls: t -> type_decl list

(** Encode the export summary as structured JSON for snapshot tests and tooling. *)
val to_json: t -> Data.Json.t

(** Render the summary as debug text. *)
val to_string: t -> string
