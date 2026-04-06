open Std

(** Host-facing module summary artifact.

    This wraps a persisted reusable summary with the module identity a host
    needs for cache lookup and ambient loading. Hosts can serialize these
    values to JSON, store them by [source_hash], and later reload them into a
    new [TypConfig.t] without depending on internal checker state. *)
type t

(** Build one host-facing module summary. *)
val make: module_name:string -> source_hash:Crypto.hash -> summary:PersistedSummary.t -> t

(** Recover the module name associated with this summary. *)
val module_name: t -> string

(** Recover the source input hash associated with this summary. *)
val source_hash: t -> Crypto.hash

(** Recover the persisted export summary carried by this module summary. *)
val summary: t -> PersistedSummary.t

(** Extract the exported environment from the underlying persisted summary. *)
val exports: t -> FileSummary.exports

(** Extract the exported lowered type declarations from the underlying
    persisted summary. *)
val type_decls: t -> FileSummary.type_decl list

module Json: sig
  (** Encode a module summary as structured JSON. *)
  val to_json: t -> Data.Json.t

  (** Decode a module summary from structured JSON. *)
  val of_json: Data.Json.t -> (t, string) result
end
