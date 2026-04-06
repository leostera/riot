open Std

(** Host-supplied configuration for one [Session]. *)
type env = (string * TypeScheme.t) list
type t = {
  (** Intrinsic language-level bindings visible in every source.

      This must stay small. Library/module APIs should arrive through persisted
      module summaries and host-provided ambient inputs, not by growing the
      default prelude. *)
  prelude: env;
  (** Host-loaded persisted module summaries available to every source in the
      session. *)
  loaded_modules: ModuleSummary.t list;
  (** Snapshot-scoped bindings synthesized from sibling sources or host context. *)
  ambient: env;
  (** Snapshot-scoped lowered type declarations synthesized from summaries. *)
  ambient_type_decls: FileSummary.type_decl list;
}

(** Default host configuration used by the current prototype and tests.

    The default keeps [prelude] limited to language-level intrinsics and seeds
    a small set of bootstrap module summaries through [loaded_modules]. Those
    seeded summaries are a temporary stand-in for real persisted module exports
    and intentionally flow through the same [ModuleSummary] boundary that hosts
    will later hydrate from cache or build outputs. *)
val default: t

(** Replace the snapshot ambient environment while preserving the base prelude. *)
val with_ambient: t -> ambient:env -> t

(** Replace the snapshot ambient type declarations while preserving the base
    prelude, loaded modules, and ambient value environment. *)
val with_ambient_type_decls: t -> ambient_type_decls:FileSummary.type_decl list -> t

(** Replace the host-loaded persisted module summaries while preserving the
    base prelude and snapshot ambient environment. *)
val with_loaded_modules: t -> loaded_modules:ModuleSummary.t list -> t
