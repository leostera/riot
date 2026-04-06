open Std

(** Semantic persistence layer for [typ] built on top of [Contentstore].

    [Typ.Store] hides the storage details needed to persist and reload
    canonical [ModuleTypings] values while keeping [typ] itself generic over
    the concrete host cache implementation. *)
type t

(** Build a [Typ.Store] on top of a generic [Contentstore]. *)
val create: Contentstore.t -> unit -> t

(** Load canonical module typings for one module name, when present. *)
val load_module_typings: t -> module_name:string -> ModuleTypings.t option

(** Load canonical module typings by their source hash, when present. *)
val load_module_typings_by_hash: t -> source_hash:Crypto.hash -> ModuleTypings.t option

(** Persist canonical module typings in both hash-addressed and module-name
    indexed forms. *)
val save_module_typings: t -> ModuleTypings.t -> (unit, string) result
