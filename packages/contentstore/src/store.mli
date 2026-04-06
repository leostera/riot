open Std

type policy = Policy.t

(** Generic content-addressable storage rooted at one filesystem directory. *)
type t

type error = string

(** Create a content-addressable store rooted at [root]. *)
val create: root:Path.t -> policy:policy -> unit -> t

(** Return the root directory backing this store. *)
val root: t -> Path.t

(** Return the stable hash-addressed directory for [hash], whether or not it
    exists yet. *)
val hash_dir_of: t -> Crypto.hash -> Path.t

(** Check whether a hash-addressed directory currently exists. *)
val exists: t -> Crypto.hash -> bool

(** Atomically commit [source_dir] into the hash-addressed location for [hash].

    If another writer already committed the same [hash], this is a no-op
    success and the caller may discard [source_dir]. *)
val commit_dir: t -> hash:Crypto.hash -> source_dir:Path.t -> (unit, error) result

(** Save one arbitrary blob in a namespaced cache area keyed by [hash]. *)
val save_blob: t -> namespace:string -> hash:Crypto.hash -> content:string -> (unit, error) result

(** Load one arbitrary blob from a namespaced cache area keyed by [hash]. *)
val load_blob: t -> namespace:string -> hash:Crypto.hash -> string option

(** Save one JSON value in a namespaced cache area keyed by [hash]. *)
val save_json_bundle:
  t -> namespace:string -> hash:Crypto.hash -> json:Data.Json.t -> (unit, error) result

(** Load one JSON value from a namespaced cache area keyed by [hash]. *)
val load_json_bundle: t -> namespace:string -> hash:Crypto.hash -> Data.Json.t option

(** Save one arbitrary blob in a mutable namespaced index keyed by [key]. *)
val save_named_blob: t -> namespace:string -> key:string -> content:string -> (unit, error) result

(** Load one arbitrary blob from a mutable namespaced index keyed by [key]. *)
val load_named_blob: t -> namespace:string -> key:string -> string option

(** Save one JSON value in a mutable namespaced index keyed by [key]. *)
val save_named_json_bundle:
  t -> namespace:string -> key:string -> json:Data.Json.t -> (unit, error) result

(** Load one JSON value from a mutable namespaced index keyed by [key]. *)
val load_named_json_bundle: t -> namespace:string -> key:string -> Data.Json.t option
