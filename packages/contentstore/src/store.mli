open Std

(** Generic content-addressable storage rooted at one filesystem directory. *)
type t

type error = string

(** Create a content-addressable store rooted at [root_dir]. *)
val create: root_dir:Path.t -> t

(** Return the root directory backing this store. *)
val root_dir: t -> Path.t

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
