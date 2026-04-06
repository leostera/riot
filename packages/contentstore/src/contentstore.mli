open Std

module Policy = Policy
module Store = Store

type t = Store.t

val create: root:Path.t -> policy:Policy.t -> unit -> t
val root: t -> Path.t
val hash_dir_of: t -> Crypto.hash -> Path.t
val exists: t -> Crypto.hash -> bool
