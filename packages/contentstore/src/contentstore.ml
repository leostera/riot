open Std

module Policy = Policy
module Store = Store

type t = Store.t

let create = Store.create
let root = Store.root
let hash_dir_of = Store.hash_dir_of
let exists = Store.exists
