open Std

type novelty = { hit_edges: int; new_edges: int }
type t

val create: unit -> t

val record: t -> bytes -> novelty

val total_edges: t -> int
