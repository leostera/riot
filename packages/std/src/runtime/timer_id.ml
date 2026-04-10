open Kernel

type t = int64

let next_id = Sync.Atomic.make 0L

let make (): t =
  let rec try_increment () =
    let current = Sync.Atomic.get next_id in
    let next = Int64.add current 1L in
    if Sync.Atomic.compare_and_set next_id current next then
      next
    else
      try_increment ()
  in
  try_increment ()

let equal: t -> t -> bool = fun a b ->
  Int64.equal a b

let compare: t -> t -> int = fun a b ->
  Int64.compare a b
