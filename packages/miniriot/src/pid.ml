open Kernel
open Kernel.Sync

type t = int

let counter = Atomic.make (-1)

let main = 0

let next () =
  let rec next_id () =
    let current = Atomic.get counter in
    let next = current + 1 in
    if Atomic.compare_and_set counter current next then next else next_id ()
  in
  next_id ()

let equal = Int.equal
let compare = Int.compare
let to_int t = t
let to_string t = "pid<" ^ (Int.to_string t)^ ">"
