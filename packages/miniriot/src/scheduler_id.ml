open Kernel

type t = Scheduler_id of int

let zero = Scheduler_id 0

let of_int value =
  if Int.compare value 0 < 0 then
    panic "Scheduler_id.of_int expects a non-negative integer";
  Scheduler_id value

let to_int (Scheduler_id value) = value
let succ (Scheduler_id value) = Scheduler_id (value + 1)
let equal (Scheduler_id a) (Scheduler_id b) = Int.equal a b
let compare (Scheduler_id a) (Scheduler_id b) = Int.compare a b
