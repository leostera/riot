open Kernel

type t = ..

type envelope = {
  msg: t;
  uid: int;
}

let uid_counter = Sync.Atomic.make 0

let envelope = fun msg ->
  let rec next_id () =
    let current = Sync.Atomic.get uid_counter in
    let next = current + 1 in
    if Sync.Atomic.compare_and_set uid_counter current next then
      next
    else
      next_id ()
  in
  { msg; uid = next_id () }
