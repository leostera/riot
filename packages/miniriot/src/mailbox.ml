open Kernel
open Kernel.Collections
open Kernel.Sync

type t = {
  producer_lock : Mutex.t;
  mutable inbox_rev : Message.envelope list;
  mutable outbox : Message.envelope list;
  size : int Atomic.t;
}

let create () =
  {
    producer_lock = Mutex.create ();
    inbox_rev = [];
    outbox = [];
    size = Atomic.make 0;
  }

let queue t msg =
  Mutex.lock t.producer_lock;
  t.inbox_rev <- msg :: t.inbox_rev;
  Mutex.unlock t.producer_lock;
  ignore (Atomic.fetch_and_add t.size 1)

let pop_outbox t =
  match t.outbox with
  | [] -> None
  | msg :: rest ->
      t.outbox <- rest;
      ignore (Atomic.fetch_and_add t.size (-1));
      Some msg

let next t =
  match pop_outbox t with
  | Some _ as msg -> msg
  | None ->
      Mutex.lock t.producer_lock;
      let drained = t.inbox_rev in
      t.inbox_rev <- [];
      Mutex.unlock t.producer_lock;
      if List.is_empty drained then
        None
      else (
        t.outbox <- List.rev drained;
        pop_outbox t)

let size t = Atomic.get t.size
let is_empty t = Int.equal (Atomic.get t.size) 0
