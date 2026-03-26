open Kernel
open Kernel.Collections
open Kernel.Sync

type process_slot = {
  process : Process.t;
  (* Runtime-owned scheduling metadata.
     Process continuations/mailboxes live on [Process.t], while ownership and
     queue membership live here so workers can transfer slots without mutating
     process internals. *)
  owner_worker : Scheduler_id.t Atomic.t;
  queued : bool Atomic.t;
}

type worker = {
  id : Scheduler_id.t;
  queue : process_slot Queue.t;
  lock : Mutex.t;
  cond : Condition.t;
}

type 'a response = {
  lock : Mutex.t;
  cond : Condition.t;
  mutable value : 'a option;
}

type reactor_command =
  | Add_timer of {
      now : int64;
      duration_nanos : int64;
      mode : Timer.mode;
      action : Timer.action;
      reply : Timer.id response;
    }
  | Cancel_timer of Timer.id
  | Register_io of {
      token : Async.Token.t;
      interest : Async.Interest.t;
      source : Async.Source.t;
      reply : (unit, IO.error) result response;
    }
  | Deregister_io of Async.Source.t

type process_shard = {
  lock : Mutex.t;
  processes : (Pid.t, process_slot) HashMap.t;
}

type process_registry = {
  shards : process_shard array;
  size : int Atomic.t;
}

type runtime_counters = {
  steals : int Atomic.t;
  failed_steals : int Atomic.t;
  remote_wakeups : int Atomic.t;
  duplicate_enqueue_races : int Atomic.t;
}

type t = {
  stop : bool Atomic.t;
  status : int Atomic.t;
  workers : worker array;
  processes : process_registry;
  counters : runtime_counters;
  relations_lock : Mutex.t;
  reactor_commands : reactor_command Queue.t;
  reactor_lock : Mutex.t;
  io_poll : Async.Poll.t;
  timer_wheel : Timer_wheel.t;
  config : Config.t;
}

type domain_context = {
  scheduler : t;
  worker_id : Scheduler_id.t option;
  mutable current_process : Process.t option;
}

let current_context : domain_context option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)
