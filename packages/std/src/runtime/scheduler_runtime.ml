open Kernel
open Kernel.Collections
open Kernel.Sync
open Scheduler_types

type deps = {
  ensure_can_run_once: unit -> unit;
  create: config:Config.t -> t;
  spawn_on_worker:
    t -> worker_id:Scheduler_id.t -> (unit -> (unit, Process.exit_reason) result) -> Pid.t;
  worker_loop: t -> worker -> unit;
  reactor_loop: t -> unit;
  join_blocking_lanes: t -> unit;
}

let run = fun deps ~config ~main ->
  deps.ensure_can_run_once ();
  let t = deps.create ~config in
  ignore (deps.spawn_on_worker t ~worker_id:Scheduler_id.zero main);
  let reactor_domain =
    Domain.spawn (fun () -> deps.reactor_loop t)
  in
  let worker_domains =
    Array.init (Array.length t.workers - 1)
      (fun idx ->
        let worker = t.workers.(idx + 1) in
        Domain.spawn
          (fun () ->
            deps.worker_loop t worker))
  in
  deps.worker_loop t t.workers.(0);
  Array.iter Domain.join worker_domains;
  Domain.join reactor_domain;
  deps.join_blocking_lanes t;
  Atomic.get t.status
