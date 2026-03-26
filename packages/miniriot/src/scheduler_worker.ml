open Kernel
open Kernel.Sync
open Scheduler_types

let loop
    ~(pop_local : worker -> process_slot option)
    ~(step_process : t -> domain_context -> process_slot -> unit)
    ~(attempt_steal : t -> worker -> bool)
    ~(wait_for_local_work : t -> worker -> process_slot option)
    (runtime : t)
    (worker : worker)
    =
  let ctx =
    {
      scheduler = runtime;
      worker_id = Some worker.id;
      current_process = None;
    }
  in
  Domain.DLS.set current_context (Some ctx);
  while not (Atomic.get runtime.stop) do
    match pop_local worker with
    | Some slot ->
        step_process runtime ctx slot
    | None ->
        if not (attempt_steal runtime worker) then
          match wait_for_local_work runtime worker with
          | None -> ()
          | Some slot -> step_process runtime ctx slot
  done;
  ctx.current_process <- None
