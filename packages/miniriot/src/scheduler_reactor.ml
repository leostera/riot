open Kernel
open Kernel.Collections
open Kernel.Sync
open Scheduler_types

let loop
    ~(has_pending_commands : t -> bool)
    ~(drain_commands : t -> reactor_command list)
    ~(handle_command : t -> reactor_command -> unit)
    ~(process_timers : t -> unit)
    ~(poll_io : t -> unit)
    (runtime : t)
    =
  Domain.DLS.set current_context
    (Some { scheduler = runtime; worker_id = None; current_process = None });
  while (not (Atomic.get runtime.stop)) || has_pending_commands runtime do
    List.iter (handle_command runtime) (drain_commands runtime);
    process_timers runtime;
    if not (Atomic.get runtime.stop) then poll_io runtime
  done
