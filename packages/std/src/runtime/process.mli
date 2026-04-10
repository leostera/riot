(** Process management and lifecycle. *)
open Kernel

(** The reason a process exited. *)
type exit_reason = exn
(** Opaque reference to a monitor registration. *)
type monitor_ref
(** Process flags. *)
type flag =
  | TrapExit of bool
module Messages: sig
  type Message.t +=
    | EXIT of { from: Pid.t; reason: (unit, exit_reason) result }
    | DOWN of { ref: monitor_ref; pid: Pid.t; reason: (unit, exit_reason) result }
end

(** Scheduler-visible process state. *)
type state =
  private | Uninitialized
  | Runnable
  | Waiting_message
  | Waiting_io of { name: string; token: Kernel.Async.Token.t; source: Kernel.Async.Source.t }
  | Running
  | Exited of (unit, exit_reason) result
  | Finalized
(** Opaque process handle. *)
type t
(** Result of spending one cooperative scheduling reduction. *)
type reduction_result =
  | Continue
  | Yield

(** Create a process from its entry function. *)
val make: (unit -> (unit, exit_reason) result) -> t

(** Initialize a process so it can be scheduled. *)
val init: t -> unit

(** Reset the process-local reduction budget to a new positive value. *)
val reset_reductions: t -> int -> unit

(** Spend one process-local reduction and report whether the process should
    perform a scheduler yield. *)
val use_reduction: t -> reduction_result

(** Return the process identifier. *)
val pid: t -> Pid.t

(** Return the current process state. *)
val state: t -> state

(** Return `true` if the process is still alive. *)
val is_alive: t -> bool

(** Return `true` if the process has exited. *)
val is_exited: t -> bool

(** Return `true` if the process is waiting for a message. *)
val is_waiting: t -> bool

(** Return `true` if the process is waiting for I/O. *)
val is_waiting_io: t -> bool

(** Return `true` if the process is runnable. *)
val is_runnable: t -> bool

(** Return `true` if the process is currently running. *)
val is_running: t -> bool

(** Return `true` if this is the main process. *)
val is_main: t -> bool

(** Transition [Waiting_message] or [Waiting_io] to [Runnable] atomically. *)
val try_set_runnable_if_waiting: t -> bool

(** Transition [Running] to [Waiting_message] atomically. *)
val try_mark_awaiting_message: t -> bool

(** Transition [Waiting_message] to [Runnable] atomically. *)
val try_mark_runnable_from_waiting_message: t -> bool

(** Return `true` if the process mailbox is empty. *)
val has_empty_mailbox: t -> bool

(** Return `true` if the process has pending messages. *)
val has_messages: t -> bool

(** Return the total number of queued messages. *)
val message_count: t -> int

(** Return the number of messages in the main mailbox queue. *)
val mailbox_count: t -> int

(** Return the number of messages in the selective-receive save queue. *)
val save_queue_count: t -> int

(** Mark the process as currently running. *)
val mark_as_running: t -> unit

(** Mark the process as runnable, if it is still alive. *)
val mark_as_runnable: t -> unit

(** Mark the process as waiting for messages. *)
val mark_as_awaiting_message: t -> unit

(** Mark the process as exited with the given reason. *)
val mark_as_exited: t -> (unit, exit_reason) result -> unit

(** Request that the process exit at its next scheduler boundary. *)
val request_exit: t -> (unit, exit_reason) result -> unit

(** Atomically read and clear a pending exit request. *)
val take_exit_request: t -> (unit, exit_reason) result option

(** Mark the process as finalized. *)
val mark_as_finalized: t -> unit

(** Return the current process continuation. *)
val cont: t -> (unit, exit_reason) result Proc_state.t

(** Replace the current process continuation. *)
val set_cont: t -> (unit, exit_reason) result Proc_state.t -> unit

(** Return the next message, prioritizing the selective-receive save queue. *)
val next_message: t -> Message.envelope option

(** Return the next message from the selective-receive save queue only. *)
val next_saved_message: t -> Message.envelope option

(** Return the next message from the main mailbox queue only. *)
val next_mailbox_message: t -> Message.envelope option

(** Add a message to the owner-local selective-receive save queue. *)
val add_to_save_queue: t -> Message.envelope -> unit

(** Send a message to the process mailbox from any scheduler domain. *)
val send_message: t -> Message.t -> unit

(** Mark the process as waiting for the given I/O operation. *)
val mark_as_awaiting_io: t -> name:string -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit

(** Record a ready I/O token for the process. *)
val add_ready_token: t -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit

(** Return the next ready I/O token, if any. *)
val get_ready_token: t -> (Kernel.Async.Token.t * Kernel.Async.Source.t) option

(** Consume all ready I/O tokens with the given callback. *)
val consume_ready_tokens: t -> (Kernel.Async.Token.t * Kernel.Async.Source.t -> unit) -> unit

(** Return `true` if the process has no ready I/O tokens. *)
val has_no_ready_tokens: t -> bool

(** Register the receive timeout timer for this process. *)
val set_receive_timeout: t -> Timer_id.t -> unit

(** Clear the receive timeout timer registration. *)
val clear_receive_timeout: t -> unit

(** Return the current receive timeout timer identifier, if any. *)
val receive_timeout: t -> Timer_id.t option

(** Register the syscall timeout timer for this process. *)
val set_syscall_timeout: t -> Timer_id.t -> unit

(** Clear the syscall timeout timer registration. *)
val clear_syscall_timeout: t -> unit

(** Return the current syscall timeout timer identifier, if any. *)
val syscall_timeout: t -> Timer_id.t option

(** Return `true` if the current receive-timeout registration matches the timer
    identifier. *)
val has_receive_timeout_id: t -> Timer_id.t -> bool

(** Return `true` if the current syscall-timeout registration matches the timer
    identifier. *)
val has_syscall_timeout_id: t -> Timer_id.t -> bool

(** Mark the current receive timeout as fired. *)
val mark_receive_timeout_fired: t -> unit

(** Mark the current syscall timeout as fired. *)
val mark_syscall_timeout_fired: t -> unit

(** Atomically read and clear the receive-timeout fired state. *)
val take_receive_timeout_fired: t -> bool

(** Atomically read and clear the syscall-timeout fired state. *)
val take_syscall_timeout_fired: t -> bool

(** {1 Process Flags} *)
(** Set the process flags. *)
val set_flags: t -> flag list -> unit

(** Return the current [trap_exit] flag. *)
val get_trap_exit: t -> bool

(** {1 Process Links and Monitors} *)
(** Create a bidirectional link between this process and the target PID. *)
val link: t -> Pid.t -> unit

(** Remove a bidirectional link. *)
val unlink: t -> Pid.t -> unit

(** Monitor a process and return the monitor reference. *)
val monitor: t -> Pid.t -> monitor_ref

(** Remove a monitor. *)
val demonitor: t -> monitor_ref -> unit

(** Look up the monitored PID associated with a monitor reference. *)
val monitored_pid_for_ref: t -> monitor_ref -> Pid.t option

(** Return the list of linked processes. *)
val get_links: t -> Pid.t list

(** Return the list of monitors created by this process. *)
val get_monitors: t -> (monitor_ref * Pid.t) list

(** Return the list of processes currently monitoring this process. *)
val get_monitored_by: t -> (Pid.t * monitor_ref) list

(** Add a process to the monitored-by list. *)
val add_monitored_by: t -> Pid.t -> monitor_ref -> unit

(** Remove a process from the monitored-by list. *)
val remove_monitored_by: t -> Pid.t -> monitor_ref -> unit

(** Return `true` if this process is linked to the given PID. *)
val is_linked: t -> Pid.t -> bool

(** Return `true` if this process is monitoring the given PID. *)
val is_monitoring: t -> Pid.t -> bool
