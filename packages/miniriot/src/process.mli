(** Process management and lifecycle *)
open Kernel

type exit_reason = exn
(** Reasons why a process might exit *)

type monitor_ref
(** Opaque reference to a monitor *)

type flag = TrapExit of bool
(** Process flags *)

module Messages : sig
  type Message.t +=
    | EXIT of { from : Pid.t; reason : (unit, exit_reason) result }
    | DOWN of {
        ref : monitor_ref;
        pid : Pid.t;
        reason : (unit, exit_reason) result;
      }
end

type state = private
  | Uninitialized
  | Runnable
  | Waiting_message
  | Waiting_io of {
      name : string;
      token : Kernel.Async.Token.t;
      source : Kernel.Async.Source.t;
    }
  | Running
  | Exited of (unit, exit_reason) result
  | Finalized(** Process state - tracks current status in scheduler *)

type t
(** Opaque process type *)

val make : (unit -> (unit, exit_reason) result) -> t
(** Create a new process from a function *)

val init : t -> unit
(** Initialize a process, making it ready to run *)

val pid : t -> Pid.t
(** Get the process ID *)

val state : t -> state
(** Get the current process state *)

val is_alive : t -> bool
(** Check if process is alive (not exited or finalized) *)

val is_exited : t -> bool
(** Check if process has exited *)

val is_waiting : t -> bool
(** Check if process is waiting for messages *)

val is_waiting_io : t -> bool
(** Check if process is waiting for I/O *)

val is_runnable : t -> bool
(** Check if process is runnable *)

val is_running : t -> bool
(** Check if process is currently running *)

val is_main : t -> bool
(** Check if this is the main process *)

val try_set_runnable_if_waiting : t -> bool
(** Transition [Waiting_message] or [Waiting_io] to [Runnable] atomically. *)

val try_mark_awaiting_message : t -> bool
(** Transition [Running] to [Waiting_message] atomically. *)

val try_mark_runnable_from_waiting_message : t -> bool
(** Transition [Waiting_message] to [Runnable] atomically. *)

val has_empty_mailbox : t -> bool
(** Check if process mailbox is empty *)

val has_messages : t -> bool
(** Check if process has pending messages *)

val message_count : t -> int
(** Get total number of messages in mailbox *)

val mailbox_count : t -> int
(** Get the number of messages currently in the main mailbox queue. *)

val save_queue_count : t -> int
(** Get the number of messages currently in the selective-receive save queue. *)

val mark_as_running : t -> unit
(** Mark process as currently running *)

val mark_as_runnable : t -> unit
(** Mark process as runnable (if alive) *)

val mark_as_awaiting_message : t -> unit
(** Mark process as waiting for messages *)

val mark_as_exited : t -> (unit, exit_reason) result -> unit
(** Mark process as exited with given reason *)

val mark_as_finalized : t -> unit
(** Mark process as finalized *)

val cont : t -> (unit, exit_reason) result Proc_state.t
(** Get process continuation *)

val set_cont : t -> (unit, exit_reason) result Proc_state.t -> unit
(** Set process continuation *)

val next_message : t -> Message.envelope option
(** Get next message, prioritizing saved selective-receive messages first. *)

val next_saved_message : t -> Message.envelope option
(** Get next message from the selective-receive save queue only. *)

val next_mailbox_message : t -> Message.envelope option
(** Get next message from the main mailbox queue only. *)

val add_to_save_queue : t -> Message.envelope -> unit
(** Add message to the owner-local selective-receive save queue. *)

val send_message : t -> Message.t -> unit
(** Send a message to the process mailbox from any scheduler domain. *)

val mark_as_awaiting_io :
  t -> name:string -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit
(** Mark process as waiting for I/O operation *)

val add_ready_token : t -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit
(** Add a ready I/O token to the process *)

val get_ready_token : t -> (Kernel.Async.Token.t * Kernel.Async.Source.t) option
(** Get next ready I/O token *)

val consume_ready_tokens :
  t -> (Kernel.Async.Token.t * Kernel.Async.Source.t -> unit) -> unit
(** Consume all ready I/O tokens with a function *)

val has_no_ready_tokens : t -> bool
(** Check if process has no ready I/O tokens *)

val set_receive_timeout : t -> Timer_id.t -> unit
(** Set the receive timeout timer ID for this process *)

val clear_receive_timeout : t -> unit
(** Clear the receive timeout timer *)

val receive_timeout : t -> Timer_id.t option
(** Get the current receive timeout timer ID, if any *)

val set_syscall_timeout : t -> Timer_id.t -> unit
(** Set the syscall timeout timer ID for this process *)

val clear_syscall_timeout : t -> unit
(** Clear the syscall timeout timer *)

val syscall_timeout : t -> Timer_id.t option
(** Get the current syscall timeout timer ID, if any *)

val has_receive_timeout_id : t -> Timer_id.t -> bool
(** Check if the current receive-timeout registration matches a timer ID. *)

val has_syscall_timeout_id : t -> Timer_id.t -> bool
(** Check if the current syscall-timeout registration matches a timer ID. *)

val mark_receive_timeout_fired : t -> unit
(** Mark the currently registered receive timeout as fired. *)

val mark_syscall_timeout_fired : t -> unit
(** Mark the currently registered syscall timeout as fired. *)

val take_receive_timeout_fired : t -> bool
(** Atomically read-and-clear receive-timeout fired state. *)

val take_syscall_timeout_fired : t -> bool
(** Atomically read-and-clear syscall-timeout fired state. *)

(** {1 Process Flags} *)

val set_flags : t -> flag list -> unit
(** Set process flags *)

val get_trap_exit : t -> bool
(** Get current trap_exit flag *)

(** {1 Process Links and Monitors} *)

val link : t -> Pid.t -> unit
(** Create bidirectional link between this process and target *)

val unlink : t -> Pid.t -> unit
(** Remove bidirectional link *)

val monitor : t -> Pid.t -> monitor_ref
(** Monitor a process. Returns reference for later demonitor *)

val demonitor : t -> monitor_ref -> unit
(** Remove a monitor *)

val monitored_pid_for_ref : t -> monitor_ref -> Pid.t option
(** Lookup the monitored PID associated with a monitor reference. *)

val get_links : t -> Pid.t list
(** Get list of linked processes *)

val get_monitors : t -> (monitor_ref * Pid.t) list
(** Get list of monitors we've created *)

val get_monitored_by : t -> (Pid.t * monitor_ref) list
(** Get list of processes monitoring us *)

val add_monitored_by : t -> Pid.t -> monitor_ref -> unit
(** Add a process to the monitored_by list (internal use) *)

val remove_monitored_by : t -> Pid.t -> monitor_ref -> unit
(** Remove a process from the monitored_by list (internal use) *)

val is_linked : t -> Pid.t -> bool
(** Check if linked to a process *)

val is_monitoring : t -> Pid.t -> bool
(** Check if monitoring a process *)
