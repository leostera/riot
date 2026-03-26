open Kernel
open Kernel.Collections

(** Reactor boundary module for scheduler internals. *)
module Make (Deps : sig
  type runtime
  type command
  type context

  val add_timer :
    runtime ->
    now:int64 ->
    duration_nanos:int64 ->
    mode:Timer.mode ->
    action:Timer.action ->
    Timer.id
  val cancel_timer : runtime -> Timer.id -> unit
  val register_io :
    runtime ->
    token:Async.Token.t ->
    interest:Async.Interest.t ->
    source:Async.Source.t ->
    (unit, IO.error) result
  val deregister_io : runtime -> Async.Source.t -> unit
  val make_context : runtime -> context
  val set_context : context option -> unit
  val should_stop : runtime -> bool
  val has_pending_commands : runtime -> bool
  val drain_commands : runtime -> command list
  val handle_command : runtime -> command -> unit
  val process_timers : runtime -> unit
  val poll_io : runtime -> unit
end) =
struct
  type runtime = Deps.runtime
  type command = Deps.command

  let add_timer = Deps.add_timer
  let cancel_timer = Deps.cancel_timer
  let register_io = Deps.register_io
  let deregister_io = Deps.deregister_io

  let loop t =
    let ctx = Deps.make_context t in
    Deps.set_context (Some ctx);
    while (not (Deps.should_stop t)) || Deps.has_pending_commands t do
      List.iter (Deps.handle_command t) (Deps.drain_commands t);
      Deps.process_timers t;
      if not (Deps.should_stop t) then Deps.poll_io t
    done
end
