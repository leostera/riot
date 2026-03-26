open Kernel

(** Worker boundary module for scheduler internals. *)
module Make (Deps : sig
  type runtime
  type state
  type slot
  type context

  val make_context : runtime -> state -> context
  val set_context : context option -> unit
  val clear_current_process : context -> unit
  val should_stop : runtime -> bool
  val pop_local : state -> slot option
  val step_process : runtime -> context -> slot -> unit
  val attempt_steal : runtime -> state -> bool
  val wait_for_local_work : runtime -> state -> slot option
end) =
struct
  type runtime = Deps.runtime
  type state = Deps.state

  let loop t worker =
    let ctx = Deps.make_context t worker in
    Deps.set_context (Some ctx);
    while not (Deps.should_stop t) do
      match Deps.pop_local worker with
      | Some slot ->
          Deps.step_process t ctx slot
      | None ->
          if not (Deps.attempt_steal t worker) then
            match Deps.wait_for_local_work t worker with
            | None -> ()
            | Some slot -> Deps.step_process t ctx slot
    done;
    Deps.clear_current_process ctx

  let attempt_steal = Deps.attempt_steal
end
