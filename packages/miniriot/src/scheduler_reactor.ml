open Kernel

(** Reactor boundary module for scheduler internals. *)
module Make (Deps : sig
  type runtime
  type command

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
  val loop : runtime -> unit
end) =
struct
  type runtime = Deps.runtime
  type command = Deps.command

  let add_timer = Deps.add_timer
  let cancel_timer = Deps.cancel_timer
  let register_io = Deps.register_io
  let deregister_io = Deps.deregister_io
  let loop = Deps.loop
end
