open Kernel

(** Worker boundary module for scheduler internals. *)
module Make (Deps : sig
  type runtime
  type state

  val loop : runtime -> state -> unit
  val attempt_steal : runtime -> state -> bool
end) =
struct
  type runtime = Deps.runtime
  type state = Deps.state

  let loop = Deps.loop
  let attempt_steal = Deps.attempt_steal
end
