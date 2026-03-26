(* Runtime module for Riot - provides reduction counting for compiler instrumentation *)

open Kernel
 
(* Domain-local storage for reduction count.
   Each worker domain tracks reductions independently. *)
let reduction_count_key = Domain.DLS.new_key (fun () -> 100)

let reset_reductions n = Domain.DLS.set reduction_count_key n

let increment_reduction_count () =
  let remaining = Domain.DLS.get reduction_count_key - 1 in
  Domain.DLS.set reduction_count_key remaining;
  if remaining <= 0 then (
    Domain.DLS.set reduction_count_key 100;
    (* Reset for next scheduling slice *)
    Effects.yield ())
