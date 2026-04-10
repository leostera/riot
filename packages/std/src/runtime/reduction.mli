(** Runtime support for cooperative reduction counting. *)
val reset_reductions: int -> unit

(** Reset the reduction count to a new value. *)
val increment_reduction_count: unit -> unit

(** Spend one cooperative reduction and yield if necessary.

    This function is injected by the Riot-patched OCaml compiler at function
    applications and loop iterations. *)
