open Std

(** Render a prototype check result into snapshot-friendly text. *)
val render_report: Check_result.t -> string
