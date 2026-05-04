open Std

(** CLI surface for [riot fuzz]. *)
val command: Std.ArgParser.command

(** Run [riot fuzz] inside a resolved workspace. *)
val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
