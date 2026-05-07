open Std

(** CLI surface for [riot trace]. *)
val command: Std.ArgParser.command

(** True when the parsed invocation inspects an existing trace artifact. *)
val is_summary: Std.ArgParser.matches -> bool

(** Run [riot trace] with optional precomputed workspace information. *)
val run_with_workspace_info:
  workspace:Riot_model.Workspace.t option ->
  workspace_error:string option ->
  Std.ArgParser.matches ->
  (unit, exn) result

(** Run [riot trace] in a known workspace. *)
val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
