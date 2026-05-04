open Std

val path: workspace:Riot_model.Workspace.t -> Path.t

val with_lock:
  workspace:Riot_model.Workspace.t ->
  on_waiting:(Path.t -> unit) ->
  (unit -> ('a, Error.t) Result.t) ->
  ('a, Error.t) Result.t
