open Std

type request
type event = Riot_build.Event.t

val request_of_workspace:
  ?on_event:(event -> unit) ->
  Riot_model.Workspace.t ->
  request

val detached_request:
  ?on_event:(event -> unit) ->
  ?packages:Riot_model.Package_name.t list ->
  unit ->
  (request, string) result

val run_string:
  request ->
  packages:Riot_model.Package_name.t list ->
  source:string ->
  (unit, string) result

val run_file:
  request ->
  path:Std.Path.t ->
  packages:Riot_model.Package_name.t list ->
  args:string list ->
  (unit, string) result

module Session: sig
  type t

  val create: ?args:string list -> request -> (t, string) result

  val build_workspace: t -> (unit, string) result

  val load_packages:
    t ->
    packages:Riot_model.Package_name.t list ->
    (unit, string) result

  val eval_phrase: t -> source:string -> (unit, string) result
end
