open Std

type scope = Request.scope =
  | Runtime
  | Dev

type t

val make:
  workspace:Riot_model.Workspace.t ->
  package_names:Riot_model.Package_name.t list ->
  targets:Riot_model.Target.Set.t ->
  scope:scope ->
  profile:Riot_model.Profile.t ->
  ?requested_parallelism:int ->
  unit ->
  t

val workspace: t -> Riot_model.Workspace.t

val package_names: t -> Riot_model.Package_name.t list

val targets: t -> Riot_model.Target.Set.t

val scope: t -> scope

val profile: t -> Riot_model.Profile.t

val requested_parallelism: t -> int option
