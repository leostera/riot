open Std

type scope = Request.scope =
  | Runtime
  | Dev

type t = {
  workspace: Riot_model.Workspace.t;
  package_names: Riot_model.Package_name.t list;
  targets: Riot_model.Target.Set.t;
  scope: scope;
  profile: Riot_model.Profile.t;
  requested_parallelism: int option;
}

let make = fun ~workspace ~package_names ~targets ~scope ~profile ?requested_parallelism () ->
  { workspace; package_names; targets; scope; profile; requested_parallelism }

let workspace = fun t -> t.workspace

let package_names = fun t -> t.package_names

let targets = fun t -> t.targets

let scope = fun t -> t.scope

let profile = fun t -> t.profile

let requested_parallelism = fun t -> t.requested_parallelism
