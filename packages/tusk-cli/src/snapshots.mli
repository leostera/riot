open Std

type pending_snapshot = {
  approved: Path.t;
  pending: Path.t;
}

val command: ArgParser.command

val discover_pending_snapshots:
  workspace_root:Path.t ->
  ?query:string ->
  unit ->
  (pending_snapshot list, IO.error) result

val approve_pending_snapshots:
  pending_snapshot list ->
  (unit, IO.error) result

val reject_pending_snapshots:
  pending_snapshot list ->
  (unit, IO.error) result

val run:
  workspace:Tusk_model.Workspace.t ->
  ArgParser.matches ->
  (unit, exn) result
