open Std

type pending_snapshot = {
  approved: Path.t;
  pending: Path.t;
}
type review_decision =
[
  `Approve
  | `Reject
  | `Ignore
  | `Quit
]
type review_summary = {
  approved_count: int;
  rejected_count: int;
  ignored_count: int;
  quit: bool;
}
val command: ArgParser.command

val discover_pending_snapshots:
  workspace_root:Path.t -> ?query:string -> unit -> (pending_snapshot list, IO.error) result

val approve_pending_snapshots: pending_snapshot list -> (unit, IO.error) result

val reject_pending_snapshots: pending_snapshot list -> (unit, IO.error) result

val parse_review_decision: string -> review_decision option

val review_pending_snapshots_with_decider:
  workspace_root:Path.t ->
  pending_snapshot list ->
  decide:(pending_snapshot -> (review_decision, IO.error) result) ->
  (review_summary, IO.error) result

val run: workspace:Riot_model.Workspace.t -> ArgParser.matches -> (unit, exn) result
