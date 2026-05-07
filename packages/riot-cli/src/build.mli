open Std

(**
   CLI support for [riot build].

   This module owns user-facing build output and delegates the actual build
   execution to [riot-build].
*)
type build_scope = Riot_build.Request.scope =
  | Runtime
  | Dev
type dev_artifacts = Riot_build.Request.dev_artifacts = {
  tests: bool;
  examples: bool;
  benches: bool;
}
type output_mode =
  | Human
  | Json
type build_progress = {
  (** Number of packages built in this render pass. *)
  mutable built_count: int;
  (** Number of cache hits reported so far. *)
  mutable cached_count: int;
  (** Number of failed packages reported so far. *)
  mutable failed_count: int;
  (** Number of skipped packages reported so far. *)
  mutable skipped_count: int;
}
type render_state
type event_renderer
type request = {
  workspace: Riot_model.Workspace.t;
  packages: Riot_model.Package_name.t list;
  targets: Riot_model.Target.request;
  scope: build_scope;
  dev_artifacts: dev_artifacts;
  profile: Riot_model.Profile.t;
  requested_parallelism: int option;
  output_mode: output_mode;
  show_finished_summary: bool;
}

val create_render_state: ?profile:string -> unit -> render_state

val create_event_renderer:
  ?profile:Riot_model.Profile.t ->
  mode:output_mode ->
  unit ->
  event_renderer

val render_event: event_renderer -> Riot_build.Event.t -> unit

val finish_event_renderer: event_renderer -> unit

(** Shared [riot build]-compatible argument surface. *)
val build_args: unit Std.ArgParser.arg list

(** Command definition for [riot build]. *)
val command: Std.ArgParser.command

(**
   Format a package-manager event for human output.

   Returns [None] when the event should stay silent in human mode.
*)
val format_pm_event:
  seen_registry_updates:string Std.Collections.HashSet.t ->
  Riot_model.Event.kind ->
  string option

(** Reset the monotonic clock used for emitted JSON build events. *)
val reset_json_clock: started_at:Std.Time.Instant.t -> unit

(** Emit a build event as JSON. *)
val write_build_event_json: Riot_build.Event.t -> unit

(**
   Format a package label for build output.

   Workspace packages render as their bare package name. External packages
   include the resolved version when present. Dev artifact and target details
   are appended when requested by the caller.
*)
val display_package_name:
  ?profile:string ->
  ?build_target:Riot_model.Target.t ->
  ?show_target:bool ->
  Riot_model.Package.t ->
  string

(** Render a structured planner error into human-readable detail lines. *)
val planning_error_lines: Riot_planner.Planning_error.t -> string list

(** Render a build-unit graph planning error into human-readable detail lines. *)
val build_unit_planning_error_lines:
  Riot_build.Internal.Build_unit_plan.error ->
  string list

(** Render one package failure from the final build error summary. *)
val build_failure_detail_lines: Riot_build.Build_result.failure -> string list

(** Package-provided fix providers that belong to workspace members. *)
val workspace_fix_providers: Riot_model.Workspace.t -> Riot_model.Fix_provider.t list

(** Render a build event in the selected output mode. *)
val write_build_event:
  ?render_state:render_state ->
  ?profile:string ->
  mode:output_mode ->
  seen_registry_updates:string Std.Collections.HashSet.t ->
  Riot_build.Event.t ->
  unit

(** Render a build phase event in the selected output mode. *)
val write_build_phase_event:
  ?render_state:render_state ->
  mode:output_mode ->
  Riot_build.Event.runtime_phase ->
  unit

(** Render a package-manager event in the selected output mode. *)
val write_pm_event:
  mode:output_mode ->
  seen_registry_updates:string Std.Collections.HashSet.t ->
  Riot_model.Event.t ->
  unit

(** Render a "building target" status event. *)
val write_building_target_event: mode:output_mode -> target:Riot_model.Target.t -> host:bool -> unit

(** Render a cache-GC event produced during the build flow. *)
val write_cache_gc_event: mode:output_mode -> Riot_store.Cache_gc.event -> unit

(** Run [riot build] in a resolved workspace. *)
val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result

(** Parse [riot build]-compatible matches into a build request. *)
val request_of_matches:
  workspace:Riot_model.Workspace.t ->
  Std.ArgParser.matches ->
  (request, exn) result

(**
   Execute a build command programmatically.

   Use this entry point when another CLI command wants the same build surface
   with explicit control over scope, profile, output mode, or package/target selection.
*)
val build_command:
  workspace:Riot_model.Workspace.t ->
  ?scope:build_scope ->
  ?dev_artifacts:dev_artifacts ->
  ?profile:string ->
  ?mode:output_mode ->
  ?show_finished_summary:bool ->
  ?requested_parallelism:int option ->
  Riot_model.Package_name.t option ->
  string option ->
  (unit, exn) result
