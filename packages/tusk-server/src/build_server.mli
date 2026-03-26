(** Build server - Handles build execution in a spawned process *)

open Tusk_model

val start :
  workspace:Tusk_model.Workspace.t ->
  load_errors:Tusk_model.Workspace_manager.load_error list ->
  toolchain:Tusk_toolchain.t ->
  store:Tusk_store.Store.t ->
  concurrency:int ->
  session_id:Tusk_model.Session_id.t ->
  client_pid:Std.Pid.t ->
  server_pid:Std.Pid.t ->
  target:Protocol.target ->
  scope:Protocol.build_scope ->
  target_arch:string option ->
  unit
(** Start a build in a spawned worker process. This function returns immediately
    after spawning the worker. The worker will send results directly to the
    client_pid.

    The [concurrency] parameter is the single build concurrency budget and is
    propagated into [Build_ctx.available_parallelism] for action execution. *)
