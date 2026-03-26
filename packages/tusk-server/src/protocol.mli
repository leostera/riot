open Std

open Tusk_model

type build_scope = Runtime | Dev
type target = All | Package of string

module BuildStats : sig
  type t

  val make : unit -> t
  val mark_started : t -> unit
  val mark_completed : t -> unit
  val inc_packages_built : t -> unit
  val inc_packages_failed : t -> unit
  val inc_cache_hits : t -> unit
  val inc_cache_misses : t -> unit
  val inc_action_cache_hits : t -> unit
  val inc_action_cache_misses : t -> unit
  val set_total_modules : t -> int -> unit
  val get_build_duration : t -> float (* seconds *)
  val get_packages_built : t -> int
  val get_packages_failed : t -> int
  val get_total_modules : t -> int
  val get_cache_hits : t -> int
  val get_cache_misses : t -> int
  val get_action_cache_hits : t -> int
  val get_action_cache_misses : t -> int
end

(** Request types that can be sent to the server *)
type request =
  | Build of {
      client_pid : Pid.t;
      target : target;
      scope : build_scope;
      target_arch : string option;
      session_id : Session_id.t;
    }
  | Ping of { client_pid : Pid.t }
  | ScanWorkspace of { client_pid : Pid.t; current_dir : Path.t }
  | GetWorkspaceConfig of { client_pid : Pid.t }
  | GetPackageInfo of { client_pid : Pid.t; package_name : string }
  | GetPackageGraph of { client_pid : Pid.t }
  | FindExecutable of { client_pid : Pid.t; name : string }
  | FindArtifact of {
      client_pid : Pid.t;
      package : string;
      kind : string;
      name : string;
    }
  | FormatFile of { client_pid : Pid.t; file_path : Path.t; check_only : bool }
  | FormatCode of {
      client_pid : Pid.t;
      code : string;
      file_path : Path.t option;
    }
  | FormatAll of { client_pid : Pid.t; mode : [ `check | `write ] }
  | NewPackage of {
      client_pid : Pid.t;
      path : Path.t;
      name : string;
      is_library : bool;
    }

(** Response types from the server *)
type response =
  | Pong
  | BuildStarted of { session_id : Session_id.t; started_at : Datetime.t }
  | BuildEvent of { session_id : Session_id.t; event : Telemetry.event }
  | BuildCompleted of {
      session_id : Session_id.t;
      completed_at : Datetime.t;
      stats : BuildStats.t;
      results : Tusk_executor.Package_builder.build_result list;
    }
  | BuildFailed of {
      session_id : Session_id.t;
      failed_at : Datetime.t;
      stats : BuildStats.t;
      built : Tusk_executor.Package_builder.build_result list;
      errors : Tusk_executor.Package_builder.build_result list;
    }
  | PlanningFailed of {
      session_id : Session_id.t;
      failed_at : Datetime.t;
      reason : string;
    }
  | CycleDetected of {
      session_id : Session_id.t;
      cycle_nodes : string list;
      detected_at : Datetime.t;
    }
  | WorkspaceConfig of { workspace : Workspace.t; toolchain : Tusk_toolchain.t }
  | PackageInfo of {
      package : Package.t;
      sources : Path.t list;
      dependencies : Package.t list;
    }
  | PackageGraph of { nodes : Package.t list }
  | ExecutableFound of { package : string; binary : string }
  | ExecutableNotFound
  | ArtifactFound of { path : Path.t }
  | ArtifactNotFound of { error : string }
  | FormatResult of { formatted_code : string; changed : bool }
  | FormatError of { error : string }
  | FormatAllResult of {
      files_formatted : int;
      files_failed : int;
      errors : (string * string) list;
    }
  | PackageCreated of { path : string; name : string }
  | PackageCreationError of { error : string }
  | PackageNotFound of {
      session_id : Session_id.t;
      package_name : string;
      available_packages : string list;
    }

(** Message types for server communication *)
type Message.t +=
  | ServerRequest of request
  | ServerResponse of response
  | UpdatePackageGraph of Tusk_planner.Package_graph.t
