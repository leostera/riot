open Std
open Std.Data
open Std.Collections
open Riot_model

(** Content-addressable storage for build artifacts *)
module Manifest = Manifest

type t
(** Abstract type representing a store *)
type node_payload_namespace =
  | DependencyResolution
  | ExternalDependencyReady
  | ToolchainReady
  | SourceAnalysis
  | ModulePlans
  | ActionSpecs

val node_payload_namespace_to_string: node_payload_namespace -> string

(** Artifact witness - proof that build outputs have been stored *)

(** Why a stored artifact entry failed verification. *)
type integrity_failure =
  | UnsafeEntryPath
  | EntryMissing
  | EntryNotRegularFile
  | EntrySizeMismatch of { expected: int; actual: int }
  | EntryHashMismatch of { expected: string; actual: string }
  | EntryUnreadable of { cause: string }
type error =
  | HashNotFound of {
      hash: Std.Crypto.hash;
    }
  | LoadManifestFailed of {
      path: Std.Path.t;
      cause: string;
    }
  | CreateTargetDirFailed of {
      path: Std.Path.t;
      cause: Std.Fs.error;
    }
  | CreateParentDirFailed of {
      path: Std.Path.t;
      cause: Std.Fs.error;
    }
  | ReadSourceMetadataFailed of {
      path: Std.Path.t;
      cause: Std.Fs.error;
    }
  | CopyArtifactFailed of {
      src: Std.Path.t;
      dst: Std.Path.t;
      cause: Std.Fs.error;
    }
  | SetCopiedArtifactPermissionsFailed of {
      src: Std.Path.t;
      dst: Std.Path.t;
      cause: Std.Fs.error;
    }
  | CreateTempDirFailed of {
      path: Std.Path.t;
      cause: Std.Fs.error;
    }
  | CheckSourceExistsFailed of {
      path: Std.Path.t;
      cause: Std.Fs.error;
    }
  | DeclaredOutputMissing of {
      path: Std.Path.t;
    }
  | DeclaredOutputOutsideSandbox of {
      path: Std.Path.t;
      sandbox_dir: Std.Path.t;
    }
  | MetadataReadFailed of {
      path: Std.Path.t;
      cause: Std.Fs.error;
    }
  | SaveManifestFailed of {
      path: Std.Path.t;
      cause: string;
    }
  | CommitArtifactsFailed of {
      source_dir: Std.Path.t;
      destination_dir: Std.Path.t;
      cause: string;
    }
  | SavePlanBundleFailed of {
      hash: Std.Crypto.hash;
      cause: string;
    }
  | SaveNodePayloadFailed of {
      namespace: node_payload_namespace;
      hash: Std.Crypto.hash;
      cause: string;
    }
  | ExportPathMustBeRelative of {
      path: Std.Path.t;
    }
  | CreatePackageOutputDirFailed of {
      path: Std.Path.t;
      cause: Std.Fs.error;
    }
  | CopyExportFailed of {
      src: Std.Path.t;
      dst: Std.Path.t;
      cause: Std.Fs.error;
    }
  | ExportSourceMissing of {
      path: Std.Path.t;
    }
  | ArtifactIntegrityFailed of {
      hash_dir: Std.Path.t;
      path: Std.Path.t;
      failure: integrity_failure;
    }

val error_message: error -> string

val integrity_failure_message: integrity_failure -> string

type export_entry = Manifest.export_entry = {
  (** Public export name looked up by CLI flows such as `riot run`. *)
  name: string;
  (** Relative path within the producing action artifact directory. *)
  path: Std.Path.t;
  (** Hex-encoded action hash that owns [path] in immutable cache storage. *)
  action_hash: string;
}
(** {1 Store Management} *)
val create: workspace:Workspace.t -> t

(** Create a new store for the given workspace *)
val create_for_lane: workspace:Workspace.t -> profile:string -> target:Riot_model.Target.t -> t

(** Create a store rooted at a specific build lane. *)
(** {1 Simple Interface} *)

val get: t -> Std.Crypto.hash -> Artifact.t option

(**
   Check if we have cached package artifacts for this hash. Returns Some artifact if
   cached, None if not. The artifact contains the list of files, warnings, and
   package exports.
*)
val get_package: t -> Std.Crypto.hash -> Artifact.t option

(** Check if we have a cached package artifact for this hash and validate stored files/exports. *)

(**
   Load cached package artifact metadata without validating stored files/exports.
   Use this for planning-only cache checks. Materialization paths should use
   [get_package] so corrupt cache entries are detected before copying.
*)
val get_package_metadata: t -> Std.Crypto.hash -> Artifact.t option

(** Check if we have a cached action artifact for this hash. *)
val get_action: t -> Std.Crypto.hash -> Artifact.t option

val load_manifest: t -> hash:Std.Crypto.hash -> Manifest.t option

(** Load the full hash manifest when present. *)
val save:
  ?ocamlc_warnings:string list ->
  ?exports:export_entry list ->
  t ->
  package:string ->
  input_hash:Std.Crypto.hash ->
  sandbox_dir:Std.Path.t ->
  outs:Std.Path.t list ->
  (Artifact.t, error) result

(**
   Save package build outputs to the store. Copies the specified output files from
   sandbox_dir to the store.
*)
val save_package:
  ?ocamlc_warnings:string list ->
  ?exports:export_entry list ->
  t ->
  package:string ->
  input_hash:Std.Crypto.hash ->
  sandbox_dir:Std.Path.t ->
  outs:Std.Path.t list ->
  (Artifact.t, error) result

(** Save package build outputs to the package artifact namespace. *)
val save_action:
  ?ocamlc_warnings:string list ->
  ?link_outputs:bool ->
  t ->
  package:string ->
  input_hash:Std.Crypto.hash ->
  sandbox_dir:Std.Path.t ->
  outs:Std.Path.t list ->
  (Artifact.t, error) result

(** Save action outputs to the action artifact namespace. *)
(** {1 Artifact Operations} *)

val promote: t -> Std.Crypto.hash -> target_dir:Std.Path.t -> (unit, error) result

(**
   Promote cached package artifacts to the target directory. Returns error if hash not
   found.
*)
val promote_action:
  ?preserve_permissions:bool ->
  t ->
  Std.Crypto.hash ->
  target_dir:Std.Path.t ->
  (unit, error) result

(** Promote cached action artifacts to the target directory. *)
val promote_action_artifact:
  ?preserve_permissions:bool ->
  t ->
  Artifact.t ->
  target_dir:Std.Path.t ->
  (unit, error) result

(** Promote a cached action artifact that has already been loaded with [get_action]. *)
val promote_action_artifact_files:
  ?preserve_permissions:bool ->
  t ->
  Artifact.t ->
  files:Std.Path.t list ->
  target_dir:Std.Path.t ->
  (unit, error) result

(** Promote selected files from a cached action artifact. *)
val save_package_from_action_artifacts:
  ?ocamlc_warnings:string list ->
  ?exports:export_entry list ->
  ?select:(Artifact.t -> Manifest.file_entry -> bool) ->
  t ->
  package:string ->
  input_hash:Std.Crypto.hash ->
  artifacts:Artifact.t list ->
  (Artifact.t, error) result

(** Save package outputs by copying files directly from action artifacts. *)
val exists: t -> Std.Crypto.hash -> bool

(** Check if artifacts for a given hash exist in the store *)
val get_artifact_paths: t -> Artifact.t -> Std.Path.t list

(**
   Get absolute paths to artifact files in the store's cache. These paths point
   to the immutable content-addressed storage and are guaranteed to exist. Use
   this instead of relying on target/debug/out which may be cleaned.
*)
val get_artifact_dir: t -> Artifact.t -> Std.Path.t

(**
   Get the cache directory containing an artifact's files. Returns the absolute
   path to the directory in immutable storage where the artifact is stored.
*)
val hash_dir_of: t -> Std.Crypto.hash -> Std.Path.t

(**
   Get the immutable cache directory for a hash, whether or not it exists yet.

   This is useful for planning and dependency summaries that need a stable
   output location before execution materializes artifacts.
*)
val action_hash_dir_of: t -> Std.Crypto.hash -> Std.Path.t

(** Get the immutable cache directory for an action artifact hash. *)
val save_node_payload:
  t ->
  namespace:node_payload_namespace ->
  hash:Std.Crypto.hash ->
  payload:string ->
  (unit, error) result

(**
   Save an opaque payload for an incremental graph node. The caller owns the
   payload codec; [riot-store] only provides namespace and hash-addressed
   persistence.
*)
val load_node_payload:
  t ->
  namespace:node_payload_namespace ->
  hash:Std.Crypto.hash ->
  string option

(** Load an opaque incremental graph node payload by namespace and input hash. *)
val save_plan_bundle: t -> hash:Std.Crypto.hash -> plan:Std.Data.Json.t -> (unit, error) result

(**
   Save a package planning bundle keyed by package input hash.

   Plan bundles are persisted in a planner-specific namespace, separate from
   artifact directories, so planning cache writes do not interfere with
   artifact cache atomicity.
*)
val load_plan_bundle: t -> hash:Std.Crypto.hash -> Std.Data.Json.t option

(** Load a cached package planning bundle by package input hash. *)
val export_source_path: t -> export_entry -> Std.Path.t option

(**
   Resolve the immutable store path for a single export entry.

   Returns [None] when the export path is absolute.
*)
val materialize_package_exports:
  t ->
  exports:export_entry list ->
  target_dir:Std.Path.t ->
  (unit, error) result

(**
   Materialize package exports from immutable action artifact locations into a
   package out directory.

   Each export entry copies from [cache/<action_hash>/<path>] to
   [target_dir/<name>].
*)
