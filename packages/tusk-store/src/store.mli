open Std
open Std.Data
open Std.Collections
open Tusk_model

(** Content-addressable storage for build artifacts *)

module Manifest = Manifest

type t
(** Abstract type representing a store *)

(** Artifact witness - proof that build outputs have been stored *)

type error = string

(** {1 Store Management} *)

val create : workspace:Workspace.t -> t
(** Create a new store for the given workspace *)

(** {1 Simple Interface} *)

val get : t -> Std.Crypto.hash -> Artifact.t option
(** Check if we have cached artifacts for this hash. Returns Some artifact if
    cached, None if not. The artifact contains the list of files. *)

val save :
  t ->
  package:string ->
  hash:Std.Crypto.hash ->
  sandbox_dir:Std.Path.t ->
  outs:Std.Path.t list ->
  (Artifact.t, error) result
(** Save build outputs to the store. Copies the specified output files from
    sandbox_dir to the store. *)

(** {1 Artifact Operations} *)

val promote :
  t -> Std.Crypto.hash -> target_dir:Std.Path.t -> (unit, error) result
(** Promote cached artifacts to the target directory. Returns error if hash not
    found. *)

val exists : t -> Std.Crypto.hash -> bool
(** Check if artifacts for a given hash exist in the store *)

val get_artifact_paths : t -> Artifact.t -> Std.Path.t list
(** Get absolute paths to artifact files in the store's cache. These paths point
    to the immutable content-addressed storage and are guaranteed to exist. Use
    this instead of relying on target/debug/out which may be cleaned. *)

val get_artifact_dir : t -> Artifact.t -> Std.Path.t
(** Get the cache directory containing an artifact's files. Returns the absolute
    path to the directory in immutable storage where the artifact is stored. *)

val hash_dir_of : t -> Std.Crypto.hash -> Std.Path.t
(** Get the immutable cache directory for a hash, whether or not it exists yet.

    This is useful for planning and dependency summaries that need a stable
    output location before execution materializes artifacts. *)

val save_plan_bundle :
  t ->
  hash:Std.Crypto.hash ->
  plan:Std.Data.Json.t ->
  (unit, error) result
(** Save a package planning bundle keyed by package input hash.

    Plan bundles are persisted in a planner-specific namespace, separate from
    artifact directories, so planning cache writes do not interfere with
    artifact cache atomicity. *)

val load_plan_bundle : t -> hash:Std.Crypto.hash -> Std.Data.Json.t option
(** Load a cached package planning bundle by package input hash. *)
