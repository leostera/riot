(** Dependency summaries produced during planning.

    A dependency summary is planner-owned state: it records immutable identity
    and where that dependency's artifacts are expected to be materialized in
    the store, without requiring that they are already built. *)

open Std
open Std.Collections
open Tusk_model

type t = {
  package : Package.t;
  artifact_dir : Path.t;
  depset : t list;
  hash : Crypto.hash;
}

val library_cmxa : t -> Path.t
(** Return the expected path to the dependency's `.cmxa` archive in the store. *)

val transitive_closure : t list -> t list
(** Flatten dependencies and their transitive deps in dependency-first order,
    deduplicated by package name. *)
