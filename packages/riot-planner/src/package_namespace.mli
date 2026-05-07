open Std
open Riot_model

type t = {
  public_root: string;
  compiled_root: string;
}

val enabled: unit -> bool

val mode_key: unit -> string

val for_package: Package.t -> t

val public_root: Package.t -> string

val compiled_root: Package.t -> string

val planning_library_name: Package.t -> string

val library_cmxa: Package.t -> Path.t
