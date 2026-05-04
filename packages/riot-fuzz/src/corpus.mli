open Std

type entry = {
  path: Path.t;
  content: string;
}

val compare_path: Path.t -> Path.t -> Order.t

val ensure_case_dirs: Path.t -> (Path.t * Path.t, Error.t) Result.t

val load_entries: Path.t -> entry list

val load: Path.t -> string list

val save_input: Path.t -> string -> string -> (Path.t, Error.t) Result.t

val seed_empty: Path.t -> (unit, Error.t) Result.t

val delete_input: Path.t -> (unit, Error.t) Result.t

type crash_artifacts = {
  stdout_path: Path.t;
  stderr_path: Path.t;
  status_path: Path.t;
}

val save_crash_artifacts:
  case_dir:Path.t ->
  crash_path:Path.t ->
  status:Afl.status ->
  stdout:string ->
  stderr:string ->
  (crash_artifacts, Error.t) Result.t
