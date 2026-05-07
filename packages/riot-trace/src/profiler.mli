open Std

type t =
  | Auto
  | Perf
  | Xctrace

type unavailable = {
  profiler: string;
  reason: string;
}

val from_string: string -> (t, string) result

val to_string: t -> string

val default_output_path: binary_name:string -> Path.t

val effective: t -> (t, unavailable) result
