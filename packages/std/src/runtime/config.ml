open Kernel

type timer_resolution =
  Second
  | Millisecond
  | Microsecond
  | Nanosecond

type t = {
  timer_resolution: timer_resolution;
  scheduler_count: int;
}

let default_scheduler_count = Int.max 1 (System.available_parallelism - 1)

let default = { timer_resolution = Millisecond; scheduler_count = default_scheduler_count }

let make = fun ?(timer_resolution = Millisecond) ?(scheduler_count = default_scheduler_count) () ->
  { timer_resolution; scheduler_count = Int.max 1 scheduler_count }

let worker_count = fun config -> config.scheduler_count

let resolution_to_nanos = function
  | Second -> 1_000_000_000L
  | Millisecond -> 1_000_000L
  | Microsecond -> 1_000L
  | Nanosecond -> 1L
