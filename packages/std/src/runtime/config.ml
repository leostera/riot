open Kernel

type timer_resolution =
  | Second
  | Millisecond
  | Microsecond
  | Nanosecond

type t = {
  timer_resolution: timer_resolution;
  scheduler_count: int;
}

let requested_scheduler_count =
  match Env.get ~var:"RIOT_SCHEDULERS" with
  | Some value -> (
      match Int.parse value with
      | Some count when count > 0 -> Some count
      | Some _
      | None -> None
    )
  | None -> None

let default_scheduler_count =
  match requested_scheduler_count with
  | Some count -> count
  | None -> Int.max 1 (Thread.available_parallelism - 1)

let default = { timer_resolution = Millisecond; scheduler_count = default_scheduler_count }

let make = fun ?(timer_resolution = Millisecond) ?(scheduler_count = default_scheduler_count) () -> {
  timer_resolution;
  scheduler_count = Int.max 1 scheduler_count;
}

let worker_count = fun config -> config.scheduler_count

let resolution_to_nanos = fun __tmp1 ->
  match __tmp1 with
  | Second -> 1_000_000_000L
  | Millisecond -> 1_000_000L
  | Microsecond -> 1_000L
  | Nanosecond -> 1L
