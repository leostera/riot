open Kernel

type id = Timer_id.t

type mode =
  One_shot
  | Interval of int64

type action =
  Wake_process of Process.t
  | Send_message of Pid.t * Message.t

type t = {
  id: id;
  mode: mode;
  mutable started_at: int64;
  mutable expires_at: int64;
  duration_nanos: int64;
  action: action;
  mutable status: 
    [
      `pending
      | `cancelled
    ];
}

let make = fun ~now ~duration_nanos ~mode ~action ->
  let id = Timer_id.make () in
  let expires_at = Int64.add now duration_nanos in
  {
    id;
    mode;
    started_at = now;
    expires_at;
    duration_nanos;
    action;
    status = `pending;
  }

let is_cancelled = fun t -> t.status = `cancelled

let cancel = fun t -> t.status <- `cancelled

let should_fire = fun t ~now -> Int64.compare now t.expires_at >= 0 && not (is_cancelled t)

let reschedule = fun t ~now ->
  match t.mode with
  | One_shot -> ()
  | Interval interval ->
      t.started_at <- now;
      t.expires_at <- Int64.add now interval
