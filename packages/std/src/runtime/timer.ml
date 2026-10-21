type id = Timer_id.t

type mode =
  | One_shot
  | Interval of int64

type action =
  | Wake_process of Process.t
  | Send_message of Pid.t * Message.t

type status =
  | Pending
  | Cancelled

type t = {
  id: id;
  mode: mode;
  mutable started_at: int64;
  mutable expires_at: int64;
  duration_nanos: int64;
  action: action;
  mutable status: status;
}

let make = fun ?id ~now ~duration_nanos ~mode ~action () ->
  let id =
    match id with
    | Some id -> id
    | None -> Timer_id.make ()
  in
  let expires_at = Int64.add now duration_nanos in
  {
    id;
    mode;
    started_at = now;
    expires_at;
    duration_nanos;
    action;
    status = Pending;
  }

let is_cancelled = fun t ->
  match t.status with
  | Cancelled -> true
  | Pending -> false

let cancel = fun t -> t.status <- Cancelled

let should_fire = fun t ~now ->
  let due =
    match Int64.compare now t.expires_at with
    | Order.EQ
    | Order.GT -> true
    | Order.LT -> false
  in
  if due then
    match is_cancelled t with
    | true -> false
    | false -> true
  else
    false

let reschedule = fun t ~now ->
  match t.mode with
  | One_shot -> ()
  | Interval interval ->
      t.started_at <- now;
      t.expires_at <- Int64.add now interval
