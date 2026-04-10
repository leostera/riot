open Kernel
open Kernel.Collections
open Kernel.Sync
open Kernel.Sync.Cell

(* Simple timing wheel implementation
   
   Uses a single level with 256 slots at the configured resolution.
   Timers that don't fit in the wheel are kept in an overflow list.
   
   Key insight: We only check slots that have elapsed since last tick,
   not iterate through every tick that passed.
*)

type t = {
  config: Config.t;
  slots: Timer.t list array;
  overflow: Timer.t list Cell.t;
  num_slots: int;
  slot_duration: int64;
  mutable current_time: int64;
  mutable current_slot: int;
  timers_by_id: (Timer_id.t, Timer.t) HashMap.t;
}

let create = fun ~config ->
  let now = Time.monotonic_time_nanos () in
  let slot_duration = Config.resolution_to_nanos config.Config.timer_resolution in
  let num_slots = 256 in
  {
    config;
    slots = Array.make num_slots [];
    overflow = Cell.create [];
    num_slots;
    slot_duration;
    current_time = now;
    current_slot = 0;
    timers_by_id = HashMap.create ();
  }

let calculate_slot = fun t expires_at ->
  let delta = Int64.sub expires_at t.current_time in
  let ticks = Int64.div delta t.slot_duration in
  if Int64.compare ticks (Int64.of_int t.num_slots) < 0 then
    let slot = Int64.to_int
      (Int64.rem (Int64.add (Int64.of_int t.current_slot) ticks) (Int64.of_int t.num_slots)) in
    Some slot
  else
    (* Timer is too far in the future - put in overflow *)
    None

let schedule_timer = fun t timer ->
  let _ = HashMap.insert t.timers_by_id timer.Timer.id timer in
  match calculate_slot t timer.Timer.expires_at with
  | Some slot -> t.slots.(slot) <- timer :: t.slots.(slot)
  | None -> t.overflow := timer :: !(t.overflow)

let add_timer = fun t ~now ~duration_nanos ~mode ~action ->
  let timer = Timer.make ~now ~duration_nanos ~mode ~action in
  schedule_timer t timer;
  timer.id

let reschedule_timer = fun t ~now timer ->
  Timer.reschedule timer ~now;
  schedule_timer t timer

let cancel_timer = fun t timer_id ->
  match HashMap.get t.timers_by_id timer_id with
  | Some timer ->
      Timer.cancel timer;
      HashMap.remove t.timers_by_id timer_id |> ignore
  | None -> ()

let tick = fun t ~now ->
  if Int64.compare now t.current_time < 0 then
    []
  else
    let expired = Cell.create [] in
    (* Calculate which slot corresponds to 'now' *)
    let ticks_elapsed = Int64.div (Int64.sub now t.current_time) t.slot_duration in
    let ticks_to_process = min (Int64.to_int ticks_elapsed) t.num_slots in
    (* Process slots from current_slot to new slot *)
    for _i = 1 to ticks_to_process do
      t.current_slot <- (t.current_slot + 1) mod t.num_slots;
      let timers = t.slots.(t.current_slot) in
      t.slots.(t.current_slot) <- [];
      (* Check each timer in this slot *)
      List.iter
        (fun timer ->
          if Timer.is_cancelled timer then
            HashMap.remove t.timers_by_id timer.id |> ignore
          else if Timer.should_fire timer ~now then
            (
              expired := timer :: !expired;
              HashMap.remove t.timers_by_id timer.id |> ignore
            )
          else
            (* Timer not yet expired - re-insert *)
            match calculate_slot t timer.Timer.expires_at with
            | Some slot -> t.slots.(slot) <- timer :: t.slots.(slot)
            | None -> t.overflow := timer :: !(t.overflow))
        timers
    done;
    (* Check overflow list for timers that now fit in the wheel *)
    let still_overflow = Cell.create [] in
    List.iter
      (fun timer ->
        if Timer.is_cancelled timer then
          HashMap.remove t.timers_by_id timer.id |> ignore
        else if Timer.should_fire timer ~now then
          (
            expired := timer :: !expired;
            HashMap.remove t.timers_by_id timer.id |> ignore
          )
        else
          match calculate_slot t timer.Timer.expires_at with
          | Some slot -> t.slots.(slot) <- timer :: t.slots.(slot)
          | None -> still_overflow := timer :: !still_overflow)
      !(t.overflow);
    t.overflow := !still_overflow;
    (* Update current time *)
    t.current_time <- now;
    !expired

let next_expiration = fun t ~now ->
  let _unused = now in
  let min_expiration = Cell.create None in
  HashMap.iter
    (fun _id timer ->
      if not (Timer.is_cancelled timer) then
        match !min_expiration with
        | None -> min_expiration := Some timer.Timer.expires_at
        | Some current_min ->
            if Int64.compare timer.Timer.expires_at current_min < 0 then
              min_expiration := Some timer.Timer.expires_at)
    t.timers_by_id;
  !min_expiration

let size = fun t -> HashMap.len t.timers_by_id
