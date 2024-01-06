open Core
open Util

module Timer = struct
  type t = {
    id : unit Ref.t;
    mode : [ `interval | `one_off ];
    mutable status : [ `pending | `finished ];
    mutable started_at : Mtime.t;
    ends_at : Mtime.Span.t;
    fn : unit -> unit;
  }

  let pp fmt t =
    let mode = if t.mode = `interval then "interval" else "one_off" in
    Format.fprintf fmt "Timer { id=%a; started_at=%a; ends_at=%a; mode=%s }"
      Ref.pp t.id Mtime.pp t.started_at Mtime.Span.pp t.ends_at mode

  let make time mode fn =
    let id = Ref.make () in
    let started_at = Mtime_clock.now () in
    let ends_at = Mtime.Span.of_uint64_ns Int64.(mul 1_000L time) in
    { id; started_at; ends_at; fn; mode; status = `pending }

  let equal a b = Ref.equal a.id b.id
  let is_finished t = t.status = `finished
end

type t = {
  timers : (Mtime.t, Timer.t) Dashmap.t;
  ids : (unit Ref.t, Timer.t) Dashmap.t;
  mutable last_t : Mtime.t; [@warning "-69"]
}

let create () =
  {
    timers = Dashmap.create 1024;
    ids = Dashmap.create 1024;
    last_t = Mtime_clock.now ();
  }

let can_tick t = not (Dashmap.is_empty t.timers)

let is_finished t timer =
  let timers = Dashmap.get_all t.ids timer in
  let timer_exists = not (List.is_empty timers) in
  let timer_is_finished = List.exists Timer.is_finished timers in
  timer_exists && timer_is_finished

let remove_timer t timer =
  let timers = Dashmap.get_all t.ids timer in
  let times = List.map (fun (timer : Timer.t) -> timer.started_at) timers in
  Dashmap.remove_by t.ids (fun (k, _) -> Ref.equal k timer);
  Dashmap.remove_all t.timers times

let clear_timer t tid =
  Dashmap.get_all t.ids tid
  |> List.iter (fun timer ->
         Log.trace (fun f -> f "Cleared timer %a" Timer.pp timer);
         Dashmap.remove t.timers timer.started_at;
         Dashmap.remove t.ids tid)

let make_timer t time mode fn =
  let timer = Timer.make time mode fn in
  Dashmap.insert t.timers timer.started_at timer;
  Dashmap.insert t.ids timer.id timer;
  Log.trace (fun f -> f "Created timer %a" Timer.pp timer);
  timer.id

let run_timer t now (_, timer) =
  let open Timer in
  let ends_at = Mtime.add_span timer.started_at timer.ends_at |> Option.get in
  let timeout = Mtime.is_earlier now ~than:ends_at in
  Log.trace (fun f ->
      f "Running timer %a with ends_at=%a -> timeout? %b" Timer.pp timer
        Mtime.pp ends_at (not timeout));
  if timeout || Timer.is_finished timer then ()
  else (
    (match timer.mode with
    | `one_off ->
        timer.status <- `finished;
        Dashmap.remove t.timers timer.started_at
    | `interval -> timer.started_at <- now);
    timer.fn ())

let tick t =
  let now = Mtime_clock.now () in
  Dashmap.iter t.timers (run_timer t now);
  t.last_t <- now
