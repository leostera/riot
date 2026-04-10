open Kernel
open Kernel.Sync
open Kernel.Sync.Cell

exception Unwind

type ('a, 'b) continuation = ('a, 'b) Effect.Shallow.continuation

type error_info = {
  exn: exn;
  backtrace: Exception.raw_backtrace;
}

type 'a t =
  | Finished of ('a, error_info) result
  | Suspended: ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled: ('a, 'b) continuation * 'a -> 'b t

let is_finished = fun x ->
  match x with
  | Finished _ -> true
  | _ -> false

type 'a step =
  | Continue of 'a
  | Discontinue of exn
  | Reperform: 'a Effect.t -> 'a step
  | Delay: 'a step
  | Suspend: 'a step
  | Yield: unit step
  | Terminate: 'a step

type ('a, 'b) step_callback = ('a step -> 'b t) -> 'a Effect.t -> 'b t

type perform = {
  perform: 'a 'b. ('a, 'b) step_callback;
} [@@unboxed]

let finished = fun x -> Finished x

let suspended_with = fun k e -> Suspended (k, e)

let handler_continue =
  let retc signal = finished (Ok signal) in
  let exnc exn =
    let backtrace = Exception.get_raw_backtrace () in
    finished (Error { exn; backtrace })
  in
  let effc: type c. c Effect.t -> ((c, 'a) continuation -> 'b) option = fun e ->
    Some (fun k -> suspended_with k e) in
  Effect.Shallow.{ retc; exnc; effc }

let continue_with = fun k v ->
  Effect.Shallow.continue_with k v handler_continue

let discontinue_with = fun k exn ->
  Effect.Shallow.discontinue_with k exn handler_continue

let unhandled_with = fun k v -> Unhandled (k, v)

let make = fun fn eff ->
  let k = Effect.Shallow.fiber fn in
  Suspended (k, eff)

let run: type a. consume_reduction:(unit -> bool) -> perform:perform -> a t -> a t option = fun ~consume_reduction ~perform t ->
  let exception Yield of a t in
  let exception Unwind in
  let t = Cell.create t in
  try
    while true do
      if consume_reduction () then
        raise_notrace (Yield !t);
      match !t with
      | Finished _ as finished ->
          raise_notrace (Yield finished)
      | Unhandled (fn, v) ->
          raise_notrace (Yield (continue_with fn v))
      | Suspended (fn, e) as suspended ->
          let k: type c. (c, a) continuation -> c step -> a t = fun fn step ->
            match step with
            | Delay ->
                suspended
            | Continue v ->
                continue_with fn v
            | Discontinue exn ->
                discontinue_with fn exn
            | Reperform eff ->
                unhandled_with fn (Effect.perform eff)
            | Yield ->
                raise_notrace (Yield (continue_with fn ()))
            | Suspend ->
                raise_notrace (Yield suspended)
            | Terminate ->
                ignore (discontinue_with fn Unwind);
                raise Unwind
          in
          t := perform.perform (k fn) e
    done;
    Some !t
  with
  | Yield t -> Some t
  | Unwind -> None

let drop = fun k exn _id ->
  let retc _signal = () in
  let exnc _exn = () in
  let effc _eff = None in
  let handler = Effect.Shallow.{ retc; exnc; effc } in
  Effect.Shallow.discontinue_with k exn handler

let unwind = fun ~id (t: 'a t) ->
  match t with
  | Finished result -> ignore result
  | Suspended (k, _) -> ignore (drop k Unwind id)
  | Unhandled (k, _) -> ignore (drop k Unwind id)
