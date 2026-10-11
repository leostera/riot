open Std
module Test = Std.Test

type Message.t +=
  | RuntimeValue of int
  | RuntimeReady of Pid.t
  | RuntimeGo
  | RuntimeReply of int
  | RuntimeTick of int
  | RuntimePlacement of bool

let deadline = Time.Duration.from_secs 1

let receive_value expected =
  receive ~timeout:deadline ~selector:(function
    | RuntimeValue value when value = expected -> `select value
    | _ -> `skip) ()

let test_fifo = fun _ctx ->
  let rec send_values value =
    if value < 128 then (send (self ()) (RuntimeValue value); send_values (value + 1)) in
  send_values 0;
  let rec check value =
    if value = 128 then Ok ()
    else
      match receive_any ~timeout:deadline () with
      | RuntimeValue actual when actual = value -> check (value + 1)
      | _ -> Error "mailbox did not preserve sender FIFO order" in
  check 0

let test_selective_receive_preserves_skipped_message = fun _ctx ->
  send (self ()) (RuntimeValue 1);
  send (self ()) (RuntimeValue 2);
  let _ = receive_value 2 in
  let _ = receive_value 1 in
  Ok ()

let test_timeout_preserves_skipped_message = fun _ctx ->
  send (self ()) (RuntimeValue 1);
  let timed_out =
    try
      let _ = receive ~timeout:(Time.Duration.from_millis 20) ~selector:(function
        | RuntimeValue 2 -> `select () | _ -> `skip) () in
      false
    with Receive_timeout -> true in
  let _ = receive_value 1 in
  if timed_out then Ok () else Error "nonmatching mailbox entry satisfied receive"

let test_remote_wakeup_batch = fun _ctx ->
  let parent = self () in
  let scheduler = match Runtime.current_scheduler_id () with
    | Some id when Runtime.Scheduler_id.to_int id = 0 -> 1
    | _ -> 0 in
  let rec start index workers =
    if index = 32 then workers
    else
      let worker = Runtime.spawn_pinned ~scheduler (fun () ->
        send parent (RuntimeReady (self ()));
        let _ = receive ~timeout:deadline ~selector:(function RuntimeGo -> `select () | _ -> `skip) () in
        send parent (RuntimeReply index);
        Ok ()) in
      start (index + 1) (worker :: workers) in
  let workers = start 0 [] in
  let rec ready remaining =
    if remaining > 0 then (
      let _ = receive ~timeout:deadline ~selector:(function RuntimeReady _ -> `select () | _ -> `skip) () in
      ready (remaining - 1)) in
  ready 32;
  List.for_each workers ~fn:(fun worker -> send worker RuntimeGo);
  let rec replies remaining seen =
    if remaining = 0 then Ok ()
    else
      let value = receive ~timeout:deadline ~selector:(function RuntimeReply value -> `select value | _ -> `skip) () in
      if value < 0 || value >= 32 || List.any seen ~fn:(fun previous -> previous = value) then
        Error "duplicate or invalid wakeup reply"
      else replies (remaining - 1) (value :: seen) in
  replies 32 []

let wait_down reference pid =
  receive ~timeout:deadline ~selector:(function
    | Actor.DOWN { ref; pid = actual; reason } when ref = reference && Pid.equal actual pid -> `select reason
    | _ -> `skip) ()

let test_monitor_completion = fun _ctx ->
  let child = spawn (fun () ->
    let _ = receive ~timeout:deadline ~selector:(function RuntimeGo -> `select () | _ -> `skip) () in
    Ok ()) in
  let reference = Actor.monitor child in
  send child RuntimeGo;
  match wait_down reference child with
  | Ok () -> Ok ()
  | Error _ -> Error "normal child completion reported abnormal DOWN"

let test_monitor_after_completion = fun _ctx ->
  let child = spawn (fun () -> Ok ()) in
  let first = Actor.monitor child in
  let _ = wait_down first child in
  let late = Actor.monitor child in
  Actor.demonitor late;  (* Already completed DOWN remains a mailbox message. *)
  match wait_down late child with
  | Ok () -> Ok ()
  | Error (Actor.No_process pid) when Pid.equal pid child -> Ok ()
  | Error _ -> Error "completed monitor returned an unrelated failure reason"

let test_monitor_completion_race_batch = fun _ctx ->
  let rec loop remaining references =
    if remaining = 0 then Ok ()
    else
      let child = spawn (fun () -> Ok ()) in
      let reference = Actor.monitor child in
      if List.any references ~fn:(fun previous -> previous = reference) then
        Error "monitor registration reused a reference"
      else
        match wait_down reference child with
        | Ok () -> loop (remaining - 1) (reference :: references)
        | Error (Actor.No_process pid) when Pid.equal pid child ->
            loop (remaining - 1) (reference :: references)
        | Error _ -> Error "monitor race returned an unrelated failure" in
  loop 64 []

let test_demonitor_before_completion = fun _ctx ->
  let child = spawn (fun () ->
    let _ = receive ~timeout:deadline ~selector:(function RuntimeGo -> `select () | _ -> `skip) () in
    Ok ()) in
  let removed = Actor.monitor child in
  Actor.demonitor removed;
  let retained = Actor.monitor child in
  send child RuntimeGo;
  let _ = wait_down retained child in
  try
    let _ = receive ~timeout:(Time.Duration.from_millis 30) ~selector:(function
      | Actor.DOWN { ref; _ } when ref = removed -> `select () | _ -> `skip) () in
    Error "demonitored live relation delivered DOWN"
  with Receive_timeout -> Ok ()

let test_trapped_link_failure = fun _ctx ->
  Actor.set_flags [Actor.TrapExit true];
  let child = spawn_link (fun () ->
    let _ = receive ~timeout:deadline ~selector:(function RuntimeGo -> `select () | _ -> `skip) () in
    Error (Failure "runtime-link-probe")) in
  send child RuntimeGo;
  let reason = receive ~timeout:deadline ~selector:(function
    | Actor.EXIT { from; reason } when Pid.equal from child -> `select reason
    | _ -> `skip) () in
  Actor.set_flags [Actor.TrapExit false];
  match reason with
  | Error (Failure "runtime-link-probe") -> Ok ()
  | _ -> Error "trapped link did not preserve the child's failure reason"

let test_timer_delivers_once = fun _ctx ->
  let timer = Timer.send_after (self ()) (RuntimeTick 1) ~after:(Time.Duration.from_millis 10) in
  let _ = receive ~timeout:deadline ~selector:(function RuntimeTick 1 -> `select () | _ -> `skip) () in
  Timer.cancel timer;
  try
    let _ = receive ~timeout:(Time.Duration.from_millis 30) ~selector:(function RuntimeTick 1 -> `select () | _ -> `skip) () in
    Error "one-shot timer delivered twice"
  with Receive_timeout -> Ok ()

let test_cancel_before_timer_deadline = fun _ctx ->
  let timer = Timer.send_after (self ()) (RuntimeTick 2) ~after:(Time.Duration.from_millis 100) in
  Timer.cancel timer;
  Timer.cancel timer;
  try
    let _ = receive ~timeout:(Time.Duration.from_millis 200) ~selector:(function RuntimeTick 2 -> `select () | _ -> `skip) () in
    Error "cancelled timer delivered after its deadline"
  with Receive_timeout -> Ok ()

let test_pinned_actor_keeps_scheduler = fun _ctx ->
  let parent = self () in
  let _ = Runtime.spawn_pinned ~scheduler:0 (fun () ->
    let rec check remaining =
      match Runtime.current_scheduler_id () with
      | Some id when Runtime.Scheduler_id.to_int id = 0 ->
          if remaining = 0 then true
          else (yield (); check (remaining - 1))
      | _ -> false in
    send parent (RuntimePlacement (check 2048));
    Ok ()) in
  let stable = receive ~timeout:deadline ~selector:(function RuntimePlacement value -> `select value | _ -> `skip) () in
  if stable then Ok () else Error "pinned actor migrated away from its scheduler"

let test_blocking_actor_uses_dedicated_lane = fun _ctx ->
  let parent = self () in
  let released = Sync.Atomic.make false in
  let _ = Runtime.spawn_blocked (fun () ->
    send parent (RuntimePlacement (Runtime.current_scheduler_id () = None));
    (* Deliberately consume this dedicated lane without explicit cooperative yielding. *)
    while not (Sync.Atomic.get released) do () done;
    send parent (RuntimeReply 99);
    Ok ()) in
  try
    let dedicated = receive ~timeout:deadline ~selector:(function RuntimePlacement value -> `select value | _ -> `skip) () in
    let _ = spawn (fun () -> send parent (RuntimeValue 99); Ok ()) in
    let _ = receive_value 99 in
    Sync.Atomic.set released true;
    let _ = receive ~timeout:deadline ~selector:(function RuntimeReply 99 -> `select () | _ -> `skip) () in
    if dedicated then Ok () else Error "blocking actor ran on a normal scheduler"
  with
  | exn -> Sync.Atomic.set released true; raise exn

let tests = Test.[
  case "mailbox preserves sender FIFO" test_fifo;
  case "selective receive preserves skipped messages" test_selective_receive_preserves_skipped_message;
  case "receive timeout preserves skipped messages" test_timeout_preserves_skipped_message;
  case "pinned wakeup batch delivers each reply once" test_remote_wakeup_batch;
  case "monitor reports normal completion" test_monitor_completion;
  case "monitoring a completed actor returns DOWN" test_monitor_after_completion;
  case "monitor completion races return unique references and DOWN" test_monitor_completion_race_batch;
  case "demonitor before completion removes relation" test_demonitor_before_completion;
  case "trapped link preserves failure reason" test_trapped_link_failure;
  case "one-shot timer delivers once" test_timer_delivers_once;
  case "cancel before timer deadline prevents delivery" test_cancel_before_timer_deadline;
  case "pinned actor keeps scheduler across yields" test_pinned_actor_keeps_scheduler;
  case "blocking lane does not stall normal actors" test_blocking_actor_uses_dedicated_lane;
]

let () = Actors.run ~config:(Runtime.Config.make ~scheduler_count:2 ())
  ~main:(Test.Cli.main ~name:"std:runtime" ~tests) ~args:Env.args ()
