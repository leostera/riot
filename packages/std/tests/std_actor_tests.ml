open Std

type Message.t +=
  | Actor_self_reply of Pid.t
  | Actor_ready of Pid.t
  | Actor_stop
  | Actor_sleeper_done of int

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let test_actor_self_in_spawned_actor = fun _ctx ->
  let parent = self () in
  ignore
    (
      Actor.spawn
        (fun () ->
          send parent (Actor_self_reply (Actor.self ()));
          Ok ())
    );
  match await
    ~what:"spawned actor self"
    (fun __tmp1 ->
      match __tmp1 with
      | Actor_self_reply pid -> Select pid
      | _ -> Skip) with
  | Error _ as err -> err
  | Ok child_pid ->
      if not (Pid.equal child_pid parent) then
        Ok ()
      else
        Error "expected child Actor.self to differ from parent pid"

let test_actor_spawn_returns_live_pid = fun _ctx ->
  let parent = self () in
  let child =
    Actor.spawn
      (fun () ->
        send parent (Actor_ready (Actor.self ()));
        receive
          ~selector:(fun __tmp1 ->
            match __tmp1 with
            | Actor_stop -> Select ()
            | _ -> Skip)
          ();
        Ok ())
  in
  let _monitor = Runtime.Actor.monitor child in
  match await
    ~what:"spawned actor ready"
    (fun __tmp1 ->
      match __tmp1 with
      | Actor_ready pid -> Select pid
      | _ -> Skip) with
  | Error _ as err -> err
  | Ok ready_pid ->
      if Pid.equal ready_pid child then (
        send child Actor_stop;
        ignore
          (
            await
              ~what:"spawned actor down"
              (fun __tmp1 ->
                match __tmp1 with
                | Runtime.Actor.DOWN { pid; _ } when Pid.equal pid child -> Select ()
                | _ -> Skip)
          );
        Ok ()
      ) else
        Error "expected spawn to return the same pid reported by the child"

let is_failure = fun exn ~message ->
  match exn with
  | Failure reason -> String.equal reason message
  | _ -> false

let test_actor_spawn_link_reports_abnormal_exit = fun _ctx ->
  Actor.set_flags [ Runtime.Actor.TrapExit true ];
  let child = Actor.spawn_link (fun () -> Error (Failure "boom")) in
  match await
    ~what:"linked actor exit"
    (fun __tmp1 ->
      match __tmp1 with
      | Runtime.Actor.EXIT { from; reason = Error exn } when Pid.equal from child
      && is_failure exn ~message:"boom" -> Select ()
      | _ -> Skip) with
  | Ok () -> Ok ()
  | Error _ as err -> err

(* Receive timeouts must wake the process even when the timer fires while the
   process is still installing it or has not parked yet. Zero-length sleeps make
   the reactor fire the timer in the same tick that schedules it, which used to
   lose the wakeup and leave the sleeper parked forever.
*)

let test_sleep_timeouts_always_wake = fun _ctx ->
  let parent = self () in
  let sleepers = 16 in
  let iterations = 400 in
  for index = 0 to sleepers - 1 do
    ignore
      (
        Actor.spawn
          (fun () ->
            for _ = 1 to iterations do
              sleep (Time.Duration.from_nanos 0)
            done;
            send parent (Actor_sleeper_done index);
            Ok ())
      )
  done;
  let rec collect finished =
    if finished = sleepers then
      Ok ()
    else
      match (
        try
          Ok (
            receive
              ~selector:(fun __tmp1 ->
                match __tmp1 with
                | Actor_sleeper_done _ -> Select ()
                | _ -> Skip)
              ~timeout:(Time.Duration.from_secs 20)
              ()
          )
        with
        | Receive_timeout -> Error ()
      ) with
      | Ok () -> collect (finished + 1)
      | Error () ->
          Error (Int.to_string (sleepers - finished)
          ^ " of "
          ^ Int.to_string sleepers
          ^ " sleepers never woke from a receive timeout")
  in
  collect 0

let tests =
  Test.[
    case "Actor.self inside spawned actor differs from parent" test_actor_self_in_spawned_actor;
    case "Actor.spawn returns a live pid" test_actor_spawn_returns_live_pid;
    case
      "Actor.spawn_link reports abnormal exit when trapping exits"
      test_actor_spawn_link_reports_abnormal_exit;
    case ~size:Large "sleep timeouts always wake the sleeper" test_sleep_timeouts_always_wake;
  ]

let main ~args = Test.Cli.main ~name:"Actor" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
