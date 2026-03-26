open Miniriot
open Miniriot.Exception

module Result = Std.Result
module Test = Std.Test
module Int = Kernel.Int
module String = Kernel.String

type Message.t += Timer_tick | Tick_observed

let int_ge a b =
  match Int.compare a b with
  | -1 -> false
  | _ -> true

let int_lt a b = match Int.compare a b with -1 -> true | _ -> false

let counters_to_string counters =
  String.concat ""
    [
      "{steals="; Int.to_string counters.steals;
      "; failed_steals="; Int.to_string counters.failed_steals;
      "; remote_wakeups="; Int.to_string counters.remote_wakeups;
      "; duplicate_enqueue_races=";
      Int.to_string counters.duplicate_enqueue_races;
      "}";
    ]

let spawn_timer_receiver ~parent =
  let receiver =
    spawn (fun () ->
        let _ =
          receive
            ~selector:(function
              | Timer_tick -> `select ()
              | _ -> `skip)
            ~timeout:5.0 ()
        in
        send parent Tick_observed;
        Result.Ok ())
  in
  let _timer_id = Timer.send_after receiver Timer_tick ~after:0.01 in
  let _ =
    receive
      ~selector:(function
        | Tick_observed -> `select ()
        | _ -> `skip)
      ~timeout:5.0 ()
  in
  Result.Ok ()

let test_trace_counters_observable_and_resettable () =
  let parent = self () in
  reset_trace_counters ();
  let _ = spawn_timer_receiver ~parent in
  let first = trace_counters () in
  if int_ge first.remote_wakeups 1 then (
    reset_trace_counters ();
    let after_reset = trace_counters () in
    if int_lt after_reset.remote_wakeups first.remote_wakeups then
      let _ = spawn_timer_receiver ~parent in
      let second = trace_counters () in
      let expected_min = Int.succ after_reset.remote_wakeups in
      if int_ge second.remote_wakeups expected_min then
        Result.Ok ()
      else
        Result.Error
          (String.concat ""
             [ "expected remote_wakeups to increment again after reset. ";
               "after_reset="; counters_to_string after_reset;
               " second="; counters_to_string second ])
    else
      Result.Error
        (String.concat ""
           [ "expected reset to lower remote_wakeups count. before=";
             counters_to_string first; " after_reset=";
             counters_to_string after_reset ]))
  else
    Result.Error
      (String.concat ""
         [ "expected at least one remote wakeup after timer delivery, got ";
           counters_to_string first ])

let test_case name fn =
  try fn () with
  | Receive_timeout ->
      Result.Error
        (String.concat "" [ "timed out in "; name ])
  | exn ->
      Result.Error
        (String.concat ""
           [ "unexpected exception in "; name; ": ";
             Kernel.Exception.to_string exn ])

let () =
  let tests =
    [
      Test.case "trace counters observable and resettable" (fun () ->
          test_case "trace counters observable/reset"
            test_trace_counters_observable_and_resettable);
    ]
  in
  let normalize_args = function
    | [] -> [ "trace_counters_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match
      Test.Cli.main ~name:"trace_counters_tests" ~tests
        ~args:(normalize_args args)
    with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args
    ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
