open Miniriot
open Miniriot.Exception

module Result = Std.Result
module Test = Std.Test
module Int = Kernel.Int
module String = Kernel.String

type Message.t += Burst_done

let int_eq a b = match Int.compare a b with 0 -> true | _ -> false
let int_gt a b = match Int.compare a b with 1 -> true | _ -> false

let spawn_burst ~parent ~tasks ~yield_count =
  let rec spawn_tasks remaining =
    if int_eq remaining 0 then
      ()
    else
      let _ =
        spawn (fun () ->
            let rec burn n =
              if int_eq n 0 then
                ()
              else (
                yield ();
                burn (Int.pred n))
            in
            burn yield_count;
            send parent Burst_done;
            Result.Ok ())
      in
      spawn_tasks (Int.pred remaining)
  in
  spawn_tasks tasks

let collect_burst_completions ~expected =
  let rec loop received =
    if int_eq received expected then
      Result.Ok ()
    else
      let () =
        receive
          ~selector:(function
            | Burst_done -> `select ()
            | _ -> `skip)
          ~timeout:20.0 ()
      in
      loop (Int.succ received)
  in
  loop 0

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

let test_steals_observable_under_load () =
  let parent = self () in
  reset_trace_counters ();
  let rounds = 16 in
  let tasks_per_round = 256 in
  let yields_per_task = 32 in
  let rec loop remaining_rounds =
    if int_eq remaining_rounds 0 then
      let counters = trace_counters () in
      Result.Error
        (String.concat ""
           [ "expected at least one successful steal; counters=";
             counters_to_string counters ])
    else (
      spawn_burst ~parent ~tasks:tasks_per_round ~yield_count:yields_per_task;
      match collect_burst_completions ~expected:tasks_per_round with
      | Result.Error _ as err -> err
      | Result.Ok () ->
          let counters = trace_counters () in
          if int_gt counters.steals 0 then
            Result.Ok ()
          else
            loop (Int.pred remaining_rounds))
  in
  loop rounds

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
      Test.case "work stealing counters observe successful steals" (fun () ->
          test_case "work stealing counters observe successful steals"
            test_steals_observable_under_load);
    ]
  in
  let normalize_args = function
    | [] -> [ "work_stealing_counters_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match
      Test.Cli.main ~name:"work_stealing_counters_tests" ~tests
        ~args:(normalize_args args)
    with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args
    ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
