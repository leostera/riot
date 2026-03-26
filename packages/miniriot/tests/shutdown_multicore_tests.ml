open Miniriot
open Miniriot.Exception

module Result = Std.Result
module Test = Std.Test
module Int = Kernel.Int
module String = Kernel.String

type Message.t += Blocker_parked of int

let int_eq a b = match Int.compare a b with 0 -> true | _ -> false

let blocker parent index =
  send parent (Blocker_parked index);
  let _ = receive_any ~timeout:60.0 () in
  Result.Ok ()

let test_shutdown_unparks_idle_workers () =
  let parent = self () in
  let blocker_count = 64 in
  let rec spawn_blockers index =
    if int_eq index blocker_count then
      ()
    else (
      let _pid = spawn (fun () -> blocker parent index) in
      spawn_blockers (Int.succ index))
  in
  let rec await_parked seen =
    if int_eq seen blocker_count then
      Result.Ok ()
    else
      let _ =
        receive
          ~selector:(function
            | Blocker_parked _ -> `select ()
            | _ -> `skip)
          ~timeout:5.0 ()
      in
      await_parked (Int.succ seen)
  in
  spawn_blockers 0;
  match await_parked 0 with
  | Result.Error _ as err -> err
  | Result.Ok () ->
      shutdown ~status:0;
      Result.Ok ()

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
    [ Test.case "shutdown wakes parked workers" (fun () ->
          test_case "shutdown wakes parked workers"
            test_shutdown_unparks_idle_workers);
    ]
  in
  let normalize_args = function
    | [] -> [ "shutdown_multicore_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match
      Test.Cli.main ~name:"shutdown_multicore_tests" ~tests
        ~args:(normalize_args args)
    with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args
    ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
