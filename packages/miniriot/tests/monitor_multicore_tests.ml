open Miniriot
open Miniriot.Exception

module Result = Std.Result
module Test = Std.Test

type Message.t += Child_ready of Pid.t | Start_crash of Pid.t

let crashing_child parent =
  let child_pid = self () in
  send parent (Child_ready child_pid);
  let target =
    receive
      ~selector:(function
        | Start_crash pid -> `select pid
        | _ -> `skip)
      ~timeout:2.0 ()
  in
  if Pid.equal target child_pid then
    Result.Error (Failure "child crash")
  else
    Result.Ok ()

let test_monitor_receives_down () =
  let parent = self () in
  let child = spawn (fun () -> crashing_child parent) in
  let _ = Process.monitor child in

  let ready_pid =
    receive
      ~selector:(function
        | Child_ready pid -> `select pid
        | _ -> `skip)
      ~timeout:2.0 ()
  in
  if Pid.equal ready_pid child then (
    send child (Start_crash child);
    let down =
      receive
        ~selector:(function
          | Process.DOWN { pid; reason; _ } -> `select (pid, reason)
          | _ -> `skip)
        ~timeout:2.0 ()
    in
    match down with
    | pid, Error _ when Pid.equal pid child -> Result.Ok ()
    | pid, Ok () when Pid.equal pid child ->
        Result.Error "expected abnormal exit reason in DOWN"
    | _ ->
        Result.Error "received DOWN for unexpected pid")
  else
    Result.Error "child ready pid mismatch"

let test_demonitor_suppresses_down () =
  let parent = self () in
  let child = spawn (fun () -> crashing_child parent) in
  let monitor_ref = Process.monitor child in

  let ready_pid =
    receive
      ~selector:(function
        | Child_ready pid -> `select pid
        | _ -> `skip)
      ~timeout:2.0 ()
  in
  if Pid.equal ready_pid child then (
    Process.demonitor monitor_ref;
    send child (Start_crash child);
    let maybe_down =
      try
        Some
          (receive
             ~selector:(function
               | Process.DOWN { pid; reason; _ } -> `select (pid, reason)
               | _ -> `skip)
             ~timeout:1.0 ())
      with
      | Receive_timeout -> None
    in
    match maybe_down with
    | None -> Result.Ok ()
    | Some _ -> Result.Error "received DOWN after demonitor")
  else
    Result.Error "child ready pid mismatch"

let test_case name fn =
  try fn () with
  | Receive_timeout ->
      Result.Error
        (Kernel.String.concat "" [ "timed out in "; name ])
  | exn ->
      Result.Error
        (Kernel.String.concat ""
           [ "unexpected exception in "; name; ": ";
             Kernel.Exception.to_string exn ])

let () =
  let tests =
    [ Test.case "monitor receives DOWN across workers" (fun () ->
          test_case "monitor receives DOWN" test_monitor_receives_down);
      Test.case "demonitor suppresses DOWN across workers" (fun () ->
          test_case "demonitor suppresses DOWN" test_demonitor_suppresses_down);
    ]
  in
  let normalize_args = function
    | [] -> [ "monitor_multicore_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match
      Test.Cli.main ~name:"monitor_multicore_tests" ~tests
        ~args:(normalize_args args)
    with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args
    ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
