open Miniriot
open Miniriot.Exception

module Result = Std.Result
module Test = Std.Test

type Message.t +=
  | Child_ready of Pid.t
  | Start_crash of Pid.t
  | Linked_ready
  | Trap_exit_observed of Pid.t

let crashing_child parent =
  let child_pid = self () in
  send parent (Child_ready child_pid);
  let target =
    receive
      ~selector:(function
        | Start_crash pid -> `select pid
        | _ -> `skip)
      ()
  in
  if Pid.equal target child_pid then
    Result.Error (Failure "child crash")
  else
    Result.Ok ()

let linked_trap_exit_observer ~parent ~child =
  Process.set_flags [ Process.TrapExit true ];
  Process.link child;
  send parent Linked_ready;
  let from_pid, reason =
    receive
      ~selector:(function
        | Process.EXIT { from; reason } -> `select (from, reason)
        | _ -> `skip)
      ~timeout:5.0 ()
  in
  if Pid.equal from_pid child then
    match reason with
    | Error _ ->
        send parent (Trap_exit_observed from_pid);
        Result.Ok ()
    | Ok () ->
        Result.Error (Failure "expected abnormal EXIT reason")
  else
    Result.Error (Failure "received EXIT from unexpected pid")

let linked_non_trap_observer ~parent ~child =
  Process.link child;
  send parent Linked_ready;
  let _ = receive_any ~timeout:20.0 () in
  Result.Ok ()

let await_child_ready expected_child =
  let pid =
    receive
      ~selector:(function
        | Child_ready pid -> `select pid
        | _ -> `skip)
      ~timeout:5.0 ()
  in
  if Pid.equal pid expected_child then
    Result.Ok ()
  else
    Result.Error "child ready pid mismatch"

let await_linked_ready () =
  let _ =
    receive
      ~selector:(function
        | Linked_ready -> `select ()
        | _ -> `skip)
      ~timeout:5.0 ()
  in
  Result.Ok ()

let test_link_trap_exit_receives_exit_message () =
  let parent = self () in
  let child = spawn (fun () -> crashing_child parent) in
  let _ = spawn (fun () -> linked_trap_exit_observer ~parent ~child) in

  match await_child_ready child with
  | Result.Error _ as err -> err
  | Result.Ok () -> (
      match await_linked_ready () with
      | Result.Error _ as err -> err
      | Result.Ok () ->
          send child (Start_crash child);
          let observed =
            receive
              ~selector:(function
                | Trap_exit_observed pid -> `select pid
                | _ -> `skip)
              ~timeout:5.0 ()
          in
          if Pid.equal observed child then
            Result.Ok ()
          else
            Result.Error "trap_exit observer reported unexpected pid")

let test_link_without_trap_exit_dies_on_abnormal_exit () =
  let parent = self () in
  let child = spawn (fun () -> crashing_child parent) in
  let observer = spawn (fun () -> linked_non_trap_observer ~parent ~child) in
  let _monitor_ref = Process.monitor observer in

  match await_child_ready child with
  | Result.Error _ as err -> err
  | Result.Ok () -> (
      match await_linked_ready () with
      | Result.Error _ as err -> err
      | Result.Ok () ->
          send child (Start_crash child);
          let pid, reason =
            receive
              ~selector:(function
                | Process.DOWN { pid; reason; _ } when Pid.equal pid observer ->
                    `select (pid, reason)
                | _ -> `skip)
              ~timeout:5.0 ()
          in
          if Pid.equal pid observer then
            match reason with
            | Error _ -> Result.Ok ()
            | Ok () -> Result.Error "expected observer to exit abnormally"
          else
            Result.Error "received DOWN for unexpected pid")

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
    [ Test.case "link trap_exit receives EXIT across workers" (fun () ->
          test_case "link trap_exit receives EXIT"
            test_link_trap_exit_receives_exit_message);
      Test.case "link without trap_exit exits across workers" (fun () ->
          test_case "link without trap_exit exits"
            test_link_without_trap_exit_dies_on_abnormal_exit);
    ]
  in
  let normalize_args = function
    | [] -> [ "link_multicore_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match
      Test.Cli.main ~name:"link_multicore_tests" ~tests
        ~args:(normalize_args args)
    with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args
    ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
