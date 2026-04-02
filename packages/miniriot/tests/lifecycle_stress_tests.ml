open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test
module Int = Kernel.Int
module String = Kernel.String
module List = Kernel.Collections.List
module HashSet = Kernel.Collections.HashSet

type Message.t +=
  | Child_ready of int * Pid.t
  | Crash_now of int

let int_eq = fun a b ->
  match Int.compare a b with
  | 0 -> true
  | _ -> false

let staged_crasher = fun parent index ->
  let pid = self () in
  send parent (Child_ready (index, pid));
  let target =
    receive
      ~selector:(
        function
        | Crash_now i -> `select i
        | _ -> `skip
      )
      ~timeout:5.0
      ()
  in
  if int_eq target index then
    Result.Error (Failure (String.concat "" [ "crash-"; Int.to_string index ]))
  else
    Result.Ok ()

let rec spawn_children = fun ~parent ~count ~spawn_fn index acc ->
  if int_eq index count then
    List.rev acc
  else
    let pid = spawn_fn index in
    spawn_children ~parent ~count ~spawn_fn (Int.succ index) ((index, pid) :: acc)

let await_ready_messages = fun count ->
  let seen = HashSet.create () in
  let rec loop received =
    if int_eq received count then
      Result.Ok ()
    else
      let index, pid =
        receive
          ~selector:(
            function
            | Child_ready (index, pid) -> `select (index, pid)
            | _ -> `skip
          )
          ~timeout:10.0
          ()
      in
      let key = String.concat ":" [ Int.to_string index; Pid.to_string pid ] in
      if HashSet.insert seen key then
        loop (Int.succ received)
      else
        Result.Error (String.concat "" [ "duplicate child readiness for "; key ])
  in
  loop 0

let crash_children = fun children ->
  List.iter (fun ((index, pid)) -> send pid (Crash_now index)) children

let collect_down_messages = fun expected ->
  let seen = HashSet.create () in
  let rec loop received =
    if int_eq received expected then
      Result.Ok ()
    else
      let pid, reason =
        receive
          ~selector:(
            function
            | Process.DOWN { pid; reason; _ } -> `select (pid, reason)
            | _ -> `skip
          )
          ~timeout:10.0
          ()
      in
      let key = Pid.to_string pid in
      if HashSet.insert seen key then
        match reason with
        | Error _ -> loop (Int.succ received)
        | Ok () -> Result.Error (String.concat "" [ "expected abnormal DOWN for pid "; key ])
      else
        Result.Error (String.concat "" [ "duplicate DOWN for pid "; key ])
  in
  loop 0

let collect_exit_messages = fun expected ->
  let seen = HashSet.create () in
  let rec loop received =
    if int_eq received expected then
      Result.Ok ()
    else
      let from_pid, reason =
        receive
          ~selector:(
            function
            | Process.EXIT { from; reason } -> `select (from, reason)
            | _ -> `skip
          )
          ~timeout:10.0
          ()
      in
      let key = Pid.to_string from_pid in
      if HashSet.insert seen key then
        match reason with
        | Error _ -> loop (Int.succ received)
        | Ok () -> Result.Error (String.concat "" [ "expected abnormal EXIT for pid "; key ])
      else
        Result.Error (String.concat "" [ "duplicate EXIT for pid "; key ])
  in
  loop 0

let test_monitor_exit_storm = fun () ->
  let parent = self () in
  let child_count = 48 in
  let spawn_fn index =
    let pid =
      spawn (fun () -> staged_crasher parent index)
    in
    let _monitor_ref = Process.monitor pid in
    pid
  in
  let children = spawn_children ~parent ~count:child_count ~spawn_fn 0 [] in
  match await_ready_messages child_count with
  | Result.Error _ as err -> err
  | Result.Ok () ->
      crash_children children;
      collect_down_messages child_count

let test_trap_exit_storm = fun () ->
  let parent = self () in
  let child_count = 48 in
  Process.set_flags [ Process.TrapExit true ];
  let spawn_fn index =
    spawn_link (fun () -> staged_crasher parent index)
  in
  let children = spawn_children ~parent ~count:child_count ~spawn_fn 0 [] in
  match await_ready_messages child_count with
  | Result.Error _ as err -> err
  | Result.Ok () ->
      crash_children children;
      collect_exit_messages child_count

let test_case = fun name fn ->
  try fn () with
  | Receive_timeout -> Result.Error (String.concat "" [ "timed out in "; name ])
  | exn -> Result.Error (String.concat
    ""
    [ "unexpected exception in "; name; ": "; Kernel.Exception.to_string exn ])

let () =
  let tests = [
    Test.case
      "monitor exit storm across workers"
      (fun _ctx -> test_case "monitor exit storm" test_monitor_exit_storm);
    Test.case
      "trap_exit link storm across workers"
      (fun _ctx -> test_case "trap_exit link storm" test_trap_exit_storm);
  ] in
  let normalize_args = function
    | [] -> [ "lifecycle_stress_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"lifecycle_stress_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
