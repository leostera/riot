open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test

type Message.t += Task of int | WorkerReady | TaskComplete of int
type Message.t += Stop_worker | Worker_stopped

(** Simple worker that processes tasks with timeout *)
let worker coordinator =
  let rec loop () =
    try
      let task =
        receive
          ~selector:(function
            | Task n -> `select (`task n)
            | Stop_worker -> `select `stop
            | _ -> `skip)
          ~timeout:0.5 ()
      in
      match task with
      | `task n ->
          Kernel.println
            (Kernel.String.concat ""
               [ "  Worker: Processing task "; Kernel.Int.to_string n ]);
          send coordinator (TaskComplete n);
          send coordinator WorkerReady;
          loop ()
      | `stop ->
          send coordinator Worker_stopped;
          Kernel.Result.Ok ()
    with Receive_timeout ->
      Kernel.println "  Worker: Timeout, sending ready signal";
      send coordinator WorkerReady;
      loop ()
  in
  loop ()

let test () =
  Kernel.println "Testing worker pool with timeout...";

  let coord_pid = self () in
  let worker_pid = spawn (fun () -> worker coord_pid) in

  let _ =
    receive
      ~selector:(function WorkerReady -> `select () | _ -> `skip)
      ~timeout:2.0 ()
  in
  let () = Kernel.println "✓ Worker sent initial ready signal via timeout" in

  send worker_pid (Task 42);

  let result =
    receive
      ~selector:(function TaskComplete n -> `select n | _ -> `skip)
      ~timeout:2.0 ()
  in
  let () =
    Kernel.println
      (Kernel.String.concat "" [ "✓ Task completed: "; Kernel.Int.to_string result ])
  in

  let _ =
    receive
      ~selector:(function WorkerReady -> `select () | _ -> `skip)
      ~timeout:2.0 ()
  in
  let () = Kernel.println "✓ Worker sent ready signal after task" in

  let _ =
    receive
      ~selector:(function WorkerReady -> `select () | _ -> `skip)
      ~timeout:2.0 ()
  in
  let () = Kernel.println "✓ Worker sent another ready signal via timeout (heartbeat)" in

  send worker_pid Stop_worker;
  let _ =
    receive
      ~selector:(function Worker_stopped -> `select () | _ -> `skip)
      ~timeout:2.0 ()
  in
  let () = Kernel.println "✓ Worker stopped cleanly" in

  Kernel.println "\n✓ All tests passed! Worker pool timeout mechanism works!";

  if Kernel.Int.equal result 42 then Result.Ok ()
  else Result.Error "Unexpected task completion result"

let test_case () =
  try test () with
  | exn -> Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [ Test.case "worker pool timeout heartbeat" test_case ] in
  let normalize_args = function
    | [] -> [ "timer_worker_pool_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match
      Test.Cli.main ~name:"timer_worker_pool_tests" ~tests
        ~args:(normalize_args args)
    with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ()
