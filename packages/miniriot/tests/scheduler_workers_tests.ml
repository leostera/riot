open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test

type Message.t += Worker_done of int

let int_eq a b = match Kernel.Int.compare a b with 0 -> true | _ -> false
let int_lt a b = match Kernel.Int.compare a b with -1 -> true | _ -> false
let int_gt a b = match Kernel.Int.compare a b with 1 -> true | _ -> false

let test () =
  let parent = self () in
  let expected = 64 in
  let seen = Kernel.Collections.HashSet.create () in
  let configured = Miniriot.Config.make ~scheduler_count:4 () in
  let worker_count = Miniriot.Config.worker_count configured in
  Kernel.println
    (Kernel.String.concat ""
       [ "✓ Configured "; Kernel.Int.to_string worker_count;
         " workers for scheduling stress test" ]);

  let () =
    match Kernel.Int.compare worker_count 1 with
    | -1 -> Kernel.panic "Expected worker_count >= 1"
    | _ -> ()
  in

  let rec spawn_children count =
    if int_eq count 0 then
      ()
    else
      let n = count in
      let _ = spawn (fun () ->
        send parent (Worker_done n);
        Kernel.Result.Ok ())
      in
      spawn_children (Kernel.Int.sub count 1)
  in

  let rec collect count =
    if int_eq count expected then
      Result.Ok ()
    else
      let received =
        try
          receive
            ~selector:(function
              | Worker_done value -> `select value
              | _ -> `skip)
            ~timeout:1.0
            ()
        with
        | Receive_timeout ->
            Kernel.panic "Timed out while collecting worker completion messages"
      in
      let inserted = Kernel.Collections.HashSet.insert seen received in
      if Kernel.Bool.not inserted then
        Result.Error
          (Kernel.String.concat ""
             [ "Duplicate Worker_done message: "; Kernel.Int.to_string received ])
      else if int_lt received 0 then
        Result.Error
          (Kernel.String.concat ""
             [ "Unexpected worker index: "; Kernel.Int.to_string received ])
      else if int_gt received expected then
        Result.Error
          (Kernel.String.concat ""
             [ "Unexpected worker index: "; Kernel.Int.to_string received ])
      else
        collect (Kernel.Int.add count 1)
  in

  spawn_children expected;
  match collect 0 with
  | Result.Ok () ->
      Kernel.println
        (Kernel.String.concat ""
           [ "✓ Multi-worker scheduler test received "; Kernel.Int.to_string expected;
             " messages" ]);
      Result.Ok ()
  | Result.Error msg -> Result.Error msg

let test_case () =
  try test () with
  | exn -> Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [ Test.case "multi-worker scheduler distribution" test_case ] in
  let normalize_args = function
    | [] -> [ "scheduler_workers_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match
      Test.Cli.main ~name:"scheduler_workers_tests" ~tests
        ~args:(normalize_args args)
    with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run
    ~main
      ~args:Std.Env.args
    ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
