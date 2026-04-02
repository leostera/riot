open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test
module Int = Kernel.Int
module String = Kernel.String
module HashSet = Kernel.Collections.HashSet

type Message.t +=
  | Stream_job_result of int * int
  | Stream_round_ok of int
  | Stream_round_error of int * string

let int_eq = fun a b ->
  match Int.compare a b with
  | 0 -> true
  | _ -> false

let int_lt = fun a b ->
  match Int.compare a b with
  | -1 -> true
  | _ -> false

let int_gt = fun a b ->
  match Int.compare a b with
  | 1 -> true
  | _ -> false

let should_yield = fun ~round ~job_id ~salt ->
  let weighted_round = Int.mul round 17 in
  let weighted_job = Int.mul job_id 13 in
  let total = Int.add (Int.add weighted_round weighted_job) salt in
  int_eq (Int.rem total 5) 0

let short_lived_worker = fun ~collector ~round ~job_id ->
  let _ = should_yield in
  let _ = round in
  send collector (Stream_job_result (round, job_id));
  Result.Ok ()

let collector = fun ~parent ~round ~jobs_per_round ->
  let seen_jobs = HashSet.create () in
  let fail msg =
    send
      parent
      (Stream_round_error (
        round,
        String.concat
          ""
          [ msg; " round="; Int.to_string round; " jobs_per_round="; Int.to_string jobs_per_round ]
      ));
    Result.Error (Failure msg)
  in
  let rec collect received =
    if int_eq received jobs_per_round then
      (
        send parent (Stream_round_ok round);
        Result.Ok ()
      )
    else
      let observed_round, job_id =
        receive
          ~selector:(
            function
            | Stream_job_result (observed_round, job_id) -> `select (observed_round, job_id)
            | _ -> `skip
          )
          ()
      in
      if int_eq observed_round round then
        if int_lt job_id 0 then
          fail (String.concat "" [ "received invalid job id "; Int.to_string job_id ])
        else if int_lt job_id jobs_per_round then
          let key = Int.to_string job_id in
          if HashSet.insert seen_jobs key then
            collect (Int.succ received)
          else
            fail (String.concat "" [ "received duplicate job id "; Int.to_string job_id ])
        else
          fail (String.concat "" [ "received invalid job id "; Int.to_string job_id ])
      else
        fail
          (String.concat
            ""
            [ "received job result from unexpected round "; Int.to_string observed_round ])
  in
  collect 0

let spawn_round_jobs = fun ~collector ~round ~jobs_per_round ->
  let rec loop job_id =
    if int_eq job_id jobs_per_round then
      ()
    else
      (
        let _ =
          spawn (fun () -> short_lived_worker ~collector ~round ~job_id)
        in
        if should_yield ~round ~job_id ~salt:3 then
          yield ();
        loop (Int.succ job_id)
      )
  in
  loop 0

let run_round = fun ~round ~jobs_per_round ->
  let parent = self () in
  let collector_pid =
    spawn (fun () -> collector ~parent ~round ~jobs_per_round)
  in
  let _ = Process.monitor collector_pid in
  spawn_round_jobs ~collector:collector_pid ~round ~jobs_per_round;
  receive
    ~selector:(
      function
      | Stream_round_ok observed_round when int_eq observed_round round ->
          `select (Result.Ok ())
      | Stream_round_error (observed_round, msg) when int_eq observed_round round ->
          `select (Result.Error msg)
      | Process.DOWN { pid; reason; _ } when Pid.equal pid collector_pid ->
          let msg = String.concat ""
            [
              "collector exited before reporting round completion. round=";
              Int.to_string round;
              " reason=";
              (
                match reason with
                | Ok () -> "normal"
                | Error exn -> Kernel.Exception.to_string exn
              )
            ]
          in
          `select (Result.Error msg)
      | _ ->
          `skip
    )
    ()

let test_short_lived_result_stream_stress = fun () ->
  (* Regression for multicore result-stream hangs and
     Continuation_already_resumed crashes when many short-lived workers fan in
     to a single selective receive loop. *)
  let rounds = 4 in
  let jobs_per_round = 128 in
  let rec loop round =
    if int_gt round rounds then
      Result.Ok ()
    else
      match run_round ~round ~jobs_per_round with
      | Result.Ok () -> loop (Int.succ round)
      | Result.Error msg -> Result.Error (String.concat
        ""
        [ "round "; Int.to_string round; " failed: "; msg ])
  in
  loop 1

let test_case = fun name fn ->
  try fn () with
  | Receive_timeout -> Result.Error (String.concat "" [ "timed out in "; name ])
  | exn -> Result.Error (String.concat
    ""
    [ "unexpected exception in "; name; ": "; Kernel.Exception.to_string exn ])

let () =
  let tests = [
    Test.case
      "short-lived result stream remains stable under sustained multicore load"
      (fun _ctx ->
        test_case "short-lived result stream remains stable under sustained multicore load" test_short_lived_result_stream_stress);
  ] in
  let normalize_args = function
    | [] -> [ "result_stream_stress_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"result_stream_stress_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
