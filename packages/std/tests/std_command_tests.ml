open Std
module Test = Std.Test

let stderr_payload_size = 256 * 1_024

let stderr_payload = String.make ~len:stderr_payload_size ~char:'e'

let stdout_payload = "stdout-finished"

type Runtime.Message.t +=
  | Parallel_command_finished of (int * (unit, string) result)

let self_executable = fun () ->
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_command_tests"

let run_capture = fun () ->
  let cmd = Command.make (self_executable ()) ~args:[ "capture-both-streams" ] in
  Command.output cmd |> Result.expect ~msg:"failed to run capture helper"

let test_command_output_drains_stdout_and_stderr = fun _ctx ->
  let output = run_capture () in
  if not (Int.equal output.status 0) then
    Error ("expected capture helper to exit 0, got " ^ Int.to_string output.status)
  else if not (String.equal output.stdout stdout_payload) then
    Error ("unexpected stdout payload: " ^ output.stdout)
  else if not (Int.equal (String.length output.stderr) stderr_payload_size) then
    Error ("unexpected stderr length: " ^ Int.to_string (String.length output.stderr))
  else
    Ok ()

let test_command_output_handles_delayed_shell_stdout = fun _ctx ->
  let cmd = Command.make "sh" ~args:[ "-c"; "sleep 0.05; printf delayed-output" ] in
  match Command.output cmd with
  | Error (Command.SystemError message) -> Error ("expected delayed shell stdout to succeed, got: "
  ^ message)
  | Ok output ->
      if not (Int.equal output.status 0) then
        Error ("expected shell command to exit 0, got " ^ Int.to_string output.status)
      else if not (String.equal output.stdout "delayed-output") then
        Error ("unexpected delayed stdout payload: " ^ output.stdout)
      else if not (String.equal output.stderr "") then
        Error ("expected empty stderr, got: " ^ output.stderr)
      else
        Ok ()

let test_command_output_handles_parallel_shell_commands = fun _ctx ->
  let parent = Runtime.self () in
  let count = 16 in
  let expected_stdout = "parallel-output" in
  let _workers =
    List.init ~count ~fn:(fun index ->
        Runtime.spawn
          (fun () ->
            let result =
              match Command.output
                (Command.make "sh" ~args:[ "-c"; "sleep 0.05; printf parallel-output" ]) with
              | Error (Command.SystemError message) -> Error message
              | Ok output when not (Int.equal output.status 0) -> Error ("expected exit 0, got "
              ^ Int.to_string output.status)
              | Ok output when not (String.equal output.stdout expected_stdout) -> Error ("unexpected stdout: "
              ^ output.stdout)
              | Ok output when not (String.equal output.stderr "") -> Error ("unexpected stderr: "
              ^ output.stderr)
              | Ok _ -> Ok ()
            in
            Runtime.send parent (Parallel_command_finished (index, result));
            Ok ()))
  in
  let rec collect pending failures =
    if pending = 0 then
      if failures = [] then
        Ok ()
      else
        Error (String.concat "; " (List.reverse failures))
    else
      match
        Runtime.receive
          ~selector:(
            function
            | Parallel_command_finished (_index, result) -> `select result
            | _ -> `skip
          )
          ~timeout:2.0
          ()
      with
      | Ok () -> collect (pending - 1) failures
      | Error failure -> collect (pending - 1) (failure :: failures)
  in
  collect count []

let test_command_output_handles_parallel_fast_exit_commands = fun _ctx ->
  let parent = Runtime.self () in
  let worker_count = 32 in
  let iterations_per_worker = 16 in
  let run_fast_exit () =
    match Command.output (Command.make "sh" ~args:[ "-c"; ":" ]) with
    | Error (Command.SystemError message) -> Error message
    | Ok output when not (Int.equal output.status 0) -> Error ("expected exit 0, got "
    ^ Int.to_string output.status)
    | Ok output when not (String.equal output.stdout "") -> Error ("expected empty stdout, got: "
    ^ output.stdout)
    | Ok output when not (String.equal output.stderr "") -> Error ("expected empty stderr, got: "
    ^ output.stderr)
    | Ok _ -> Ok ()
  in
  let rec run_n remaining =
    if remaining = 0 then
      Ok ()
    else
      match run_fast_exit () with
      | Error _ as err -> err
      | Ok () -> run_n (remaining - 1)
  in
  let _workers =
    List.init ~count:worker_count ~fn:(fun index ->
        Runtime.spawn
          (fun () ->
            let result = run_n iterations_per_worker in
            Runtime.send parent (Parallel_command_finished (index, result));
            Ok ()))
  in
  let rec collect pending failures =
    if pending = 0 then
      if failures = [] then
        Ok ()
      else
        Error (String.concat "; " (List.reverse failures))
    else
      match
        Runtime.receive
          ~selector:(
            function
            | Parallel_command_finished (_index, result) -> `select result
            | _ -> `skip
          )
          ~timeout:5.0
          ()
      with
      | Ok () -> collect (pending - 1) failures
      | Error failure -> collect (pending - 1) (failure :: failures)
  in
  collect worker_count []

let test_scheduler_exception_is_reported_on_stderr = fun _ctx ->
  let cmd = Command.make (self_executable ()) ~args:[ "scheduler-failure" ] in
  match Command.output cmd with
  | Error (Command.SystemError message) -> Error message
  | Ok output ->
      if output.status = 0 then
        Error "scheduler failure helper unexpectedly succeeded"
      else if not (String.equal output.stdout "") then
        Error "scheduler exception diagnostics leaked into stdout"
      else if not (String.contains output.stderr "scheduler-diagnostic-regression") then
        Error "scheduler swallowed the process exception diagnostic"
      else if not (String.contains output.stderr "[Scheduler]") then
        Error "scheduler exception diagnostic lost its context"
      else
        Ok ()

let test_scheduler_trace_preserves_stdout_payload = fun _ctx ->
  let cmd = Command.make (self_executable ()) ~args:[ "scheduler-trace" ] in
  match Command.output cmd with
  | Error (Command.SystemError message) -> Error message
  | Ok output ->
      if not (Int.equal output.status 0) then
        Error "scheduler trace helper failed"
      else if not (String.equal output.stdout "trace-payload") then
        Error "scheduler trace changed stdout payload"
      else if not (String.contains output.stderr "[Scheduler.Trace] shutdown") then
        Error "enabled scheduler tracing produced no shutdown diagnostic"
      else
        Ok ()

let meta_tests = [
  Test.case "command output drains stdout and stderr without deadlock" test_command_output_drains_stdout_and_stderr;
  Test.case "command output handles delayed shell stdout" test_command_output_handles_delayed_shell_stdout;
  Test.case "command output handles parallel shell commands" test_command_output_handles_parallel_shell_commands;
  Test.case "command output handles parallel fast exit commands" test_command_output_handles_parallel_fast_exit_commands;
  Test.case "scheduler exceptions report on stderr" test_scheduler_exception_is_reported_on_stderr;
  Test.case "scheduler tracing preserves stdout payload" test_scheduler_trace_preserves_stdout_payload;
]

let capture_main = fun () ->
  eprint stderr_payload;
  print stdout_payload;
  Ok ()

let meta_main = fun ~args ->
  let normalize_args = function
    | [] -> [ "std_command_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  Test.Cli.main ~name:"std_command_tests" ~tests:meta_tests ~args:(normalize_args args)

let main = fun ~args ->
  match args with
  | _ :: "capture-both-streams" :: _ -> capture_main ()
  | _ :: "scheduler-failure" :: _ -> panic "scheduler-diagnostic-regression"
  | _ :: "scheduler-trace" :: _ ->
      Runtime.enable_trace ();
      print "trace-payload";
      Ok ()
  | _ -> meta_main ~args

let () = Runtime.run ~main ~args:Env.args ()
