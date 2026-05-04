open Std

module Test = Std.Test

let stderr_payload_size = 256 * 1_024

let stderr_payload = String.make ~len:stderr_payload_size ~char:'e'

let stdout_payload = "stdout-finished"

let streamed_stdout_payload = "first line\nsecond line\nthird line\n"

type Runtime.Message.t +=
  | Parallel_command_finished of (int * (unit, string) result)

let self_executable = fun () ->
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_command_tests"

let run_capture = fun () ->
  let cmd = Command.make (self_executable ()) ~args:[ "capture-both-streams" ] in
  Command.output cmd
  |> Result.expect ~msg:"failed to run capture helper"

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
  | Error (Command.SystemError message) ->
      Error ("expected delayed shell stdout to succeed, got: " ^ message)
  | Ok output ->
      if not (Int.equal output.status 0) then
        Error ("expected shell command to exit 0, got " ^ Int.to_string output.status)
      else if not (String.equal output.stdout "delayed-output") then
        Error ("unexpected delayed stdout payload: " ^ output.stdout)
      else if not (String.equal output.stderr "") then
        Error ("expected empty stderr, got: " ^ output.stderr)
      else
        Ok ()

let test_command_output_streams_stdout_lines = fun _ctx ->
  let seen_lines = ref [] in
  let cmd = Command.make (self_executable ()) ~args:[ "capture-stdout-lines" ] in
  match Command.output ~on_stdout_line:(fun line -> seen_lines := line :: !seen_lines) cmd with
  | Error (Command.SystemError message) ->
      Error ("expected streamed stdout helper to succeed, got: " ^ message)
  | Ok output ->
      let actual_lines = List.reverse !seen_lines in
      let expected_lines = [ "first line\n"; "second line\n"; "third line\n" ] in
      let same_lines =
        if not (Int.equal (List.length actual_lines) (List.length expected_lines)) then
          false
        else
          List.zip actual_lines expected_lines
          |> List.for_all (fun (actual, expected) -> String.equal actual expected)
      in
      if not (Int.equal output.status 0) then
        Error ("expected streamed helper to exit 0, got " ^ Int.to_string output.status)
      else if not same_lines then
        Error ("unexpected streamed lines: " ^ String.concat "" actual_lines)
      else if not (String.equal output.stdout streamed_stdout_payload) then
        Error ("unexpected streamed stdout payload: " ^ output.stdout)
      else if not (String.equal output.stderr "") then
        Error ("expected empty stderr, got: " ^ output.stderr)
      else
        Ok ()

let test_command_output_emits_idle_callbacks = fun _ctx ->
  let seen = ref [] in
  let cmd = Command.make "sh" ~args:[ "-c"; "sleep 0.05; printf idle-done" ] in
  match Command.output
    ~on_idle:(fun elapsed -> seen := Time.Duration.to_micros elapsed :: !seen)
    ~idle_interval:(Time.Duration.from_millis 10)
    cmd with
  | Error (Command.SystemError message) ->
      Error ("expected idle callback command to succeed, got: " ^ message)
  | Ok output ->
      if not (Int.equal output.status 0) then
        Error ("expected idle callback command to exit 0, got " ^ Int.to_string output.status)
      else if List.is_empty !seen then
        Error "expected at least one idle callback"
      else if not (String.equal output.stdout "idle-done") then
        Error ("unexpected idle callback stdout payload: " ^ output.stdout)
      else
        Ok ()

let test_command_output_times_out = fun _ctx ->
  let cmd = Command.make "sh" ~args:[ "-c"; "exec sleep 2" ] in
  let started = Time.Instant.now () in
  match Command.output ~timeout:(Time.Duration.from_millis 20) cmd with
  | Error (Command.SystemError message) ->
      Error ("expected timeout command to return output, got: " ^ message)
  | Ok output ->
      let elapsed_ms = Time.Duration.to_millis (Time.Instant.elapsed started) in
      if not (Int.equal output.status 137) then
        Error ("expected timeout status 137, got " ^ Int.to_string output.status)
      else if elapsed_ms > 1_000 then
        Error ("expected timeout command to return promptly, elapsed ms: "
        ^ Int.to_string elapsed_ms)
      else
        Ok ()

let test_command_output_limits_captured_streams = fun _ctx ->
  let cmd = Command.make (self_executable ()) ~args:[ "capture-both-streams" ] in
  match Command.output ~max_output_bytes:6 cmd with
  | Error (Command.SystemError message) ->
      Error ("expected limited capture helper to succeed, got: " ^ message)
  | Ok output ->
      if not (Int.equal output.status 0) then
        Error ("expected limited capture helper to exit 0, got " ^ Int.to_string output.status)
      else if not (String.equal output.stdout "stdout") then
        Error ("unexpected limited stdout payload: " ^ output.stdout)
      else if not (Int.equal (String.length output.stderr) 6) then
        Error ("unexpected limited stderr length: " ^ Int.to_string (String.length output.stderr))
      else
        Ok ()

let test_command_output_returns_when_child_exits_with_inherited_writer = fun _ctx ->
  let cmd = Command.make "sh" ~args:[ "-c"; "printf parent-done; (sleep 2) &" ] in
  let started = Time.Instant.now () in
  match Command.output
    ~on_idle:(fun _elapsed -> ())
    ~idle_interval:(Time.Duration.from_millis 10)
    cmd with
  | Error (Command.SystemError message) ->
      Error ("expected inherited writer command to succeed, got: " ^ message)
  | Ok output ->
      let elapsed_us = Time.Duration.to_micros (Time.Instant.elapsed started) in
      if not (Int.equal output.status 0) then
        Error ("expected inherited writer command to exit 0, got " ^ Int.to_string output.status)
      else if not (String.equal output.stdout "parent-done") then
        Error ("unexpected inherited writer stdout payload: " ^ output.stdout)
      else if elapsed_us > 1_000_000 then
        Error ("expected command output to return before inherited writer closed, elapsed us: "
        ^ Int.to_string elapsed_us)
      else
        Ok ()

let test_command_output_handles_parallel_shell_commands = fun _ctx ->
  let parent = Runtime.self () in
  let count = 16 in
  let expected_stdout = "parallel-output" in
  let _workers =
    List.init
      ~count
      ~fn:(fun index ->
        Runtime.spawn
          (fun () ->
            let result =
              match Command.output
                (Command.make "sh" ~args:[ "-c"; "sleep 0.05; printf parallel-output" ]) with
              | Error (Command.SystemError message) -> Error message
              | Ok output when not (Int.equal output.status 0) ->
                  Error ("expected exit 0, got " ^ Int.to_string output.status)
              | Ok output when not (String.equal output.stdout expected_stdout) ->
                  Error ("unexpected stdout: " ^ output.stdout)
              | Ok output when not (String.equal output.stderr "") ->
                  Error ("unexpected stderr: " ^ output.stderr)
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
      match Runtime.receive
        ~selector:(fun __tmp1 ->
          match __tmp1 with
          | Parallel_command_finished (_index, result) -> Select result
          | _ -> Skip)
        ~timeout:5.0
        () with
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
    | Ok output when not (Int.equal output.status 0) ->
        Error ("expected exit 0, got " ^ Int.to_string output.status)
    | Ok output when not (String.equal output.stdout "") ->
        Error ("expected empty stdout, got: " ^ output.stdout)
    | Ok output when not (String.equal output.stderr "") ->
        Error ("expected empty stderr, got: " ^ output.stderr)
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
    List.init
      ~count:worker_count
      ~fn:(fun index ->
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
      match Runtime.receive
        ~selector:(fun __tmp1 ->
          match __tmp1 with
          | Parallel_command_finished (_index, result) -> Select result
          | _ -> Skip)
        ~timeout:5.0
        () with
      | Ok () -> collect (pending - 1) failures
      | Error failure -> collect (pending - 1) (failure :: failures)
  in
  collect worker_count []

let meta_tests = [
  Test.case
    "command output drains stdout and stderr without deadlock"
    test_command_output_drains_stdout_and_stderr;
  Test.case
    "command output handles delayed shell stdout"
    test_command_output_handles_delayed_shell_stdout;
  Test.case "command output streams stdout lines" test_command_output_streams_stdout_lines;
  Test.case "command output emits idle callbacks" test_command_output_emits_idle_callbacks;
  Test.case "command output times out long-running processes" test_command_output_times_out;
  Test.case
    "command output limits captured stream sizes"
    test_command_output_limits_captured_streams;
  Test.case
    "command output returns after child exit even when another process inherited stdout"
    test_command_output_returns_when_child_exits_with_inherited_writer;
  Test.case
    ~size:Large
    "command output handles parallel shell commands"
    test_command_output_handles_parallel_shell_commands;
  Test.case
    ~size:Large
    "command output handles parallel fast exit commands"
    test_command_output_handles_parallel_fast_exit_commands;
]

let capture_main = fun () ->
  eprint stderr_payload;
  print stdout_payload;
  Ok ()

let capture_stdout_lines_main = fun () ->
  print "first line\n";
  print "second line\n";
  print "third line\n";
  Ok ()

let meta_main = fun ~args ->
  let normalize_args = fun __tmp1 ->
    match __tmp1 with
    | [] -> [ "std_command_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  Test.Cli.main ~name:"std_command_tests" ~tests:meta_tests ~args:(normalize_args args) ()

let main ~args =
  match args with
  | _ :: "capture-both-streams" :: _ -> capture_main ()
  | _ :: "capture-stdout-lines" :: _ -> capture_stdout_lines_main ()
  | _ -> meta_main ~args

let () = Runtime.run ~main ~args:Env.args ()
