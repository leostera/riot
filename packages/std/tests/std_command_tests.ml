open Std
module Test = Std.Test

let stderr_payload_size = 256 * 1_024

let stderr_payload = String.make stderr_payload_size 'e'

let stdout_payload = "stdout-finished"

let self_executable = fun () ->
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_command_tests"

let run_capture = fun () ->
  let cmd = Command.make (self_executable ()) ~args:[ "capture-both-streams" ] in
  Command.output cmd |> Result.expect ~msg:"failed to run capture helper"

let run_fd_check = fun fd_nums ->
  let check_script = "for fd in \"$@\"; do if [ -e \"/dev/fd/$fd\" ]; then echo \"$fd\"; exit 1; fi; done" in
  let args = "-c" :: check_script :: "check-fd-closed-on-exec" :: List.map Int.to_string fd_nums in
  let cmd = Command.make "/bin/sh" ~args in
  Command.output cmd |> Result.expect ~msg:"failed to run fd inheritance helper"

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

let test_command_spawn_does_not_inherit_pipe_fds = fun _ctx ->
  let pipe = Kernel.Fd.pipe () in
  let close_pipe () =
    Kernel.Fd.close pipe.read_fd;
    Kernel.Fd.close pipe.write_fd
  in
  let result =
    try
      let output = run_fd_check [ Kernel.Fd.to_int pipe.read_fd; Kernel.Fd.to_int pipe.write_fd ] in
      if Int.equal output.status 0 then
        Ok ()
      else
        Error ("expected helper to observe closed pipe fds, got "
        ^ Int.to_string output.status
        ^ ": "
        ^ output.stderr)
    with
    | exn ->
        close_pipe ();
        raise exn
  in
  close_pipe ();
  result

let meta_tests = [
  Test.case "command output drains stdout and stderr without deadlock" test_command_output_drains_stdout_and_stderr;
  Test.case "command spawn does not inherit unrelated pipe fds" test_command_spawn_does_not_inherit_pipe_fds;
]

let capture_main = fun () ->
  ignore (Unix.alarm 1);
  eprint stderr_payload;
  print stdout_payload;
  ignore (Unix.alarm 0);
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
  | _ -> meta_main ~args

let () = Miniriot.run ~main ~args:Env.args ()
