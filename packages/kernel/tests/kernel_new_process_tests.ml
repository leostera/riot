open Std
module Test = Std.Test
module Kernel = Kernel

let ( let* ) value fn = Result.and_then value ~fn

let lift_process result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error)

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Async.error_to_string error)

let lift_file result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)

let is_would_block error =
  match error with
  | Kernel.Fs.File.System system_error -> Kernel.SystemError.would_block system_error
  | _ -> false

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix (fun tempdir -> fn (Kernel.Path.from_string (Path.to_string tempdir))) with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let with_process = fun process fn ->
  protect
    ~finally:(fun () ->
      let _ = Kernel.Process.close process in
      ())
    (fun () -> fn process)

let rec close_processes = fun processes ->
  match processes with
  | [] -> ()
  | process :: rest ->
      let _ = Kernel.Process.close process in
      close_processes rest

let with_file = fun file fn ->
  protect
    ~finally:(fun () ->
      let _ = Kernel.Fs.File.close file in
      ())
    fn

let with_poll = fun fn ->
  let* poll = lift_async (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

let wait_for = fun poll ~token ~interest ~source ~pred ->
  let* () = lift_async (Kernel.Async.Poll.register poll token interest source) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
      let found =
        List.any events ~fn:(fun event ->
          Kernel.Async.Token.equal token (Kernel.Async.Event.token event) && pred event)
      in
      if found then
        Ok ()
      else
        Error "expected readiness event")

let wait_readable = fun poll ~token source ->
  wait_for poll ~token ~interest:Kernel.Async.Interest.readable ~source ~pred:Kernel.Async.Event.is_readable

let wait_writable = fun poll ~token source ->
  wait_for poll ~token ~interest:Kernel.Async.Interest.writable ~source ~pred:Kernel.Async.Event.is_writable

let read_once = fun poll ~token file ->
  let buffer = Kernel.Bytes.create ~size:128 in
  let rec loop () =
    match Kernel.Fs.File.read file buffer with
    | Kernel.Result.Ok count -> Ok (Kernel.Bytes.sub_string buffer ~offset:0 ~len:count)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () = wait_readable poll ~token (Kernel.Fs.File.to_source file) in
          loop ()
        else
          Error (Kernel.Fs.File.error_to_string error)
  in
  loop ()

let read_all = fun poll ~token file ->
  let buffer = Kernel.Bytes.create ~size:128 in
  let rec loop parts =
    match Kernel.Fs.File.read file buffer with
    | Kernel.Result.Ok 0 -> Ok (Kernel.String.concat "" (List.reverse parts))
    | Kernel.Result.Ok count ->
        loop (Kernel.Bytes.sub_string buffer ~offset:0 ~len:count :: parts)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () = wait_readable poll ~token (Kernel.Fs.File.to_source file) in
          loop parts
        else
          Error (Kernel.Fs.File.error_to_string error)
  in
  loop []

let write_all = fun poll ~token file buffer ->
  let rec loop pos len =
    if len = 0 then
      Ok ()
    else
      match Kernel.Fs.File.write file ~pos ~len buffer with
      | Kernel.Result.Ok written ->
          if written <= 0 then
            Error "expected process pipe write to make progress"
          else
            loop (pos + written) (len - written)
      | Kernel.Result.Error error ->
          if is_would_block error then
            let* () = wait_writable poll ~token (Kernel.Fs.File.to_source file) in
            loop pos len
          else
            Error (Kernel.Fs.File.error_to_string error)
  in
  loop 0 (Kernel.Bytes.length buffer)

let wait_for_exit = fun poll ~token process ->
  let exit_poll_timeout = 1_000_000L in
  let rec loop () =
    let* status = lift_process (Kernel.Process.try_wait process) in
    match status with
    | Some status -> Ok status
    | None ->
        let source = Kernel.Process.to_source process in
        let* () = lift_async
          (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
        protect
          ~finally:(fun () ->
            let _ = Kernel.Async.Poll.deregister poll source in
            ())
          (fun () ->
            let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:exit_poll_timeout poll) in
            loop ())
  in
  loop ()

let wait_for_priority_token = fun poll ~token ->
  let rec loop attempts =
    if attempts = 0 then
      Error "expected process source to report priority readiness"
    else
      let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll) in
      if
        List.any events ~fn:(fun event ->
          Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
          && Kernel.Async.Event.is_priority event)
      then
        Ok ()
      else
        loop (attempts - 1)
  in
  loop 8

let send_signal_command = fun ~signal process ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* helper = lift_process
    (Kernel.Process.spawn
      ~program:"/bin/kill"
      ~args:[|signal; Kernel.Int.to_string (Kernel.Process.pid process)|]
      ~stdio
      ()) in
  with_process helper
    (fun helper ->
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make ("kill-helper", signal)) helper in
          if status = Kernel.Process.Exited 0 then
            Ok ()
          else
            Error "expected /bin/kill to deliver the requested signal cleanly"))

let test_current_pid_is_positive = fun _ctx ->
  if Kernel.Process.current_pid () > 0 then
    Ok ()
  else
    Error "expected current_pid to be positive"

let test_stdout_pipe_roundtrips = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/echo" ~args:[|"-n"; "hello"|] ~stdio ()) in
  with_process process
    (fun process ->
      match Kernel.Process.stdout process with
      | None -> Error "expected stdout pipe"
      | Some stdout ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 520) process in
              let* payload = read_all poll ~token:(Kernel.Async.Token.make 501) stdout in
              if payload = "hello" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected echo stdout payload and zero exit status"))

let test_stdin_and_stdout_pipes_roundtrip = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
  with_process process
    (fun process ->
      match (Kernel.Process.stdin process, Kernel.Process.stdout process) with
      | (Some stdin, Some stdout) ->
          with_poll
            (fun poll ->
              let payload = Kernel.Bytes.from_string "ping" in
              let* () = write_all poll ~token:(Kernel.Async.Token.make 502) stdin payload in
              let* () = lift_file (Kernel.Fs.File.close stdin) in
              let* echoed = read_once poll ~token:(Kernel.Async.Token.make 503) stdout in
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 521) process in
              if echoed = "ping" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected cat to echo stdin and exit cleanly")
      | _ -> Error "expected stdin and stdout pipes")

let test_stderr_redirect_to_stdout_merges_streams = fun _ctx ->
  let stdio =
    Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.RedirectToStdout } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "printf out; printf err >&2"|] ~stdio ()) in
  with_process process
    (fun process ->
      match Kernel.Process.stdout process with
      | None -> Error "expected stdout pipe"
      | Some stdout ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 522) process in
              let* payload = read_all poll ~token:(Kernel.Async.Token.make 504) stdout in
              if payload = "outerr" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected redirected stderr to be merged into stdout"))

let test_stdout_and_stderr_pipes_remain_drainable_after_exit = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Pipe } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "printf out; printf err >&2"|] ~stdio ()) in
  with_process process
    (fun process ->
      match (Kernel.Process.stdout process, Kernel.Process.stderr process) with
      | (Some stdout, Some stderr) ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 536) process in
              let* stdout_payload = read_all poll ~token:(Kernel.Async.Token.make 509) stdout in
              let* stderr_payload = read_all poll ~token:(Kernel.Async.Token.make 510) stderr in
              if
                status = Kernel.Process.Exited 0 && stdout_payload = "out" && stderr_payload = "err"
              then
                Ok ()
              else
                Error "expected stdout and stderr pipes to stay drainable after process exit")
      | _ -> Error "expected stdout and stderr pipes")

let test_try_wait_reports_running_then_exit = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.05"|] ~stdio ()) in
  with_process process
    (fun process ->
      let* status = lift_process (Kernel.Process.try_wait process) in
      match status with
      | Some _ -> Error "expected process to still be running on immediate try_wait"
      | None ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 523) process in
              if status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected process to exit cleanly after async poll"))

let test_process_source_reports_exit_ready = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.01"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 531) process in
          if status = Kernel.Process.Exited 0 then
            Ok ()
          else
            Error "expected async process source to report a clean exit"))

let test_process_source_reregister_updates_token = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let token_a = Kernel.Async.Token.make "first-process-token" in
          let token_b = Kernel.Async.Token.make "second-process-token" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token_a Kernel.Async.Interest.priority source) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Async.Poll.deregister poll source in
              ())
            (fun () ->
              let* () = lift_async
                (Kernel.Async.Poll.reregister poll token_b Kernel.Async.Interest.priority source) in
              let* () = wait_for_priority_token poll ~token:token_b in
              match Kernel.Process.try_wait process with
              | Kernel.Result.Ok (Some (Kernel.Process.Exited 0)) -> Ok ()
              | Kernel.Result.Ok _ -> Error "expected reregistered process source to preserve exit readiness"
              | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error))))

let test_process_close_preserves_registered_exit_source = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let token = Kernel.Async.Token.make "closed-process-source" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Async.Poll.deregister poll source in
              ())
            (fun () ->
              let* () = lift_process (Kernel.Process.close process) in
              let* () = wait_for_priority_token poll ~token in
              match Kernel.Process.try_wait process with
              | Kernel.Result.Ok (Some (Kernel.Process.Exited 0)) -> Ok ()
              | Kernel.Result.Ok _ -> Error "expected closed process handles to leave exit readiness intact"
              | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error))))

let test_process_source_deregister_before_close_is_harmless = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let* () = lift_async
            (Kernel.Async.Poll.register
              poll
              (Kernel.Async.Token.make "process-deregister-then-close")
              Kernel.Async.Interest.priority
              source) in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let* () = lift_process (Kernel.Process.close process) in
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 533) process in
          if status = Kernel.Process.Exited 0 then
            Ok ()
          else
            Error "expected deregister-before-close process ownership to preserve exit observation"))

let test_try_wait_is_stable_after_priority_readiness = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.02"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let token = Kernel.Async.Token.make "process-priority-then-try-wait" in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Async.Poll.deregister poll source in
              ())
            (fun () ->
              let* () = wait_for_priority_token poll ~token in
              let* first = lift_process (Kernel.Process.try_wait process) in
              let* second = lift_process (Kernel.Process.try_wait process) in
              match (first, second) with
              | (Some (Kernel.Process.Exited 0), Some (Kernel.Process.Exited 0)) -> Ok ()
              | _ -> Error "expected try_wait to stay stable after process priority readiness")))

let test_process_registration_after_exit_reports_readiness = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; ":"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          (* Reap first: kqueue can no longer attach EVFILT_PROC. A registration
             that reports success must still deliver a retry hint, not park forever. *)
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 540) process in
          if status != Kernel.Process.Exited 0 then
            Error "expected process to exit before late registration"
          else
            let token = Kernel.Async.Token.make "late-process-registration" in
            let source = Kernel.Process.to_source process in
            let* () = lift_async
              (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
            protect
              ~finally:(fun () ->
                let _ = Kernel.Async.Poll.deregister poll source in
                ())
              (fun () ->
                let* events = lift_async (Kernel.Async.Poll.poll ~timeout:10_000_000L poll) in
                if List.any events ~fn:(fun event ->
                    Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
                    && Kernel.Async.Event.is_priority event) then
                  Ok ()
                else
                  Error "successful late process registration lost readiness")))

let expect_priority_events = fun ~tokens events ->
  if List.length events != List.length tokens then
    Error "unexpected pending process event count"
  else if List.all tokens ~fn:(fun token ->
      List.any events ~fn:(fun event ->
        Kernel.Async.Event.is_priority event
        && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))) then
    Ok ()
  else
    Error "pending process event used a stale or unexpected token"

let with_exited_process = fun poll fn ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; ":"|] ~stdio ()) in
  with_process process
    (fun process ->
      let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make "reap-before-pending") process in
      if status = Kernel.Process.Exited 0 then
        fn (Kernel.Process.to_source process)
      else
        Error "expected process to exit before testing pending readiness")

let test_pending_process_readiness_lifecycle = fun _ctx ->
  with_poll
    (fun poll ->
      with_exited_process poll
        (fun source ->
          let original = Kernel.Async.Token.make "pending-original" in
          let replacement = Kernel.Async.Token.make "pending-replacement" in
          let register token = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
          let select () = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
          let* () = register original in
          let* () = lift_async
            (Kernel.Async.Poll.reregister poll replacement Kernel.Async.Interest.priority source) in
          let* events = select () in
          let* () = expect_priority_events ~tokens:[replacement] events in
          let* events = select () in
          let* () = expect_priority_events ~tokens:[] events in
          let* () = register original in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let* events = select () in
          let* () = expect_priority_events ~tokens:[] events in
          let* () = register original in
          let* () = register replacement in
          let* events = select () in
          let* () = expect_priority_events ~tokens:[replacement] events in
          let* events = select () in
          expect_priority_events ~tokens:[] events))

let test_pending_process_readiness_respects_poll_limit = fun _ctx ->
  with_poll
    (fun poll ->
      with_exited_process poll
        (fun first ->
          with_exited_process poll
            (fun second ->
              let first_token = Kernel.Async.Token.make "pending-first" in
              let second_token = Kernel.Async.Token.make "pending-second" in
              let* () = lift_async
                (Kernel.Async.Poll.register poll first_token Kernel.Async.Interest.priority first) in
              let* () = lift_async
                (Kernel.Async.Poll.register poll second_token Kernel.Async.Interest.priority second) in
              let* () =
                match Kernel.Async.Poll.poll ~max_events:0 ~timeout:0L poll with
                | Kernel.Result.Error _ -> Ok ()
                | Kernel.Result.Ok _ -> Error "expected zero event limit to be rejected"
              in
              let* first_events = lift_async
                (Kernel.Async.Poll.poll ~max_events:1 ~timeout:0L poll) in
              let* second_events = lift_async
                (Kernel.Async.Poll.poll ~max_events:1 ~timeout:0L poll) in
              if List.length first_events != 1 || List.length second_events != 1 then
                Error "pending process hints ignored the poll limit or lost its remainder"
              else
                let* () = expect_priority_events ~tokens:[first_token; second_token]
                  (first_events @ second_events) in
                let* events = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
                expect_priority_events ~tokens:[] events)))

let test_kill_reports_signaled_status = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 5"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* () = lift_process (Kernel.Process.kill process ~signal:9) in
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 524) process in
          if status = Kernel.Process.Signaled 9 then
            Ok ()
          else
            Error "expected killed process to report a signaled status"))

let test_sigterm_reports_signaled_status = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 5"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* () = lift_process (Kernel.Process.kill process ~signal:15) in
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 525) process in
          if status = Kernel.Process.Signaled 15 then
            Ok ()
          else
            Error "expected sigterm to report the delivered signal number"))

let test_kill_then_close_preserves_signaled_status = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 5"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* () = lift_process (Kernel.Process.kill process ~signal:9) in
          let* () = lift_process (Kernel.Process.close process) in
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 537) process in
          if status = Kernel.Process.Signaled 9 then
            Ok ()
          else
            Error "expected kill then close to preserve signaled exit observation"))

let test_close_then_kill_preserves_signaled_status = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 5"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* () = lift_process (Kernel.Process.close process) in
          let* () = lift_process (Kernel.Process.kill process ~signal:15) in
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 538) process in
          if status = Kernel.Process.Signaled 15 then
            Ok ()
          else
            Error "expected close then kill to preserve signaled exit observation"))

let test_non_zero_exit_status_roundtrips = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "exit 7"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 526) process in
          if status = Kernel.Process.Exited 7 then
            Ok ()
          else
            Error "expected process exit status to preserve a non-zero code"))

let test_spawn_applies_custom_environment = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn
      ~program:"/bin/sh"
      ~args:[|"-c"; "printf %s \"$KERNEL_NEW_PROCESS_TEST\""|]
      ~env:[|("KERNEL_NEW_PROCESS_TEST", "env-ok")|]
      ~stdio
      ()) in
  with_process process
    (fun process ->
      match Kernel.Process.stdout process with
      | None -> Error "expected stdout pipe"
      | Some stdout ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 527) process in
              let* payload = read_all poll ~token:(Kernel.Async.Token.make 505) stdout in
              if payload = "env-ok" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected spawned process to see custom environment"))

let test_spawn_applies_current_dir = fun _ctx ->
  with_tempdir "kernel_new_process"
    (fun tempdir ->
      let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
      let* process = lift_process
        (Kernel.Process.spawn ~program:"/bin/pwd" ~args:[||] ~current_dir:tempdir ~stdio ()) in
      with_process process
        (fun process ->
          match Kernel.Process.stdout process with
          | None -> Error "expected stdout pipe"
          | Some stdout ->
              with_poll
                (fun poll ->
                  let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 528) process in
                  let* payload = read_all poll ~token:(Kernel.Async.Token.make 506) stdout in
                  let output =
                    if
                      String.length payload != 0
                      && String.get_unchecked payload ~at:(String.length payload - 1) = '\n'
                    then
                      String.sub payload ~offset:0 ~len:(String.length payload - 1)
                    else
                      payload
                  in
                  let* expected = lift_file (Kernel.Fs.File.canonicalize tempdir) in
                  let* actual = lift_file
                    (Kernel.Fs.File.canonicalize (Kernel.Path.from_string output)) in
                  if
                    Kernel.Path.to_string actual = Kernel.Path.to_string expected
                    && status = Kernel.Process.Exited 0
                  then
                    Ok ()
                  else
                    Error "expected spawned process to run in the configured current_dir")))

let test_file_backed_stdio_roundtrips = fun _ctx ->
  with_tempdir "kernel_new_process"
    (fun tempdir ->
      let input_path = Kernel.Path.(tempdir / "stdin.txt") in
      let output_path = Kernel.Path.(tempdir / "stdout.txt") in
      let* input_file = lift_file (Kernel.Fs.File.open_write input_path) in
      let* () =
        with_file input_file
          (fun () ->
            let* _ = lift_file
              (Kernel.Fs.File.write input_file (Kernel.Bytes.from_string "file-stdio")) in
            Ok ())
      in
      let* stdin_file = lift_file (Kernel.Fs.File.open_read input_path) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close stdin_file in
          ())
        (fun () ->
          let* stdout_file = lift_file
            (Kernel.Fs.File.open_write ~create:true ~truncate:true output_path) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Fs.File.close stdout_file in
              ())
            (fun () ->
              let stdio =
                Kernel.Process.{
                  stdin = Stdin.File stdin_file;
                  stdout = Stdout.File stdout_file;
                  stderr = Stderr.Null
                } in
              let* process = lift_process
                (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
              let* status =
                with_process
                  process
                  (fun process ->
                    with_poll
                      (fun poll -> wait_for_exit poll ~token:(Kernel.Async.Token.make 529) process))
              in
              let* output = lift_file (Kernel.Fs.File.open_read output_path) in
              let buffer = Kernel.Bytes.create ~size:32 in
              let* payload =
                with_file output
                  (fun () ->
                    let* count = lift_file (Kernel.Fs.File.read output buffer) in
                    Ok (Kernel.Bytes.sub_string buffer ~offset:0 ~len:count))
              in
              if status = Kernel.Process.Exited 0 && payload = "file-stdio" then
                Ok ()
              else
                Error "expected file-backed stdio to roundtrip through the child process")))

let test_file_backed_stdio_uses_no_kernel_pipes = fun _ctx ->
  with_tempdir "kernel_new_process"
    (fun tempdir ->
      let input_path = Kernel.Path.(tempdir / "stdin.txt") in
      let output_path = Kernel.Path.(tempdir / "stdout.txt") in
      let* input_file = lift_file (Kernel.Fs.File.open_write input_path) in
      let* () =
        with_file input_file
          (fun () ->
            let* _ = lift_file
              (Kernel.Fs.File.write input_file (Kernel.Bytes.from_string "file-stdio")) in
            Ok ())
      in
      let* stdin_file = lift_file (Kernel.Fs.File.open_read input_path) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close stdin_file in
          ())
        (fun () ->
          let* stdout_file = lift_file
            (Kernel.Fs.File.open_write ~create:true ~truncate:true output_path) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Fs.File.close stdout_file in
              ())
            (fun () ->
              let stdio =
                Kernel.Process.{
                  stdin = Stdin.File stdin_file;
                  stdout = Stdout.File stdout_file;
                  stderr = Stderr.Null
                } in
              let* process = lift_process
                (Kernel.Process.spawn ~program:"/usr/bin/true" ~args:[||] ~stdio ()) in
              with_process process
                (fun process ->
                  let* status =
                    with_poll
                      (fun poll -> wait_for_exit poll ~token:(Kernel.Async.Token.make 532) process)
                  in
                  if
                    status = Kernel.Process.Exited 0
                    && Kernel.Process.stdin process = None
                    && Kernel.Process.stdout process = None
                    && Kernel.Process.stderr process = None
                  then
                    Ok ()
                  else
                    Error "expected file-backed stdio to avoid creating kernel pipe handles"))))

let test_inherited_stdio_uses_no_kernel_pipes = fun _ctx ->
  let stdio =
    Kernel.Process.{ stdin = Stdin.Inherit; stdout = Stdout.Inherit; stderr = Stderr.Inherit } in
  let* process = lift_process (Kernel.Process.spawn ~program:"/usr/bin/true" ~args:[||] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 530) process in
          if
            status = Kernel.Process.Exited 0
            && Kernel.Process.stdin process = None
            && Kernel.Process.stdout process = None
            && Kernel.Process.stderr process = None
          then
            Ok ()
          else
            Error "expected inherited stdio to avoid creating kernel pipes"))

let test_requested_pipe_ownership_matches_stdio = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Null; stderr = Stderr.Pipe } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "cat >&2"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          match (
            Kernel.Process.stdin process,
            Kernel.Process.stdout process,
            Kernel.Process.stderr process
          ) with
          | (Some stdin, None, Some stderr) ->
              let payload = Kernel.Bytes.from_string "pipes" in
              let* () = write_all poll ~token:(Kernel.Async.Token.make 507) stdin payload in
              let* () = lift_file (Kernel.Fs.File.close stdin) in
              let* echoed = read_once poll ~token:(Kernel.Async.Token.make 508) stderr in
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 533) process in
              if echoed = "pipes" && status = Kernel.Process.Exited 0 then
                Ok ()
              else
                Error "expected pipe-backed stdio ownership to match the requested channels"
          | _ -> Error "expected only stdin and stderr kernel pipes to be present"))

let test_spawn_missing_program_reports_no_such_file = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  match Kernel.Process.spawn ~program:"/definitely/missing/kernel-new-process" ~args:[||] ~stdio () with
  | Kernel.Result.Error (Kernel.Process.System Kernel.SystemError.NoSuchFileOrDirectory) ->
      Ok ()
  | Kernel.Result.Error error ->
      Error (Kernel.Process.error_to_string error)
  | Kernel.Result.Ok process ->
      let _ = Kernel.Process.close process in
      Error "expected missing program spawn to fail"

let test_process_close_tolerates_preclosed_pipe_handles = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
  match (Kernel.Process.stdin process, Kernel.Process.stdout process) with
  | (Some stdin, Some stdout) ->
      let* () = lift_file (Kernel.Fs.File.close stdin) in
      let* () = lift_file (Kernel.Fs.File.close stdout) in
      let* () = lift_process (Kernel.Process.close process) in
      Ok ()
  | _ ->
      let _ = Kernel.Process.close process in
      Error "expected process pipes to exist"

let test_try_wait_is_stable_after_exit = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "exit 0"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* first = wait_for_exit poll ~token:(Kernel.Async.Token.make 534) process in
          let* second = lift_process (Kernel.Process.try_wait process) in
          match second with
          | Some status when first = Kernel.Process.Exited 0 && status = Kernel.Process.Exited 0 -> Ok ()
          | Some _ -> Error "expected try_wait to remain stable after the process exits"
          | None -> Error "expected exited process to stay observable through try_wait"))

let test_kill_after_exit_reports_no_such_process = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "exit 0"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* _ = wait_for_exit poll ~token:(Kernel.Async.Token.make 535) process in
          match Kernel.Process.kill process ~signal:9 with
          | Kernel.Result.Error (Kernel.Process.System Kernel.SystemError.NoSuchProcess) -> Ok ()
          | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error)
          | Kernel.Result.Ok () -> Error "expected signaling an exited process to report no_such_process"))

let test_process_close_is_idempotent = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
  let* () = lift_process (Kernel.Process.close process) in
  let* () = lift_process (Kernel.Process.close process) in
  Ok ()

let test_repeated_spawn_and_poll_exit_stays_healthy = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  with_poll
    (fun poll ->
      let rec loop remaining =
        if remaining = 0 then
          Ok ()
        else
          let* process = lift_process
            (Kernel.Process.spawn ~program:"/usr/bin/true" ~args:[||] ~stdio ()) in
          let outcome =
            with_process process
              (fun process ->
                let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make (600 + remaining)) process in
                if status = Kernel.Process.Exited 0 then
                  Ok ()
                else
                  Error "expected repeated spawned process to exit cleanly")
          in
          let* () = outcome in
          loop (remaining - 1)
      in
      loop 32)

let test_many_process_sources_report_burst_exit_readiness = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Null; stderr = Stderr.Null } in
  with_poll
    (fun poll ->
      let rec spawn_many remaining acc =
        if remaining = 0 then
          Ok (List.reverse acc)
        else
          let* process = lift_process (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
          spawn_many (remaining - 1) (process :: acc)
      in
      let* processes = spawn_many 12 [] in
      let sources = List.map processes ~fn:Kernel.Process.to_source in
      let rec deregister_all = function
        | [] -> ()
        | source :: rest ->
            let _ = Kernel.Async.Poll.deregister poll source in
            deregister_all rest
      in
      protect
        ~finally:(fun () ->
          deregister_all sources;
          close_processes processes)
        (fun () ->
          let rec trigger_exit_burst = function
            | [] -> Ok ()
            | process :: rest -> (
                match Kernel.Process.stdin process with
                | None -> Error "expected burst-exit process to own a stdin pipe"
                | Some stdin ->
                    let* () = lift_file (Kernel.Fs.File.close stdin) in
                    trigger_exit_burst rest
              )
          in
          let rec register index = function
            | [] -> Ok ()
            | process :: rest ->
                let* () = lift_async
                  (Kernel.Async.Poll.register
                    poll
                    (Kernel.Async.Token.make index)
                    Kernel.Async.Interest.priority
                    (Kernel.Process.to_source process)) in
                register (index + 1) rest
          in
          let seen = Kernel.Array.make ~count:12 ~value:false in
          let rec mark = function
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_priority event then
                  let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 12 then
                    Kernel.Array.set seen ~at:token ~value:true;
                  mark rest
          in
          let rec all_seen index =
            if index = 12 then
              true
            else if Kernel.Array.get_unchecked seen ~at:index then
              all_seen (index + 1)
            else
              false
          in
          let rec mark_observed index = function
            | [] -> Ok ()
            | process :: rest ->
                if Kernel.Array.get_unchecked seen ~at:index then
                  mark_observed (index + 1) rest
                else
                  let* status = lift_process (Kernel.Process.try_wait process) in
                  match status with
                  | Some (Kernel.Process.Exited 0) ->
                      Kernel.Array.set seen ~at:index ~value:true;
                      mark_observed (index + 1) rest
                  | Some _ ->
                      Error "expected burst-exit process sources to preserve a clean exit status"
                  | None ->
                      mark_observed (index + 1) rest
          in
          let rec poll_until attempts =
            let* () = mark_observed 0 processes in
            if all_seen 0 then
              Ok ()
            else if attempts = 0 then
              Error "expected many process sources to report exit readiness after a burst exit"
            else
              let* events = lift_async
                (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
              mark events;
              poll_until (attempts - 1)
          in
          let rec verify = function
            | [] -> Ok ()
            | process :: rest ->
                let* first = lift_process (Kernel.Process.try_wait process) in
                let* second = lift_process (Kernel.Process.try_wait process) in
                match (first, second) with
                | (Some (Kernel.Process.Exited 0), Some (Kernel.Process.Exited 0)) -> verify rest
                | _ -> Error "expected burst-exit processes to stay observable through repeated try_wait"
          in
          let* () = register 0 processes in
          let* () = trigger_exit_burst processes in
          let* () = poll_until 16 in
          verify processes))

let test_many_process_sources_report_burst_signals = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  with_poll
    (fun poll ->
      let rec spawn_many remaining acc =
        if remaining = 0 then
          Ok (List.reverse acc)
        else
          let* process = lift_process
            (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 5"|] ~stdio ()) in
          spawn_many (remaining - 1) (process :: acc)
      in
      let* processes = spawn_many 12 [] in
      let sources = List.map processes ~fn:Kernel.Process.to_source in
      let rec deregister_all = function
        | [] -> ()
        | source :: rest ->
            let _ = Kernel.Async.Poll.deregister poll source in
            deregister_all rest
      in
      protect
        ~finally:(fun () ->
          deregister_all sources;
          close_processes processes)
        (fun () ->
          let rec signal_all = function
            | [] -> Ok ()
            | process :: rest ->
                let* () = lift_process (Kernel.Process.kill process ~signal:15) in
                signal_all rest
          in
          let rec register index = function
            | [] -> Ok ()
            | process :: rest ->
                let* () = lift_async
                  (Kernel.Async.Poll.register
                    poll
                    (Kernel.Async.Token.make index)
                    Kernel.Async.Interest.priority
                    (Kernel.Process.to_source process)) in
                register (index + 1) rest
          in
          let seen = Kernel.Array.make ~count:12 ~value:false in
          let rec mark = function
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_priority event then
                  let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 12 then
                    Kernel.Array.set seen ~at:token ~value:true;
                  mark rest
          in
          let rec all_seen index =
            if index = 12 then
              true
            else if Kernel.Array.get_unchecked seen ~at:index then
              all_seen (index + 1)
            else
              false
          in
          let rec mark_observed index = function
            | [] -> Ok ()
            | process :: rest ->
                if Kernel.Array.get_unchecked seen ~at:index then
                  mark_observed (index + 1) rest
                else
                  let* status = lift_process (Kernel.Process.try_wait process) in
                  match status with
                  | Some (Kernel.Process.Signaled 15) ->
                      Kernel.Array.set seen ~at:index ~value:true;
                      mark_observed (index + 1) rest
                  | Some _ ->
                      Error "expected burst-signaled process sources to preserve a signaled status"
                  | None ->
                      mark_observed (index + 1) rest
          in
          let rec poll_until attempts =
            let* () = mark_observed 0 processes in
            if all_seen 0 then
              Ok ()
            else if attempts = 0 then
              Error "expected many process sources to report signaled readiness after a kill burst"
            else
              let* events = lift_async
                (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
              mark events;
              poll_until (attempts - 1)
          in
          let rec verify = function
            | [] -> Ok ()
            | process :: rest ->
                let* first = lift_process (Kernel.Process.try_wait process) in
                let* second = lift_process (Kernel.Process.try_wait process) in
                match (first, second) with
                | (Some (Kernel.Process.Signaled 15), Some (Kernel.Process.Signaled 15)) -> verify rest
                | _ -> Error "expected burst-signaled processes to stay observable through repeated try_wait"
          in
          let* () = register 0 processes in
          let* () = signal_all processes in
          let* () = poll_until 16 in
          verify processes))

let test_default_stdio_is_all_inherit = fun _ctx ->
  match Kernel.Process.default_stdio with
  | Kernel.Process.{ stdin=Stdin.Inherit; stdout=Stdout.Inherit; stderr=Stderr.Inherit } -> Ok ()
  | _ -> Error "expected Process.default_stdio to inherit all three stdio streams"

let test_process_pid_is_positive_and_stable_before_and_after_exit = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "exit 0"|] ~stdio ()) in
  with_process process
    (fun process ->
      let before = Kernel.Process.pid process in
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 910) process in
          let after = Kernel.Process.pid process in
          if before > 0 && before = after && status = Kernel.Process.Exited 0 then
            Ok ()
          else
            Error "expected Process.pid to be positive and stable across exit observation"))

let test_all_null_stdio_creates_no_owned_pipes = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process (Kernel.Process.spawn ~program:"/usr/bin/true" ~args:[||] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 911) process in
          if
            status = Kernel.Process.Exited 0
            && Kernel.Process.stdin process = None
            && Kernel.Process.stdout process = None
            && Kernel.Process.stderr process = None
          then
            Ok ()
          else
            Error "expected all-null stdio to create no owned kernel pipes"))

let test_stdin_null_delivers_immediate_eof_to_the_child = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn
      ~program:"/bin/sh"
      ~args:[|"-c"; "if IFS= read -r line; then exit 1; else exit 0; fi"|]
      ~stdio
      ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 912) process in
          if status = Kernel.Process.Exited 0 then
            Ok ()
          else
            Error "expected Stdin.Null to present immediate eof to the child"))

let test_stdout_null_discards_stdout_without_creating_a_pipe = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Pipe } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "printf out; printf err >&2"|] ~stdio ()) in
  with_process process
    (fun process ->
      match (Kernel.Process.stdout process, Kernel.Process.stderr process) with
      | (None, Some stderr) ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 913) process in
              let* payload = read_all poll ~token:(Kernel.Async.Token.make 914) stderr in
              if status = Kernel.Process.Exited 0 && payload = "err" then
                Ok ()
              else
                Error "expected Stdout.Null to discard stdout while leaving stderr capture intact")
      | _ -> Error "expected Stdout.Null to avoid creating a stdout pipe")

let test_stderr_null_discards_stderr_without_creating_a_pipe = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Pipe; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "printf out; printf err >&2"|] ~stdio ()) in
  with_process process
    (fun process ->
      match (Kernel.Process.stdout process, Kernel.Process.stderr process) with
      | (Some stdout, None) ->
          with_poll
            (fun poll ->
              let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 915) process in
              let* payload = read_all poll ~token:(Kernel.Async.Token.make 916) stdout in
              if status = Kernel.Process.Exited 0 && payload = "out" then
                Ok ()
              else
                Error "expected Stderr.Null to discard stderr while leaving stdout capture intact")
      | _ -> Error "expected Stderr.Null to avoid creating a stderr pipe")

let test_spawn_with_empty_env_still_inherits_unrelated_parent_vars = fun _ctx ->
  let name = "RIOT_KERNEL_NEW_PROCESS_PARENT_VAR" in
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let _ = Kernel.Env.remove ~var:name in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Env.remove ~var:name in
      ())
    (fun () ->
      match Kernel.Env.set ~var:name ~value:"present" with
      | Kernel.Result.Error error -> Error (Kernel.Env.error_to_string error)
      | Kernel.Result.Ok () ->
          let* process = lift_process
            (Kernel.Process.spawn
              ~program:"/bin/sh"
              ~args:[|"-c"; "test \"$RIOT_KERNEL_NEW_PROCESS_PARENT_VAR\" = present"|]
              ~env:[||]
              ~stdio
              ()) in
          with_process process
            (fun process ->
              with_poll
                (fun poll ->
                  let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 917) process in
                  if status = Kernel.Process.Exited 0 then
                    Ok ()
                  else
                    Error "expected env:[||] to keep unrelated parent variables visible")))

let test_spawn_env_override_leaves_unrelated_parent_vars_intact = fun _ctx ->
  let preserved = "RIOT_KERNEL_NEW_PROCESS_KEEP" in
  let replaced = "RIOT_KERNEL_NEW_PROCESS_REPLACE" in
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let _ = Kernel.Env.remove ~var:preserved in
  let _ = Kernel.Env.remove ~var:replaced in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Env.remove ~var:preserved in
      let _ = Kernel.Env.remove ~var:replaced in
      ())
    (fun () ->
      match (
        Kernel.Env.set ~var:preserved ~value:"keep",
        Kernel.Env.set ~var:replaced ~value:"before"
      ) with
      | (Kernel.Result.Ok (), Kernel.Result.Ok ()) ->
          let* process = lift_process
            (Kernel.Process.spawn
              ~program:"/bin/sh"
              ~args:[|
                "-c";
                "test \"$RIOT_KERNEL_NEW_PROCESS_REPLACE\" = after && test \"$RIOT_KERNEL_NEW_PROCESS_KEEP\" = keep"
              |]
              ~env:[|(replaced, "after")|]
              ~stdio
              ()) in
          with_process process
            (fun process ->
              with_poll
                (fun poll ->
                  let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 918) process in
                  if status = Kernel.Process.Exited 0 then
                    Ok ()
                  else
                    Error "expected env overrides to leave unrelated parent vars intact"))
      | (Kernel.Result.Error error, _) ->
          Error (Kernel.Env.error_to_string error)
      | (_, Kernel.Result.Error error) ->
          Error (Kernel.Env.error_to_string error))

let test_file_backed_stdin_honors_the_current_file_offset = fun _ctx ->
  with_tempdir "kernel_new_process"
    (fun tempdir ->
      let input_path = Kernel.Path.(tempdir / "stdin.txt") in
      let* input_file = lift_file (Kernel.Fs.File.open_write input_path) in
      let* () =
        with_file input_file
          (fun () ->
            let* written = lift_file
              (Kernel.Fs.File.write input_file (Kernel.Bytes.from_string "012345")) in
            if written = 6 then
              Ok ()
            else
              Error "expected stdin offset fixture write to make progress")
      in
      let* stdin_file = lift_file (Kernel.Fs.File.open_read input_path) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close stdin_file in
          ())
        (fun () ->
          let scratch = Kernel.Bytes.create ~size:2 in
          let* consumed = lift_file (Kernel.Fs.File.read stdin_file scratch) in
          if consumed != 2 then
            Error "expected to advance the file-backed stdin handle before spawning"
          else
            let stdio =
              Kernel.Process.{
                stdin = Stdin.File stdin_file;
                stdout = Stdout.Pipe;
                stderr = Stderr.Null
              } in
            let* process = lift_process
              (Kernel.Process.spawn ~program:"/bin/cat" ~args:[||] ~stdio ()) in
            with_process process
              (fun process ->
                match Kernel.Process.stdout process with
                | None -> Error "expected stdout pipe for file-backed stdin offset test"
                | Some stdout ->
                    with_poll
                      (fun poll ->
                        let* status = wait_for_exit poll ~token:(Kernel.Async.Token.make 919) process in
                        let* payload = read_all poll ~token:(Kernel.Async.Token.make 920) stdout in
                        if status = Kernel.Process.Exited 0 && payload = "2345" then
                          Ok ()
                        else
                          Error "expected file-backed stdin to honor the current file offset"))))

let test_try_wait_observes_stopped_processes = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "while :; do sleep 1; done"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let token = Kernel.Async.Token.make 921 in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Async.Poll.deregister poll source in
              let _ = Kernel.Process.kill process ~signal:9 in
              ())
            (fun () ->
              let* () = send_signal_command ~signal:"-STOP" process in
              let rec wait_for_stop attempts =
                if attempts = 0 then
                  Error "expected try_wait to observe a stopped process"
                else
                  let* status = lift_process (Kernel.Process.try_wait process) in
                  match status with
                  | Some (Kernel.Process.Stopped _) ->
                      Ok ()
                  | Some _ ->
                      Error "expected a temporary stopped status before the final exit"
                  | None ->
                      let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
                      wait_for_stop (attempts - 1)
              in
              wait_for_stop 8)))

let test_stopped_then_continued_process_eventually_reports_final_exit = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "kill -STOP $$; exit 0"|] ~stdio ()) in
  with_process process
    (fun process ->
      with_poll
        (fun poll ->
          let source = Kernel.Process.to_source process in
          let token = Kernel.Async.Token.make 923 in
          let* () = lift_async
            (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source) in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Async.Poll.deregister poll source in
              let _ = Kernel.Process.kill process ~signal:9 in
              ())
            (fun () ->
              let rec wait_for_stop attempts =
                if attempts = 0 then
                  Error "expected the self-stopping process to report a Stopped status first"
                else
                  let* status = lift_process (Kernel.Process.try_wait process) in
                  match status with
                  | Some (Kernel.Process.Stopped _) ->
                      Ok ()
                  | Some _ ->
                      Error "expected a stopped status before continuing the process"
                  | None ->
                      let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
                      wait_for_stop (attempts - 1)
              in
              let* () = wait_for_stop 8 in
              let* () = send_signal_command ~signal:"-CONT" process in
              match wait_for_exit poll ~token:(Kernel.Async.Token.make 924) process with
              | Ok (Kernel.Process.Exited 0) -> Ok ()
              | Ok _ -> Error "expected SIGCONT to let the stopped child reach its final Exited 0 state"
              | Error error -> Error error)))

let test_kill_rejects_invalid_signal_numbers = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  let* process = lift_process
    (Kernel.Process.spawn ~program:"/bin/sh" ~args:[|"-c"; "sleep 0.05"|] ~stdio ()) in
  with_process process
    (fun process ->
      match Kernel.Process.kill process ~signal:(-1) with
      | Kernel.Result.Error (Kernel.Process.System Kernel.SystemError.InvalidArgument) -> Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Process.error_to_string error)
      | Kernel.Result.Ok () -> Error "expected Process.kill to reject an invalid signal number")

let test_spawn_missing_current_dir_reports_no_such_file = fun _ctx ->
  let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
  match Kernel.Process.spawn
    ~program:"/usr/bin/true"
    ~args:[||]
    ~current_dir:(Kernel.Path.from_string "/definitely/missing/kernel-new-process-dir")
    ~stdio
    () with
  | Kernel.Result.Error (Kernel.Process.System Kernel.SystemError.NoSuchFileOrDirectory) ->
      Ok ()
  | Kernel.Result.Error error ->
      Error (Kernel.Process.error_to_string error)
  | Kernel.Result.Ok process ->
      let _ = Kernel.Process.close process in
      Error "expected spawn with a missing current_dir to fail"

let test_spawn_regular_file_current_dir_reports_not_directory = fun _ctx ->
  with_tempdir "kernel_new_process"
    (fun tempdir ->
      let file_path = Kernel.Path.(tempdir / "plain.txt") in
      let* file = lift_file (Kernel.Fs.File.open_write file_path) in
      let* _ =
        with_file
          file
          (fun () -> lift_file (Kernel.Fs.File.write file (Kernel.Bytes.from_string "riot")))
      in
      let stdio = Kernel.Process.{ stdin = Stdin.Null; stdout = Stdout.Null; stderr = Stderr.Null } in
      match Kernel.Process.spawn ~program:"/usr/bin/true" ~args:[||] ~current_dir:file_path ~stdio () with
      | Kernel.Result.Error (Kernel.Process.System Kernel.SystemError.NotDirectory) ->
          Ok ()
      | Kernel.Result.Error error ->
          Error (Kernel.Process.error_to_string error)
      | Kernel.Result.Ok process ->
          let _ = Kernel.Process.close process in
          Error "expected spawn with a regular-file current_dir to report not_directory")

let tests = [
  Test.case "Process default_stdio is all Inherit" test_default_stdio_is_all_inherit;
  Test.case "Process current_pid is positive" test_current_pid_is_positive;
  Test.case "Process pid is positive and stable before and after exit" test_process_pid_is_positive_and_stable_before_and_after_exit;
  Test.case "Process all-null stdio creates no owned pipes" test_all_null_stdio_creates_no_owned_pipes;
  Test.case "Process Stdin.Null gives the child immediate eof" test_stdin_null_delivers_immediate_eof_to_the_child;
  Test.case "Process Stdout.Null discards stdout without creating a pipe" test_stdout_null_discards_stdout_without_creating_a_pipe;
  Test.case "Process Stderr.Null discards stderr without creating a pipe" test_stderr_null_discards_stderr_without_creating_a_pipe;
  Test.case "Process stdout pipe roundtrips" test_stdout_pipe_roundtrips;
  Test.case "Process stdin and stdout pipes roundtrip" test_stdin_and_stdout_pipes_roundtrip;
  Test.case "Process stderr redirect_to_stdout merges streams" test_stderr_redirect_to_stdout_merges_streams;
  Test.case "Process stdout and stderr pipes remain drainable after exit" test_stdout_and_stderr_pipes_remain_drainable_after_exit;
  Test.case "Process try_wait reports running then exit" test_try_wait_reports_running_then_exit;
  Test.case "Process source reports exit readiness" test_process_source_reports_exit_ready;
  Test.case "Process source reregister updates token" test_process_source_reregister_updates_token;
  Test.case "Process close preserves registered exit source" test_process_close_preserves_registered_exit_source;
  Test.case "Process source deregister before close is harmless" test_process_source_deregister_before_close_is_harmless;
  Test.case "Process try_wait is stable after priority readiness" test_try_wait_is_stable_after_priority_readiness;
  Test.case "Process registration after exit reports readiness" test_process_registration_after_exit_reports_readiness;
  Test.case "Process pending readiness token lifecycle" test_pending_process_readiness_lifecycle;
  Test.case "Process pending readiness respects poll limit" test_pending_process_readiness_respects_poll_limit;
  Test.case "Process kill reports signaled status" test_kill_reports_signaled_status;
  Test.case "Process sigterm reports signaled status" test_sigterm_reports_signaled_status;
  Test.case "Process kill then close preserves signaled status" test_kill_then_close_preserves_signaled_status;
  Test.case "Process close then kill preserves signaled status" test_close_then_kill_preserves_signaled_status;
  Test.case "Process preserves non-zero exit status" test_non_zero_exit_status_roundtrips;
  Test.case "Process spawn applies custom environment" test_spawn_applies_custom_environment;
  Test.case "Process spawn with env:[||] still inherits unrelated parent vars" test_spawn_with_empty_env_still_inherits_unrelated_parent_vars;
  Test.case "Process spawn env overrides leave unrelated parent vars intact" test_spawn_env_override_leaves_unrelated_parent_vars_intact;
  Test.case "Process spawn applies current_dir" test_spawn_applies_current_dir;
  Test.case "Process spawn with a missing current_dir reports no-such-file" test_spawn_missing_current_dir_reports_no_such_file;
  Test.case "Process spawn with a regular-file current_dir reports not_directory" test_spawn_regular_file_current_dir_reports_not_directory;
  Test.case "Process file-backed stdio roundtrips" test_file_backed_stdio_roundtrips;
  Test.case "Process file-backed stdin honors the current file offset" test_file_backed_stdin_honors_the_current_file_offset;
  Test.case "Process file-backed stdio uses no kernel pipes" test_file_backed_stdio_uses_no_kernel_pipes;
  Test.case "Process inherited stdio uses no kernel pipes" test_inherited_stdio_uses_no_kernel_pipes;
  Test.case "Process requested pipe ownership matches stdio" test_requested_pipe_ownership_matches_stdio;
  Test.case "Process spawn missing program reports no-such-file" test_spawn_missing_program_reports_no_such_file;
  Test.case "Process close tolerates preclosed pipe handles" test_process_close_tolerates_preclosed_pipe_handles;
  Test.case "Process try_wait observes stopped processes" test_try_wait_observes_stopped_processes;
  Test.case "Process Stopped then SIGCONT eventually reports final exit" test_stopped_then_continued_process_eventually_reports_final_exit;
  Test.case "Process kill rejects invalid signal numbers" test_kill_rejects_invalid_signal_numbers;
  Test.case "Process try_wait is stable after exit" test_try_wait_is_stable_after_exit;
  Test.case "Process kill after exit reports no-such-process" test_kill_after_exit_reports_no_such_process;
  Test.case "Process close is idempotent" test_process_close_is_idempotent;
  Test.case ~size:Test.Large "Process many sources report burst exit readiness" test_many_process_sources_report_burst_exit_readiness;
  Test.case ~size:Test.Large "Process many sources report burst signals" test_many_process_sources_report_burst_signals;
  Test.case ~size:Test.Large "Process repeated spawn and poll exit stays healthy" test_repeated_spawn_and_poll_exit_stays_healthy;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_process_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
