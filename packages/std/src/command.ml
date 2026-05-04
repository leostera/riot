open Global
open Collections

type error =
  | SystemError of string

module Stdio = struct
  type t =
    | Null
    | Inherit
    | Pipe
    | File of Fs.File.t

  let null = fun () -> Null

  let inherit_ = fun () -> Inherit

  let pipe = fun () -> Pipe

  let from_file = fun file -> File file
end

type status = int

type output = {
  stdout: string;
  stderr: string;
  status: status;
}

type state =
  | Pending
  | Running of {
      proc: Kernel.Process.t;
      stdout: Fs.File.t option;
      stderr: Fs.File.t option;
    }
  | Exited of output

type t = {
  cmd: string;
  args: string list;
  env: (string * string) list;
  cwd: string option;
  mutable state: state;
}

let pipe_read_retry_interval = Time.Duration.from_millis 1

let pipe_drain_retries_after_process_exit = 50

let is_file_would_block = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Fs.File.System error -> Kernel.SystemError.would_block error
  | Kernel.Fs.File.InvalidSlice _ -> false

(** Command - OS process spawning and management *)
let make = fun ?cwd ?(env = []) ?(args = []) cmd ->
  {
    cmd;
    args;
    env;
    cwd;
    state = Pending;
  }

let is_shell_safe_char = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '-'
  | '.'
  | '/'
  | ':'
  | '+'
  | '='
  | ','
  | '@'
  | '%' -> true
  | _ -> false

let shell_quote = fun value ->
  if String.equal value "" then
    "''"
  else if String.for_all ~fn:is_shell_safe_char value then
    value
  else
    "'" ^ String.concat "'\"'\"'" (String.split ~by:"'" value) ^ "'"

let to_string = fun t ->
  let command = String.concat " " (List.map (t.cmd :: t.args) ~fn:shell_quote) in
  let command =
    match t.env with
    | [] -> command
    | env ->
        String.concat " " (List.map env ~fn:(fun (key, value) -> key ^ "=" ^ shell_quote value))
        ^ " "
        ^ command
  in
  match t.cwd with
  | Some cwd -> "cd " ^ shell_quote cwd ^ " && " ^ command
  | None -> command

let default_idle_interval = Time.Duration.from_secs 1

let stdio_of_config = fun stdin stdout stderr ->
  let stdin_config =
    match stdin with
    | Stdio.Null -> Kernel.Process.Stdin.Null
    | Stdio.Inherit -> Kernel.Process.Stdin.Inherit
    | Stdio.Pipe -> Kernel.Process.Stdin.Pipe
    | Stdio.File file -> Kernel.Process.Stdin.File file
  in
  let stdout_config =
    match stdout with
    | Stdio.Null -> Kernel.Process.Stdout.Null
    | Stdio.Inherit -> Kernel.Process.Stdout.Inherit
    | Stdio.Pipe -> Kernel.Process.Stdout.Pipe
    | Stdio.File file -> Kernel.Process.Stdout.File file
  in
  let stderr_config =
    match stderr with
    | Stdio.Null -> Kernel.Process.Stderr.Null
    | Stdio.Inherit -> Kernel.Process.Stderr.Inherit
    | Stdio.Pipe -> Kernel.Process.Stderr.Pipe
    | Stdio.File file -> Kernel.Process.Stderr.File file
  in
  Kernel.Process.{ stdin = stdin_config; stdout = stdout_config; stderr = stderr_config }

let kernel_status_code = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Process.Running -> 0
  | Kernel.Process.Exited code -> code
  | Kernel.Process.Signaled n -> 128 + n
  | Kernel.Process.Stopped n -> 128 + n

let process_exit_poll_interval = Time.Duration.from_millis 1

let blocking_sleep = fun duration -> Kernel.Thread.sleep_ns (Time.Duration.to_nanos duration)

let timeout_status_code = 137

let timeout_elapsed = fun started timeout ->
  match timeout with
  | None -> false
  | Some timeout -> Time.Duration.compare (Time.Instant.elapsed started) timeout != Order.LT

let maybe_kill_for_timeout = fun ~started ~timeout ~timed_out proc ->
  if (not !timed_out) && timeout_elapsed started timeout then (
    timed_out := true;
    let _ = Kernel.Process.kill proc ~signal:9 in
    ()
  )

let wait_for_exit = fun ?timeout proc ->
  let started = Time.Instant.now () in
  let timed_out = ref false in
  let rec loop () =
    match Kernel.Process.try_wait proc with
    | Error err -> Error (SystemError (Kernel.Process.error_to_string err))
    | Ok None ->
        maybe_kill_for_timeout ~started ~timeout ~timed_out proc;
        blocking_sleep process_exit_poll_interval;
        loop ()
    | Ok (Some status) -> Ok (status, !timed_out)
  in
  loop ()

let cwd_path = fun cwd ->
  match cwd with
  | None -> Ok None
  | Some cwd -> Ok (Some (Kernel.Path.from_string cwd))

let fs_error = fun action err -> Error (SystemError (action ^ ": " ^ IO.error_message err))

let fs_file_error = fun action err ->
  Error (SystemError (action ^ ": " ^ Fs.File.error_to_string err))

let output_to_temp_files = fun ?timeout t ->
  match Fs.with_tempdir
    ~prefix:"std-command-"
    (fun tempdir ->
      let stdout_path = Path.(tempdir / Path.v "stdout") in
      let stderr_path = Path.(tempdir / Path.v "stderr") in
      match (Fs.File.create stdout_path, Fs.File.create stderr_path) with
      | (Error err, _) -> fs_file_error "failed to create stdout capture file" err
      | (_, Error err) -> fs_file_error "failed to create stderr capture file" err
      | (Ok stdout_file, Ok stderr_file) -> (
          let stdio =
            stdio_of_config Stdio.Null (Stdio.File stdout_file) (Stdio.File stderr_file)
          in
          match cwd_path t.cwd with
          | Error _ as err ->
              let _ = Fs.File.close stdout_file in
              let _ = Fs.File.close stderr_file in
              err
          | Ok current_dir -> (
              match Kernel.Process.spawn
                ~program:t.cmd
                ~args:(Array.from_list t.args)
                ~env:(Array.from_list t.env)
                ?current_dir
                ~stdio
                () with
              | Error err ->
                  let _ = Fs.File.close stdout_file in
                  let _ = Fs.File.close stderr_file in
                  Error (SystemError (Kernel.Process.error_to_string err))
              | Ok proc -> (
                  let _ = Fs.File.close stdout_file in
                  let _ = Fs.File.close stderr_file in
                  t.state <- Running { proc; stdout = None; stderr = None };
                  match wait_for_exit ?timeout proc with
                  | Error _ as err ->
                      let _ = Kernel.Process.close proc in
                      err
                  | Ok (exit_status, timed_out) -> (
                      let _ = Kernel.Process.close proc in
                      match (Fs.read stdout_path, Fs.read stderr_path) with
                      | (Error err, _) -> fs_error "failed to read stdout capture file" err
                      | (_, Error err) -> fs_error "failed to read stderr capture file" err
                      | (Ok stdout, Ok stderr) ->
                          let status =
                            if timed_out then
                              timeout_status_code
                            else
                              kernel_status_code exit_status
                          in
                          let result = { stdout; stderr; status } in
                          t.state <- Exited result;
                          Ok result
                    )
                )
            )
        )) with
  | Error err -> fs_error "failed to create command capture tempdir" err
  | Ok result -> result

let command_pipe_chunk_size = 4_096

type pipe_read =
  | Pipe_closed
  | Pipe_read of int
  | Pipe_would_block

let read_pipe_once = fun file buffer ->
  match Kernel.Fs.File.read file buffer ~pos:0 ~len:command_pipe_chunk_size with
  | Ok 0 -> Ok Pipe_closed
  | Ok bytes_read -> Ok (Pipe_read bytes_read)
  | Error err when is_file_would_block err -> Ok Pipe_would_block
  | Error err -> Error err

let append_limited_chunk = fun ?max_output_bytes builder buffer bytes_read ->
  let bytes_to_add =
    match max_output_bytes with
    | None -> bytes_read
    | Some max_output_bytes ->
        let remaining = max_output_bytes - StringBuilder.length builder in
        if remaining <= 0 then
          0
        else
          Int.min remaining bytes_read
  in
  if bytes_to_add > 0 then
    StringBuilder.add_subbytes builder buffer 0 bytes_to_add

let append_stdout_chunk = fun
  ?max_output_bytes ~on_stdout_line ~stdout_buffer ~line_buffer buffer bytes_read ->
  append_limited_chunk ?max_output_bytes stdout_buffer buffer bytes_read;
  Option.for_each
    on_stdout_line
    ~fn:(fun on_stdout_line ->
      for i = 0 to bytes_read - 1 do
        let ch = Kernel.Bytes.get_unchecked buffer ~at:i in
        StringBuilder.add_char line_buffer ch;
        if ch = '\n' then (
          on_stdout_line (StringBuilder.contents line_buffer);
          StringBuilder.clear line_buffer
        )
      done)

let flush_stdout_line = fun ~on_stdout_line ~line_buffer ->
  Option.for_each
    on_stdout_line
    ~fn:(fun on_stdout_line ->
      let line = StringBuilder.contents line_buffer in
      if not (String.equal line "") then (
        on_stdout_line line;
        StringBuilder.clear line_buffer
      ))

let output_with_pipes = fun
  ?max_output_bytes ?timeout ~on_stdout_line ~on_idle ~idle_interval t proc stdout_fd stderr_fd ->
  let stdout_buffer = StringBuilder.create ~size:4_096 in
  let stderr_buffer = StringBuilder.create ~size:4_096 in
  let stdout_line_buffer = StringBuilder.create ~size:256 in
  let stdout_chunk = Kernel.Bytes.create ~size:command_pipe_chunk_size in
  let stderr_chunk = Kernel.Bytes.create ~size:command_pipe_chunk_size in
  let stdout_closed = ref false in
  let stderr_closed = ref false in
  let process_status = ref None in
  let drain_retries = ref 0 in
  let started = Time.Instant.now () in
  let timed_out = ref false in
  let last_idle_us = ref 0 in
  let finish_error err =
    let _ = Kernel.Process.close proc in
    err
  in
  let read_stdout () =
    if !stdout_closed then
      Ok false
    else
      match read_pipe_once stdout_fd stdout_chunk with
      | Error err -> Error err
      | Ok Pipe_closed ->
          stdout_closed := true;
          Ok false
      | Ok Pipe_would_block -> Ok false
      | Ok (Pipe_read bytes_read) ->
          append_stdout_chunk
            ?max_output_bytes
            ~on_stdout_line
            ~stdout_buffer
            ~line_buffer:stdout_line_buffer
            stdout_chunk
            bytes_read;
          Ok true
  in
  let read_stderr () =
    if !stderr_closed then
      Ok false
    else
      match read_pipe_once stderr_fd stderr_chunk with
      | Error err -> Error err
      | Ok Pipe_closed ->
          stderr_closed := true;
          Ok false
      | Ok Pipe_would_block -> Ok false
      | Ok (Pipe_read bytes_read) ->
          append_limited_chunk ?max_output_bytes stderr_buffer stderr_chunk bytes_read;
          Ok true
  in
  let rec drain read_any =
    match (read_stdout (), read_stderr ()) with
    | (Error err, _)
    | (_, Error err) -> Error err
    | (Ok stdout_read, Ok stderr_read) ->
        if stdout_read || stderr_read then (
          maybe_kill_for_timeout ~started ~timeout ~timed_out proc;
          drain true
        ) else
          Ok read_any
  in
  let observe_process () =
    match !process_status with
    | Some _ -> Ok ()
    | None -> (
        match Kernel.Process.try_wait proc with
        | Error err -> Error (SystemError (Kernel.Process.error_to_string err))
        | Ok None -> Ok ()
        | Ok (Some status) ->
            process_status := Some status;
            Ok ()
      )
  in
  let maybe_idle data_read =
    if not data_read then
      Option.for_each
        on_idle
        ~fn:(fun on_idle ->
          let elapsed = Time.Instant.elapsed started in
          let elapsed_us = Time.Duration.to_micros elapsed in
          let idle_interval_us = Time.Duration.to_micros idle_interval in
          if elapsed_us - !last_idle_us >= idle_interval_us then (
            last_idle_us := elapsed_us;
            on_idle elapsed
          ))
  in
  let rec loop () =
    match drain false with
    | Error err ->
        finish_error
          (Error (SystemError ("Failed to read from command '"
          ^ t.cmd
          ^ "': "
          ^ Fs.File.error_to_string err)))
    | Ok data_read -> (
        match observe_process () with
        | Error _ as err -> finish_error err
        | Ok () ->
            maybe_kill_for_timeout ~started ~timeout ~timed_out proc;
            let process_done = Option.is_some !process_status in
            if data_read then
              drain_retries := 0
            else if process_done then
              drain_retries := !drain_retries + 1;
            let readers_closed = !stdout_closed && !stderr_closed in
            if
              process_done
              && (readers_closed || !drain_retries >= pipe_drain_retries_after_process_exit)
            then (
              flush_stdout_line ~on_stdout_line ~line_buffer:stdout_line_buffer;
              let _ = Kernel.Process.close proc in
              let exit_status = Option.unwrap !process_status in
              let result = {
                status =
                  if !timed_out then
                    timeout_status_code
                  else
                    kernel_status_code exit_status;
                stdout = StringBuilder.contents stdout_buffer;
                stderr = StringBuilder.contents stderr_buffer;
              }
              in
              t.state <- Exited result;
              Ok result
            ) else (
              maybe_idle data_read;
              if not data_read then
                blocking_sleep pipe_read_retry_interval;
              loop ()
            )
      )
  in
  loop ()

let output = fun ?on_stdout_line ?on_idle ?idle_interval ?max_output_bytes ?timeout t ->
  match t.state with
  | Exited out -> Ok out
  | Running _ -> Error (SystemError "Command is already running")
  | Pending when Option.is_none on_stdout_line
  && Option.is_none on_idle
  && Option.is_none max_output_bytes ->
      let _ = idle_interval in
      output_to_temp_files ?timeout t
  | Pending -> (
      (* Build stdio config to capture stdout and stderr *)
      let stdio = stdio_of_config Stdio.Null Stdio.Pipe Stdio.Pipe in
      match cwd_path t.cwd with
      | Error _ as err -> err
      | Ok current_dir -> (
          (* Spawn the process *)
          match Kernel.Process.spawn
            ~program:t.cmd
            ~args:(Array.from_list t.args)
            ~env:(Array.from_list t.env)
            ?current_dir
            ~stdio
            () with
          | Error err -> Error (SystemError (Kernel.Process.error_to_string err))
          | Ok proc ->
              (* Get piped file descriptors *)
              let stdout_fd =
                Kernel.Process.stdout proc
                |> Option.unwrap
              in
              let stderr_fd =
                Kernel.Process.stderr proc
                |> Option.unwrap
              in
              (* Update state to Running *)
              t.state <- Running { proc; stdout = Some stdout_fd; stderr = Some stderr_fd };
              output_with_pipes
                ?max_output_bytes
                ?timeout
                ~on_stdout_line
                ~on_idle
                ~idle_interval:(Option.unwrap_or ~default:default_idle_interval idle_interval)
                t
                proc
                stdout_fd
                stderr_fd
        )
    )

let status = fun t ->
  match t.state with
  | Exited out -> Ok out.status
  | Running _ -> Error (SystemError "Command is already running")
  | Pending -> (
      (* Build stdio config to inherit stdin, stdout and stderr (don't capture) *)
      let stdio = stdio_of_config Stdio.Inherit Stdio.Inherit Stdio.Inherit in
      match cwd_path t.cwd with
      | Error _ as err -> err
      | Ok current_dir -> (
          (* Spawn the process *)
          match Kernel.Process.spawn
            ~program:t.cmd
            ~args:(Array.from_list t.args)
            ~env:(Array.from_list t.env)
            ?current_dir
            ~stdio
            () with
          | Error err -> Error (SystemError (Kernel.Process.error_to_string err))
          | Ok proc ->
              match wait_for_exit proc with
              | Error _ as err -> err
              | Ok (exit_status, _timed_out) ->
                  let _ = Kernel.Process.close proc in
                  let status_code = kernel_status_code exit_status in
                  t.state <- Exited { status = status_code; stdout = ""; stderr = "" };
                  Ok status_code
        )
    )
