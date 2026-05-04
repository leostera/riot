open Std

type result = {
  status: Afl.status;
  stdout: string;
  stderr: string;
}

let max_capture_bytes = 262_144

let target_env = fun (target: Types.target) ->
  if List.exists (fun (name, _value) -> String.equal name "RIOT_SCHEDULERS") target.env then
    target.env
  else
    ("RIOT_SCHEDULERS", "1") :: target.env

let status_of_command = fun (output: Command.output) ->
  if Int.equal output.status 137 then
    Afl.Timed_out 9
  else if output.status >= 128 then
    Afl.Signaled (output.status - 128)
  else
    Afl.Exited output.status

let run = fun ~(target:Types.target) ~input_path ~timeout_ms ->
  let cmd =
    Command.make
      target.program
      ~args:(target.args ~input_path)
      ~env:(target_env target)
      ?cwd:(Option.map target.cwd ~fn:Path.to_string)
  in
  match Command.output
    ~timeout:(Time.Duration.from_millis timeout_ms)
    ~max_output_bytes:max_capture_bytes
    cmd with
  | Ok output ->
      Ok { status = status_of_command output; stdout = output.stdout; stderr = output.stderr }
  | Error (Command.SystemError message) -> Error (Error.Runtime_error message)
