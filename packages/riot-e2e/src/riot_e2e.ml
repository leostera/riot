open Std
open Std.Result.Syntax

module Test = Std.Test

type command_output = Command.output

let command_error_message = fun (Command.SystemError message) -> message

let with_tempdir_result = fun ?prefix fn ->
  match Fs.with_tempdir ?prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let render_output = fun (output: command_output) ->
  let stdout =
    if String.equal output.stdout "" then
      "<empty>"
    else
      output.stdout
  in
  let stderr =
    if String.equal output.stderr "" then
      "<empty>"
    else
      output.stderr
  in
  "status=" ^ Int.to_string output.status ^ ", stdout=" ^ stdout ^ ", stderr=" ^ stderr

let run_binary = fun ?cwd ?(env = []) binary_path args ->
  let cwd = Option.map cwd ~fn:Path.to_string in
  Command.make (Path.to_string binary_path) ?cwd ~env ~args
  |> Command.output
  |> Result.map_err ~fn:command_error_message

let run_binary_with_stdin = fun ?cwd ?(env = []) binary_path ~stdin args ->
  with_tempdir_result
    ~prefix:"riot_e2e_stdin_"
    (fun tempdir ->
      let stdin_path = Path.(tempdir / Path.v "stdin") in
      let* () =
        Fs.write stdin stdin_path
        |> Result.map_err ~fn:IO.error_message
      in
      let cwd = Option.map cwd ~fn:Path.to_string in
      let shell = "input=$1; shift; cat \"$input\" | exec \"$@\"" in
      Command.make
        "/bin/sh"
        ?cwd
        ~env
        ~args:([
          "-c";
          shell;
          "riot-e2e-stdin";
          Path.to_string stdin_path;
          Path.to_string binary_path;
        ]
        @ args)
      |> Command.output
      |> Result.map_err ~fn:command_error_message)

let run_riot = fun ctx ?cwd ?(env = []) args ->
  let* riot_binary_path = Test.Context.require_binary ctx "riot" in
  run_binary ?cwd ~env riot_binary_path args

let run_riot_with_stdin = fun ctx ?cwd ?(env = []) ~stdin args ->
  let* riot_binary_path = Test.Context.require_binary ctx "riot" in
  run_binary_with_stdin ?cwd ~env riot_binary_path ~stdin args

let expect_success = fun ~cmd (output: command_output) ->
  if Int.equal output.status 0 then
    Ok output
  else
    Error (cmd ^ " failed: " ^ render_output output)

let expect_failure_contains = fun ~cmd ~needle (output: command_output) ->
  let text = output.stdout ^ output.stderr in
  if Int.equal output.status 0 then
    Error (cmd ^ " unexpectedly succeeded: " ^ render_output output)
  else if String.contains text needle then
    Ok output
  else
    Error (cmd ^ " failed without expected text `" ^ needle ^ "`: " ^ render_output output)

let assert_exists = fun path ->
  match Fs.exists path with
  | Ok true -> Ok ()
  | Ok false -> Error ("expected path to exist: " ^ Path.to_string path)
  | Error err -> Error ("failed to stat path " ^ Path.to_string path ^ ": " ^ IO.error_message err)

let assert_contains = fun path needle ->
  let* content =
    Fs.read_to_string path
    |> Result.map_err ~fn:IO.error_message
  in
  if String.contains content needle then
    Ok ()
  else
    Error ("expected " ^ Path.to_string path ^ " to contain `" ^ needle ^ "`")

let assert_not_contains = fun path needle ->
  let* content =
    Fs.read_to_string path
    |> Result.map_err ~fn:IO.error_message
  in
  if String.contains content needle then
    Error ("expected " ^ Path.to_string path ^ " not to contain `" ^ needle ^ "`")
  else
    Ok ()

let assert_executable = fun path ->
  let* metadata =
    Fs.metadata path
    |> Result.map_err ~fn:IO.error_message
  in
  let permissions = Fs.Metadata.permissions metadata in
  if Fs.Permissions.user_execute permissions then
    Ok ()
  else
    Error ("expected path to be executable: " ^ Path.to_string path)

let assert_output_contains = fun ~cmd (output: command_output) needle ->
  let text = output.stdout ^ output.stderr in
  if String.contains text needle then
    Ok ()
  else
    Error (cmd ^ " output did not contain `" ^ needle ^ "`: " ^ render_output output)

let assert_output_not_contains = fun ~cmd (output: command_output) needle ->
  let text = output.stdout ^ output.stderr in
  if String.contains text needle then
    Error (cmd ^ " output unexpectedly contained `" ^ needle ^ "`: " ^ render_output output)
  else
    Ok ()

let write_text = fun path content ->
  let* () =
    match Path.parent path with
    | None -> Ok ()
    | Some parent ->
        Fs.create_dir_all parent
        |> Result.map_err ~fn:IO.error_message
  in
  Fs.write content path
  |> Result.map_err ~fn:IO.error_message

let with_initialized_workspace = fun ?(init_args = []) ctx workspace_name fn ->
  with_tempdir_result
    ~prefix:"riot_e2e_init_"
    (fun root ->
      let workspace_root = Path.(root / Path.v workspace_name) in
      let* init_output = run_riot ctx ~cwd:root ("init" :: workspace_name :: init_args) in
      let* _ = expect_success ~cmd:"riot init" init_output in
      fn workspace_root)
