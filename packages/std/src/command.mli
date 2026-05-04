(* FIXME: move this module into Std.Process.Command *)
(**
   Process spawning and management.

   This module provides a safe, composable API for spawning and managing
   external processes. Similar to Rust's `std::process::Command`.

   ## Examples

   Basic command execution:

   ```ocaml open Std

   (* Run a simple command *) let result = Command.make "ls" ~args:["-la"] |>
   Command.output in

   match result with | Ok output -> println "Files:\n%s" output.stdout; println
   "Exit code: %d" output.status | Error (SystemError msg) -> println "Failed:
   %s" msg

   (* Check exit status only *) match Command.make "test"
   ~args:["-f"; "file.txt"] |> Command.status with | Ok 0 -> println "File
   exists" | Ok _ -> println "File not found" | Error _ -> println "Command
   failed" ```

   ## Command Building

   Commands are built using a builder pattern with optional parameters:

   ```ocaml let cmd = Command.make "npm" ~args:["install"; "--production"]
   ~cwd:(Path.v "/app") ~env:[("NODE_ENV", "production")] ```

   ## Error Handling

   All command execution returns a `Result` type. Commands that exit with
   non-zero status are NOT considered errors - only system-level failures
   (command not found, permission denied, etc.) return `Error`.
*)
open Global

(**
   Process exit status code.

   By convention:
   - 0 indicates success
   - Non-zero indicates failure
   - Specific codes may have special meanings per command
*)
type status = int
(** Output from a completed process including streams and exit status *)
type output = {
  stdout: string;
  (** Standard output captured as string *)
  stderr: string;
  (** Standard error captured as string *)
  status: status;
  (** Exit status code *)
}
(** The type of a command configuration, ready to be executed *)
type t
(**
   System-level errors when spawning or running commands.

   Note: A command exiting with non-zero status is NOT an error. Only
   failures to start the process return `Error`.
*)
type error =
  | SystemError of string

(**
   Creates a new command configuration.

   ## Parameters

   - `cwd`: Working directory for the command (default: current directory)
   - `env`: Environment variables to set (adds to current environment)
   - `args`: Command-line arguments to pass
   - The command/program name to execute

   ## Examples

   ```ocaml (* Simple command *) let ls = Command.make "ls" in

   (* With arguments *) let grep = Command.make "grep"
   ~args:["-r"; "TODO"; "."] in

   (* With working directory *) let npm = Command.make "npm" ~args:["install"]
   ~cwd:(Path.v "/project") in

   (* With environment *) let build = Command.make "cargo"
   ~args:["build"; "--release"] ~env:[("RUST_BACKTRACE", "1")] in

   (* Complex example *) let deploy = Command.make "kubectl"
   ~args:["apply"; "-f"; "deploy.yaml"] ~cwd:(Path.v "/k8s")
   ~env:[("KUBECONFIG", "/home/user/.kube/prod")] ```

   ## Path Resolution

   The command is resolved using the system's PATH environment variable unless
   an absolute path is provided.
*)
val make: ?cwd:string -> ?env:(string * string) list -> ?args:string list -> string -> t

(**
   Render a command as a shell-style string for logging and debugging.

   This is intended for observability only. Execution still goes through the
   structured command value and does not invoke a shell.
*)
val to_string: t -> string

(**
   Executes command and captures its output.

   Runs the command as a child process, waits for completion, and returns
   stdout, stderr, and exit status.

   When `timeout` is provided, the child is sent SIGKILL after the duration
   elapses and the returned status is 137. When `max_output_bytes` is provided,
   captured stdout and stderr are each truncated to that many bytes while the
   child streams are still drained.

   ## Examples

   ```ocaml (* Capture command output *) match Command.make "git"
   ~args:["status"] |> Command.output with | Ok out when out.status = 0 ->
   println "Git status:\n%s" out.stdout | Ok out -> Printf.eprintf "Git failed
   with code %d:\n%s" out.status out.stderr | Error (SystemError e) ->
   Printf.eprintf "Failed to run git: %s\n" e

   (* Parse JSON output *) let get_docker_images () = Command.make "docker"
   ~args:["images"; "--format"; "json"] |> Command.output |> Result.and_then
   (fun out -> if out.status = 0 then Data.Json.parse out.stdout else Error
   (SystemError out.stderr)) ```

   ## Output Limits

   By default, both stdout and stderr are fully captured in memory. For commands
   that produce large output, consider `max_output_bytes`, pipes, or temporary
   files.

   ## Character Encoding

   Output is expected to be UTF-8. Invalid UTF-8 bytes may be replaced with
   replacement characters.
*)
val output:
  ?on_stdout_line:(string -> unit) ->
  ?on_idle:(Time.Duration.t -> unit) ->
  ?idle_interval:Time.Duration.t ->
  ?max_output_bytes:int ->
  ?timeout:Time.Duration.t ->
  t ->
  (output, error) result

(**
   Executes command and returns only its exit status.

   Runs the command as a child process without capturing output. Stdout and
   stderr are inherited from the parent process.

   ## Examples

   ```ocaml (* Check if command succeeds *) match Command.make "make"
   ~args:["test"] |> Command.status with | Ok 0 -> println "Tests passed!" | Ok
   n -> Printf.eprintf "Tests failed with code %d\n" n | Error (SystemError e)
   -> Printf.eprintf "Cannot run tests: %s\n" e

   (* Use as a condition *) let has_git () = Command.make "git"
   ~args:["--version"] |> Command.status |> Result.map (fun s -> s = 0) |>
   Result.unwrap_or ~default:false

   (* Run interactive commands *) let edit_file path = Command.make
   (Sys.getenv_opt "EDITOR" |> Option.unwrap_or ~default:"vi")
   ~args:[Path.to_string path] |> Command.status ```

   ## Use Cases

   Prefer `status` over `output` when:
   - You only care about success/failure
   - The command produces large output you don't need
   - The command is interactive
   - You want output to appear in real-time

   ## See Also

   - `output` - When you need to capture stdout/stderr
*)
val status: t -> (status, error) result
