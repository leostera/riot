open Std

(** Main entry point for riot *)
let () =
  Std.Log.(set_level Info);
  (* Ignore SIGPIPE to prevent exit code 141 when output is piped *)
  Kernel.System.(set_signal sigpipe Signal_ignore);
  Runtime.run ~main:Riot_cli.Cli.main ~args:Env.args ()
