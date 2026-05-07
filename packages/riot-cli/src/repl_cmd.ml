open Std

let command =
  let open ArgParser in
  command "repl"
  |> about "Start the Riot REPL"

let run = fun ~request _matches ->
  match Riot_repl.run request with
  | Ok () -> Ok ()
  | Error message ->
      eprintln ("\027[1;31mError\027[0m: " ^ message);
      Error (Failure message)
